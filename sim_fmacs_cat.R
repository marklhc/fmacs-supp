library(SimDesign)
library(lavaan)
library(pinsearch)

# Define conditions
design <- createDesign(
    n = c(100, 250, 1000)
)
set.seed(1855)
# Helper
get_ucov <- function(p, scale = sqrt(0.1), n = 5) {
    W <- matrix(rnorm(p * n), nrow = n)
    WtW <- crossprod(W)
    D <- diag(1 / sqrt(diag(WtW))) * scale
    D %*% WtW %*% D
}
# Fixed objects
fixed <- list(
    p = 6,
    lambda = c(.3, .7, .4, .5, .6, .4) + .3,
    dlambda = list(
        c(0, 0, 0, 0, 0, 0),
        c(.2, 0, 0, 0, 0, 0),
        c(.4, 0, 0, 0, 0, 0),
        c(.6, 0, 0, 0, 0, 0)
    ),
    tau = c(0.5, -0.5, 1, 0, -2, 0),
    dtau = list(
        c(0, 0, 0, 0, 0, 0),
        c(0, -.1, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 0),
        c(0, .1, 0, 0, 0, 0)
    ),
    alpha = c(0, -0.25, 0.25, 0.5),
    psi = c(1, 0.85, 1.15, 0.7),
    # ucov = replicate(4, get_ucov(6), simplify = FALSE),
    ucov = replicate(4, diag(.1, 6), simplify = FALSE),
    ninv_ind = c(1, 2)
)
# lavaan syntax
fixed$mod <- paste(
    "f =~",
    paste0("y", seq_len(fixed$p), collapse = " + "), "\n",
    paste0("y", seq_len(fixed$p), "~~ 1 * y", seq_len(fixed$p),
           collapse = "\n")
)
# Compute implied means and covariances
fixed <- within(fixed, {
    lambdag <- lapply(dlambda, FUN = \(x) x + lambda)
    taug <- lapply(dtau, FUN = \(x) x + tau)
    covy <- mapply(\(lam, psi) tcrossprod(lam) * psi + diag(length(lam)),
                   lam = lambdag, psi = psi,
                   SIMPLIFY = FALSE)
    meany <- mapply(\(lam, al) lam * al,
                    lam = lambdag, al = alpha,
                    SIMPLIFY = FALSE)
})
# Population effect size
fixed$fmacs_pop <- local({
    pooled_sd <- lapply(1:2, FUN = \(j) {
        mapply(pinsearch:::var_from_thres,
            thres = lapply(fixed$taug, FUN = \(x) x[j]),
            mean = lapply(fixed$meany, FUN = \(x) x[j]),
            sd = lapply(fixed$covy, FUN = \(x) sqrt(x[j, j]))
        ) |>
            mean() |>
            sqrt()
    })
    fmacs_ordered(
        thresholds = do.call(rbind, fixed$taug) |>
            `colnames<-`(1:6),
        loadings = do.call(rbind, fixed$lambdag),
        latent_mean = 0,
        latent_sd = 1,
        pooled_item_sd = unlist(pooled_sd)
    )[1:2]
})
# Function for data generation
# sim_y <- function(n, lambda, nu, alpha, psi, Theta) {
#     covy <- tcrossprod(lambda) * psi + Theta
#     meany <- nu + lambda * alpha
#     MASS::mvrnorm(n, mu = meany, Sigma = covy)
# }
generate <- function(condition, fixed_objects) {
    ylist <- lapply(seq_along(fixed_objects$covy),
        FUN = function(g) {
            yg <- MASS::mvrnorm(
                condition$n,
                mu = fixed_objects$meany[[g]],
                Sigma = fixed_objects$covy[[g]]
            )
            yg <- vapply(seq_len(ncol(yg)), FUN = \(j) {
                findInterval(yg[, j], fixed_objects$tau[j])
            }, FUN.VALUE = integer(nrow(yg)))
            colnames(yg) <- paste0("y", seq_len(fixed_objects$p))
            cbind(yg, group  = g)
        })
    do.call(rbind, ylist)
}
sim1 <- generate(design[1, ], fixed_objects = fixed)

# Analysis
analyze <- function(condition, dat, fixed_objects) {
    # Define lavaan syntax
    pinv_fit <- cfa(
        fixed_objects$mod,
        data = dat,
        group = "group", std.lv = TRUE,
        ordered = TRUE,
        group.equal = c("loadings", "thresholds"),
        group.partial = c(
            paste0("f=~y", fixed_objects$ninv_ind[1]),
            paste0("y", fixed_objects$ninv_ind, "|t1")
        ),
        parameterization = "theta"
    )
    stopifnot(
        "Model not converged" =
            lavInspect(pinv_fit, what = "converged")
    )
    as.vector(pinsearch::pin_effsize(pinv_fit))
}

analyze_bc <- function(condition, dat, fixed_objects) {
    # Define lavaan syntax
    pinv_fit <- cfa(
        fixed_objects$mod,
        data = dat,
        group = "group", std.lv = TRUE,
        ordered = TRUE,
        group.equal = c("loadings", "thresholds"),
        group.partial = c(
            paste0("f=~y", fixed_objects$ninv_ind[1]),
            paste0("y", fixed_objects$ninv_ind, "|t1")
        ),
        parameterization = "theta"
    )
    stopifnot(
        "Model not converged" =
            lavInspect(pinv_fit, what = "converged")
    )
    f_orig <- as.vector(pinsearch::pin_effsize(pinv_fit))
    f_boot <- lavaan::bootstrapLavaan(pinv_fit,
        R = 249,
        FUN = pinsearch::pin_effsize
    )
    pmax(0, 2 * f_orig - colMeans(f_boot, na.rm = TRUE))
}

# Evaluate/Summarize
evaluate <- function(condition, results, fixed_objects) {
    c(
        bias = colMeans(results) - fixed_objects$fmacs_pop,
        robust_bias = apply(results, 2, mean, trim = .1) -
            fixed_objects$fmacs_pop,
        emp_sd = apply(results, 2, sd),
        emp_mad = apply(results, 2, mad)
    )
}
out <- runSimulation(design,
    replications = 1000,
    parallel = TRUE,
    generate = generate,
    analyse = list(naive = analyze,
                   bc_boot = analyze_bc),
                   # bc_form = analyze_bc2),
    summarise = evaluate,
    filename = "results-fmacs-cat",
    save_results = TRUE,
    packages = c("MASS", "lavaan", "pinsearch"),
    fixed_objects = fixed
)
