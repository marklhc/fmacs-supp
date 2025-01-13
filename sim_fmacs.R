library(SimDesign)
library(lavaan)
library(pinsearch)

# Define conditions
design <- createDesign(
    n = c(100, 250, 1000)
)

# Fixed objects
set.seed(1855)
# Helper
get_ucov <- function(p, scale = sqrt(0.1), n = 5) {
    W <- matrix(rnorm(p * n), nrow = n)
    WtW <- crossprod(W)
    D <- diag(1 / sqrt(diag(WtW))) * scale
    D %*% WtW %*% D
}
fixed <- list(
    p = 6,
    lambda = c(.3, .7, .4, .5, .6, .4) + .3,
    dlambda = list(
        c(0, 0, 0, 0, 0, 0),
        c(.2, 0, 0, 0, 0, 0),
        c(.4, 0, 0, 0, 0, 0),
        c(.6, 0, 0, 0, 0, 0)
    ),
    nu = c(2, 3, 1.5, 3.5, 2, 3),
    dnu = list(
        c(0, 0.1, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 0),
        c(0, -0.1, 0, 0, 0, 0),
        c(0, 0, 0, 0, 0, 0)
    ),
    alpha = c(0, -0.25, 0.25, 0.5),
    psi = c(1, 0.85, 1.15, 0.7),
    theta = c(1, 1.2, .8, .9, 1, 1) - .1,
    dtheta = matrix(
        runif(24, min = -0.2, max = 0.2),
        nrow = 4
    ),
    # ucov = replicate(4, get_ucov(6), simplify = FALSE),
    ucov = replicate(4, diag(.1, 6), simplify = FALSE),
    ninv_ind = c(1, 2)
)
# lavaan syntax
fixed$mod <- paste(
    "f =~",
    paste0("y", seq_len(fixed$p), collapse = " + ")
)
# Compute implied means and covariances
fixed <- within(fixed, {
    lambdag <- lapply(dlambda, FUN = \(x) x + lambda)
    nug <- lapply(dnu, FUN = \(x) x + nu)
    Thetag <- lapply(seq_along(ucov),
        FUN = function(g) {
            diag(theta + dtheta[g, ]) + ucov[[g]]
        })
    covy <- mapply(\(lam, psi, th) tcrossprod(lam) * psi + th,
                   lam = lambdag, psi = psi, th = Thetag,
                   SIMPLIFY = FALSE)
    meany <- mapply(\(lam, al, nu) nu + lam * al,
                    lam = lambdag, al = alpha, nu = nug,
                    SIMPLIFY = FALSE)
})

# Population reliability
fixed <- within(fixed, {
    comprel <- mapply(
        \(lam, psi, th) {
            sum(lam)^2 * psi / (sum(lam)^2 * psi + sum(th))
        },
        lam = lambdag, psi = psi, th = Thetag,
        SIMPLIFY = FALSE
    )
})

# Population effect size
fixed$fmacs_pop <- local({
    pooled_sd <- lapply(fixed$covy, FUN = \(x) diag(x)) |>
        do.call(what = rbind) |>
        colMeans() |>
        sqrt()

    fmacs(
        intercepts = do.call(rbind, fixed$nug),
        loadings = do.call(rbind, fixed$lambdag),
        latent_mean = 0,
        latent_sd = 1,
        pooled_item_sd = pooled_sd
    )
})[1:2]

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
        group.equal = c("loadings", "intercepts"),
        group.partial = c(
            paste0("f=~y", fixed_objects$ninv_ind[1]),
            paste0("y", fixed_objects$ninv_ind, "~1")
        )
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
        group.equal = c("loadings", "intercepts"),
        group.partial = c(
            paste0("f=~y", fixed_objects$ninv_ind[1]),
            paste0("y", fixed_objects$ninv_ind, "~1")
        )
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

analyze_bc2 <- function(condition, dat, fixed_objects) {
    # Define lavaan syntax
    pinv_fit <- cfa(
        fixed_objects$mod,
        data = dat,
        group = "group", std.lv = TRUE,
        group.equal = c("loadings", "intercepts"),
        group.partial = c(
            paste0("f=~y", fixed_objects$ninv_ind),
            paste0("y", fixed_objects$ninv_ind, "~1")
        )
    )
    f_orig <- pinsearch::pin_effsize(pinv_fit)
    ns <- lavInspect(pinv_fit, what = "nobs")
    ng <- length(ns)
    f2_bias <- (ng - 1) / ng * sum(1 / ns)
    sqrt(pmax(0, f_orig^2 - f2_bias))
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
    filename = "results-fmacs-int",
    save_results = TRUE,
    packages = c("MASS", "lavaan", "pinsearch"),
    fixed_objects = fixed
)
