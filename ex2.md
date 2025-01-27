# Example 2: Computing Effect Sizes for Noninvariance With Multiple
Grouping Variables

2025-01-26

## Load Data

``` r
# Data from https://osf.io/wxjsg
orig_dat <- haven::read_sav(
    here::here("LUI2018_class4mplus.sav")
)
dat <- orig_dat |>
    filter(eth %in% c(1, 2, 4), gender %in% 1:2) |>
    mutate( # across(class1:class15, .fns = as.numeric),
        across(c(eth, gender), .fns = as_factor),
        group = interaction(eth, gender)
    ) |>
    arrange(group)
# dat[,9:23] <- sapply(dat %>% select("class1":"class15"), FUN = function(x) ifelse(x > 3, 4, x))
# head(dat)
```

## Descriptive Statistics

``` r
dat[-8] |>
    datasummary(
        formula = (class1 + class2 + class3 + class4 + class5 +
            class6 + class7 + class8 + class9 + class10 +
            class11 + class12 + class13 + class14 + class15) ~
            eth * gender *
            (N + Mean + SD)
    )
```

|  | White/European American (e.g. German, Polish, Irish) / Male / N | White/European American (e.g. German, Polish, Irish) / Male / Mean | White/European American (e.g. German, Polish, Irish) / Male / SD | White/European American (e.g. German, Polish, Irish) / Female / N | White/European American (e.g. German, Polish, Irish) / Female / Mean | White/European American (e.g. German, Polish, Irish) / Female / SD | White/European American (e.g. German, Polish, Irish) / Other, please specify / N | White/European American (e.g. German, Polish, Irish) / Other, please specify / Mean | White/European American (e.g. German, Polish, Irish) / Other, please specify / SD | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Male / N | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Male / Mean | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Male / SD | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Female / N | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Female / Mean | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Female / SD | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Other, please specify / N | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Other, please specify / Mean | Asian/Asian American (e.g., Chinese, Filipino, Asian Indian) / Other, please specify / SD | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Male / N | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Male / Mean | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Male / SD | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Female / N | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Female / Mean | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Female / SD | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Other, please specify / N | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Other, please specify / Mean | Black/African American (e.g., Jamaican, Somalian, Afro-Caribbean) / Other, please specify / SD | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Male / N | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Male / Mean | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Male / SD | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Female / N | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Female / Mean | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Female / SD | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Other, please specify / N | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Other, please specify / Mean | Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban) / Other, please specify / SD |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1\. Parties with alcohol are an integral part of college life. | 162 | 3.80 | 1.23 | 356 | 3.52 | 1.23 | 0 |  |  | 86 | 3.07 | 1.35 | 141 | 2.91 | 1.41 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.55 | 1.40 | 116 | 3.47 | 1.32 | 0 |  |  |
| 2\. To become drunk is a college rite of passage. | 162 | 3.17 | 1.35 | 356 | 2.67 | 1.31 | 0 |  |  | 86 | 2.12 | 1.16 | 141 | 2.20 | 1.31 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.57 | 1.41 | 116 | 2.66 | 1.35 | 0 |  |  |
| 3\. I would prefer it if my college was not considered a party school. | 162 | 2.65 | 1.21 | 356 | 3.14 | 1.18 | 0 |  |  | 86 | 3.17 | 1.11 | 141 | 3.52 | 1.13 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.03 | 1.03 | 116 | 2.95 | 1.18 | 0 |  |  |
| 4\. The reward at the end of a hard week of studying should be a weekend of heavy drinking. | 162 | 2.98 | 1.28 | 356 | 2.35 | 1.25 | 0 |  |  | 86 | 2.01 | 1.18 | 141 | 1.76 | 1.02 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.57 | 1.26 | 116 | 2.25 | 1.23 | 0 |  |  |
| 5\. I think that the students who do not go out to parties or bars are not enjoying their college experience. | 162 | 2.65 | 1.28 | 356 | 1.99 | 1.13 | 0 |  |  | 86 | 2.06 | 1.16 | 141 | 1.83 | 1.13 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.30 | 1.18 | 116 | 2.43 | 1.27 | 0 |  |  |
| 6\. Missing class due to a hangover is part of being a true college student. | 162 | 1.96 | 1.09 | 356 | 1.74 | 0.96 | 0 |  |  | 86 | 1.60 | 0.90 | 141 | 1.53 | 0.82 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 1.80 | 1.12 | 116 | 1.72 | 0.98 | 0 |  |  |
| 7\. A college party is not a true college party without alcohol. | 162 | 3.57 | 1.29 | 356 | 2.99 | 1.36 | 0 |  |  | 86 | 2.65 | 1.38 | 141 | 2.38 | 1.43 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.28 | 1.38 | 116 | 3.25 | 1.30 | 0 |  |  |
| 8\. Alcohol is not an important aspect of college life. | 162 | 2.62 | 1.22 | 356 | 2.97 | 1.25 | 0 |  |  | 86 | 3.15 | 1.42 | 141 | 3.52 | 1.34 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.86 | 1.32 | 116 | 3.04 | 1.24 | 0 |  |  |
| 9\. Attending parties in college is the easiest way to make friends. | 162 | 3.34 | 1.23 | 356 | 2.79 | 1.22 | 0 |  |  | 86 | 2.93 | 1.15 | 141 | 2.53 | 1.28 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.18 | 1.23 | 116 | 3.09 | 1.21 | 0 |  |  |
| 10\. Drinking alcohol is a social event in which every college student partakes. | 162 | 2.86 | 1.30 | 356 | 2.38 | 1.24 | 0 |  |  | 86 | 2.53 | 1.20 | 141 | 2.28 | 1.21 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.86 | 1.28 | 116 | 2.72 | 1.32 | 0 |  |  |
| 11\. College is a time for experimentation with alcohol. | 162 | 3.54 | 1.15 | 356 | 3.04 | 1.16 | 0 |  |  | 86 | 2.80 | 1.22 | 141 | 2.70 | 1.18 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.08 | 1.27 | 116 | 3.17 | 1.21 | 0 |  |  |
| 12\. A good college party should include drinking games such as beer pong, flip cup, power hour, etc. | 162 | 3.35 | 1.24 | 356 | 2.74 | 1.19 | 0 |  |  | 86 | 2.87 | 1.27 | 141 | 2.48 | 1.25 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.14 | 1.34 | 116 | 3.03 | 1.21 | 0 |  |  |
| 13\. Blacking out or forgetting part or all of the previous night’s events is to be expected in college. | 162 | 2.56 | 1.28 | 356 | 1.94 | 1.13 | 0 |  |  | 86 | 1.87 | 1.11 | 141 | 1.79 | 1.07 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.34 | 1.27 | 116 | 2.18 | 1.27 | 0 |  |  |
| 14\. It is okay to drink in college, even if you are under age. | 162 | 3.62 | 1.19 | 355 | 3.34 | 1.21 | 0 |  |  | 86 | 2.67 | 1.40 | 141 | 2.63 | 1.39 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 3.41 | 1.34 | 116 | 3.07 | 1.19 | 0 |  |  |
| 15\. The chance to drink and party in college is just as important as the academic experience. | 162 | 2.59 | 1.38 | 356 | 2.24 | 1.25 | 0 |  |  | 86 | 2.29 | 1.28 | 141 | 1.98 | 1.14 | 0 |  |  | 0 |  |  | 0 |  |  | 0 |  |  | 76 | 2.57 | 1.35 | 116 | 2.38 | 1.42 | 0 |  |  |

## Specification search

### Configural invariance

``` r
config_mod <- "
f1 =~ class1 + class2 + class3 + class4 + class5 + class6 + 
      class7 + class8 + class9 + class10 + class11 + class12 + 
      class13 + class14 + class15
"
config_fit <- cfa(config_mod, data = dat, group = "group", estimator = "MLR",
                  missing = "fiml")
# Release covariance between class1 and class2
config_fit2 <- update(config_fit, c(config_mod, "class1 ~~ class2"))
anova(config_fit, config_fit2)
```


    Scaled Chi-Squared Difference Test (method = "satorra.bentler.2001")

    lavaan->lavTestLRT():  
       lavaan NOTE: The "Chisq" column contains standard test statistics, not the 
       robust test that should be reported per model. A robust difference test is 
       a function of two standard (not robust) statistics.
                 Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
    config_fit2 534 40207 41543 1030.0                                  
    config_fit  540 40258 41566 1093.7     42.573       6  1.417e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Forward search (similar to Yoon and Millsap 2007); this takes several
minutes to run.

``` r
ps <- pinSearch(paste0(config_mod, "\nclass1 ~~ class2"), data = dat,
                group = "group", estimator = "MLR", missing = "fiml",
                type = "residual.covariances")
```

``` r
knitr::kable(ps[[2]])
```

| group | lhs     | rhs     | type       |
|------:|:--------|:--------|:-----------|
|     4 | class14 |         | intercepts |
|     4 | class1  |         | intercepts |
|     2 | class2  |         | intercepts |
|     3 | class2  |         | intercepts |
|     6 | class4  |         | intercepts |
|     5 | class4  |         | intercepts |
|     5 | class8  |         | intercepts |
|     2 | class4  |         | intercepts |
|     5 | class7  |         | intercepts |
|     5 | class3  |         | intercepts |
|     2 | class7  |         | intercepts |
|     6 | class5  |         | intercepts |
|     1 | class6  | class6  | residuals  |
|     1 | class14 | class14 | residuals  |
|     3 | class11 | class11 | residuals  |
|     3 | class6  | class6  | residuals  |
|     2 | class14 | class14 | residuals  |
|     5 | class14 | class14 | residuals  |
|     1 | class15 | class15 | residuals  |

Parameter Estimates

Group labels:

``` r
lavInspect(ps[[1]], what = "group.label")
```

    [1] "White/European American (e.g. German, Polish, Irish).Male"                   
    [2] "Asian/Asian American (e.g., Chinese, Filipino, Asian Indian).Male"           
    [3] "Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban).Male"  
    [4] "White/European American (e.g. German, Polish, Irish).Female"                 
    [5] "Asian/Asian American (e.g., Chinese, Filipino, Asian Indian).Female"         
    [6] "Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban).Female"

``` r
lavInspect(ps[[1]], what = "est") |>
    lapply(FUN = `[[`, "lambda") |>
    do.call(what = cbind) |>
    knitr::kable(digits = 3, caption = "Loading Estimates",
                 col.names = paste0("G", 1:6))
```

|         |     G1 |     G2 |     G3 |     G4 |     G5 |     G6 |
|:--------|-------:|-------:|-------:|-------:|-------:|-------:|
| class1  |  0.963 |  0.963 |  0.963 |  0.963 |  0.963 |  0.963 |
| class2  |  1.000 |  1.000 |  1.000 |  1.000 |  1.000 |  1.000 |
| class3  | -0.564 | -0.564 | -0.564 | -0.564 | -0.564 | -0.564 |
| class4  |  0.948 |  0.948 |  0.948 |  0.948 |  0.948 |  0.948 |
| class5  |  0.759 |  0.759 |  0.759 |  0.759 |  0.759 |  0.759 |
| class6  |  0.567 |  0.567 |  0.567 |  0.567 |  0.567 |  0.567 |
| class7  |  1.060 |  1.060 |  1.060 |  1.060 |  1.060 |  1.060 |
| class8  | -0.562 | -0.562 | -0.562 | -0.562 | -0.562 | -0.562 |
| class9  |  0.751 |  0.751 |  0.751 |  0.751 |  0.751 |  0.751 |
| class10 |  0.774 |  0.774 |  0.774 |  0.774 |  0.774 |  0.774 |
| class11 |  0.906 |  0.906 |  0.906 |  0.906 |  0.906 |  0.906 |
| class12 |  0.920 |  0.920 |  0.920 |  0.920 |  0.920 |  0.920 |
| class13 |  0.776 |  0.776 |  0.776 |  0.776 |  0.776 |  0.776 |
| class14 |  0.943 |  0.943 |  0.943 |  0.943 |  0.943 |  0.943 |
| class15 |  0.835 |  0.835 |  0.835 |  0.835 |  0.835 |  0.835 |

Loading Estimates

``` r
lavInspect(ps[[1]], what = "est") |>
    lapply(FUN = `[[`, "nu") |>
    do.call(what = cbind) |>
    knitr::kable(digits = 3, caption = "Intercept Estimates",
                 col.names = paste0("G", 1:6))
```

|         |    G1 |    G2 |    G3 |    G4 |    G5 |    G6 |
|:--------|------:|------:|------:|------:|------:|------:|
| class1  | 3.835 | 3.835 | 3.835 | 4.098 | 3.835 | 3.835 |
| class2  | 3.209 | 2.848 | 2.861 | 3.209 | 3.209 | 3.209 |
| class3  | 2.753 | 2.753 | 2.753 | 2.753 | 2.988 | 2.753 |
| class4  | 2.942 | 2.684 | 2.942 | 2.942 | 2.662 | 2.653 |
| class5  | 2.539 | 2.539 | 2.539 | 2.539 | 2.539 | 2.754 |
| class6  | 2.042 | 2.042 | 2.042 | 2.042 | 2.042 | 2.042 |
| class7  | 3.636 | 3.403 | 3.636 | 3.636 | 3.386 | 3.636 |
| class8  | 2.668 | 2.668 | 2.668 | 2.668 | 2.982 | 2.668 |
| class9  | 3.319 | 3.319 | 3.319 | 3.319 | 3.319 | 3.319 |
| class10 | 2.949 | 2.949 | 2.949 | 2.949 | 2.949 | 2.949 |
| class11 | 3.555 | 3.555 | 3.555 | 3.555 | 3.555 | 3.555 |
| class12 | 3.368 | 3.368 | 3.368 | 3.368 | 3.368 | 3.368 |
| class13 | 2.488 | 2.488 | 2.488 | 2.488 | 2.488 | 2.488 |
| class14 | 3.558 | 3.558 | 3.558 | 3.934 | 3.558 | 3.558 |
| class15 | 2.755 | 2.755 | 2.755 | 2.755 | 2.755 | 2.755 |

Intercept Estimates

``` r
lavInspect(ps[[1]], what = "est") |>
    lapply(FUN = `[[`, "theta") |>
    lapply(FUN = diag) |>
    do.call(what = cbind) |>
    knitr::kable(digits = 3, caption = "Uniqueness Estimates",
                 col.names = paste0("G", 1:6))
```

|         |    G1 |    G2 |    G3 |    G4 |    G5 |    G6 |
|:--------|------:|------:|------:|------:|------:|------:|
| class1  | 0.818 | 0.818 | 0.818 | 0.818 | 0.818 | 0.818 |
| class2  | 0.815 | 0.815 | 0.815 | 0.815 | 0.815 | 0.815 |
| class3  | 1.065 | 1.065 | 1.065 | 1.065 | 1.065 | 1.065 |
| class4  | 0.661 | 0.661 | 0.661 | 0.661 | 0.661 | 0.661 |
| class5  | 0.889 | 0.889 | 0.889 | 0.889 | 0.889 | 0.889 |
| class6  | 0.907 | 0.568 | 0.843 | 0.568 | 0.568 | 0.568 |
| class7  | 0.793 | 0.793 | 0.793 | 0.793 | 0.793 | 0.793 |
| class8  | 1.341 | 1.341 | 1.341 | 1.341 | 1.341 | 1.341 |
| class9  | 0.995 | 0.995 | 0.995 | 0.995 | 0.995 | 0.995 |
| class10 | 1.016 | 1.016 | 1.016 | 1.016 | 1.016 | 1.016 |
| class11 | 0.618 | 0.618 | 0.955 | 0.618 | 0.618 | 0.618 |
| class12 | 0.734 | 0.734 | 0.734 | 0.734 | 0.734 | 0.734 |
| class13 | 0.846 | 0.846 | 0.846 | 0.846 | 0.846 | 0.846 |
| class14 | 0.497 | 1.215 | 0.757 | 0.757 | 1.069 | 0.757 |
| class15 | 1.272 | 0.956 | 0.956 | 0.956 | 0.956 | 0.956 |

Uniqueness Estimates

## Effect Size

``` r
# Obtain fmacs effect size
(f_omni <- pin_effsize(ps[[1]]))
```

          class1-f1 class2-f1  class3-f1 class4-f1  class5-f1 class7-f1  class8-f1
    fmacs 0.0992441 0.1020962 0.07248248  0.110443 0.05987566 0.0773709 0.08807182
          class14-f1
    fmacs  0.1451998

``` r
# fmacs by gender
(f_gender <- pin_effsize(ps[[1]], group_factor = c(1, 1, 1, 2, 2, 2)))
```

           class1-f1  class2-f1 class3-f1  class4-f1  class5-f1   class7-f1
    fmacs 0.03002924 0.07919834 0.0297728 0.03763601 0.02669946 0.001833995
           class8-f1 class14-f1
    fmacs 0.03617625 0.04393448

``` r
# fmacs by ethnicity
(f_eth <- pin_effsize(ps[[1]], group_factor = c(1, 2, 3, 1, 2, 3)))
```

           class1-f1 class2-f1  class3-f1  class4-f1  class5-f1  class7-f1
    fmacs 0.04839808 0.0639283 0.04181952 0.08905502 0.03558941 0.07382416
           class8-f1 class14-f1
    fmacs 0.05081395 0.07080914

``` r
# interaction (using contrast matrix)
contr <- local({
    gen <- factor(c("F", "M"))
    contrasts(gen) <- contr.sum(length(gen))
    eth <- factor(1:3)
    contrasts(eth) <- contr.sum(length(eth))
    model.matrix(~ gen * eth, data = expand.grid(eth = eth, gen = gen))
})
(f_int <- pin_effsize(ps[[1]], contrast = contr[, 5:6, drop = FALSE]))
```

           class1-f1 class2-f1  class3-f1  class4-f1  class5-f1   class7-f1
    fmacs 0.04839808 0.0639283 0.04181952 0.04569283 0.03558941 0.002576069
           class8-f1 class14-f1
    fmacs 0.05081395 0.07080914

Bootstrapping

``` r
fmacs_est <- c(f_omni, f_gender, f_eth, f_int)
```

``` r
ps_refit <- lavaan(parTable(ps[[1]]),
    data = dat, group = "group", estimator = "MLR",
    missing = "fiml"
)
ps_boot <- bootstrapLavaan(ps_refit,
    R = 200,
    FUN = \(x) c(
        pin_effsize(x),
        pin_effsize(x, group_factor = c(1, 1, 1, 2, 2, 2)),
        pin_effsize(x, group_factor = c(1, 2, 3, 1, 2, 3)),
        pin_effsize(x, contrast = contr[, 5:6, drop = FALSE])
    )
)
saveRDS(ps_boot, here::here("ex2_ps_boot.rds"))
```

``` r
fmacs_boot <- structure(list(
    t0 = fmacs_est,
    t = ps_boot,
    R = 200, class = "boot"
))
# Bias
fmacs_bias <- colMeans(fmacs_boot$t, na.rm = TRUE) - fmacs_est
# Bias corrected fMACS
fmacs_est_bc <- pmax(0, fmacs_est - fmacs_bias)
# 95% Percentile CI
# Loop over indices
fmacs_boot_ci <- lapply(seq_len(ncol(fmacs_boot$t)), \(x) {
    boot_ci <- boot::boot.ci(
        fmacs_boot,
        type = c("norm", "basic", "perc"), index = x
    )
    list(
        norm = boot_ci$norm[2:3],
        basic = boot_ci$basic[4:5], perc = boot_ci$perc[4:5]
    )
})
```

``` r
f_omni_ci <- vapply(fmacs_boot_ci[1:8],
    FUN = \(x) paste0(
        "[", round(x$perc[1], 2), ", ",
        round(x$perc[2], 2), "]"
    ),
    FUN.VALUE = character(1)
)
f_gender_ci <- vapply(fmacs_boot_ci[9:16],
    FUN = \(x) paste0(
        "[", round(x$perc[1], 2), ", ",
        round(x$perc[2], 2), "]"
    ),
    FUN.VALUE = character(1)
)
f_eth_ci <- vapply(fmacs_boot_ci[17:24],
    FUN = \(x) paste0(
        "[", round(x$perc[1], 2), ", ",
        round(x$perc[2], 2), "]"
    ),
    FUN.VALUE = character(1)
)
f_int_ci <- vapply(fmacs_boot_ci[25:32],
    FUN = \(x) paste0(
        "[", round(x$perc[1], 2), ", ",
        round(x$perc[2], 2), "]"
    ),
    FUN.VALUE = character(1)
)
# Helper for formatting numbers
# Render as table
item_names <- gsub("class", replacement = "Item ", colnames(f_omni)) |>
    gsub(pattern = "-f1", replacement = "")
t(rbind(f_omni, f_gender, f_eth, f_int)) |>
    as.data.frame() |>
    `dimnames<-`(list(
        item_names,
        c("Overall", "Gender", "Ethnicity", "Gender x Ethnicity")
    )) |>
    knitr::kable(digits = 2, caption = "$f_\\text{MACS}$ effect sizes")
```

<div id="tbl-fmacs-ex2">

Table 1: $f_\text{MACS}$ effect sizes

<div class="cell-output-display">

|         | Overall | Gender | Ethnicity | Gender x Ethnicity |
|:--------|--------:|-------:|----------:|-------------------:|
| Item 1  |    0.10 |   0.03 |      0.05 |               0.05 |
| Item 2  |    0.10 |   0.08 |      0.06 |               0.06 |
| Item 3  |    0.07 |   0.03 |      0.04 |               0.04 |
| Item 4  |    0.11 |   0.04 |      0.09 |               0.05 |
| Item 5  |    0.06 |   0.03 |      0.04 |               0.04 |
| Item 7  |    0.08 |   0.00 |      0.07 |               0.00 |
| Item 8  |    0.09 |   0.04 |      0.05 |               0.05 |
| Item 14 |    0.15 |   0.04 |      0.07 |               0.07 |

</div>

</div>

### Test-level effect sizes

``` r
pin_effsize(ps[[1]], item_weights = rep(1, 15))
```

            item_sum
    fmacs 0.09783036

``` r
# Bootstrap
pstest_boot <- bootstrapLavaan(ps_refit,
    R = 200,
    FUN = \(x) c(
        pin_effsize(x, item_weights = rep(1, 15))
    )
)
saveRDS(pstest_boot, here::here("ex2_pstest_boot.rds"))
```

``` r
fmacstest_est <- pin_effsize(ps[[1]], item_weights = rep(1, 15))
fmacstest_boot <- structure(list(
    t0 = fmacstest_est,
    t = pstest_boot,
    R = 200, class = "boot"
))
# Bias
fmacstest_bias <- colMeans(fmacstest_boot$t, na.rm = TRUE) - fmacstest_est
# Bias corrected fMACS
fmacstest_est_bc <- pmax(0, fmacstest_est - fmacstest_bias)
# 95% Percentile CI
boot::boot.ci(
        fmacstest_boot,
        type = c("norm", "basic", "perc")
    )
```

    BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
    Based on 200 bootstrap replicates

    CALL : 
    boot::boot.ci(boot.out = fmacstest_boot, type = c("norm", "basic", 
        "perc"))

    Intervals : 
    Level      Normal              Basic              Percentile     
    95%   ( 0.0713,  0.1184 )   ( 0.0690,  0.1169 )   ( 0.0788,  0.1267 )  
    Calculations and Intervals on Original Scale
    Some basic intervals may be unstable
    Some percentile intervals may be unstable

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-yoon2007" class="csl-entry">

Yoon, Myeongsun, and Roger E. Millsap. 2007. “Detecting Violations of
Factorial Invariance Using Data-Based Specification Searches: A Monte
Carlo Study.” *Structural Equation Modeling: A Multidisciplinary
Journal* 14 (3): 435–63. <https://doi.org/10.1080/10705510701301677>.

</div>

</div>
