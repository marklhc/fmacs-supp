# Illustrative Example (Lui., 2019) with Categorical Data

2025-01-08

This is the same example as [ex2.qmd](ex2.qmd), but with the items
treated as categorical. Only ethnicity is included as the grouping
variable.

## Load Data

``` r
orig_dat <- haven::read_sav(
    here::here("LUI2018_class4mplus.sav")
)
dat <- orig_dat |>
    filter(eth %in% c(1, 2, 4)) |>
    mutate(eth = as_factor(eth)) |>
    arrange(eth)
```

## Specification search

### Configural invariance

``` r
config_mod <- "
f1 =~ class1 + class2 + class3 + class4 + class5 + class6 + 
      class7 + class8 + class9 + class10 + class11 + class12 + 
      class13 + class14 + class15
"
config_fit <- cfa(config_mod, data = dat, group = "eth",
                  ordered = TRUE)
# Release covariance between class1 and class2
config_fit2 <- update(config_fit, c(config_mod, "class1 ~~ class2"))
anova(config_fit, config_fit2)
```


    Scaled Chi-Squared Difference Test (method = "satorra.2000")

    lavaan->lavTestLRT():  
       lavaan NOTE: The "Chisq" column contains standard test statistics, not the 
       robust test that should be reported per model. A robust difference test is 
       a function of two standard (not robust) statistics.
                 Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
    config_fit2 267         379.37                                  
    config_fit  270         427.69     46.498       3  4.445e-10 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Forward search and effect size

``` r
ps1 <- pinSearch(paste0(config_mod, "\nclass1 ~~ class2"),
    data = dat,
    group = "eth", ordered = TRUE,
    type = "residual.covariances",
    effect_size = TRUE
)
ps1
```

    $`Partial Invariance Fit`
    lavaan 0.6-19 ended normally after 54 iterations

      Estimator                                       DWLS
      Optimization method                           NLMINB
      Number of model parameters                       232
      Number of equality constraints                   141

      Number of observations per group:                                           Used       Total
        White/European American (e.g. German, Polish, Irish)                       517         518
        Asian/Asian American (e.g., Chinese, Filipino, Asian Indian)               229         229
        Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban)      192         192

    Model Test User Model:
                                                  Standard      Scaled
      Test Statistic                               547.183     664.564
      Degrees of freedom                               404         404
      P-value (Chi-square)                           0.000       0.000
      Scaling correction factor                                  1.099
      Shift parameter                                          166.653
        simple second-order correction                                
      Test statistic for each group:
        White/European American (e.g. German, Polish, Irish)  245.155     245.155
        Asian/Asian American (e.g., Chinese, Filipino, Asian Indian)  230.275     230.275
        Hispanic/Latino/Spanish American (e.g., Mexican, Puerto Rican, Cuban)  189.135     189.135

    $`Non-Invariant Items`
       group     lhs     rhs       type
    1      2      f1  class3   loadings
    2      3      f1  class9   loadings
    3      3      f1 class15   loadings
    4      3      f1  class8   loadings
    5      1 class10      t2 thresholds
    6      3  class3      t3 thresholds
    7      2  class8      t4 thresholds
    8      1 class14      t2 thresholds
    9      3  class9      t4 thresholds
    10     3 class12      t4 thresholds
    11     2 class10      t1 thresholds

    $effect_size
          class3-f1  class8-f1  class9-f1 class10-f1 class12-f1 class14-f1
    fmacs 0.0993501 0.03733775 0.07815185 0.07370135 0.02865815 0.04439193
          class15-f1
    fmacs 0.06316425

``` r
# Test-level effect size
pin_effsize(ps1[[1]], item_weights = rep(1, 15))
```

            item_sum
    fmacs 0.02894319
