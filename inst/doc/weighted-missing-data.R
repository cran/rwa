## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(rwa)

## ----weighted-estimate--------------------------------------------------------
d <- mtcars
d$observation_weight <- rep(c(1, 2, 5, 3), 8)
fit <- rwa(d, "mpg", c("hp", "wt", "disp"), method = "multiple",
           weight = "observation_weight", sort = FALSE)
fit$result
c(rwa = fit$rsquare,
  weighted_lm = summary(lm(mpg ~ hp + wt + disp, data = d,
                           weights = observation_weight))$r.squared)

## ----weight-scale-------------------------------------------------------------
d$scaled_weight <- 100 * d$observation_weight
scaled <- rwa_multiregress(d, "mpg", c("hp", "wt", "disp"), weight = "scaled_weight")
all.equal(fit$result, scaled$result)
c(original_effective = fit$n_effective, scaled_effective = scaled$n_effective)

## ----missing-data-------------------------------------------------------------
d$mpg[1] <- NA
d$observation_weight[2] <- NA
d$hp[3] <- NA
missing_fit <- rwa(d, "mpg", c("hp", "wt", "disp"), method = "multiple",
                   weight = "observation_weight")
retained <- complete.cases(d[c("mpg", "hp", "wt", "disp", "observation_weight")])
c(n = missing_fit$n, retained_rows = sum(retained),
  n_weighted = missing_fit$n_weighted,
  retained_weight_sum = sum(d$observation_weight[retained]))

## ----weighted-plot, fig.width=7, fig.height=4---------------------------------
plot_rwa(fit)

## ----bootstrap, eval=FALSE----------------------------------------------------
# set.seed(42)
# boot_fit <- rwa(d, "mpg", c("hp", "wt", "disp"), method = "multiple",
#                 weight = "observation_weight", bootstrap = TRUE,
#                 n_bootstrap = 1000, comprehensive = TRUE)
# boot_fit$bootstrap$ci_results$random_comparison

