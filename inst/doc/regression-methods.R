## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  error = FALSE,
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(rwa)
library(dplyr)
library(ggplot2)

## ----multiregress-basic-------------------------------------------------------
# Direct use of rwa_multiregress()
result_multi <- rwa_multiregress(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt")
)

# View results
result_multi$result

## ----multiregress-interpret---------------------------------------------------
# R-squared: Total variance explained
cat("R-squared:", round(result_multi$rsquare, 4), "\n")
cat("This means", round(result_multi$rsquare * 100, 1), 
    "% of variance in mpg is explained by these predictors.\n\n")

# Number of observations
cat("Sample size:", result_multi$n, "\n\n")

# Relative importance breakdown
cat("Relative Importance (Rescaled Weights sum to 100%):\n")
result_multi$result %>%
  arrange(desc(Rescaled.RelWeight)) %>%
  mutate(Rescaled.RelWeight = round(Rescaled.RelWeight, 2)) %>%
  select(Variables, Rescaled.RelWeight)

## ----multiregress-signs-------------------------------------------------------
# With sign information
result_signed <- rwa_multiregress(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  applysigns = TRUE
)

result_signed$result %>%
  select(Variables, Raw.RelWeight, Sign.Rescaled.RelWeight, Sign)

## ----multiregress-correlations------------------------------------------------
# Correlation between predictors
cat("Predictor Correlation Matrix (RXX):\n")
round(result_multi$RXX, 3)

# Correlation of predictors with outcome
cat("\nPredictor-Outcome Correlations (RXY):\n")
round(result_multi$RXY, 3)

## ----logit-setup--------------------------------------------------------------
# Create binary outcome: high efficiency (1) vs low efficiency (0)
mtcars_binary <- mtcars %>%
  mutate(high_mpg = ifelse(mpg > median(mpg), 1, 0))

# Check distribution
table(mtcars_binary$high_mpg)

## ----logit-basic--------------------------------------------------------------
# Logistic regression RWA
result_logit <- rwa_logit(
  df = mtcars_binary,
  outcome = "high_mpg",
  predictors = c("cyl", "disp", "hp", "wt")
)

# View results
result_logit$result

## ----logit-interpret----------------------------------------------------------
# Lambda (analogous to R-squared for logistic regression)
cat("Lambda (pseudo R-squared):", round(result_logit$lambda, 4), "\n")
cat("Sample size:", result_logit$n, "\n\n")

# Relative importance
cat("Relative Importance for Predicting High Fuel Efficiency:\n")
result_logit$result %>%
  arrange(desc(Rescaled.RelWeight)) %>%
  mutate(Rescaled.RelWeight = round(Rescaled.RelWeight, 2))

## ----logit-signs--------------------------------------------------------------
# With direction information
result_logit_signed <- rwa_logit(
  df = mtcars_binary,
  outcome = "high_mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  applysigns = TRUE
)

result_logit_signed$result %>%
  select(Variables, Rescaled.RelWeight, Sign)

## ----rwa-auto-----------------------------------------------------------------
# For continuous outcome - automatically uses multiple regression
result_auto_multi <- rwa(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt")
)

# For binary outcome - automatically uses logistic regression
result_auto_logit <- rwa(
  df = mtcars_binary,
  outcome = "high_mpg",
  predictors = c("cyl", "disp", "hp", "wt")
)

## ----rwa-explicit-------------------------------------------------------------
# Force multiple regression
result_explicit_multi <- rwa(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  method = "multiple"
)

# Force logistic regression (requires binary outcome)
result_explicit_logit <- rwa(
  df = mtcars_binary,
  outcome = "high_mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  method = "logistic"
)

## ----rwa-features-------------------------------------------------------------
# Sort results by importance
result_sorted <- rwa(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  sort = TRUE
)

result_sorted$result

# Visualize with plot_rwa()
plot_rwa(result_sorted)

## ----rwa-multiple-only, eval=FALSE--------------------------------------------
# # Weighted RWA: multiple regression only
# rwa(df, "satisfaction", c("x1", "x2"), weight = "survey_weight")
# 
# # Warns, and the weight is not applied
# rwa(df, "binary_outcome", c("x1", "x2"), weight = "survey_weight")

## ----iris-multi---------------------------------------------------------------
# Predict petal length from other measurements
iris_result <- rwa_multiregress(
  df = iris,
  outcome = "Petal.Length",
  predictors = c("Sepal.Length", "Sepal.Width", "Petal.Width"),
  applysigns = TRUE
)

cat("R-squared:", round(iris_result$rsquare, 4), "\n\n")
iris_result$result

# Visualize
plot_rwa(iris_result)

## ----iris-logit---------------------------------------------------------------
# Create binary outcome for setosa classification
iris_binary <- iris %>%
  mutate(is_setosa = ifelse(Species == "setosa", 1, 0))

# Logistic RWA
iris_logit <- rwa_logit(
  df = iris_binary,
  outcome = "is_setosa",
  predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  applysigns = TRUE
)

cat("Pseudo R-squared:", round(iris_logit$rsquare, 4), "\n\n")
iris_logit$result

## ----compare-methods----------------------------------------------------------
# Create comparison dataset
comparison_data <- mtcars %>%
  mutate(high_mpg = ifelse(mpg > median(mpg), 1, 0))

# Multiple regression on continuous mpg
multi_result <- rwa_multiregress(
  comparison_data, "mpg", 
  c("cyl", "disp", "hp", "wt")
)

# Logistic regression on binary high_mpg
logit_result <- rwa_logit(
  comparison_data, "high_mpg", 
  c("cyl", "disp", "hp", "wt")
)

# Compare rankings
comparison <- data.frame(
  Variable = multi_result$result$Variables,
  Multiple_Pct = round(multi_result$result$Rescaled.RelWeight, 1),
  Logistic_Pct = round(logit_result$result$Rescaled.RelWeight, 1)
)

comparison %>%
  arrange(desc(Multiple_Pct))

## ----best-practices-----------------------------------------------------------
# Always check outcome distribution for binary variables
table(mtcars_binary$high_mpg)

# Ensure reasonable sample size
cat("Sample size:", nrow(mtcars), "\n")
cat("Predictors:", 4, "\n")
cat("Observations per predictor:", nrow(mtcars) / 4, "\n")

## ----bootstrap-multi----------------------------------------------------------
# Bootstrap with multiple regression
result_boot <- rwa(
  df = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "wt"),
  bootstrap = TRUE,
  n_bootstrap = 1000
)

result_boot$result

