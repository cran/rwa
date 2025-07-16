## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%",
  error = FALSE,
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(rwa)
library(dplyr)
library(ggplot2)

## ----bootstrap-basic----------------------------------------------------------
# Bootstrap analysis with 1000 samples
result_bootstrap <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"),
      bootstrap = TRUE,
      n_bootstrap = 1000,
      conf_level = 0.95)

# View results with confidence intervals
result_bootstrap$result

## ----bootstrap-interpretation-------------------------------------------------
# Bootstrap-specific information
cat("Bootstrap samples used:", result_bootstrap$bootstrap$n_bootstrap, "\n")

# Detailed CI information
print(result_bootstrap$bootstrap$ci_results$raw_weights)

# Identify significant predictors
significant_vars <- result_bootstrap$result %>%
  filter(Raw.Significant == TRUE) %>%
  pull(Variables)

cat("Significant predictors:", paste(significant_vars, collapse = ", "))

## ----bootstrap-comprehensive--------------------------------------------------
# Comprehensive bootstrap with focal variable comparison
result_comprehensive <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear", "wt"),
      bootstrap = TRUE,
      comprehensive = TRUE,
      focal = "wt",  # Compare other variables to weight
      n_bootstrap = 500)  # Fewer samples for speed

# Access all bootstrap results
names(result_comprehensive$bootstrap$ci_results)

## ----bootstrap-parameters-----------------------------------------------------
# Example with different parameters
custom_bootstrap <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp"),
      bootstrap = TRUE,
      n_bootstrap = 2000,  # More samples for precision
      conf_level = 0.99)   # 99% confidence intervals

custom_bootstrap$result

## ----rescaled-ci-warning------------------------------------------------------
# Rescaled CIs (use with caution)
result_rescaled_ci <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp"),
      bootstrap = TRUE,
      include_rescaled_ci = TRUE,
      n_bootstrap = 500)

# Note the warning message about interpretation
result_rescaled_ci$result

## ----diamonds-example---------------------------------------------------------
# Analyze diamond price drivers
diamonds_subset <- diamonds %>%
  select(price, carat, depth, table, x, y, z) %>%
  sample_n(1000)  # Sample for faster computation

diamond_rwa <- diamonds_subset %>%
  rwa(outcome = "price",
      predictors = c("carat", "depth", "table", "x", "y", "z"),
      bootstrap = TRUE,
      applysigns = TRUE,
      n_bootstrap = 500)

print(diamond_rwa$result)

## ----diamonds-interpretation--------------------------------------------------
# Focus on significant predictors (results are already sorted by importance)
significant_drivers <- diamond_rwa$result %>%
  filter(Raw.Significant == TRUE) %>%
  select(Variables, Rescaled.RelWeight, Sign.Rescaled.RelWeight)

cat("Significant diamond price drivers (sorted by importance):\n")
print(significant_drivers)

cat("\nModel R-squared:", round(diamond_rwa$rsquare, 3))

## ----sample-size--------------------------------------------------------------
# Check your sample size
n_obs <- mtcars %>% 
  select(mpg, cyl, disp, hp, gear) %>% 
  na.omit() %>% 
  nrow()

cat("Sample size:", n_obs)
cat("\nRecommended bootstrap samples:", min(2000, n_obs * 10))

# Rule of thumb: At least 1000 bootstrap samples, more for smaller datasets

## ----ci-interpretation--------------------------------------------------------
# Examine CI characteristics
ci_data <- result_bootstrap$bootstrap$ci_results$raw_weights
print(head(ci_data))

# Assess precision
ci_analysis <- ci_data %>%
  mutate(
    significant = ci_lower > 0 | ci_upper < 0,
    ci_width = ci_upper - ci_lower,
    precision = case_when(
      ci_width < 0.05 ~ "High precision",
      ci_width < 0.15 ~ "Medium precision", 
      TRUE ~ "Low precision"
    )
  )

print(ci_analysis)

## ----bootstrap-methods--------------------------------------------------------
# Check which methods were used
ci_methods <- result_bootstrap$bootstrap$ci_results$raw_weights %>%
  count(ci_method)

print(ci_methods)

## ----performance-tips---------------------------------------------------------
# For large datasets or many predictors, consider:

# 1. Reduce bootstrap samples for initial exploration
quick_result <- mtcars %>%
  rwa(outcome = "mpg", 
      predictors = c("cyl", "disp"), 
      bootstrap = TRUE, 
      n_bootstrap = 500)  # Faster

# 2. Use comprehensive analysis only when needed
# comprehensive = TRUE adds computational overhead

# 3. Consider parallel processing for very large analyses
# (not currently implemented but could be future enhancement)

## ----memory-usage-------------------------------------------------------------
# Bootstrap objects can be large - access specific components
str(result_bootstrap$bootstrap, max.level = 1)

# For memory efficiency, extract only needed results
ci_summary <- result_bootstrap$bootstrap$ci_results$raw_weights %>%
  select(variable, ci_lower, ci_upper, ci_method)

print(ci_summary)

## ----troubleshooting----------------------------------------------------------
# 1. Check for perfect multicollinearity
cor_check <- mtcars %>%
  select(cyl, disp, hp, gear) %>%
  cor()

# Look for correlations = 1.0 (excluding diagonal)
perfect_cor <- which(abs(cor_check) == 1 & cor_check != diag(diag(cor_check)), arr.ind = TRUE)

if(length(perfect_cor) > 0) {
  cat("Perfect multicollinearity detected - remove redundant variables")
} else {
  cat("No perfect multicollinearity detected")
}

# 2. Ensure adequate sample size
min_sample_size <- 5 * length(c("cyl", "disp", "hp", "gear"))  # 5 obs per predictor
actual_sample_size <- nrow(na.omit(mtcars[c("mpg", "cyl", "disp", "hp", "gear")]))

cat("\nMinimum recommended sample size:", min_sample_size)
cat("\nActual sample size:", actual_sample_size)

## ----reporting-example--------------------------------------------------------
# Generate a summary report
report_data <- result_bootstrap$result %>%
  filter(Raw.Significant == TRUE) %>%
  arrange(desc(Rescaled.RelWeight)) %>%
  select(Variables, Rescaled.RelWeight, Raw.RelWeight.CI.Lower, Raw.RelWeight.CI.Upper)

cat("Relative Weights Analysis Results\n")
cat("=================================\n")
cat("Sample size:", result_bootstrap$n, "\n")
cat("Bootstrap samples:", result_bootstrap$bootstrap$n_bootstrap, "\n")
cat("Model R-squared:", round(result_bootstrap$rsquare, 3), "\n\n")
cat("Significant Predictors:\n")
print(report_data)

