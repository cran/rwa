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

## ----theoretical-demonstration------------------------------------------------
# Create controlled scenario to demonstrate RWA's theoretical properties
set.seed(123)
n <- 200

# Generate predictors with known correlation structure
x1 <- rnorm(n)
x2 <- 0.7 * x1 + 0.3 * rnorm(n)  # r ≈ 0.7 with x1
x3 <- 0.5 * x1 + 0.8 * rnorm(n)  # r ≈ 0.5 with x1
x4 <- rnorm(n)                    # Independent

# True population model with known coefficients
y <- 0.6 * x1 + 0.4 * x2 + 0.3 * x3 + 0.2 * x4 + rnorm(n, sd = 0.5)

theory_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)

# Compare traditional regression vs RWA
lm_theory <- lm(y ~ x1 + x2 + x3 + x4, data = theory_data)
rwa_theory <- rwa(theory_data, "y", c("x1", "x2", "x3", "x4"))

# Show how multicollinearity affects traditional coefficients
cat("True population contributions (designed into simulation):\n")
true_contributions <- c(0.6, 0.4, 0.3, 0.2)
names(true_contributions) <- c("x1", "x2", "x3", "x4")
print(true_contributions)

cat("\nStandardized regression coefficients (distorted by multicollinearity):\n")
std_betas <- summary(lm_theory)$coefficients[2:5, "Estimate"]
names(std_betas) <- c("x1", "x2", "x3", "x4")
print(round(std_betas, 3))

cat("\nRWA weights (better reflect true importance despite correlations):\n")
rwa_weights_theory <- rwa_theory$result$Raw.RelWeight
names(rwa_weights_theory) <- rwa_theory$result$Predictors
print(round(rwa_weights_theory, 3))

# Calculate correlation between methods and true values
cor_with_true <- data.frame(
  Method = c("Std_Betas", "RWA_Weights"),
  Correlation_with_True = c(
    cor(abs(std_betas), true_contributions),
    cor(rwa_weights_theory[names(true_contributions)], true_contributions)
  )
)
print("Correlation with true population values:")
print(cor_with_true)

## ----basic-example------------------------------------------------------------
# Basic RWA
result_basic <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"))

# View the results
result_basic$result

## ----output-explanation-------------------------------------------------------
# Predictor variables used
result_basic$predictors

# Model R-squared
result_basic$rsquare

# Number of complete observations
result_basic$n

# Correlation matrices (for advanced users)
str(result_basic$RXX)  # Predictor correlation matrix
str(result_basic$RXY)  # Predictor-outcome correlations

## ----interpret-results--------------------------------------------------------
# Results are sorted by default (most important first)
result_basic$result

# Raw weights sum to R-squared
sum(result_basic$result$Raw.RelWeight)
result_basic$rsquare

# Rescaled weights sum to 100%
sum(result_basic$result$Rescaled.RelWeight)

## ----sorting-example----------------------------------------------------------
# Default behavior: sorted by importance (descending)
result_sorted <- mtcars %>%
  rwa(outcome = "mpg", predictors = c("cyl", "disp", "hp", "gear"))

result_sorted$result

# Preserve original predictor order
result_unsorted <- mtcars %>%
  rwa(outcome = "mpg", predictors = c("cyl", "disp", "hp", "gear"), sort = FALSE)

result_unsorted$result

## ----signs-example------------------------------------------------------------
result_signs <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear"),
      applysigns = TRUE)

result_signs$result

## ----visualization, fig.width=8, fig.height=5---------------------------------
# Generate RWA results 
rwa_result <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp", "gear", "wt"))

# Create plot
rwa_result %>% plot_rwa()

# The rescaled relative weights
rwa_result$result

## ----eval=FALSE---------------------------------------------------------------
# vignette("bootstrap-confidence-intervals", package = "rwa")

## ----bootstrap-example--------------------------------------------------------
# Basic bootstrap analysis
bootstrap_result <- mtcars %>%
  rwa(outcome = "mpg",
      predictors = c("cyl", "disp", "hp"),
      bootstrap = TRUE,
      n_bootstrap = 500)  # Reduced for speed

# View significant predictors
bootstrap_result$result %>%
  filter(Raw.Significant == TRUE) %>%
  select(Variables, Rescaled.RelWeight, Raw.RelWeight.CI.Lower, Raw.RelWeight.CI.Upper)

## ----diamonds-example---------------------------------------------------------
# Analyze diamond price drivers
diamonds_subset <- diamonds %>%
  select(price, carat, depth, table, x, y, z) %>%
  sample_n(1000)  # Sample for faster computation

diamond_rwa <- diamonds_subset %>%
  rwa(outcome = "price",
      predictors = c("carat", "depth", "table", "x", "y", "z"),
      applysigns = TRUE)

diamond_rwa$result

## ----eval=FALSE---------------------------------------------------------------
# vignette("bootstrap-confidence-intervals", package = "rwa")

## ----regression-comparison----------------------------------------------------
# Traditional regression
lm_model <- lm(mpg ~ cyl + disp + hp + gear, data = mtcars)
lm_summary <- summary(lm_model)

# Display regression summary
print(lm_summary)

# RWA results
rwa_model <- mtcars %>%
  rwa(outcome = "mpg", predictors = c("cyl", "disp", "hp", "gear"))

# Compare importance rankings
comparison <- data.frame(
  Variable = rwa_model$predictors,
  RWA_Rescaled = rwa_model$result$Rescaled.RelWeight,
  RWA_Rank = rank(-rwa_model$result$Rescaled.RelWeight)
)

print(comparison)

## ----efficiency-demonstration-------------------------------------------------
# Demonstrate computational considerations
predictors <- c("cyl", "disp", "hp", "gear")
n_predictors <- length(predictors)

cat("Number of predictors:", n_predictors, "\n")
cat("Dominance analysis would require", 2^n_predictors, "subset models\n")
cat("RWA solves this in a single matrix operation\n")

# Show RWA speed (for demonstration)
start_time <- Sys.time()
rwa_speed_test <- mtcars %>% rwa(outcome = "mpg", predictors = predictors)
end_time <- Sys.time()
cat("RWA computation time:", round(as.numeric(end_time - start_time, units = "secs"), 4), "seconds\n")

## ----redundancy-demonstration-------------------------------------------------
# Demonstrate the redundancy limitation
set.seed(456)
x1_orig <- rnorm(100)
x1_dup <- x1_orig + rnorm(100, sd = 0.05)  # Nearly identical (r ≈ 0.99)
y_simple <- 0.8 * x1_orig + rnorm(100, sd = 0.5)

redundant_data <- data.frame(y = y_simple, x1_original = x1_orig, 
                           x1_duplicate = x1_dup)

cat("Correlation between 'different' predictors:", cor(x1_orig, x1_dup), "\n")

# RWA correctly splits redundant variance
redundant_rwa <- rwa(redundant_data, "y", c("x1_original", "x1_duplicate"))
print("RWA with redundant predictors:")
print(redundant_rwa$result)

cat("\nEach variable appears less important individually,")
cat("\nbut together they account for most variance.\n")
cat("Combined contribution:", 
    sum(redundant_rwa$result$Raw.RelWeight), "\n")

## ----sample-size--------------------------------------------------------------
# Check your sample size
n_obs <- mtcars %>% 
  select(mpg, cyl, disp, hp, gear) %>% 
  na.omit() %>% 
  nrow()

cat("Sample size:", n_obs)
cat("\nRule of thumb: At least 5-10 observations per predictor")

## ----eval=FALSE---------------------------------------------------------------
# vignette("bootstrap-confidence-intervals", package = "rwa")

## ----multicollinearity-check--------------------------------------------------
# Check correlation matrix
cor_matrix <- mtcars %>%
  select(cyl, disp, hp, gear) %>%
  cor()

# Look for high correlations (>0.9)
high_cor <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  cat("High correlations detected between variables")
}

## ----missing-data-------------------------------------------------------------
# Check for missing data patterns
missing_summary <- mtcars %>%
  select(mpg, cyl, disp, hp, gear) %>%
  summarise_all(~sum(is.na(.)))

print(missing_summary)

## ----eval=FALSE---------------------------------------------------------------
# vignette("bootstrap-confidence-intervals", package = "rwa")

