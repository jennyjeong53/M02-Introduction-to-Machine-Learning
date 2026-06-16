library(tidyverse)
library(broom)
library(ggplot2)

# 1. Fit a model predicting mpg from wt alone
fit_simple <- lm(mpg ~ wt, data = mtcars)

## Use tidy() to see the coefficients
tidy(fit_simple)

## Use glance() to check model fit
glance(fit_simple)

# 2. Use augment() and print the first 10 rows
augment(fit_simple) |> 
  head(10)

# 3. Plot fitted values vs. residuals
augment(fit_simple) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(color = "#005030", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#FFB81C") +
  labs(
    title = "Fitted Values vs. Residuals",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

# 4. Add hp and cyl to the model
fit_multi <- lm(mpg ~ wt + hp + cyl, data = mtcars)

tidy(fit_multi)

glance(fit_multi)

augment(fit_multi) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(color = "#005030", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#FFB81C") +
  labs(
    title = "Fitted Values vs. Residuals: Multiple Regression",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()






library(tibble)
set.seed(42)
n <- 1000

customer_data <- tibble(
  customer_id = 1:n,
  tenure = sample(1:60, n, replace = TRUE),
  monthly_spend = round(rnorm(n, mean = 85, sd = 25), 2),
  num_products = sample(
    1:5,
    n,
    replace = TRUE,
    prob = c(0.3, 0.3, 0.2, 0.1, 0.1)
  ),
  num_complaints = rpois(n, lambda = 0.5),
  last_login_days = sample(1:90, n, replace = TRUE),
  region = sample(c("West", "East", "South", "Midwest"), n, replace = TRUE),
  churn = factor(
    ifelse(
      0.05 *
        num_complaints +
        0.02 * last_login_days -
        0.015 * tenure -
        0.005 * monthly_spend +
        rnorm(n, 0, 0.5) >
        0.3,
      "yes",
      "no"
    )
  )
)

glimpse(customer_data)

# 1. Fit a linear model predicting monthly_spend from tenure and num_products
spend_fit <- lm(
  monthly_spend ~ tenure + num_products + region,
  data = customer_data
)

## 1-1. Print the model fit using tidy(), summary()

tidy(spend_fit)

summary(spend_fit)

# 2. Check model quality
glance(spend_fit)


# 3. Visualize residuals
augment(spend_fit) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.3, color = "#005030") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#FFB81C") +
  labs(
    title = "Residuals: Monthly Spend Model",
    x = "Fitted values ($)",
    y = "Residuals"
  ) +
  theme_minimal()


# 4. Add num_complaints and last_login_days as predictors

spend_fit2 <- lm(
  monthly_spend ~ tenure + num_products + region +
    num_complaints + last_login_days,
  data = customer_data
)

# Check coefficients and significance
tidy(spend_fit2)

# Check overall model quality
glance(spend_fit2)

# Residual plot for the updated model
augment(spend_fit2) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.3, color = "#005030") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#FFB81C") +
  labs(
    title = "Residuals: Updated Monthly Spend Model",
    x = "Fitted values ($)",
    y = "Residuals"
  ) +
  theme_minimal()