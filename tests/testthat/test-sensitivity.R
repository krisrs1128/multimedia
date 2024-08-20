library(multimedia)

xy_data <- demo_spline()
exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
model <- multimedia(exper, outcome_estimator = glmnet_model(lambda = 1e-2)) |>
  estimate(exper)
indirect_overall(model)

sensitivity_curve <- sensitivity(model, exper, n_rho = 3, n_bootstrap = 10)
test_that("Sensitivity curve has correct bounds.", {
  expect_equal(nrow(sensitivity_curve), 3 * n_outcomes(model))
})

test_that("Sensitivity curve has correct column names.", {
  expect_named(sensitivity_curve, c("outcome", "rho", "indirect_effect", "indirect_effect_standard_error"))
})

test_that("Sensitivity at rho = 0 agrees with original indirect effect", {
  sensitivity0 <- sensitivity_curve |>
    filter(rho == 0)

  indirect_diff <- indirect_overall(model) |>
    effect_summary() |>
    left_join(sensitivity0, by = "outcome") |>
    mutate(difference = abs(indirect_effect.x - indirect_effect.y))
  expect_true(all(indirect_diff$difference < 0.05))
})