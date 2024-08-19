
xy_data <- demo_spline()
exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
model <- multimedia(exper, outcome_estimator = glmnet_model(lambda = 1e-2)) |>
  estimate(exper)

sensitivity_curve <- sensitivity(model, exper, n_rho = 10)
test_that("Sensitivity curve has correct bounds.", {
  expect_equal(nrow(sensitivity_curve), 10 * n_outcomes(model))
})

test_that("Sensitivity curve has correct column names.", {
  expect_named(sensitivity_curve, c("outcome", "rho", "indirect_overall"))
})