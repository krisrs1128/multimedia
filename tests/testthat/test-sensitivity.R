
xy_data <- demo_spline()
exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
model <- multimedia(exper, outcome_estimator = rf_model(num.trees = 1e3)) |>
  estimate(exper)

sensitivity_curve <- sensitivity(model, exper, n_rho = 10)
test_that("Sensitivity curve has correct bounds.", {
  expect_equal(nrow(sensitivity_curve), 10)
})

test_that("Sensitivity curve has correct column names.", {
  expect_named(sensitivity_curve, c("rho", "indirect_overall"))
})