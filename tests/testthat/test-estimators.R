
library(multimedia)

# Joy dataset example
exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))

test_that("Estimation with RF works in a simple case.", {
  model <- multimedia(exper) |>
    estimate(exper)

  predictions <- predict(model)
  expect_equal(names(predictions), c("mediators", "outcomes"))
  expect_equal(colnames(predictions$mediators), mediators(model))
  expect_equal(mediators(model), paste0("ASV", 1:5))
})

test_that("Estimation with glmnet works in a simple case", {
  model <- multimedia(exper, glmnet_model(lambda = .1)) |>
    estimate(exper)

  predictions <- predict(model)
  expect_equal(names(predictions), c("mediators", "outcomes"))
  expect_equal(colnames(predictions$mediators), mediators(model))
  expect_equal(mediators(model), paste0("ASV", 1:5))
})
