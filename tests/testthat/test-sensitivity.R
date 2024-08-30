set.seed(20240826)

xy_data <- demo_spline()
exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
model <- multimedia(exper, outcome_estimator = glmnet_model(lambda = 1e-2)) |>
    estimate(exper)
overall <- indirect_overall(model)

subset_indices <- expand.grid(mediator = n_mediators(model), outcome = n_outcomes(model))
rho_seq <- c(-0.2, 0, 0.2)
n_contrast <- 2
sensitivity_curve <- sensitivity(model, exper, subset_indices, rho_seq, n_bootstrap = 10)
test_that("Sensitivity curve has correct bounds.", {
    expect_equal(nrow(sensitivity_curve), 3 * n_contrast * n_outcomes(model))
})

test_that("Sensitivity curve has correct column names.", {
    expect_named(sensitivity_curve, c("rho", colnames(overall), "indirect_effect_standard_error"))
})

test_that("Sensitivity at rho = 0 agrees with original indirect effect", {
    sensitivity0 <- sensitivity_curve |>
        filter(rho == 0)

    indirect_diff <- overall |>
        effect_summary() |>
        left_join(sensitivity0, by = "outcome") |>
        mutate(difference = abs(indirect_effect.x - 2 * indirect_effect.y))
    expect_true(all(indirect_diff$difference < 0.05))
})
