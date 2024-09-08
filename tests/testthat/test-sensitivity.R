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

test_that("Raises error on inappropriate model input.", {
    model <- multimedia(exper, lnm_model())
    expect_error(check_supported(model))

    model <- multimedia(exper, brms_model())
    expect_error(check_supported(model))
})

pathwise <- sensitivity_pathwise(model, exper, subset_indices, rho_seq, n_bootstrap = 2)

test_that("Pathwise sensitivity curve has correct column names.", {
    expect_named(
        pathwise,
        c("rho", "outcome", "direct_setting", "contrast", "mediator",
          "indirect_effect", "indirect_effect_standard_error")
    )
})

test_that("Pathwise sensitivity covers all mediators", {
    expect_equal(nrow(pathwise), 4 * length(rho_seq))
    expect_true(all(mediators(model) %in% pathwise$mediator))
    expect_true(all(outcomes(model) %in% pathwise$outcome))
})

perturb <- matrix(
    c(
        0, 3, 0,
        3, 0, 0,
        0, 0, 0
    ),
    nrow = 3, byrow = TRUE
)
nu_seq <- c(-0.2, 0, 0.2)
curve <- sensitivity_perturb(model, exper, perturb, nu_seq, n_bootstrap = 2)

test_that("Perturbation sensitivity has the correct column names", {
    expect_named(
        curve,
        c("nu", colnames(overall), "indirect_effect_standard_error")
    )
})

test_that("All perturbation values appear in the sensitivity curve.", {
    expect_equal(unique(curve$nu), nu_seq)
})

test_that("All outcomes appear in the sensitivity curve.", {
    expect_equal(unique(curve$outcome), outcomes(model))
})