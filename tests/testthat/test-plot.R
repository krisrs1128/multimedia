exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
ie <- multimedia(exper) |>
    estimate(exper) |>
    indirect_pathwise() |>
    effect_summary()

test_that("Plot mediators returns on standard pathwise output.", {
    g <- plot_mediators(ie, exper)
    expect_s3_class(g, "patchwork")
})

test_that("Can customize number of digits in plot_mediators output.", {
    g <- plot_mediators(ie, exper, n_digit = 1)
    expect_s3_class(g, "patchwork")
    g <- plot_mediators(ie, exper, n_digit = 3)
    expect_s3_class(g, "patchwork")
})

test_that("We can plot the sensitivity data.", {
    xy_data <- demo_spline()
    exper <- mediation_data(
        xy_data,
        starts_with("outcome"),
        "treatment",
        "mediator"
    )
    model <- multimedia(exper, outcome_estimator = glmnet_model(lambda = 1e-2)) |>
        estimate(exper)
    overall <- indirect_overall(model)

    subset_indices <- expand.grid(
        mediator = n_mediators(model),
        outcome = n_outcomes(model)
    )
    rho_seq <- c(-0.2, 0.2)
    sensitivity_curve <- sensitivity(
        model, exper, subset_indices, rho_seq,
        n_bootstrap = 2
    )

    g <- plot_sensitivity(sensitivity_curve)
    expect_s3_class(g, "gg")
})
