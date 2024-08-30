set.seed(20240826)

# Fit random forest on toy data
xy_data <- demo_spline()
exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
fit <- multimedia(exper, outcome_estimator = rf_model(num.trees = 1e3)) |>
    estimate(exper)

test_that("Can estimate direct effects on toy dataset.", {
    effects <- direct_effect(fit) |>
        effect_summary()
    expect_s3_class(effects, "tbl_df")
    expect_equal(effects$outcome, outcomes(fit))
})

test_that("Can estimate indirect effects on toy dataset.", {
    effects <- indirect_overall(fit) |>
        effect_summary()
    expect_s3_class(effects, "tbl_df")
    expect_equal(effects$outcome, outcomes(fit))
})

test_that("Can estimate indirect pathwise effects on toy dataset.", {
    effects <- indirect_pathwise(fit) |>
        effect_summary()
    expect_s3_class(effects, "tbl_df")
    expect_equal(effects$outcome, outcomes(fit))
})
