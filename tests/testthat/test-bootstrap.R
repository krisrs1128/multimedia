set.seed(20240826)

# Joy dataset example
exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))

test_that("Bootstrap with RF works in a simple case.", {
    model <- multimedia(exper) |>
        estimate(exper)

    fs <- list(direct = direct_effect)
    samples <- bootstrap(model, exper, fs, B = 2)
    expect_equal(names(samples), "direct")
    expect_equal(
        colnames(samples$direct),
        c("bootstrap", "outcome", "indirect_setting", "contrast", "direct_effect")
    )
    expect_equal(unique(samples$direct$bootstrap), seq_len(2))
})

test_that("Bootstrap with glmnet works in a simple case", {
    model <- multimedia(exper, glmnet_model(lambda = .1)) |>
        estimate(exper)

    fs <- list(direct = direct_effect)
    samples <- bootstrap(model, exper, fs, B = 2)
    expect_equal(names(samples), "direct")
    expect_equal(
        colnames(samples$direct),
        c("bootstrap", "outcome", "indirect_setting", "contrast", "direct_effect")
    )
    expect_equal(unique(samples$direct$bootstrap), seq_len(2))
})
