set.seed(20240826)

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

test_that("Can compile and estimate a BRMS model.", {
    model <- multimedia(exper, brms_model())
    expect_equal(outcome_model(model)@model_type, "brms_model()")
    model <- estimate(model, exper)
    expect_s4_class(model, "multimedia")
    y_hat <- predict(model)
    expect_named(y_hat, c("mediators", "outcomes"))
    expect_named(y_hat$mediators, paste0("ASV", 1:5))

    samples <- multimedia:::brms_sampler(model@outcome@estimates)
    expect_named(samples, "PHQ")
    expect_equal(dim(samples), c(100, 1))
})