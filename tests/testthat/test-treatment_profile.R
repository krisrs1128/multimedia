exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
fit <- multimedia(exper) |>
    estimate(exper)

test_that("Can create a treatment profile from a data.frame.", {
    t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 5)))
    profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
    expect_s4_class(profile, "treatment_profile")
    expect_named(profile@t_mediator, paste0("ASV", 1:5))
    expect_named(profile@t_outcome, "PHQ")
    expect_equal(
        unique(profile@t_mediator$ASV1$treatment),
        as.factor(c("0", "1"))
    )
    expect_s3_class(profile@t_mediator$ASV1$treatment, "factor")
})

test_that("Can create treatment profile from a vector.", {
    t1 <- rep(0:1, 5)
    profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
    expect_s4_class(profile, "treatment_profile")
    expect_named(profile@t_mediator, paste0("ASV", 1:5))
    expect_named(profile@t_outcome, "PHQ")
    expect_equal(
        unique(profile@t_mediator$ASV1$treatment),
        c(0, 1)
    )
    expect_type(profile@t_mediator$ASV1$treatment, "integer")
    expect_equal(profile@t_mediator$ASV1$treatment, t1)
})

test_that("Can create treatment profile from scratch.", {
    t1 <- list(m = data.frame(treatment = rep(0:1, 5)))
    profile <- new("treatment_profile", t_mediator = t1, t_outcome = t1)
    expect_s4_class(profile, "treatment_profile")
    t2 <- list(t = data.frame(treatment = rep(0:1, 2)))
    expect_error(new("treatment_profile", t_mediator = t1, t_outcome = t2))
})

test_that("check_profile raises errors in invalid inputs.", {
    setClass("TestProfile", slots = c(t_mediator = "list", t_outcome = "list"))
    test_profile <- new(
        "TestProfile",
        t_mediator = list(),
        t_outcome = list(data.frame(A = 1:10), data.frame(A = 1:2))
    )
    expect_error(multimedia:::check_profile(test_profile))

    test_profile <- new(
        "TestProfile",
        t_mediator = list(data.frame(A = 1:10), data.frame(A = 1:10)),
        t_outcome = list(data.frame(A = 1:10), data.frame(A = 1:2))
    )
    expect_error(multimedia:::check_profile(test_profile))
})
