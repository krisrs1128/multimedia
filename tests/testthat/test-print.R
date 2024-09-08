test_that("ANSI handler converts R code to HTML tagged version", {
    output <- ansi_aware_handler("test")
    expect_equal(output, "<pre class=\"r-output\"><code>test</code></pre>")
})

test_that("Show methods give expected output for mediation data object.", {
    exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
    expect_output(print(exper), "\\[Mediation Data\\]")
    expect_output(print(exper), "100 samples with measurements for,")
})

test_that("vec_sub correctly truncates long outputs", {
    expect_equal(multimedia:::vec_sub(LETTERS[1:10]), "A, B, ...")
    expect_equal(multimedia:::vec_sub(1:10), "1, 2, ...")
})

test_that("Show methods give expected output for a fitted multimedia object.", {
    exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
    model <- multimedia(exper) |>
        estimate(exper)
    expect_output(print(model), "\\[Multimedia Analysis\\]")
    expect_output(print(model), "Treatments: treatment")
    expect_output(print(model), "Mediators: ASV1, ASV2, ...")
    expect_output(print(model), "\\[Models\\]")
    expect_output(print(model), "mediation: A fitted")
    expect_output(print(model), "outcome: A fitted")
})
