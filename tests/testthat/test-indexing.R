demo_joy()

exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
test_that("Can successfully subset rows of an experiment", {
    subset <- exper[1, ]
    expect_equal(nrow(subset), 1)
    expect_equal(subset@outcomes$PHQ[1], exper@outcomes$PHQ[1])
    expect_equal(subset@mediators$ASV1[1], exper@mediators$ASV1[1])
    expect_equal(subset@treatments$treatment[1], exper@treatments$treatment[1])
})

test_that("Can successfully bootstrap resample rows of an experiment", {
    ix <- sample(nrow(exper), nrow(exper))
    exper2 <- exper[ix, ]
    expect_equal(nrow(exper2), nrow(exper))
    expect_equal(exper2@treatments$treatment, exper@treatments$treatment[ix])
})
