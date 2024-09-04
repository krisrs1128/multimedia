

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