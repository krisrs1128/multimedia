exper <- demo_joy() |>
    mediation_data("PHQ", "treatment", starts_with("ASV"))
fit <- multimedia(exper) |>
    estimate(exper)

test_that("Can nullify treatment to mediator path", {
    nullified <- nullify(fit, "T->M")
    edges <- nullified@edges |>
        tidygraph::activate(edges) |>
        as.data.frame()
    expect_contains(edges$state, c("active", "inactive"))
    expect_equal(as.integer(table(edges$state)), c(12, 5))
})

test_that("Can nullify the treatment to outcome path.", {
    nullified <- nullify(fit, "T->Y")
    edges <- nullified@edges |>
        tidygraph::activate(edges) |>
        as.data.frame()
    expect_contains(edges$state, c("active", "inactive"))
    expect_equal(as.integer(table(edges$state)), c(16, 1))
})

test_that("Can nullify the mediator to outcome path.", {
    nullified <- nullify(fit, "M->Y")
    edges <- nullified@edges |>
        tidygraph::activate(edges) |>
        as.data.frame()
    expect_contains(edges$state, c("active", "inactive"))
    expect_equal(as.integer(table(edges$state)), c(12, 5))
    expect_true(all(edges$state[12:16] == "inactive"))
})

test_that("Can contrast data simulated from real and synthetic.", {
    contrast_data <- fit |>
        null_contrast(exper)
    expect_named(
        contrast_data,
        c("source", "outcome", "indirect_setting", "contrast", "direct_effect")
    )
    expect_equal(unique(contrast_data$source), c("real", "synthetic"))
})

test_that("Can compute false discovery rates given contrast data.", {
    fdr_data <- fit |>
        null_contrast(exper) |>
        fdr_summary("direct_effect")
    expect_named(
        fdr_data,
        c("source", "outcome", "direct_effect", "rank", "fdr_hat", "keep")
    )
    expect_equal(sort(fdr_data$source), c("real", "synthetic"))

    fdr_data <- fit |>
        null_contrast(exper, "M->Y", indirect_overall) |>
        fdr_summary("indirect_overall")
    expect_named(
        fdr_data,
        c("source", "outcome", "indirect_effect", "rank", "fdr_hat", "keep")
    )
    expect_equal(sort(fdr_data$source), c("real", "synthetic"))

    fdr_data <- fit |>
        null_contrast(exper, "M->Y", indirect_pathwise) |>
        fdr_summary("indirect_pathwise")
    expect_named(
        fdr_data,
        c("source", "outcome", "mediator", "indirect_effect", "rank", "fdr_hat", "keep")
    )
    expect_equal(sort(unique(fdr_data$source)), c("real", "synthetic"))
})
