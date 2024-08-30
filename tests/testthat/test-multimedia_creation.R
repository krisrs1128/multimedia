test_that("Can create a multimedia object from SummarizedExperiment", {
    m <- mediation_data(demo_joy(), "PHQ", "treatment", dplyr::starts_with("ASV"))
    expect_equal(class(m@treatments), "data.frame")
    expect_equal(dim(m@treatments), c(100, 1))

    m <- multimedia:::from_summarized_experiment(demo_joy(), "PHQ", "treatment", dplyr::starts_with("ASV"), NULL)
    model <- multimedia(m)
    expect_equal(mediators(model), paste0("ASV", 1:5))
})

data(mindfulness)
test_that("Can create a multimedia object from phyloseq", {
    m <- mediation_data(mindfulness, phyloseq::taxa_names(mindfulness), "treatment", dplyr::starts_with("mediator"))
    model <- multimedia(m)
    expect_equal(sort(outcomes(model)), sort(phyloseq::taxa_names(mindfulness)))
})
