#' data.frame with samples from p
#' @importFrom tibble as_tibble
random_numeric <- function(nrow, ncol, p = rnorm) {
  matrix(p(nrow * ncol), nrow = nrow, ncol = ncol) |>
    as_tibble() |>
    rename_with(~ str_remove(., "V"))
}

#' A Demo Dataset
#'
#' This is a simple dataset with random data, used simply to illustrate the
#' design of multimedia. There is no real association between any treatments,
#' mediators, or outcomes. It always returns a single outcome (PHQ)
#' SummarizedExperiment, and it randomly assigns samples to Treatment and
#' Control (see the colData). It stores all the hypothetical mediator data in
#' the assay slot.
#'
#' @param n_samples How many random samples to generate?
#' @param n_mediators How many random mediators to generate?
#' @param n_pretreatment How many random pretreatment variables?
#' @return SE The summarized experiment containing random data.
#' @importFrom glue glue
#' @importFrom dplyr bind_cols rename_with
#' @importFrom tibble tibble
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @export
demo_joy <- function(n_samples = 100, n_mediators = 5, n_pretreatment = 3) {
  treatment <- sample(c("Treatment", "Control"), n_samples, replace = TRUE)
  mediators <- random_numeric(n_samples, n_mediators) |>
    rename_with(~ glue("ASV{.}"))
  outcome <- random_numeric(n_samples, 1)[[1]]

  SummarizedExperiment(
    assays = SimpleList(counts = t(as.matrix(mediators))),
    colData = DataFrame(treatment = treatment, PHQ = outcome)
  )
}
