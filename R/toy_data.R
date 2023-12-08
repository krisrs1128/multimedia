#' @importFrom tibble as_tibble
random_numeric <- function(nrow, ncol, p = \(n) rnorm(n)) {
  matrix(p(nrow * ncol), nrow = nrow, ncol = ncol) |>
    as_tibble()
}

#' @importFrom glue glue
#' @importFrom dplyr bind_cols rename_with
#' @importFrom tibble tibble
#' @importFrom S4Vectors SimpleList DataFrame
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @export
demo_joy <- function(n_samples = 100, n_taxa = 20, n_mediators = 5,
                     n_pretreatment = 3) {
  treatment <- sample(c("Treatment", "Control"), n_samples, replace = TRUE)
  mediators <- random_numeric(n_samples, n_mediators) |>
    rename_with(~ glue("AS{.}"))
  outcome <- random_numeric(n_samples, 1)[[1]]

  SummarizedExperiment(
    assays = SimpleList(counts = t(as.matrix(mediators))),
    colData = DataFrame(treatment = treatment, PHQ = outcome)
  )
}
