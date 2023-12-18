#' @importFrom tibble as_tibble
random_numeric <- function(nrow, ncol, p = rnorm) {
  matrix(p(nrow * ncol), nrow = nrow, ncol = ncol) |>
    as_tibble() |>
    rename_with(~ str_remove(., "V"))
}

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

#' @export
demo_police <- function(n_samples = 100, n_outcomes = 20, n_mediators = 5) {
  treatment <- sample(c("Treatment", "Control"), n_samples, replace = TRUE)
  mediators <- random_numeric(n_samples, n_mediators) |>
    rename_with(~ glue("Diet{.}"))
  col_data <- random_numeric(n_samples, n_outcomes, \(n) rpois(n, 100)) |>
    rename_with(~ glue("ASV{.}")) |>
    bind_cols(treatment = treatment) |>
    DataFrame()
  SummarizedExperiment(
    assays = SimpleList(counts = t(as.matrix(mediators))),
    colData = col_data
  )
}