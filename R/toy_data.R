#' data.frame with samples from p
#' @importFrom tibble as_tibble
#' @examples
#' random_numeric(10, 2)
#' random_numeric(10, 2, runif)
#' @noRd
random_numeric <- function(nrow, ncol, p = rnorm) {
  matrix(p(nrow * ncol), nrow = nrow, ncol = ncol) |>
    as.data.frame() |>
    rename_with(~ gsub("V", "", .))
}

#' A Demo Dataset (Random)
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
#' @examples
#' demo_joy()
#' 
#' demo_joy(n_samples = 2, n_mediators = 20)
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

matnorm <- \(n, m, ...) matrix(rnorm(n * m, ...), n, m)

#' Generate Random Spline
#'
#' This generates random spline functions. It is used in the toy nonlinear
#' dataset created in `demo_spline()`. This is not necessary for the essential
#' multimedia workflow, it is only exported for potential independent interest.
#'
#' @seealso demo_spline
#' @examples
#' x <- seq(-2, 2, length.out = 100)
#' f <- spline_fun(sd = 0.3)
#' fx <- f(x)
#' plot(x, fx[, 1])
#' @noRd
spline_fun <- function(D = 2, knots = NULL, h_ix = 1:10, ...) {
  if (is.null(knots)) {
    knots <- seq(-4, 4, length.out = 5)
  }

  h_dim <- splines::ns(h_ix, knots = knots, intercept = TRUE)
  B <- matnorm(D, ncol(h_dim))
  function(x) {
    H <- splines::ns(x, knots = knots, intercept = TRUE)
    scale(H %*% t(B) + rnorm(nrow(H), ...))
  }
}

#' A Demo Dataset (Spline)
#'
#' This is a simple dataset with nonlinear relationships between the outcome and
#' mediators. It is used simply to illustrate the design of multimedia. The
#' mediator->outcome effect is generated from a random spline function.
#'
#' @param n_samples The number of samples to generate in the toy example
#' @param tau The true direct efefcts associated with the two outcomes. Defaults
#'   to 2, 2.
#' @return xy A tibble whose columns include the treatment, mediation, and
#'   outcome variables.
#' @examples
#' demo_spline()
#' demo_spline(20)
#' demo_spline(20, c(0, 10))
#' @export
demo_spline <- function(n_samples = 5e3, tau = c(2, 2)) {
  if (n_samples < 15) {
    cli_abort("Spline generation requires at least 15 samples.")
  }

  treatment <- rep(c(0, 1), each = n_samples / 2)
  mediator <- rnorm(n_samples, 3 * (treatment - 0.5), 2)
  f <- spline_fun(sd = 0.2)
  y <- f(mediator) + matrix(treatment, ncol = 1) %*% tau
  colnames(y) <- glue("outcome_{1:2}")
  bind_cols(y, mediator = mediator, treatment = factor(treatment))
}
