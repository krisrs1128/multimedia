#' data.frame with samples from p
#' @importFrom stats rnorm
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

#' @importFrom stats rnorm
matnorm <- \(n, m, ...) matrix(rnorm(n * m, ...), n, m)

#' Generate Random Spline
#'
#' This generates random spline functions. It is used in the toy nonlinear
#' dataset created in `demo_spline()`. This is not necessary for the essential
#' multimedia workflow, it is only exported for potential independent interest.
#'
#' @param D The number random spline functions to generate internally.
#' @param knots The location of knots to use in the spline functions. Defaults
#'   to -4, -2, 0, 2, 4.
#' @param h_ix The locations along which to generate the underlying spline
#'   function.
#' @param ... Additional arguments to pass to `rnorm` during the random noise
#'   generation for each call of the returned function.
#' @return A function that can be used to sample points along the random spline
#'   functions. Takes argument 'x' giving the input to the spline and returns a
#'   D-dimensional response y giving random samples for each of the D splines..
#' @importFrom stats rnorm
#' @seealso demo_spline
spline_fun <- function(D = 2, knots = NULL, h_ix = seq_len(10), ...) {
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
#' @param tau The true direct effects associated with the two outcomes. Defaults
#'   to 2, 2.
#' @return xy A data.frame whose columns include the treatment, mediation, and
#'   outcome variables.
#' @importFrom stats rnorm
#' @examples
#' demo_spline()
#' @export
demo_spline <- function(n_samples = 5e3, tau = c(2, 2)) {
    if (n_samples < 15) {
        cli_abort("Spline generation requires at least 15 samples.")
    }

    treatment <- rep(c(0, 1), each = n_samples / 2)
    mediator <- rnorm(n_samples, 3 * (treatment - 0.5), 2)
    f <- spline_fun(sd = 0.2)
    y <- f(mediator) + matrix(treatment, ncol = 1) %*% tau
    colnames(y) <- glue("outcome_{seq_len(2)}")
    bind_cols(y, mediator = mediator, treatment = factor(treatment))
}

#' Mindfulness Dataset
#'
#' Data from a study of the relationship between mindfulness and the microbiome,
#' stored as a phyloseq object. Measurements are from 50 subjects before and
#' after a real and active control mindfulness intervention. We are interested
#' in changes in subject-level metadata, stored in the sample_data slot
#' @format An object of class phyloseq.
#' @return A phyloseq object with 307 samples and 55 taxa. Samples are described
#'   by 7 variables (potential mediators) in the sample_data slot. There is no
#'   associated phylogenetic tree.
#' @name mindfulness
#' @docType data
#' @keywords data
#' @examples
#' data(mindfulness)
NULL
