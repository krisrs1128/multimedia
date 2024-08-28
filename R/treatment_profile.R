#' Fail Fast for Poorly Specified Profiles
#'
#' Specifying counterfactuals in general mediation analysis is tricky. We
#' need to have a hypothetical treatment that applies to the mediator and
#' outcome separately, even though in reality only one treatment can ever be
#' assigned to a single treatment. This helper checks that the specification of
#' mediator and outcome treatments is reasonable.
#' @param object An object of class `treatment_profile` that we want to check.
#' @importFrom dplyr n_distinct
#' @importFrom purrr map
#' @importFrom cli cli_abort
#' @noRd
check_profile <- function(object) {
    if (is(object@t_mediator, "list")) {
        m_samples <- map(object@t_mediator, nrow)
        y_samples <- map(object@t_outcome, nrow)
        if (n_distinct(y_samples) != 1) {
            cli_abort(
                "Number of rows in treatment profile must
                agree across all outcomes."
            )
        }

        if (!all(m_samples %in% y_samples)) {
            cli_abort(
                "Number of samples in treatment profile must
                agree across mediators and outcomes."
            )
        }
    }
}

#' Define a `treatment_profile` object
#'
#' For general mediation analysis, we need to provide counterfactuals for both
#' the outcome and mediator components of each sample. That is, we need to
#' understand Y(t, M(t')) where t and t' may not be the same.
#' `treatment_profile` classes place some more structural requirements on
#' treatment profiles, so that later effect estimation can make simplifying
#' assumptions. This function creates a treatment profile from a collection of
#' possible mediator and outcome treatments.
#'
#' @param x An object of class `multimedia` specifying the complete mediation
#'   analysis DAG. The treatment, mediator, and outcome names are necessary to
#'   build a profile of counterfactual treatments across each of these
#'   variables.
#' @param t_mediator A data.frame whose columns store treatment names and whose
#'   values are the treatment assignments to each sample (row). Defaults to
#'   NULL, in which case this type of data.frame is constructed from the
#'   treatment assignments in the `mediation_data`'s `@treatment` slot. Each
#'   column must be either a numeric or factor variable.
#' @param t_outcome A data.frame analogous to `t_mediator`, but applying to the
#'   outcome node.
#' @return An object of class `treatment_profile` giving treatment assignments
#'   for both mediation and outcome terms.
#' @importFrom purrr map_lgl set_names
#' @seealso check_profile
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#'
#' t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 5)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
#' profile
#'
#' t2 <- data.frame(treatment = factor(rep(0, 10)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t2)
#' profile
#' @export
setup_profile <- function(x, t_mediator = NULL, t_outcome = NULL) {
    # default mediator and outcome
    if (is.null(t_mediator)) {
        t_mediator <- x@treatments
    }
    if (is.null(t_outcome)) {
        t_outcome <- x@treatments
    }

    # if given as a vector, convert to a data.frame
    if (is.factor(t_mediator) | is.numeric(t_mediator)) {
        t_mediator <- data.frame(t_mediator) |>
            set_names(treatments(x))
    }
    if (is.factor(t_outcome) | is.numeric(t_outcome)) {
        t_outcome <- data.frame(t_outcome) |>
            set_names(treatments(x))
    }

    # apply treatments to all mediators/outcomes
    if (!is(t_mediator, "list")) {
        t_mediator <- t_mediator |>
            replicate(n_mediators(x), expr = _, simplify = FALSE) |>
            set_names(mediators(x))
    }
    if (!is(t_outcome, "list")) {
        t_outcome <- t_outcome |>
            replicate(n_outcomes(x), expr = _, simplify = FALSE) |>
            set_names(outcomes(x))
    }

    if (any(map_lgl(t_mediator[[1]], is.character)) ||
        any(map_lgl(t_outcome[[1]], is.character))) {
        cli_abort(
            "All treatment columns must be either numeric or factor variables.
        Character column detected."
        )
    }

    new("treatment_profile", t_mediator = t_mediator, t_outcome = t_outcome)
}

#' Define a Treatment Profile
#'
#' This class ensures appropriate structure of the treatment assignments for
#' mediator and outcome variables. It enforces certain structural requirements
#' (e.g., that the number of samples is the same under the mediator and outcome
#' counterfactuals) using the `check_profile` function.
#' @seealso setup_profile check_profile
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#'
#' # helpers for defining treatment profiles
#' t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 5)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
#' profile
#'
#' t2 <- data.frame(treatment = factor(rep(0, 10)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t2)
#' profile
#' @export
setClass(
    "treatment_profile",
    representation(
        t_mediator = "ANY",
        t_outcome = "ANY"
    ),
    validity = check_profile
)
