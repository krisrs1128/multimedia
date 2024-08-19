#' Sample New Mediator/Outcome Data
#'
#' This generalizes the built-in sample method to the multimedia class. Given an
#' estimated multimedia object, this function supports sampling along the
#' estimated DAG. It first samples `M* | T, X` and then `Y* | M*, T, X`. Each
#' sampling step will call the sample method within the mediation and outcome
#' models that make up the multimedia object on which this is called.
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'   estimate(exper)
#' samples <- sample(fit)
#' samples@mediators
#' samples@outcomes
#'
#' # sampling with just different "n" has no effect.
#' samples <- sample(fit, 100)
#'
#' # Instead sample at a new treatment configuration
#' t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 50)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
#' samples <- sample(fit, profile = profile)
#' samples@mediators
#' samples@outcomes
#' @export
setMethod("sample", "multimedia", function(
    x, size, pretreatment = NULL,
    profile = NULL,
    mediators = NULL, ...) {
  if (missing(size)) {
    size <- 1
  } else {
    cli_warn("The size argument in sample is ignored. Please adjust the treatment profile to adjust the number of samples.")
  }
  if (is.null(profile)) {
    profile <- setup_profile(x)
  }

  # if mediators are not provided, sample them
  if (is.null(mediators)) {
    mediators <- list()
    for (i in seq_along(profile@t_mediator)) {
      mediators[[i]] <- bind_cols(pretreatment, profile@t_mediator[[i]]) |>
        x@mediation@sampler(x@mediation@estimates, newdata = _, indices = i, ...)
    }
    mediators <- bind_cols(mediators)
  }

  # sample outcome given everything else
  outcomes <- list()
  nm <- names(profile@t_outcome)
  for (i in seq_along(profile@t_outcome)) {
    outcomes[[nm[i]]] <- bind_cols(
      pretreatment,
      profile@t_outcome[[i]],
      mediators
    ) |>
      x@outcome@sampler(x@outcome@estimates, newdata = _, indices = i)
  }
  outcomes <- bind_cols(outcomes)

  new("mediation_data",
    outcomes = outcomes,
    mediators = mediators,
    treatments = profile@t_outcome[[1]],
    pretreatments = pretreatment
  )
})

#' Predictions from a Single Outcome
#'
#' @param object An object of class `model` containing an estimated model.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param name A string or index specifying which of the dimensions of a
#'   multiresponse prediction to extract.
#' @return A vector of predicted values for the outcome of interest.
#' @noRd
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'   estimate(exper)
#' multimedia:::predict_across(fit@outcome, NULL, "outcome_1")
#' multimedia:::predict_across(fit@outcome, NULL, "outcome_2")
#'
#' # predict at newdata
#' newdata <- bind_mediation(exper)
#' multimedia:::predict_across(fit@outcome, newdata[1:5, ], "outcome_2")
#' @importFrom purrr map_dfc
predict_across <- function(object, newdata, name) {
  # many univariate models
  if (is(object@estimates, "list")) {
    if (length(name) == 1) {
      return(object@predictor(object@estimates[[name]], newdata))
    } else {
      predictions <- suppressMessages({
        map_dfc(name, ~ object@predictor(object@estimates[[.]], newdata))
      })
      colnames(predictions) <- name
      return(predictions)
    }
  }

  # single multivariate model
  object@predictor(object@estimates, newdata)[, name]
}

#' Predictions from a Multimedia Class
#'
#' This generalizes the built-in predict method to the multimedia class.  Given
#' an estimated multimedia object, this function supports prediction along the
#' estimated DAG. It first predicts `hat[M] | T, X` and then `hat[Y] | hat[M],
#' T, X`. Each prediction step will call the prediction method within the
#' mediation and outcome models that make up the multimedia object on which this
#' is called. By passing in new treatment, mediator, or pretreatment data, you
#' can predict forward along the DAG using these as inputs.
#' @param object An object of class multimedia containing the estimated
#'   mediation and outcome models whose mediation and outcome predictions we
#'   want to obtain.
#' @param profile An object of class `treatment_profile` containing the
#'   treatment profile to consider in the difference. Defaults to a profile with
#'   all the unique treatment configurations observed in the original data,
#'   shared across both the mediators and outcomes.
#' @return A list with two elements:
#'   $mediators: A data.frame containing predicted values for the mediators. Each
#'   row corresponds to one row of the newdata, or one row of the default
#'   treatment profile, if no newdata is given.
#'
#'   $outcomes: A data.frame containing predicted values for the outcomes, given
#'   either (i) the predicted values of the mediators or (ii) the provided
#'   values of the mediators. Each row corresponds to one row of the newdata, or
#'   one row of the default treatment profile, if no newdata is given.
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper, rf_model()) |>
#'   estimate(exper)
#' predict(fit)
#'
#' # at new treatment configurations
#' t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 5)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
#' predict(fit, profile)
#'
#' # at new treatment and mediator configurations
#' mediators <- data.frame(mediator = rnorm(10, 0, 1))
#' predict(fit, profile, mediators)
#' @export
setMethod("predict", "multimedia", function(
    object, profile = NULL, mediators = NULL, pretreatment = NULL,
    ...) {
  if (is.null(profile)) {
    profile <- setup_profile(object)
  }
  if (is.null(mediators)) {
    nm <- names(profile@t_mediator)
    mediators <- list()
    for (i in seq_along(profile@t_mediator)) {
      mediators[[nm[i]]] <- bind_cols(pretreatment, profile@t_mediator[[i]]) |>
        predict_across(object@mediation, newdata = _, name = nm[i])
    }
    mediators <- bind_cols(mediators)
  }

  # predict outcome given everything else
  nm <- names(profile@t_outcome)
  outcomes <- list()
  for (i in seq_along(profile@t_outcome)) {
    outcomes[[nm[i]]] <- bind_cols(
      pretreatment,
      profile@t_outcome[[i]],
      mediators
    ) |>
      predict_across(object@outcome, newdata = _, name = nm[i]) |>
      as.numeric()
  }
  list(mediators = mediators, outcomes = bind_cols(outcomes))
})
