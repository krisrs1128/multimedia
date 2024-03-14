#' Sample New Mediator/Outcome Data
#'
#' This generalizes the built-in sample method to the multimedia class. Given an
#' estimated multimedia object, this function supports sampling along the
#' estimated DAG. It first samples `M* | T, X` and then `Y* | M*, T, X`. Each
#' sampling step will call the sample method within the mediation and outcome
#' models that make up the multimedia object on which this is called.
#'
#' @export
setMethod("sample", "multimedia", function(
    x, size, pretreatment = NULL,
    profile = NULL,
    mediators = NULL, ...) {
  if (missing(size)) {
    size <- 1
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
predict_across <- function(object, newdata, name) {
  if (is(object@estimates, "list")) {
    return(object@predictor(object@estimates[[name]], newdata))
  }

  object@predictor(object@estimates, newdata)[, name]
}

#' Predictions from a Multimedia Class
#'
#' This generalizes the built-in predict method to the multimedia class.  Given
#' an estimated multimedia object, this function supports prediction along the
#' estimated DAG. It first predicts `hat[M] | T, X` and then `hat[Y] | hat[M],
#' T, X`. Each prediction step will call the prediction method within the
#' mediation and outcome models that make up the multimedia object on which this
#' is called.
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
