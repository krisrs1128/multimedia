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
  for (i in seq_along(profile@t_outcome)) {
    outcomes[[i]] <- bind_cols(
      pretreatment,
      profile@t_outcome[[i]],
      mediators
    ) |>
      x@outcome@sampler(x@outcome@estimates, newdata = _, indices = i)
  }

  list(mediators = mediators, outcomes = bind_cols(outcomes))
})

predict_across <- function(object, newdata, index = 1) {
  if (is(object@estimates, "list")) {
    return(object@predictor(object@estimates[[index]], newdata))
  }

  object@predictor(object@estimates, newdata)[, index]
}

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
        predict_across(object@mediation, newdata = _, i)
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
      predict_across(object@outcome, newdata = _, index = i) |>
      as.numeric()
  }
  list(mediators = mediators, outcomes = bind_cols(outcomes))
})
