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
        x@mediation@sampler(x@mediation@estimates, new_data = _, i)
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
      x@outcome@sampler(x@outcome@estimates, new_data = _, i)
  }

  list(mediators = mediators, outcomes = bind_cols(outcomes))
})

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
      mediator_covariates <- bind_cols(pretreatment, profile@t_mediator[[i]])
      mediators[[nm[i]]] <- predict(
        object@mediation@estimates[[i]],
        mediator_covariates
      )
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
      predict(object@outcome@estimates[[i]], newdata = _) |>
      as.numeric()
  }
  list(mediators = mediators, outcomes = bind_cols(outcomes))
})
