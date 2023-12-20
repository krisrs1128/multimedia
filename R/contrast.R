#' @export
contrast_predictions <- function(model, profile1, profile2, ...) {
  y_hat_1 <- predict(model, profile1, ...)
  y_hat_2 <- predict(model, profile2, ...)
  path_difference(y_hat_1, y_hat_2)
}

#' @export
contrast_samples <- function(model, profile1, profile2, ...) {
  y1 <- sample(model, profile = profile1, ...)
  y2 <- sample(model, profile = profile2, ...)
  path_difference(y1, y2)
}

path_difference <- function(y1, y2) {
  list(
    mediators = y1$mediators - y2$mediators,
    outcomes = y1$outcomes - y2$outcomes
  )
}

adjust_defaults <- function(model, t1 = NULL, t2 = NULL, t_mediator = NULL,
                            t_outcome = NULL) {
  if (is.null(t1) || is.null(t2)) {
    unique_treatments <- unique(setup_profile(model)@t_outcome[[1]])
    t1 <- unique_treatments[1, ]
    t2 <- unique_treatments[2, ]
  }

  if (is.character(t1) || is.character(t2)) {
    t1 <- tibble(factor(t1))
    t2 <- tibble(factor(t2))
    names(t1) <- treatments(model)[1]
    names(t2) <- treatments(model)[1]
  }

  if (is.null(t_mediator)) {
    t_mediator <- list(t1, t2)
    names(t_mediator) <- c(t1[[1]], t2[[1]])
  }

  if (is.null(t_outcome)) {
    t_outcome <- list(t1, t2)
    names(t_outcome) <- c(t1[[1]], t2[[1]])
  }

  if (is.null(names(t_mediator))) {
    names(t_mediator) <- seq_along(t_mediator)
  }

  if (is.null(names(t_outcome))) {
    names(t_outcome) <- seq_along(t_outcome)
  }

  list(t1 = t1, t2 = t2, t_mediator = t_mediator, t_outcome = t_outcome)
}

#' Direct Effects from Estimated Model
#' @importFrom dplyr everything mutate select bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @export
direct_effect <- function(model, exper = NULL, t1 = 1, t2 = 2) {
  pretreatment <- NULL
  if (!is.null(exper)) {
    pretreatment <- exper@pretreatments
  }

  result <- list()
  t_ <- model@treatments

  for (i in seq_along(nrow(t_))) {
    profile1 <- setup_profile(model, t_[i, ], model@treatments[t1, ])
    profile2 <- setup_profile(model, t_[i, ], model@treatments[t2, ])

    result[[i]] <- contrast_predictions(
      model,
      profile1,
      profile2,
      pretreatment = pretreatment
    )[["outcomes"]] |>
      pivot_longer(everything(), names_to = "outcome", values_to = "direct_effect") |>
      mutate(contrast = parse_name(t_, t1, t2))
  }

  bind_rows(result, .id = "indirect_setting") |>
    mutate(indirect_setting = t_[[1]][as.integer(indirect_setting)]) |>
    select(outcome, indirect_setting, contrast, direct_effect)
}

parse_name <- function(t_, t1, t2) {
  glue("{apply(t_[t1,], 1, paste0, collapse=',')} - {apply(t_[t2,], 1, paste0, collapse=',')}")
}

#' Overall Indirect Effect
#' @examples
#' t1 <- tibble(treatment = as.factor("Treatment"))
#' t2 <- tibble(treatment = as.factor("Control"))
#' t_outcome <- list(t1, t2)
#' indirect_overall(model, t1, t2, t_outcome)
#' indirect_overall(model, "Treatment", "Control")
#' @export
indirect_overall <- function(model, exper = NULL, t1 = 1, t2 = 2) {
  pretreatment <- NULL
  if (!is.null(exper)) {
    pretreatment <- exper@pretreatments
  }

  result <- list()
  t_ <- model@treatments

  for (i in seq_along(nrow(t_))) {
    profile1 <- setup_profile(model, model@treatments[t1, ], t_[i, ])
    profile2 <- setup_profile(model, model@treatments[t2, ], t_[i, ])

    result[[i]] <- contrast_predictions(
      model,
      profile1,
      profile2,
      pretreatment = pretreatment
    )[["outcomes"]] |>
      pivot_longer(
        everything(),
        names_to = "outcome",
        values_to = "indirect_effect"
      ) |>
      mutate(contrast = parse_name(t_, t1, t2))
  }

  bind_rows(result, .id = "direct_setting") |>
    mutate(direct_setting = t_[[1]][as.integer(direct_setting)]) |>
    select(outcome, direct_setting, contrast, indirect_effect)
}

#' Indirect Effects via Single Mediation Paths
#' @examples
#' model <- multimedia(
#'   demo_joy(),
#'   outcomes = "PHQ",
#'   treatments = "treatment",
#'   mediators = starts_with("ASV")
#' ) |> estimate(demo_joy())
#' t1 <- tibble(treatment = as.factor("Treatment"))
#' t2 <- tibble(treatment = as.factor("Control"))
#' t_outcome <- list(t1, t2)
#' indirect_pathwise(model, t1, t2, t_outcome)
#' indirect_pathwise(model, "Treatment", "Control")
#' @export
indirect_pathwise <- function(model, exper = NULL, t1 = 1, t2 = 2) {
  pretreatment <- NULL
  if (!is.null(exper)) {
    pretreatment <- exper@pretreatments
  }

  k <- 1
  result <- list()
  m <- mediators(model)
  for (i in seq_along(t_outcome)) {
    profile2 <- setup_profile(model, t2, t_outcome[[i]]) |>
      bind_cols(pretreatment)

    for (j in seq_along(m)) {
      profile1 <- profile2
      profile1@t_mediator[[j]] <- t1
      result[[k]] <- contrast_predictions(model, profile1, profile2)[["outcomes"]] |>
        pivot_longer(
          everything(),
          names_to = "outcome",
          values_to = "indirect_effect"
        ) |>
        mutate(
          mediator = m[j],
          contrast = parse_name(t1, t2),
          direct_setting = names(t_outcome)[i]
        )
      k <- k + 1
    }
  }

  bind_rows(result) |>
    select(outcome, mediator, direct_setting, contrast, indirect_effect)
}
