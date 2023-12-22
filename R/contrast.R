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
  for (i in seq_len(nrow(t_))) {
    profile1 <- setup_profile(model, t_[i, ], t_[t1, ])
    profile2 <- setup_profile(model, t_[i, ], t_[t2, ])

    y_hat <- contrast_predictions(
      model,
      profile1,
      profile2,
      pretreatment = pretreatment
    )[["outcomes"]] |>
    colMeans()

    result[[i]] <- tibble(
      outcome = names(y_hat),
      direct_effect = y_hat,
      contrast = parse_name(t_, t1, t2)
    )
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

  t_ <- model@treatments
  result <- list()
  for (i in seq_len(nrow(t_))) {
    profile1 <- setup_profile(model, t_[t1, ], t_[i, ])
    profile2 <- setup_profile(model, t_[t2, ], t_[i, ])

    y_hat <- contrast_predictions(
      model,
      profile1,
      profile2,
      pretreatment = pretreatment
    )[["outcomes"]] |>
      colMeans()

    result[[i]] <- tibble(
      outcome = names(y_hat),
      indirect_effect = y_hat,
      contrast = parse_name(t_, t1, t2)
    )
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
#' @importFrom cli cli_text
#' @export
indirect_pathwise <- function(model, exper = NULL, t1 = 1, t2 = 2) {
  pretreatment <- NULL
  if (!is.null(exper)) {
    pretreatment <- exper@pretreatments
  }

  k <- 1
  m <- mediators(model)
  t_ <- model@treatments
  result <- list()
  for (i in seq_len(nrow(t_))) {
    cli_text(glue("Indirect effects for direct setting {i}"))
    profile2 <- setup_profile(model, t_[t2, ], t_[i, ])
    y_hat_2 <- predict(model, profile2, pretreatment = pretreatment)

    pb <- progress_bar$new(total = length(m), format = "[:bar] Mediator: :current/:total ETA: :eta")
    for (j in seq_along(m)) {
      profile1 <- profile2
      profile1@t_mediator[[j]] <- t_[t1, ]
      y_hat_1 <- predict(model, profile1, pretreatment = pretreatment)
      y_hat <- colMeans(path_difference(y_hat_1, y_hat_2)[["outcomes"]])

      result[[k]] <- tibble(
        outcome = names(y_hat),
        indirect_effect = y_hat,
        mediator = m[j],
        contrast = parse_name(t_, t1, t2),
        direct_setting = t_[[1]][i]
      )
      pb$tick()
      k <- k + 1
    }
  }

  bind_rows(result) |>
    select(outcome, mediator, direct_setting, contrast, indirect_effect)
}
