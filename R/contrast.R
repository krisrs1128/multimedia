contrast_predictions <- function(model, profile1, profile2) {
  y_hat_1 <- predict(model, profile1)
  y_hat_2 <- predict(model, profile2)
  path_difference(y_hat_1, y_hat_2)
}

contrast_samples <- function(model, profile1, profile2) {
  y1 <- sample(model, profile1)
  y2 <- sample(model, profile2)
  path_difference(y1, y2)
}

path_difference <- function(y1, y2) {
  list(
    mediators = y1$mediators - y2$mediators,
    outcomes = y1$outcomes - y2$outcomes
  )
}

adjust_defaults <- function(model, t1 = NULL, t2 = NULL, t_mediator = NULL,
                            t_outcome) {
  if (is.null(t1) | is.null(t2)) {
    unique_treatments <- unique(setup_profile(model)@t_outcome[[1]])
    t1 <- unique_treatments[1, ]
    t2 <- unique_treatments[2, ]
  }

  if (is.character(t1) | is.character(t2)) {
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

  list(t1 = t1, t2 = t2, t_mediator = t_mediator, t_outcome = t_outcome)
}

direct_effect <- function(model, t1 = NULL, t2 = NULL, t_mediator = NULL) {
  t_prep <- adjust_defaults(model, t1, t2, t_mediator)
  t1 <- t_prep$t1
  t2 <- t_prep$t2
  t_mediator <- t_prep$t_mediator

  result <- list()
  for (i in seq_along(t_mediator)) {
    profile1 <- setup_profile(model, t_mediator[[i]], t1)
    profile2 <- setup_profile(model, t_mediator[[i]], t2)
    result[[i]] <- contrast_predictions(model, profile1, profile2)[["outcomes"]] |>
      pivot_longer(everything(), names_to = "outcome", values_to = "direct_effect") |>
      mutate(contrast = glue("{apply(t1, 1, paste0, collapse=',')} - {apply(t2, 1, paste0, collapse=',')}"))
  }

  if (is.null(names(t_mediator))) {
    names(t_mediator) <- seq_along(t_mediator)
  }

  bind_rows(result, .id = "indirect_setting") |>
    mutate(indirect_setting = names(t_mediator)[as.integer(indirect_setting)]) |>
    select(outcome, indirect_setting, contrast, direct_effect)
}

indirect_overall <- function(model, t1 = NULL, t2 = NULL, t_outcome = NULL) {
  t_prep <- adjust_defaults(model, t1, t2, NULL, t_outcome)
  t1 <- t_prep$t1
  t2 <- t_prep$t2
  t_outcome <- t_prep$t_outcome

  result <- list()
  for (i in seq_along(t_outcome)) {
    profile1 <- setup_profile(model, t1, t_outcome[[i]])
    profile2 <- setup_profile(model, t2, t_outcome[[i]])
    result[[i]] <- contrast_predictions(model, profile1, profile2)[["mediators"]] |>
      pivot_longer(everything(), names_to = "mediator", values_to = "indirect_effect") |>
      mutate(contrast = glue("{apply(t1, 1, paste0, collapse=',')} - {apply(t2, 1, paste0, collapse=',')}"))
  }

  if (is.null(names(t_outcome))) {
    names(t_outcome) <- seq_along(t_outcome)
  }

  bind_rows(result, .id = "direct_setting") |>
    mutate(direct_setting = names(t_outcome)[as.integer(direct_setting)]) |>
    select(mediator, direct_setting, contrast, indirect_effect)
}

indirect_pathwise <- function(model) {

}
