#' Estimate the Difference between Profiles
#'
#' Given a fitted multimedia model, contrast the mediation and outcome
#' predictions associated wtih two treatment profiles.
#'
#' @param model An object of class multimedia containing the estimated mediation
#'  and outcome models whose mediation and outcome predictions we want to
#'  compare.
#' @param profile1 An object of class `treatment_profile` containing the first
#'  treatment profile to consider in the difference.
#' @param profile2 An object of class `treatment_profile` containing the second
#'  treatment profile to consider in the difference.
#' @return A list with two elements, `mediators` and `outcomes`, containing the
#'  differences in the predicted M(T') - M(T) and Y(T', M(T')) - Y(T, M(T))
#'  between the two profiles T and T'.
#' @export
contrast_predictions <- function(model, profile1, profile2, ...) {
  y_hat_1 <- predict(model, profile1, ...)
  y_hat_2 <- predict(model, profile2, ...)
  path_difference(y_hat_1, y_hat_2)
}

#' Difference between Samples at Contrasting Profiles
#'
#' Given a fitted multimedia model, contrast sampled mediation and outcome data
#' associated wtih two treatment profiles.
#'
#' @param model An object of class multimedia containing the estimated mediation
#'  and outcome models whose mediation and outcome predictions we want to
#'  compare.
#' @param profile1 An object of class `treatment_profile` containing the first
#'  treatment profile to consider in the difference.
#' @param profile2 An object of class `treatment_profile` containing the second
#'  treatment profile to consider in the difference.
#' @return A list with two elements, `mediators` and `outcomes`, containing the
#'  differences in the sampled M(T') - M(T) and Y(T', M(T')) - Y(T, M(T))
#'  between the two profiles T and T'.
#' @export
contrast_samples <- function(model, profile1, profile2, ...) {
  y1 <- sample(model, profile = profile1, ...)
  y2 <- sample(model, profile = profile2, ...)

  y1 <- list(mediators = y1@mediators, outcomes = y1@outcomes)
  y2 <- list(mediators = y2@mediators, outcomes = y2@outcomes)
  path_difference(y1, y2)
}

path_difference <- function(y1, y2) {
  list(
    mediators = y1$mediators - y2$mediators,
    outcomes = y1$outcomes - y2$outcomes
  )
}

#' Direct Effects from Estimated Model
#'
#' Estimate direct effects associated with a multimedia model. These estimates
#' are formed using Equation (10) of our preprint:
#' 
#' \deqn{
#' \frac{1}{2} \sum_{j = 0}^{1} \sum_{i = 1}^{N} \hat{Y}_{i}(t2, \hat{M}(t2)) - \hat{Y}_{i}(t1, \hat{M}(t1)) 
#' }
#' 
#' Rather than providing this average, this function returns the estimated
#' difference for each $j$. To average across all j, this result can be passed
#' to the ' `effect_summary` function.
#' @param model An object of class multimedia containing the estimated mediation
#'  and outcome models whose mediation and outcome predictions we want to
#'  compare.
#' @param exper An object of class multimedia_data containing the mediation and
#'   outcome data from which the direct effects are to be estimated.
#' @param t1 The reference level of the treatment to be used when computing the
#'  direct effect.
#' @param t2 The alternative level of the treatment to be used when computing
#'  the direct effect.
#' @return A data.frame summarizing the direct effects associated with different
#'  settings of j in the equation above. 
#' @seealso effect_summary
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
#' 
#' Direct Effects from Estimated Model
#'
#' Estimate direct effects associated with a multimedia model. These estimates
#' are formed using Equation (10) of our preprint:
#' 
#' \deqn{
#' \frac{1}{2} \sum_{j = 0}^{1} \sum_{i = 1}^{N} \hat{Y}_{i}(t2, \hat{M}(t2)) - \hat{Y}_{i}(t1, \hat{M}(t1)) 
#' }
#' 
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
      # if (m[j] == "gVeillonella") {
      #  browser()
      # }
      y_hat_1 <- predict(model, profile1, pretreatment = pretreatment)
      y_diff <- colMeans(path_difference(y_hat_1, y_hat_2)[["outcomes"]])

      result[[k]] <- tibble(
        outcome = names(y_diff),
        indirect_effect = y_diff,
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

#' Average Effects across j
#' 
#' This averages direct or indirect effects across settings j, leading to 
#' the effect estimates given in equation (10) of the preprint.
#' @param effects The output from direct_effect or indirect_effect. A data.frame
#'   containing effect estimates for each variable and indirect/direct setting
#'   along rows.
#' @return A version of the input with all indirect/direct settings averaged
#'   over.
#' @importFrom dplyr group_by summarize arrange filter slice_max
#' @seealso direct_effect indirect_effect
#' @export
effect_summary <- function(effects, N = 10) {
  effect_type <- tail(colnames(effects), 1)
  if ("mediator" %in% colnames(effects)) {
    effects <- group_by(effects, outcome, mediator)
  } else {
    effects <- group_by(effects, outcome)
  }

  effects |>
    summarise(across(matches("effect"), mean)) |>
    slice_max(.data[[effect_type]], n = N) |>
    arrange(.data[[effect_type]]) |>
    filter(.data[[effect_type]] != 0)
}
