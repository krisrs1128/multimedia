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
#' @param ... Additional arguments to pass to `predict()`.
#' @return A list with two elements, `mediators` and `outcomes`, containing the
#'  differences in the predicted M(T') - M(T) and Y(T', M(T')) - Y(T, M(T))
#'  between the two profiles T and T'.
#' @export
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' model <- multimedia(exper) |>
#'     estimate(exper)
#' t1 <- data.frame(treatment = factor("Treatment"))
#' t2 <- data.frame(treatment = factor("Control"))
#' profile1 <- setup_profile(model, t1, t1)
#' profile2 <- setup_profile(model, t2, t2)
#' contrast_predictions(model, profile1, profile2)
contrast_predictions <- function(model, profile1, profile2, ...) {
    y_hat_1 <- predict(model, profile1, ...)
    y_hat_2 <- predict(model, profile2, ...)
    path_difference(y_hat_1, y_hat_2)
}

#' Difference between Samples at Contrasting Profiles
#'
#' Given a fitted multimedia model, contrast sampled mediation and outcome data
#' associated with two treatment profiles.
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
#' @param ... Additional arguments to pass to `sample()`.
#' @return A list with two elements, mediators and outcomes. These contrast the
#'   values of the mediator and outcomes under the two profiles T and T'.
#' @export
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' model <- multimedia(exper) |>
#'     estimate(exper)
#' t1 <- data.frame(treatment = factor("Treatment"))
#' t2 <- data.frame(treatment = factor("Control"))
#' profile1 <- setup_profile(model, t1, t1)
#' profile2 <- setup_profile(model, t2, t2)
#' contrast_samples(model, profile1, profile2)
#'
#' samples <- purrr::map(
#'     seq_len(100),
#'     ~ contrast_samples(model, profile1, profile2)
#' )
#' hist(sapply(samples, \(x) x[[1]]$ASV1))
#' hist(sapply(samples, \(x) x[[1]]$ASV2))
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
#' are formed using Equation (10) of our paper. Rather than providing this
#' average, this function returns the estimated difference for each $j$. To
#' average across all j, this result can be passed to the ' `effect_summary`
#' function.
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
#' @importFrom tidyselect any_of
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @importFrom rlang .data
#' @export
#' @examples
#' # example with null data
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#'
#' direct_effect(fit)
#' direct_effect(fit, t1 = 2, t2 = 1)
#' direct_effect(fit, t1 = 2, t2 = 2)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#' direct_effect(fit)
direct_effect <- function(model, exper = NULL, t1 = 1, t2 = 2) {
    pretreatment <- NULL
    if (!is.null(exper)) {
        pretreatment <- exper@pretreatments
    }

    result <- list()
    t_ <- model@treatments
    for (i in seq_len(nrow(t_))) {
        profile1 <- setup_profile(
            model, t_[i, , drop = FALSE], t_[t1, , drop = FALSE]
        )
        profile2 <- setup_profile(
            model, t_[i, , drop = FALSE], t_[t2, , drop = FALSE]
        )

        y_hat <- contrast_predictions(
            model, profile1, profile2,
            pretreatment = pretreatment
        )[["outcomes"]] |>
            colMeans()

        result[[i]] <- data.frame(
            outcome = names(y_hat), direct_effect = y_hat,
            contrast = rep(parse_name(t_, t1, t2), n_outcomes(model)),
            row.names = NULL
        )
    }

    bind_rows(result, .id = "indirect_setting") |>
        mutate(
            indirect_setting = t_[[1]][as.integer(.data$indirect_setting)]
        ) |>
        select(
            any_of(c(
                "outcome", "indirect_setting", "contrast",
                "direct_effect"
            ))
        )
}

parse_name <- function(t_, t1, t2) {
    glue(
        "{
            apply(t_[t1,, drop=F], 1, paste0, collapse=',')
        } - {
            apply(t_[t2,, drop=F], 1, paste0, collapse=',')
        }"
    )
}

#' Overall Indirect Effect
#'
#' Direct Effects from Estimated Model
#'
#' Estimate direct effects associated with a multimedia model. These estimates
#' are formed using Equation (10) of our preprint.
#'
#' @param model An object of class multimedia containing the estimated mediation
#'  and outcome models whose mediation and outcome predictions we want to
#'  compare.
#' @param exper An object of class multimedia_data containing the mediation and
#'   outcome data from which the direct effects are to be estimated.
#' @param t1 The reference level of the treatment to be used when computing the
#'  indirect effect.
#' @param t2 The alternative level of the treatment to be used when computing
#'  the indirect effect.
#' @return A data.frame summarizing the overall indirect effects associated with
#'   different settings of j in the equation above.
#' @examples
#' # example with null data
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#'
#' indirect_overall(fit)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#' indirect_overall(fit)
#' @export
indirect_overall <- function(model, exper = NULL, t1 = 1, t2 = 2) {
    pretreatment <- NULL
    if (!is.null(exper)) {
        pretreatment <- exper@pretreatments
    }

    t_ <- model@treatments
    result <- list()
    for (i in seq_len(nrow(t_))) {
        profile1 <- setup_profile(
            model, t_[t1, , drop = FALSE], t_[i, , drop = FALSE]
        )
        profile2 <- setup_profile(
            model, t_[t2, , drop = FALSE], t_[i, , drop = FALSE]
        )

        y_hat <- contrast_predictions(
            model,
            profile1,
            profile2,
            pretreatment = pretreatment
        )[["outcomes"]] |>
            colMeans()

        result[[i]] <- data.frame(
            outcome = names(y_hat),
            indirect_effect = y_hat,
            contrast = rep(parse_name(t_, t1, t2), n_outcomes(model)),
            row.names = NULL
        )
    }

    bind_rows(result, .id = "direct_setting") |>
        mutate(
            direct_setting = t_[[1]][as.integer(.data$direct_setting)]
        ) |>
        select(any_of(
            c("outcome", "direct_setting", "contrast", "indirect_effect")
        ))
}

#' Indirect Effects via Single Mediation Paths
#' @param model An object of class multimedia containing the estimated mediation
#'  and outcome models whose mediation and outcome predictions we want to
#'  compare.
#' @param exper An object of class multimedia_data containing the mediation and
#'   outcome data from which the direct effects are to be estimated.
#' @param t1 The reference level of the treatment to be used when computing the
#'  (pathwise) indirect effect.
#' @param t2 The alternative level of the treatment to be used when computing
#'  the (pathwise) indirect effect.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A data.frame summarizing the pathwise (per-mediator) indirect effects
#'   associated with different settings of the direct effect.
#' @examples
#' # example with null data
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#' indirect_pathwise(fit)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#' indirect_pathwise(fit)
#' @importFrom cli cli_text
#' @export
indirect_pathwise <- function(
    model, exper = NULL, t1 = 1, t2 = 2, progress = TRUE) {
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
        profile2 <- setup_profile(
            model, t_[t2, , drop = FALSE], t_[i, , drop = FALSE]
        )
        y_hat_2 <- predict(model, profile2, pretreatment = pretreatment)

        pb <- progress_bar$new(
            total = length(m),
            format = "[:bar] Mediator: :current/:total ETA: :eta"
        )
        for (j in seq_along(m)) {
            profile1 <- profile2
            profile1@t_mediator[[j]] <- t_[t1, , drop = FALSE]
            y_hat_1 <- predict(model, profile1, pretreatment = pretreatment)
            y_diff <- colMeans(path_difference(y_hat_1, y_hat_2)[["outcomes"]])

            result[[k]] <- data.frame(
                outcome = names(y_diff),
                indirect_effect = y_diff,
                mediator = rep(m[j], n_outcomes(model)),
                contrast = rep(parse_name(t_, t1, t2), n_outcomes(model)),
                direct_setting = t_[[1]][i],
                row.names = NULL
            )
            if (progress) pb$tick()
            k <- k + 1
        }
    }

    bind_rows(result) |>
        select(any_of(c(
            "outcome", "mediator", "direct_setting", "contrast",
            "indirect_effect"
        )))
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
#' @importFrom dplyr group_by summarize  across
#' @importFrom tidyselect matches
#' @importFrom utils tail
#' @importFrom rlang .data
#' @seealso direct_effect indirect_effect
#' @examples
#' # example with null data
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     direct_effect() |>
#'     effect_summary()
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     direct_effect() |>
#'     effect_summary()
#' @export
effect_summary <- function(effects) {
    if ("mediator" %in% colnames(effects)) {
        effects <- group_by(effects, .data$outcome, .data$mediator)
    } else {
        effects <- group_by(effects, .data$outcome)
    }

    effects |>
        summarise(across(matches("effect"), mean))
}
