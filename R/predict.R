#' Sample New Mediator/Outcome Data
#'
#' This generalizes the built-in sample method to the multimedia class. Given an
#' estimated multimedia object, this function supports sampling along the
#' estimated DAG. It first samples `M* | T, X` and then `Y* | M*, T, X`. Each
#' sampling step will call the sample method within the mediation and outcome
#' models that make up the multimedia object on which this is called.
#' @param x An object of class multimedia containing the estimated
#'   mediation and outcome models whose mediation and outcome samples we
#'   want to obtain.
#' @param size A placeholder argument to agree with the default `sample` method
#'   in R base. We always return the number of samples as set in either the
#'   original input `x` or a new input `profile`.
#' @param profile An object of class `treatment_profile` containing the
#'   treatment profile to consider in the difference. Defaults to a profile with
#'   all the unique treatment configurations observed in the original data,
#'   shared across both the mediators and outcomes.
#' @param mediators By default, we will return outcome predictions using the
#'   predicted mediators from the mediation model. Modify this argument if you
#'   would like to directly control the mediation inputs for the outcome model.
#'   Must be a data.frame whose columns are named to match the
#'   `mediators(object)`.
#' @param pretreatment By default, we will return mediation and outcome model
#'   predictions using the same pretreatment variables as used when initially
#'   estimating the models (like setting `newdata = NULL` in usual `predict`).
#'   To pass in different pretreatment variables, provide a data.frame here
#'   whose columns match the pretreatments as the originally trained mediation
#'   and outcome models.
#' @param ... Additional options to pass to the @sampler method in the estimated
#'   mediation model.
#' @return An object of class `multimedia` with mediator and outcome slots
#'   sampled according to the description above.
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#' samples <- sample(fit)
#' mediators(samples)
#' outcomes(samples)
#'
#' # sampling with just different "n" has no effect.
#' samples <- sample(fit, 100)
#'
#' # Instead sample at a new treatment configuration
#' t1 <- data.frame(treatment = factor(rep(c(0, 1), each = 50)))
#' profile <- setup_profile(fit, t_mediator = t1, t_outcome = t1)
#' samples <- sample(fit, profile = profile)
#' mediators(samples)
#' outcomes(samples)
#' @importFrom cli cli_warn
#' @export
setMethod("sample", "multimedia", function(
    x, size, pretreatment = NULL,
    profile = NULL,
    mediators = NULL, ...) {
    if (missing(size)) {
        size <- 1
    } else {
        cli_warn(
            "The size argument in sample is ignored. Please adjust the
            treatment profile to adjust the number of samples."
        )
    }
    if (is.null(profile)) {
        profile <- setup_profile(x)
    }

    # if mediators are not provided, sample them
    if (is.null(mediators)) {
        mediators <- list()
        for (i in seq_along(profile@t_mediator)) {
            mediators[[i]] <- bind_cols(
                pretreatment, profile@t_mediator[[i]]
            ) |>
                x@mediation@sampler(
                    x@mediation@estimates, newdata = _, indices = i, ...
                )
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

#' Predict a Subset of Responses
#'
#' Predict across selected responses in a mediation model object. This is a
#' lower-level version of the predict method that applies to objets of class
#' mediation_model. Rather than giving predictions across all outcomes, it
#' supports predictions across a subset specified as a vector. This can be
#' convenient when we want to analyze how a specific subset of outcomes changes
#' and do not need to run predictions across all possible.
#'
#' @param object An object of class `model` containing an estimated model.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param name A string or index specifying which of the dimensions of a
#'   multiresponse prediction to extract.
#' @return A vector of predicted values for the outcome of interest.
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#' predict_across(outcome_model(fit), NULL, "outcome_1")
#'
#' # predict at newdata
#' newdata <- bind_mediation(exper)
#' predict_across(
#'     outcome_model(fit),
#'     newdata[seq_len(5), ],
#'     c("outcome_1", "outcome_2")
#' )
#' @importFrom purrr map set_names
#' @importFrom dplyr bind_cols
#' @export
predict_across <- function(object, newdata, name) {
    # many univariate models
    if (is(object@estimates, "list")) {
        if (length(name) == 1) {
            return(object@predictor(object@estimates[[name]], newdata))
        } else {
            predictions <- map(
                name,
                ~ object@predictor(object@estimates[[.]], newdata),
            ) |>
                set_names(name) |>
                bind_cols()
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
#' @param mediators By default, we will return outcome predictions using the
#'   predicted mediators from the mediation model. Modify this argument if you
#'   would like to directly control the mediation inputs for the outcome model.
#'   Must be a data.frame whose columns are named to match the
#'   `mediators(object)`.
#' @param pretreatment By default, we will return mediation and outcome model
#'   predictions using the same pretreatment variables as used when initially
#'   estimating the models (like setting `newdata = NULL` in usual `predict`).
#'   To pass in different pretreatment variables, provide a data.frame here
#'   whose columns match the pretreatments as the originally trained mediation
#'   and outcome models.
#' @param ... A placeholder to agree with `predict` in base R. Not ever used.
#' @return A list with two elements:
#'   $mediators: A data.frame containing predicted values for the mediators.
#'   Each row corresponds to one row of the newdata, or one row of the default
#'   treatment profile, if no newdata is given.
#'
#'   $outcomes: A data.frame containing predicted values for the outcomes, given
#'   either (i) the predicted values of the mediators or (ii) the provided
#'   values of the mediators. Each row corresponds to one row of the newdata, or
#'   one row of the default treatment profile, if no newdata is given.
#' @examples
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper, glmnet_model()) |>
#'     estimate(exper)
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
            mediators[[nm[i]]] <- bind_cols(
                pretreatment, profile@t_mediator[[i]]
            ) |>
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
