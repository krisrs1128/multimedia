
#' Core Sensitivity Analysis Function
#' 
#' For causal identification, mediation analysis relies on several untestable
#' assumptions. One important one is that there is no confounding between the
#' counterfactual mediator and outcome variables. Even though we can never know
#' whether this exists, we can measure the sensitivity of our conclusions to the
#' existence/strength of such a confounder. In this function, we approach this
#' by inducing (unallowable) correlation between the mediator and outcome model
#' residuals, simulate forward, and see how conclusions change.
#' 
#' This function is an internal, core function that is wrapped by many
#' sensitivity analysis wrappers. It allows general `summarization` functions as
#' input. This allows us to apply the same residual-correlation to many kinds of
#' output analysis, e.g., both indirect overall and pathwise effects.
#' 
#' @param summarization A function whose outputs we want to evaluate for
#'   sensitivity to potentially unmeasured confounding. E.g.,
#'   `indirect_pathwise`. The function must output its summaries as a tidy
#'   `data.frame`.
#' @param model A `multimedia` object containing the fitted models for
#'   sensitivity analysis. Note that since our approach relies on correlating
#'   simulated residual error, it is only applicable to models of class
#'   `lm_model()`, `glmnet_model()` and `rf_model()`.
#' @param exper The original `mediation_data` class object used to fit `model`.
#'   These observations will be resampled to support bootstrap confidence
#'   interval construction of the sensitivity curve.
#' @param n_rho We will evaluate correlations Cor(e', e) between mediation and
#'   outcome model errors ranging from `[-1, 1]` with density determined by this
#'   parameter. Defaults to 10.
#' @param n_bootstrap The number of bootstrap resamples used to build confidence
#'   bands around the sensitivity curves. Defaults to 100.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A `date.frame` giving the outputs of the `summarization` function
#'   across many values of the correlation rho.
#' @importFrom cli cli_abort
#' @importFrom dplyr bind_rows
#' @importFrom tidyselect ends_with
#' @importFrom stats sd
#' @noRd
sensitivity_internal <- function(summarization, model, exper, n_rho = 10, 
                                 n_bootstrap = 100, progress = TRUE) {
  model_types <- c(model@mediation@model_type, model@outcome@model_type)
  supported_models <- c("rf_model()", "lm_model()", "glmnet_model()")
  if (!all(model_types %in% supported_models)) {
    cli_abort("Sensitivity analysis is only supported for models of type lm_model(), glmnet_model(), and rf_model().")
  }

  rho_seq <- seq(-1, 1, length.out = n_rho)
  sensitivity_curve <- list()
  k <- 1
  pb <- progress_bar$new(total = n_rho * n_bootstrap, format = "[:bar] :current/:total ETA: :eta")
  for (i in seq_len(n_rho)) {
    for (b in seq_len(n_bootstrap)) {
      exper_ <- exper[sample(nrow(exper)), ]
      samples <- sensitivity_sample(model, exper, rho_seq[i])
      t_ <- model@treatments
      for (j in seq_len(nrow(t_))) {
        ix <- apply(exper_@treatments, 1, \(x) x == t_[j, ])
        exper_@outcomes[ix, ] <- samples[[j]]$outcomes[ix, ]
        exper_@mediators[ix, ] <- samples[[j]]$mediators[ix, ]
      }

      sensitivity_curve[[k]] <- estimate(model, exper_) |>
        summarization() |>
        mutate(rho = rho_seq[i], bootstrap = b)
      k <- k + 1
      if (progress) pb$tick()
    }
  }

  bind_rows(sensitivity_curve) |>
    group_by(.data$outcome, .data$rho) |>
    summarise(across(ends_with("effect"), c(`_` = mean, standard_error = sd))) |>
    rename_with(~ gsub("__$", "", .))
}

#' Sensitivity Analysis for Overall Indirect Effect
#' 
#' For causal identification, mediation analysis relies on several untestable
#' assumptions. One important one is that there is no confounding between the
#' counterfactual mediator and outcome variables. Even though we can never know
#' whether this exists, we can measure the sensitivity of our conclusions to the
#' existence/strength of such a confounder. In this function, we approach this
#' by inducing (unallowable) correlation between the mediator and outcome model
#' residuals, simulate forward, and see how the estimated overall indirect
#' effect changes.
#' 
#' @param model A `multimedia` object containing the fitted models for
#'   sensitivity analysis. Note that since our approach relies on correlating
#'   simulated residual error, it is only applicable to models of class
#'   `lm_model()`, `glmnet_model()` and `rf_model()`.
#' @param exper The original `mediation_data` class object used to fit `model`.
#'   These observations will be resampled to support bootstrap confidence
#'   interval construction of the sensitivity curve.
#' @param n_rho We will evaluate correlations Cor(e', e) between mediation and
#'   outcome model errors ranging from `[-1, 1]` with density determined by this
#'   parameter. Defaults to 10.
#' @param n_bootstrap The number of bootstrap resamples used to build confidence
#'   bands around the sensitivity curves. Defaults to 100.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A `date.frame` giving the outputs of `indirect_overall` across many
#'   values of the correlation rho.
#' @export
sensitivity <- function(model, exper, n_rho = 10, n_bootstrap = 100, progress = TRUE) {
  (\(x) indirect_overall(x) |> 
    effect_summary()) |>
    sensitivity_internal(model, exper, n_rho, n_bootstrap, progress)
}

#' Sensitivity Analysis for Pathwise Indirect Effects
#' 
#' For causal identification, mediation analysis relies on several untestable
#' assumptions. One important one is that there is no confounding between the
#' counterfactual mediator and outcome variables. Even though we can never know
#' whether this exists, we can measure the sensitivity of our conclusions to the
#' existence/strength of such a confounder. In this function, we approach this
#' by inducing (unallowable) correlation between the mediator and outcome model
#' residuals, simulate forward, and see how the estimated pathwise indirect
#' effects change.
#' 
#' @param model A `multimedia` object containing the fitted models for
#'   sensitivity analysis. Note that since our approach relies on correlating
#'   simulated residual error, it is only applicable to models of class
#'   `lm_model()`, `glmnet_model()` and `rf_model()`.
#' @param exper The original `mediation_data` class object used to fit `model`.
#'   These observations will be resampled to support bootstrap confidence
#'   interval construction of the sensitivity curve.
#' @param n_rho We will evaluate correlations Cor(e', e) between mediation and
#'   outcome model errors ranging from `[-1, 1]` with density determined by this
#'   parameter. Defaults to 10.
#' @param n_bootstrap The number of bootstrap resamples used to build confidence
#'   bands around the sensitivity curves. Defaults to 100.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A `date.frame` giving the outputs of `indirect_overall` across many
#'   values of the correlation rho.
#' @export
sensitivity_pathwise <- function(model, exper, n_rho = 10, n_bootstrap = 100, progress = TRUE) {
  (\(x) indirect_pathwise(x) |> 
    effect_summary()) |>
    sensitivity_internal(model, exper, n_rho, n_bootstrap, progress)
}

#' @importFrom dplyr arrange across everything
sorted_treatments <- function(model) {
  unique(model@treatments) |>
    arrange(across(everything()))
}

#' Sample from an SEM with Correlated Errors
#' 
#' @param model A `multimedia` object containing the fitted models for
#'   sensitivity analysis. Note that since our approach relies on correlating
#'   simulated residual error, it is only applicable to models of class
#'   `lm_model()`, `glmnet_model()` and `rf_model()`.
#' @param exper The original `mediation_data` class object used to fit `model`.
#'   These observations will be resampled to support bootstrap confidence
#'   interval construction of the sensitivity curve.
#' @param rho The value of the correlation between all pairs of mediators and
#'   outcomes.
#' @importFrom MASS mvrnorm
#' @examples
#' xy_data <- demo_spline()
#' exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
#' model <- multimedia(exper, outcome_estimator = rf_model(num.trees = 1e3)) |>
#'  estimate(exper)
#' multimedia:::sensitivity_sample(model, exper)
#' @noRd
sensitivity_sample <- function(model, exper, rho = 0.0) {
  Nm <- n_mediators(model)
  Ny <- n_outcomes(model)
  Sigma <- covariance_matrix(model, rho)
  epsilon <- mvrnorm(nrow(exper), rep(0, Nm + Ny), Sigma)

  t_ <- model@treatments
  samples <- list()
  for (i in seq_len(nrow(t_))) {
    exper_ <- bind_mediation(exper)
    exper_[, treatments(model)] <- t_[i,, drop=FALSE]

    # sample the mediators and outcomes
    m <- predict_across(model@mediation, exper_, mediators(model)) + epsilon[, seq_len(Nm), drop = FALSE]
    exper_[, mediators(model)] <- m
    y <- predict_across(model@outcome, exper_, outcomes(model)) + epsilon[, seq(Nm + 1, Nm + Ny), drop = FALSE]

    # rename and save
    colnames(m) <- mediators(model)
    colnames(y) <- outcomes(model)
    samples[[i]] <- list(mediators = m, outcomes = y)
  }

  samples
}

#' Extract SDs from multimedia model objects
#' 
#' @examples
#' xy_data <- demo_spline()
#' exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
#' model <- multimedia(exper, outcome_estimator = rf_model(num.trees = 1e3)) |>
#'  estimate(exper)
#' standard_deviations(model@outcome)
#' standard_deviations(model@mediation)
#' @importFrom purrr map_dbl
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @noRd
standard_deviations <- function(model) {
  switch(model@model_type,
    "glmnet_model()" = map_dbl(model@estimates, ~ deviance(.) / .$nobs),
    "lm_model()" = map_dbl(model@estimates, ~ summary(.)$sigma),
    "rf_model()" = map_dbl(model@estimates, ~ .$prediction.error),
    cli_abort(glue("Unsupported model type: {model@model_type}"))
  )
}

covariance_matrix <- function(model, rho = 1) {
  sigma_m <- standard_deviations(model@mediation)
  sigma_y <- standard_deviations(model@outcome)

  # Initialize with variances
  Nm <- n_mediators(model)
  Ny <- n_outcomes(model)
  Sigma <- 0.5 * diag(c(sigma_m ^ 2, sigma_y ^ 2))

  # Fill in covariances
  ix <- list(seq_len(Nm), seq(Nm + 1, Nm + Ny))
  Sigma[ix[[1]], ix[[2]]] <- rho * sigma_m %*% t(sigma_y)
  Sigma <- Sigma + t(Sigma)

  # Ensure PSD
  eSigma <- eigen(Sigma)
  eSigma$values <- pmax(0, eSigma$values)
  eSigma$vectors %*% diag(eSigma$values) %*% t(eSigma$vectors)
}