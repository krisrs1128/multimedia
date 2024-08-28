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
#' @param confound_ix A data.frame specifying which mediator/outcome should
#'   be allowed to be correlated. Should have two columns: 'mediator' and
#'   'outcome' specifying which pairs of mediators and outcomes should be
#'   correlated. Defaults to NULL, which creates a data.frame with no rows (and
#'   so enforcing independence between mediators and outcomes)
#' @param rho_seq We will evaluate correlations Cor(e', e) between mediation and
#'   outcome model errors ranging along this grid. Defaults to NULL, which
#'   internally sets the sequence to rho = [-0.9, -0.7 ..., 0.7, 0.9].
#' @param n_bootstrap The number of bootstrap resamples used to build confidence
#'   bands around the sensitivity curves. Defaults to 100.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A `date.frame` giving the outputs of the `summarization` function
#'   across many values of the correlation rho.
#' @importFrom cli cli_abort
#' @importFrom rlang sym
#' @importFrom dplyr bind_rows rename_with
#' @importFrom tidyselect ends_with
#' @importFrom stats sd
#' @noRd
sensitivity_factory <- function(summarization, model, exper, sampler, sensitivity_seq = NULL,
                                n_bootstrap = 100, progress = TRUE) {
  model_types <- c(model@mediation@model_type, model@outcome@model_type)
  supported_models <- c("rf_model()", "lm_model()", "glmnet_model()")
  if (!all(model_types %in% supported_models)) {
    cli_abort("Sensitivity analysis is only supported for models of type lm_model(), glmnet_model(), and rf_model().")
  }

  sensitivity_curve <- list()
  k <- 1
  pb <- progress_bar$new(
    total = length(sensitivity_seq) * n_bootstrap,
    format = "[:bar] :current/:total ETA: :eta"
  )
  for (i in seq_along(sensitivity_seq)) {
    for (b in seq_len(n_bootstrap)) {
      exper_ <- exper[sample(nrow(exper)), ]
      samples <- sampler(model, exper, sensitivity_seq[i])

      t_ <- model@treatments
      for (j in seq_len(nrow(t_))) {
        ix <- apply(exper_@treatments, 1, \(x) x == t_[j, ])
        exper_@outcomes[ix, ] <- samples[[j]]$outcomes[ix, ]
        exper_@mediators[ix, ] <- samples[[j]]$mediators[ix, ]
      }

      sensitivity_curve[[k]] <- estimate(model, exper_) |>
        summarization() |>
        mutate(perturbation = sensitivity_seq[i], bootstrap = b)
      k <- k + 1
      if (progress) pb$tick()
    }
  }

  bind_rows(sensitivity_curve) |>
    group_by(
      .data$outcome,
      !!sym(
        ifelse("mediator" %in% colnames(sensitivity_curve[[1]]), "mediator", "")
      ),
      .data$perturbation
    ) |>
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
#' @param confound_ix A data.frame specifying which mediator/outcome should
#'   be allowed to be correlated. Should have two columns: 'mediator' and
#'   'outcome' specifying which pairs of mediators and outcomes should be
#'   correlated. Defaults to NULL, which creates a data.frame with no rows (and
#'   so enforcing independence between mediators and outcomes)
#' @param rho_seq We will evaluate correlations Cor(e', e) between mediation and
#'   outcome model errors ranging along this grid. Defaults to NULL, which
#'   internally sets the sequence to rho = [-0.9, -0.7 ..., 0.7, 0.9].
#' @param n_bootstrap The number of bootstrap resamples used to build confidence
#'   bands around the sensitivity curves. Defaults to 100.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A `date.frame` giving the outputs of `indirect_overall` across many
#'   values of the correlation rho.
#' @examples
#' xy_data <- demo_spline()
#' exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
#' model <- multimedia(exper, outcome_estimator = glmnet_model(lambda = 1e-2)) |>
#'   estimate(exper)
#' rho_seq <- c(-0.2, 0, 0.2)
#' sensitivity(model, exper, subset_indices, rho_seq, n_bootstrap = 2)
#' @export
sensitivity <- function(model, exper, confound_ix = NULL, rho_seq = NULL,
                        n_bootstrap = 100, progress = TRUE) {
  if (is.null(rho_seq)) {
    rho_seq <- seq(-0.9, 0.9, by = 0.2)
  }

  summarization <- \(x) {
    indirect_overall(x) |>
      effect_summary()
  }

  sampler <- \(model, exper, rho) {
    sensitivity_subset_sample(model, exper, confound_ix, rho)
  }
  sensitivity_factory(summarization, model, exper, sampler, rho_seq, n_bootstrap, progress) |>
    rename(rho = "perturbation")
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
#' @param confound_ix A data.frame specifying which mediator/outcome should
#'   be allowed to be correlated. Should have two columns: 'mediator' and
#'   'outcome' specifying which pairs of mediators and outcomes should be
#'   correlated. Defaults to NULL, which creates a data.frame with no rows (and
#'   so enforcing independence between mediators and outcomes)
#' @param rho_seq We will evaluate correlations Cor(e', e) between mediation and
#'   outcome model errors ranging along this grid. Defaults to NULL, which
#'   internally sets the sequence to rho = [-0.9, -0.7 ..., 0.7, 0.9].
#' @param n_bootstrap The number of bootstrap resamples used to build confidence
#'   bands around the sensitivity curves. Defaults to 100.
#' @param progress A logical indicating whether to show a progress bar.
#' @return A `date.frame` giving the outputs of `indirect_overall` across many
#'   values of the correlation rho.
#' @examples
#' xy_data <- demo_spline()
#' exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
#' model <- multimedia(exper, outcome_estimator = glmnet_model(lambda = 1e-2)) |>
#'   estimate(exper)
#' rho_seq <- c(-0.2, 0, 0.2)
#' subset_indices <- expand.grid(mediator = n_mediators(model), outcome = n_outcomes(model))
#' sensitivity_pathwise(model, exper, subset_indices, rho_seq, n_bootstrap = 2)
#' @export
sensitivity_pathwise <- function(model, exper, confound_ix = NULL,
                                 rho_seq = NULL, n_bootstrap = 100,
                                 progress = TRUE) {
  summarization <- \(x) {
    indirect_pathwise(x) |>
      effect_summary()
  }
  sampler <- \(model, exper, rho) {
    sensitivity_subset_sample(model, exper, confound_ix, rho)
  }
  sensitivity_factory(summarization, model, exper, sampler, rho_seq, n_bootstrap, progress) |>
    rename(rho = "perturbation")
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
#' @param confound_ix A data.frame specifying which mediator/outcome should
#'   be allowed to be correlated. Should have two columns: 'mediator' and
#'   'outcome' specifying which pairs of mediators and outcomes should be
#'   correlated. Defaults to NULL, which creates a data.frame with no rows (and
#'   so enforcing independence between mediators and outcomes)
#' @param rho The value of the correlation between all pairs of mediators and
#'   outcomes.
#' @importFrom MASS mvrnorm
#' @examples
#' xy_data <- demo_spline()
#' exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
#' model <- multimedia(exper, outcome_estimator = rf_model(num.trees = 1e3)) |>
#'   estimate(exper)
#' multimedia:::sensitivity_sample(model, exper)
#' @noRd
sensitivity_subset_sample <- function(model, exper, confound_ix = NULL, rho = 0.0) {
  Nm <- n_mediators(model)
  Ny <- n_outcomes(model)
  epsilon <- covariance_matrix(model, confound_ix, rho) |>
    mvrnorm(nrow(exper), rep(0, Nm + Ny), Sigma = _)
  sensitivity_sample(model, exper, epsilon)
}

sensitivity_sample <- function(model, exper, epsilon) {
  t_ <- model@treatments
  Nm <- n_mediators(model)
  Ny <- n_outcomes(model)

  samples <- list()
  for (i in seq_len(nrow(t_))) {
    exper_ <- bind_mediation(exper)
    exper_[, treatments(model)] <- t_[i, , drop = FALSE]

    # sample the mediators and outcomes
    m <- predict_across(model@mediation, exper_, mediators(model)) +
      epsilon[, seq_len(Nm), drop = FALSE]
    exper_[, mediators(model)] <- m
    y <- predict_across(model@outcome, exper_, outcomes(model)) +
      epsilon[, seq(Nm + 1, Nm + Ny), drop = FALSE]

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
#'   estimate(exper)
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

#' @noRd
covariance_matrix <- function(model, confound_ix = NULL, rho = 0.0) {
  sigma_m <- standard_deviations(model@mediation)
  sigma_y <- standard_deviations(model@outcome)
  covariance <- diag(c(sigma_m^2, sigma_y^2))
  if (rho == 0 || is.null(confound_ix)) {
    return(covariance)
  }
  if (!(all(c("mediator", "outcome") %in% colnames(confound_ix)))) {
    cli_abort(glue("Argument confound_ix seems incorrectly formatted. Are you sure you have columns called 'mediator' and 'outcome'? We found {colnames(confound_ix)}"))
  }

  # Fill in covariances
  Nm <- n_mediators(model)
  for (i in seq_len(nrow(confound_ix))) {
    m_ix <- confound_ix$mediator[i]
    y_ix <- confound_ix$outcome[i]
    covariance[m_ix, Nm + y_ix] <- rho * sigma_m[m_ix] * sigma_y[y_ix]
    covariance[Nm + y_ix, m_ix] <- rho * sigma_m[m_ix] * sigma_y[y_ix]
  }

  # Ensure positive semi-definite
  ecov <- eigen(covariance)
  ecov$values <- pmax(0, ecov$values)
  ecov$vectors %*% diag(ecov$values) %*% t(ecov$vectors)
}

#' @noRd
sensitivity_perturb_sample <- function(model, exper, perturb = NULL, nu = 0.0) {
  Nm <- n_mediators(model)
  Ny <- n_outcomes(model)
  epsilon <- (covariance_matrix(model) + nu * perturb) |>
    mvrnorm(nrow(exper), rep(0, Nm + Ny), Sigma = _)
  sensitivity_sample(model, exper, epsilon)
}

#' @export
sensitivity_perturb <- function(model, exper, perturb, nu_seq = NULL, n_bootstrap = 100, progress = TRUE) {
  if (is.null(nu_seq)) {
    nu_seq <- seq(-0.1, 0.1, by = 0.04)
  }

  summarization <- \(x) {
    indirect_overall(x) |>
      effect_summary()
  }

  sampler <- \(model, exper, nu) {
    sensitivity_perturb_sample(model, exper, perturb, nu)
  }
  sensitivity_factory(summarization, model, exper, sampler, nu_seq, n_bootstrap, progress) |>
    rename(nu = "perturbation")
}
