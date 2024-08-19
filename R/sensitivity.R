
#' @importFrom dplyr bind_rows
sensitivity <- function(model, exper, n_rho = 25) {
  model_types <- c(model@mediation@model_type, model@outcome@model_type)
  supported_models <- c("rf_model()", "lm_model()", "glmnet_model()")
  if (!all(model_types %in% supported_models)) {
    cli_message("Sensitivity analysis is only supported for models of type lm_model(), glmnet_model(), and rf_model()")
  }

  rho_seq <- seq(-1, 1, length.out = n_rho)
  sensitivity_curve <- list()
  for (i in seq_len(n_rho)) {
    samples <- sensitivity_sample(model, exper, rho_seq[i])
    sensitivity_curve[[i]] <- list(
      outcome = outcomes(model),
      rho = rho_seq[i],
      indirect_overall = colMeans(path_difference(samples$y1, samples$y2)[["outcome"]])
    )
  }

  bind_rows(sensitivity_curve)
}

#' @examples
#' sensitivity_sample(model, exper)
sensitivity_sample <- function(model, exper, rho) {
  Nm <- n_mediators(model)
  Ny <- n_outcomes(model)
  Sigma <- covariance_matrix(model, rho)
  epsilon <- MASS::mvrnorm(nrow(exper), rep(0, Nm + Ny), Sigma)

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

#' @example
#' xy_data <- demo_spline()
#' exper <- mediation_data(xy_data, starts_with("outcome"), "treatment", "mediator")
#' model <- multimedia(exper, outcome_estimator = rf_model(num.trees = 1e3)) |>
#'  estimate(exper)
#' standard_deviations(model@outcome)
#' standard_deviations(model@mediation)
#' @importFrom cli cli_abort
#' @importFrom glue glue
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