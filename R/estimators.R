#' @export
setClass(
  "model",
  representation(
    estimator = "ANY",
    estimates = "ANY",
    sampler = "ANY",
    model_type = "ANY"
  )
)

sub_formula <- function(formula, yj) {
  update.formula(formula, as.formula(glue("{yj} ~ .")))
}

#' @importFrom formula.tools lhs.vars
#' @examples
#' mat <- data.frame(matrix(rnorm(100), 25, 4))
#' colnames(mat) <- c("y1", "y2", "x1", "x2")
#' plm <- parallelize(lm)
#' plm(y1 + y2 ~ x1 + x2, mat)
parallelize <- function(f) {
  function(formula, ...) {
    models <- list()

    ys <- lhs.vars(formula)
    for (j in seq_along(ys)) {
      fmla <- sub_formula(formula, ys[j])
      models[[ys[j]]] <- f(fmla, ...)
    }
    models
  }
}

mediation_formula <- function(edges) {
  edges <- edges %E>%
    filter(state == "active")

  mediators <- edges %N>%
    filter(node_type == "mediator") |>
    pull(name)
  predictors <- edges %N>%
    filter(node_type %in% c("pretreatment", "treatment")) |>
    pull(name)

  glue("{paste0(mediators, collapse = '+')} ~ {paste0(predictors, collapse = '+')}") |>
    as.formula()
}

outcome_formula <- function(edges) {
  edges <- edges %E>%
    filter(state == "active")

  response <- edges %N>%
    filter(node_type == "outcome") |>
    pull(name)
  predictors <- edges %N>%
    filter(!(node_type == "outcome")) |>
    pull(name)

  glue("{paste0(response, collapse = '+')} ~ {paste0(predictors, collapse = '+')}") |>
    as.formula()
}

#' @export
estimate <- function(model, exper) {
  # estimate mediation model
  data <- bind_mediation(exper)

  f <- mediation_formula(model@edges)
  mediation_est <- model@mediation@estimator
  model@mediation@estimates <- mediation_est(f, data)

  # estimate outcome model
  f <- outcome_formula(model@edges)
  outcome_est <- model@outcome@estimator
  model@outcome@estimates <- outcome_est(f, data)
  model@treatments <- unique(exper@treatments)

  model
}

#' Linear Model across Responses
lm_model <- function() {
  new(
    "model",
    estimator = parallelize(lm),
    estimates = NULL,
    sampler = lm_sampler,
    model_type = "lm_model()"
  )
}

#' @importFrom dplyr bind_cols
lm_sampler <- function(fits, newdata = NULL, indices = NULL, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    sigma <- summary(fits[[i]])$sigma
    y_ <- predict(fits[[i]], newdata = newdata, ...)
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}

#' @importFrom glmnetUtils cv.glmnet
#' @export
glmnet_model <- function(...) {
  new(
    "model",
    estimator = parallelize(\(fmla, data) cv.glmnet(fmla, data, ...)),
    estimates = NULL,
    sampler = glmnet_sampler,
    model_type = "glmnet_model()"
  )
}

#' @export
glmnet_sampler <- function(fits, newdata = NULL, indices = NULL, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    lambda <- which(fits[[i]]$lambda == fits[[i]]$lambda.1se)
    n_samples <- length(fits[[i]]$foldid)
    sigma <- deviance(fits[[i]]$glmnet.fit)[lambda] / n_samples
    y_ <- predict(fits[[i]], newdata = newdata, ...)
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}

brms_model_params <- function(...) {
  defaults <- list(chains = 1)
  modifyList(defaults, list(...))
}

#' @importFrom brms brm
#' @importFrom rlang inject !!!
#' @export
brms_model <- function(...) {
  params <- brms_model_params(...)
  new(
    "model",
    estimator = parallelize(\(fmla, data) inject(brm(fmla, data, !!!params))),
    estimates = NULL,
    sampler = brms_sampler,
    model_type = "brms_model()"
  )
}

#' @importFrom brms posterior_predict
#' @export
brms_sampler <- function(fits, newdata = NULL, indices = NULL, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    y_hats[[nm[i]]] <- posterior_predict(
      fits[[i]],
      newdata,
      resp = nm[i],
      ndraws = 1,
      ...
    ) |>
      as.numeric()
  }
  bind_cols(y_hats)
}

#' @importFrom miniLNM lnm
#' @export
lnm_model <- function() {
  new(
    "model",
    estimator = lnm,
    estimates = NULL,
    sampler = lnm_sampler,
    model_type = "lnm_model()"
  )
}

#' @export
lnm_sampler <- function(fit, newdata = NULL, indices = NULL, ...) {
  y_star <- sample(fit, newdata = newdata, ...)
  if (is.null(indices)) {
    return(y_star)
  }
  y_star[, indices]
}
