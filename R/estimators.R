#' @export
setClass(
  "model",
  representation(
    estimator = "ANY",
    estimates = "ANY",
    sampler = "ANY",
    model_type = "ANY",
    predictor = "ANY"
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

#' @importFrom tidygraph %N>%
edges_df <- function(edges) {
  nodes <- edges %N>%
    as_tibble()

  edges %E>%
    as_tibble() |>
    left_join(nodes, by = c("from" = "id")) |>
    dplyr::rename(
      node_type_from = node_type,
      name_from = name
    ) |>
    left_join(nodes, by = c("to" = "id")) |>
    dplyr::rename(
      node_type_to = node_type,
      name_to = name
    )
}

edges_to_formula <- function(edges) {
  collapse <- \(e, v) {
    pull(e, eval(v)) |>
      unique() |>
      paste0(collapse = "+")
  }

  predictors <- collapse(edges, quote(name_from))
  outcomes <- collapse(edges, quote(name_to))
  glue("{outcomes} ~ {predictors}") |>
    as.formula()
}

mediation_formula <- function(edges) {
  edges %E>%
    filter(state == "active") |>
    edges_df() |>
    filter(node_type_to == "mediator") |>
    edges_to_formula()
}

outcome_formula <- function(edges) {
  edges %E>%
    filter(state == "active") |>
    edges_df() |>
    filter(node_type_to == "outcome") |>
    edges_to_formula()
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
    predictor = predict,
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

glmnet_model_params <- function(...) {
  defaults <- list(nfolds = 3)
  modifyList(defaults, list(...))
}

#' @importFrom glmnetUtils cv.glmnet
#' @export
glmnet_model <- function(...) {
  params <- glmnet_model_params(...)
  new(
    "model",
    estimator = parallelize(\(fmla, data) inject(cv.glmnet(fmla, data, !!!params))),
    estimates = NULL,
    sampler = glmnet_sampler,
    predictor = \(object, ...) {
      predict(object, s = object$lambda.1se, ...)[, 1]
    },
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
    sigma <- deviance(fits[[i]]$glmnet.fit)[lambda] / fits[[i]]$glmnet.fit$nobs
    y_ <- predict(fits[[i]], newdata = newdata, ...)[, "lambda.1se"]
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}

brms_model_params <- function(...) {
  defaults <- list(chains = 1)
  modifyList(defaults, list(...))
}

brm_cache <- function(formula, data, ...) {
  models <- list()

  ys <- lhs.vars(formula)
  models[[1]] <- brm(sub_formula(formula, ys[1]), data, ...)
  for (j in seq(2, length(ys))) {
    fmla <- sub_formula(formula, ys[j])
    models[[ys[j]]] <- update(models[[1]], fmla, newdata = data, recompile = FALSE, ...)
  }
  models
}

#' @importFrom brms brm
#' @importFrom rlang inject !!!
#' @export
brms_model <- function(...) {
  params <- brms_model_params(...)
  new(
    "model",
    estimator = \(fmla, data) inject(brm_cache(fmla, data, !!!params)),
    estimates = NULL,
    sampler = brms_sampler,
    predictor = \(object, ...) predict(object, ...)[, "Estimate"],
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
    predictor = predict,
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

#' Random forest model
#' @importFrom ranger ranger
#' @export
rf_model <- function(...) {
  new(
    "model",
    estimator = parallelize(\(fmla, data) ranger(fmla, data, ...)),
    estimates = NULL,
    sampler = rf_sampler,
    model_type = "rf_model()",
    predictor = \(object, ...) predict(object, ...)$predictions
  )
}

#' @importFrom dplyr bind_cols
#' @export
rf_sampler <- function(fits, newdata = NULL, indices = NULL, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    sigma <- sqrt(fits[[i]]$prediction.error)
    y_ <- predict(fits[[i]], newdata = newdata, ...)$predictions
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}
