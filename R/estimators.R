#' Representation of an Outcome or Mediation Model
#'
#' To work with many model types simultaneously, multimedia uses a model class
#' with the necessary mediation model functionality that wraps any specific
#' implementation. The slots below define the generally required functionality
#' for any specific implementation.
#'
#' @slot estimator A function that takes a formula, input data frame X, and an
#'   response data.frame $Y$ and returns a model. For example, for the random
#'   forest model, this is created by wrapping `parallelize()` on the ranger
#'   function random forest estimation function.
#' @slot estimates A list containing the estimated model.
#' @slot sampler A function that supports sampling new responses from the
#'   estimated model.
#' @slot model_type A string specifying the type of model associated with the
#'   class. For example, "rf_model()" denotes a random forest model.
#' @slot predictor A function that returns fitted predictions given new inputs.
#'   For example, this can be the original predict() method for a multivariate
#'   response model, or it can be a loop over predicts for each feature in the
#'   mediation or outcome model.
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

#' Parallelize Estimation across Responses
#'
#' For many mediation and outcome models, we simply want to apply a univariate
#' model across all response variable. Parallelize enables this conversion. For
#' example, applying parallelize to ranger returns a function that estimates
#' separate random forest models for each response on the left hand side of a
#' formula.
#' @param f A function for estimating a single response model given a formula
#'  and input dataset. This is the model that we would like to parallelize
#'  across responses.
#' @return f_multi A function that takes a formula and dataset and applies f to
#'  each response on the left hand side of the original formula.
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
    pb <- progress_bar$new(total = length(ys), format = "[:bar] :current/:total ETA: :eta")
    for (j in seq_along(ys)) {
      fmla <- sub_formula(formula, ys[j])
      models[[ys[j]]] <- f(fmla, ...)
      pb$tick()
    }
    models
  }
}

#' Define Edges associated with the Mediation Analysis DAG
#'
#' Given a collection of nodes, define the edges between all possible inputs and
#' outputs.
#'
#' @param edges A tidygraph graph storing edges between all variables in the
#'  mediation analysis. For example, if a treatment node leads into a mediation
#'  node, this will be included in the graph (as will all pretreatment to mediator,
#'  mediator to outcome, etc.)
#'
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

#' Construct a Formula from a Graph
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

#' Generate a Mediation Formula
#'
#' Given the relationship between all variables in the mediation analysis DAG,
#' return a formula of the form
#'
#' m1 + m2 + ... ~ treatment_1 + ... + pretreatment_T
#'
#' where all nodes leading into the mediators are inputs and all the mediators
#' are the responses.
#' @param edges A tidygraph graph storing edges between all variables in the
#'  mediation analysis. For example, if a treatment node leads into a mediation
#'  node, this will be included in the graph (as will all pretreatment to
#'  mediator, mediator to outcome, etc.)
#' @return A formula object with mediators on the LHS.
mediation_formula <- function(edges) {
  edges %E>%
    filter(state == "active") |>
    edges_df() |>
    filter(node_type_to == "mediator") |>
    edges_to_formula()
}

#' Generate an Outcome Formula
#'
#' Given the relationship between all variables in the mediation analysis DAG,
#' return a formula of the form
#'
#' m1 + m2 + ... ~ treatment_1 + ... + pretreatment_T
#'
#' where all nodes leading into the mediators are inputs and all the mediators
#' are the responses.
#' @param edges A tidygraph graph storing edges between all variables in the
#'  mediation analysis. For example, if a treatment node leads into a mediation
#'  node, this will be included in the graph (as will all pretreatment to
#'  mediator, mediator to outcome, etc.)
#' @return A formula object with outcomes on the LHS.
outcome_formula <- function(edges) {
  edges %E>%
    filter(state == "active") |>
    edges_df() |>
    filter(node_type_to == "outcome") |>
    edges_to_formula()
}

#' Estimate a Mediation Model
#'
#' `estimate` provides a unified interface to estimate all the models that can
#'  be encapsulated within a `multimedia` class. It simply calls the
#  `@estimator` slots in the mediation and outcome model components of the
#'  multimedia object. The resulting estimates can be used for downstream direct
#'  effect estimation.
#'
#' @param model An object of class multimedia containing the estimated mediation
#'  and outcome models whose mediation and outcome predictions we want to
#'  compare.
#' @param exper An object of class multimedia_data containing the mediation and
#'   outcome data from which the direct effects are to be estimated.
#' @return A version of the input modified in place so that the @estimates slot
#'  has been filled.
#'
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
#' @export
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

#' Sample a Linear Model
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
  defaults <- list(intercept = FALSE, lambda = 0.01)
  modifyList(defaults, list(...))
}

#' @importFrom glmnetUtils glmnet
#' @export
glmnet_model <- function(...) {
  params <- glmnet_model_params(...)
  new(
    "model",
    estimator = parallelize(\(fmla, data) inject(glmnet(fmla, data, !!!params))),
    estimates = NULL,
    sampler = glmnet_sampler,
    predictor = \(object, ...) {
      predict(object, s = object$lambda.1se, ...)[, 1]
    },
    model_type = "glmnet_model()"
  )
}

#' @export
glmnet_sampler <- function(fits, newdata = NULL, indices = NULL, lambda_ix = 1, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    sigma <- deviance(fits[[i]]) / fits[[i]]$nobs
    y_ <- predict(fits[[i]], newdata = newdata, ...)[, lambda_ix]
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}

brms_model_params <- function(...) {
  defaults <- list(chains = 1, refresh = 0, silent = 0)
  modifyList(defaults, list(...))
}

brm_cache <- function(formula, data, ...) {
  models <- list()

  ys <- lhs.vars(formula)
  models[[ys[1]]] <- brm(sub_formula(formula, ys[1]), data, ...)
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
    predictor = \(object, ...) predict(object, robust = TRUE, ...)[, "Estimate"],
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
lnm_model <- function(...) {
  new(
    "model",
    estimator = \(fmla, data) inject(lnm(fmla, data, ...)),
    estimates = NULL,
    sampler = lnm_sampler,
    predictor = predict,
    model_type = "lnm_model()"
  )
}

#' @importFrom miniLNM sample
#' @importFrom formula.tools lhs.vars
lnm_sampler <- function(fit, newdata = NULL, indices = NULL, ...) {
  nm <- lhs.vars(fit@formula)
  if (is.null(indices)) {
    indices <- seq_along(nm)
  }

  sample(fit, newdata = newdata, ...)[, nm[indices], drop = FALSE]
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
    sigma <- fits[[i]]$prediction.error
    y_ <- predict(fits[[i]], data = newdata, ...)$predictions
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}
