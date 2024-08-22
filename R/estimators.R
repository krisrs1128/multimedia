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
#' @examples
#' m <- lm_model()
#' m@estimator(mpg ~ hp + wt, data = mtcars)
#'
#' m <- rf_model()
#' m@estimator(mpg ~ hp + wt, data = mtcars)
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
#'
#' prf <- parallelize(ranger::ranger)
#' prf(mpg + hp ~ wt + disp + cyl, data = mtcars)
#' @export
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
#' @noRd
edges_df <- function(edges) {
  nodes <- edges %N>%
    as.data.frame()

  edges %E>%
    as.data.frame() |>
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @importFrom dplyr arrange across
#' @importFrom tidyselect everything
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'   estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'   estimate(exper)
#'
#' # example with another model
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper, rf_model()) |>
#'   estimate(exper)
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
  model@treatments <- unique(exper@treatments) |>
    arrange(across(everything()))

  model
}

#' Linear Model across Responses
#'
#' Apply a linear model in parallel across each response $y$ in an outcome or
#' mediation model. This is often useful for mediator models with few
#' pretreatment variables, since each input is low-dimensional, even when there
#' are many responses.
#'
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a linear model.
#' @seealso model
#' @examples
#' m <- lm_model()
#' m@estimator(mpg ~ hp + wt, data = mtcars)
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
#'
#' Draw samples
#' @examples
#' fit <- lm(mpg ~ hp + wt, data = mtcars)
#' multimedia:::lm_sampler(list(f = fit))
#'
#' plm <- parallelize(lm)
#' fit <- plm(mpg + disp ~ hp + wt, data = mtcars)
#' multimedia:::lm_sampler(fit)
#' @importFrom dplyr bind_cols
#' @noRd
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

#' Default parameters for glmnet_model
#' @noRd
glmnet_model_params <- function(...) {
  defaults <- list(intercept = FALSE, lambda = 0.01)
  modifyList(defaults, list(...))
}

#' Regularized Glmnet Model across Responses
#'
#' Apply a regularized (generalized) linear model in parallel across each
#' response $y$ in an outcome or mediation model. This can be helpful when we
#' have many mediators or pretreatment variables, making the input
#' high-dimensional.
#'
#' @param ... Keyword parameters passed to glmnet.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a lienar model.
#' @seealso model lm_model rf_model
#' @importFrom glmnetUtils glmnet
#' @importFrom insight check_if_installed
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper, glmnet_model(lambda = 1)) |>
#'   estimate(exper)
#'
#' multimedia(exper, glmnet_model(lambda = 1), glmnet_model()) |>
#'   estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper, glmnet_model(lambda = 0.1)) |>
#'   estimate(exper)
#' @export
glmnet_model <- function(...) {
  check_if_installed(
    c("glmnet", "glmnetUtils"),
    "to use a glmnet regression model for multimedia estimation."
  )
  requireNamespace("glmnetUtils", quietly = TRUE)

  params <- glmnet_model_params(...)
  new(
    "model",
    estimator = parallelize(\(fmla, data) inject(glmnetUtils::glmnet(fmla, data, !!!params))),
    estimates = NULL,
    sampler = glmnet_sampler,
    predictor = \(object, ...) {
      predict(object, s = object$lambda.1se, ...)[, 1]
    },
    model_type = "glmnet_model()"
  )
}

#' Sample from a Glmnet Model
#'
#' This assumes a continuous response, so that the out-of-sample MSE can be used
#' to estimate the outcome variability sigma.
#' @examples
#' m <- glmnet_model()
#' fit <- m@estimator(mpg ~ hp + wt, data = mtcars)
#' multimedia:::glmnet_sampler(fit, mtcars)
#'
#' plm <- parallelize(glmnetUtils::glmnet)
#' fit <- plm(mpg + disp ~ hp + wt, data = mtcars)
#' multimedia:::glmnet_sampler(fit, mtcars)
#' @noRd
glmnet_sampler <- function(fits, newdata = NULL, indices = NULL, lambda_ix = 1, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }
  requireNamespace("glmnetUtils", quietly = TRUE)

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    sigma <- deviance(fits[[i]]) / fits[[i]]$nobs
    y_ <- predict(fits[[i]], newdata = newdata, ...)[, lambda_ix]
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}

#' Default parameters for brms model
#' @noRd
brms_model_params <- function(...) {
  defaults <- list(chains = 1, refresh = 0, silent = 0)
  modifyList(defaults, list(...))
}

#' Refit BRMS Models without Recompilation
#'
#' The most time-consuming part of using BRMS in parallel across many responses
#' is waiting for compilation to complete. It is more efficient instead to
#' compile the model once and estimate many models using different datasets.
#' That is the approach adopted in this function, which speeds up over a naive
#' loop.
#'
#' @param formula A multiresponse/multi-input formula of the form
#'  \deqn{
#'  y1 + y2 + ... ~ x1 + x2 + ..
#'  }
#'  with which to estimate a BRMS model
#' @param data A data.frame containing all the variables in the formula and used
#'  as the basis for estimation.
#' @return models A list of estimated BRMS models. The j^th element contains
#'  y[j] ~ x1 + x2 + ...
#' @noRd
brm_cache <- function(formula, data, ...) {
  models <- list()

  ys <- lhs.vars(formula)
  models[[ys[1]]] <- brms::brm(sub_formula(formula, ys[1]), data, ...)
  if (length(ys) == 1) {
    return(models)
  }

  for (j in seq(2, length(ys))) {
    fmla <- sub_formula(formula, ys[j])
    models[[ys[j]]] <- update(models[[1]], fmla, newdata = data, recompile = FALSE, ...)
  }
  models
}

#' Bayesian Regression Model across Responses
#'
#' Apply a Bayesian regression model in parallel across each response $y$ in
#' an outcome or mediation model. This can be helpful when we want to share information across related
#' @param ... Keyword parameters passed to brm..
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a Bayesian regression model.
#' @importFrom rlang inject !!!
#' @seealso glmnet_model lnm_model rf_model lm_model
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper, brms_model()) |>
#'   estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper, brms_model()) |>
#'   estimate(exper)
#' fit@outcome
#' @export
brms_model <- function(...) {
  check_if_installed("brms", "to use a BRMS model for multimedia estimation.")
  params <- brms_model_params(...)
  requireNamespace("brms", quietly = TRUE)
  new(
    "model",
    estimator = \(fmla, data) inject(brm_cache(fmla, data, !!!params)),
    estimates = NULL,
    sampler = brms_sampler,
    predictor = \(object, ...) predict(object, robust = TRUE, ...)[, "Estimate"],
    model_type = "brms_model()"
  )
}

#' Sample from a Bayesian Regression Model
#'
#' This samples from the posterior predictive for each component in
#' a multiresponse Bayesian Regression model.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which we want to sample.
#' @importFrom insight check_if_installed
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' fit <- multimedia(exper, brms_model()) |>
#'   estimate(exper)
#' brms_sampler(fit@outcome@estimates)
#' @noRd
brms_sampler <- function(fits, newdata = NULL, indices = NULL, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }
  check_if_installed("brms", "to use a BRMS model for multimedia estimation.")

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    y_hats[[nm[i]]] <- brms::posterior_predict(
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

#' Logistic Normal Multinomial Model
#'
#' Apply a logistic normal multinomial model to jointly model a vector of count
#' responses $y$ in an outcome or mediation model. This is a common choice for
#' data where the parameter of interest is the composition across responses
#' (e.g., microbiome).
#'
#' @param ... Keyword parameters passed to lnm.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a lienar model.
#' @seealso model lm_model rf_model glmnet_model brms_model
#' @examples
#' m <- lnm_model()
#' mat <- data.frame(matrix(rpois(250, 10), 25, 10))
#' colnames(mat) <- paste0("y", 1:6)
#' fit <- m@estimator(y1 + y2 + y3 + y4 ~ y5 + y6, mat)
#' @export
lnm_model <- function(...) {
  check_if_installed(
    "miniLNM",
    "to use a LNM model for multimedia estimation. Please run devtools::install_github('krisrs1128/miniLNM')",
    prompt = FALSE
  )
  new(
    "model",
    estimator = \(fmla, data) inject(miniLNM::lnm(fmla, data, ...)),
    estimates = NULL,
    sampler = lnm_sampler,
    predictor = predict,
    model_type = "lnm_model()"
  )
}

#' Sample from the Logistic Normal Multinomial
#'
#' This samples from the posterior predictive of a fitted logistic-normal
#' multinomial model.
#'
#' @param fit The fitted LNM model from which to draw posterior predictive
#'   samples.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which to draw samples.
#' @return y_star A data.frame of samples y associated wtih the new inputs.
#' @importFrom formula.tools lhs.vars
#' @examples
#' m <- lnm_model()
#' mat <- data.frame(matrix(rpois(250, 10), 25, 10))
#' colnames(mat) <- paste0("y", 1:6)
#' fit <- m@estimator(y1 + y2 + y3 + y4 ~ y5 + y6, mat)
#' lnm_sampler(fit, depth = 10)
#' lnm_sampler(fit, depth = 100)
#' @noRd
lnm_sampler <- function(fit, newdata = NULL, indices = NULL, ...) {
  nm <- lhs.vars(fit@formula)
  if (is.null(indices)) {
    indices <- seq_along(nm)
  }

  miniLNM::sample(fit, newdata = newdata, ...)[, nm[indices], drop = FALSE]
}

#' Random Forest Model
#'
#' Apply a random forest model in parallel across a vector of responses $y$ in
#' either an outcome or mediation model. This is a natural choice when the
#' relationship between inputs and outputs is thought to be nonlinear.
#' Internally, each of the models across the response are estimated using
#' ranger.
#'
#' @param ... Keyword parameters passed to ranger.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a lienar model.
#' @examples
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper, rf_model(num.trees = 10)) |>
#'   estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper, rf_model(num.trees = 20, max.depth = 2)) |>
#'   estimate(exper)
#' @seealso model lm_model rf_model glmnet_model brms_model
#' @export
rf_model <- function(...) {
  check_if_installed("ranger", "to use a random forest model for multimedia estimation.")
  requireNamespace("ranger", quietly = TRUE)

  new(
    "model",
    estimator = parallelize(\(fmla, data) ranger::ranger(fmla, data, ...)),
    estimates = NULL,
    sampler = rf_sampler,
    model_type = "rf_model()",
    predictor = \(object, ...) predict(object, ...)$predictions
  )
}

#' Sample from a Random Forest Model
#'
#' This assumes a continuous response, so that the out-of-sample MSE can be used
#' to estimate the outcome variability \eqn{\sigma}.
#'
#' @param fit The fitted LNM model from which to draw posterior predictive
#'   samples.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which to draw samples.
#' @return y_star A data.frame of samples y associated wtih the new inputs.
#' @importFrom dplyr bind_cols
#' @examples
#' m <- rf_model()
#' fit <- m@estimator(mpg ~ hp + wt, data = mtcars)
#' multimedia:::rf_sampler(fit, mtcars)
#'
#' prf <- parallelize(ranger::ranger)
#' fit <- prf(mpg + disp ~ hp + wt, data = mtcars)
#' multimedia:::rf_sampler(fit, mtcars)
#' @noRd
rf_sampler <- function(fits, newdata = NULL, indices = NULL, ...) {
  if (is.null(indices)) {
    indices <- seq_along(fits)
  }
  requireNamespace("ranger", quietly = TRUE)

  nm <- names(fits)
  y_hats <- list()
  for (i in indices) {
    sigma <- fits[[i]]$prediction.error
    y_ <- predict(fits[[i]], data = newdata, ...)$predictions
    y_hats[[nm[i]]] <- y_ + rnorm(length(y_), 0, sigma)
  }

  bind_cols(y_hats)
}
