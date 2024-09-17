#' Representation of an Outcome or Mediation Model
#'
#' To work with many model types simultaneously, multimedia uses a model class
#' with the necessary mediation model functionality that wraps any specific
#' implementation. The slots below define the generally required functionality
#' for any specific implementation.
#'
#' @slot estimator A function that takes a formula, input data frame X, and an
#'   response data.frame $Y$ and returns a model. For example, for the random
#'   forest model, this is created by wrapping `parallelize()` on the ranger()
#'   function for random forest estimation function using the 'ranger' package.
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
#' estimator(m)(mpg ~ hp + wt, data = mtcars)
#'
#' m <- rf_model()
#' estimator(m)(mpg ~ hp + wt, data = mtcars)
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

#' Helper to Modify Formulas
#' @param formula The original formula whose response we want to modify.
#' @param yj The desired response term for the formula.
#' @return A new formula object with the LHS replaced by yj.
#' @importFrom stats update.formula as.formula
sub_formula <- function(formula, yj) {
    update.formula(formula, as.formula(glue("{yj} ~ .")))
}

#' Parallelize Estimation across Responses
#'
#' For many mediation and outcome models, we simply want to apply a univariate
#' model across all response variable. Parallelize enables this conversion. For
#' example, applying parallelize to ranger() returns a function that estimates
#' separate random forest models for each response on the left hand side of a
#' formula.
#' @param f A function for estimating a single response model given a formula
#'  and input dataset. This is the model that we would like to parallelize
#'  across responses.
#' @param progress A logical indicating whether to show a progress bar.
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
parallelize <- function(f, progress = TRUE) {
    function(formula, ...) {
        models <- list()

        ys <- lhs.vars(formula)
        pb <- progress_bar$new(
            total = length(ys),
            format = "[:bar] :current/:total ETA: :eta"
        )
        for (j in seq_along(ys)) {
            fmla <- sub_formula(formula, ys[j])
            models[[ys[j]]] <- f(fmla, ...)
            if (progress) pb$tick()
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
#'  node, this will be included in the graph (as will all pretreatment to
#'  mediator, mediator to outcome, etc.)
#' @importFrom tidygraph %N>%
#' @importFrom dplyr left_join rename
#' @importFrom rlang .data
#' @noRd
edges_df <- function(edges) {
    nodes <- edges %N>%
        as.data.frame()

    edges %E>%
        as.data.frame() |>
        left_join(nodes, by = c("from" = "id")) |>
        dplyr::rename(
            node_type_from = "node_type",
            name_from = "name"
        ) |>
        left_join(nodes, by = c("to" = "id")) |>
        dplyr::rename(
            node_type_to = "node_type",
            name_to = "name"
        )
}

#' Construct a Formula from a Graph
#' @importFrom dplyr pull
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
#' @importFrom rlang .data
#' @noRd
mediation_formula <- function(edges) {
    edges %E>%
        filter(.data$state == "active") |>
        edges_df() |>
        filter(.data$node_type_to == "mediator") |>
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
#' @importFrom rlang .data
#' @noRd
outcome_formula <- function(edges) {
    edges %E>%
        filter(.data$state == "active") |>
        edges_df() |>
        filter(.data$node_type_to == "outcome") |>
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
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'     estimate(exper)
#'
#' # example with another model
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper, glmnet_model()) |>
#'     estimate(exper)
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
#' @param progress A logical indicating whether to show a progress bar during
#'   estimation.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a linear model.
#' @seealso model
#' @importFrom stats lm
#' @examples
#' m <- lm_model()
#' estimator(m)(mpg ~ hp + wt, data = mtcars)
#' @export
lm_model <- function(progress = TRUE) {
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
#' Draw samples from a fitted linear model.
#'
#' @param fits The fitted linear model model from which to draw samples.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which to draw samples.
#' @param ... Additional parameters passed to lm.predict
#' @return y_star A data.frame of samples y associated with the new inputs.
#' @examples
#' fit <- lm(mpg ~ hp + wt, data = mtcars)
#' lm_sampler(list(f = fit))
#'
#' plm <- parallelize(lm)
#' fit <- plm(mpg + disp ~ hp + wt, data = mtcars)
#' lm_sampler(fit)
#' @importFrom dplyr bind_cols
#' @importFrom stats lm
#' @export
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
#' @importFrom utils modifyList
#' @noRd
glmnet_model_params <- function(...) {
    defaults <- list(intercept = FALSE, lambda = 0.01)
    modifyList(defaults, list(...))
}

#' Regularized 'Glmnet' Model across Responses
#'
#' Apply a regularized (generalized) linear model in parallel across each
#' response $y$ in an outcome or mediation model. This can be helpful when we
#' have many mediators or pretreatment variables, making the input
#' high-dimensional.
#'
#' @param progress A logical indicating whether to show a progress bar during
#'   estimation.
#' @param ... Keyword parameters passed to package 'glmnet'.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a lienar model.
#' @seealso model lm_model rf_model
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper, glmnet_model(lambda = 1)) |>
#'     estimate(exper)
#'
#' multimedia(exper, glmnet_model(lambda = 1), glmnet_model()) |>
#'     estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper, glmnet_model(lambda = 0.1)) |>
#'     estimate(exper)
#' @importFrom glmnetUtils glmnet
#' @export
glmnet_model <- function(progress = TRUE, ...) {
    params <- glmnet_model_params(...)
    new(
        "model",
        estimator = parallelize(
            \(fmla, data) inject(glmnet(fmla, data, !!!params)),
            progress = progress
        ),
        estimates = NULL,
        sampler = glmnet_sampler,
        predictor = \(object, ...) {
            predict(object, s = object$lambda.1se, ...)[, 1]
        },
        model_type = "glmnet_model()"
    )
}

#' Sample from a 'Glmnet' Package Model
#'
#' This assumes a continuous response, so that the out-of-sample MSE can be used
#' to estimate the outcome variability sigma.
#'
#' @param fits The fitted 'glmnet' package model model from which to draw
#'   samples.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which to draw samples.
#' @param lambda_ix A regularization strength parameter used to maintain
#'   consistency with estimation. Not used during sampling.
#' @param ... Additional parameters to pass to predict.glmnet
#' @return y_star A data.frame of samples y associated with the new inputs.
#' @importFrom stats deviance
#' @examples
#' m <- glmnet_model()
#' fit <- estimator(m)(mpg ~ hp + wt, data = mtcars)
#' glmnet_sampler(fit, mtcars)
#'
#' plm <- parallelize(glmnetUtils::glmnet)
#' fit <- plm(mpg + disp ~ hp + wt, data = mtcars)
#' glmnet_sampler(fit, mtcars)
#' @export
glmnet_sampler <- function(
    fits, newdata = NULL, indices = NULL, lambda_ix = 1, ...) {
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

#' Default parameters for 'BRMS' package models
#' @noRd
brms_model_params <- function(...) {
    defaults <- list(chains = 1, refresh = 0, silent = 0)
    modifyList(defaults, list(...))
}

#' Refit 'BRMS' Models without Recompilation
#'
#' The most time-consuming part of using 'BRMS' in parallel across many
#' responses is waiting for compilation to complete. It is more efficient
#' instead to compile the model once and estimate many models using different
#' datasets. That is the approach adopted in this function, which speeds up over
#' a naive loop.
#'
#' @param formula A multiresponse/multi-input formula of the form
#'  \deqn{
#'  y1 + y2 + ... ~ x1 + x2 + ..
#'  }
#'  with which to estimate a 'BRMS' model
#' @param data A data.frame containing all the variables in the formula and used
#'  as the basis for estimation.
#' @return models A list of estimated 'BRMS' models. The j^th element contains
#'  y[j] ~ x1 + x2 + ...
#' @importFrom stats update
#' @importFrom brms brm
#' @noRd
brm_cache <- function(formula, data, ...) {
    models <- list()

    ys <- lhs.vars(formula)
    models[[ys[1]]] <- brm(sub_formula(formula, ys[1]), data, ...)
    if (length(ys) == 1) {
        return(models)
    }

    for (j in seq(2, length(ys))) {
        fmla <- sub_formula(formula, ys[j])
        models[[ys[j]]] <- update(
            models[[1]], fmla,
            newdata = data, recompile = FALSE, ...
        )
    }
    models
}

#' Bayesian Regression Model across Responses
#'
#' Apply a Bayesian regression model in parallel across each response $y$ in an
#' outcome or mediation model. This can be helpful when we want to share
#' information across related
#' @param ... Keyword parameters passed to brm.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a Bayesian regression model.
#' @importFrom rlang inject !!!
#' @seealso glmnet_model lnm_model rf_model lm_model
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper, brms_model()) # call estimate() on this to fit
#' @export
brms_model <- function(...) {
    params <- brms_model_params(...)
    new(
        "model",
        estimator = \(fmla, data) inject(brm_cache(fmla, data, !!!params)),
        estimates = NULL,
        sampler = brms_sampler,
        predictor = \(object, ...) {
            predict(object, robust = TRUE, ...)[, "Estimate"]
        },
        model_type = "brms_model()"
    )
}

#' Accessor for Model Estimators
#' @param object An object of class `model` whose estimator we would like to
#'   call.
#' @return A function that can be called with formula and data arguments, like
# `   lm()`.
#' @examples
#' m <- lm_model()
#' estimator(m)(mpg ~ hp + wt, data = mtcars)
#' @export
estimator <- function(object) {
    object@estimator
}

#' Accessor for Outcome Models
#' @param object An object of class multimedia whose outcome model estimates we
#'   would like to extract.
#' @return A list containing all the fitted outcome models.
#' @examples
#' data(mindfulness)
#' exper <- mediation_data(
#'     mindfulness,
#'     phyloseq::taxa_names(mindfulness),
#'     "treatment",
#'     starts_with("mediator"),
#'     "subject"
#' )
#'
#' m <- multimedia(exper)
#' outcome_models(m)
#' @export
outcome_models <- function(object) {
    object@outcome@estimates
}

#' Accessor for Outcome Models
#' @param object An object of class multimedia whose outcome model estimates we
#'   would like to extract.
#' @return A list containing all the fitted mediation models.
#' @examples
#' data(mindfulness)
#' exper <- mediation_data(
#'     mindfulness,
#'     phyloseq::taxa_names(mindfulness),
#'     "treatment",
#'     starts_with("mediator"),
#'     "subject"
#' )
#'
#' m <- multimedia(exper)
#' mediation_models(m)
#' @export
mediation_models <- function(object) {
    object@mediation@estimates
}


#' Sample from a Bayesian Regression Model
#'
#' This samples from the posterior predictive for each component in
#' a multiresponse Bayesian Regression model.
#' @param fits The fitted 'BRMS' model model from which to draw samples.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which we want to sample.
#' @param ... Additional arguments to pass to `posterior_predict` in the 'brms'
#'   package.
#' @return A data.frame containing a single posterior predictive sample at each
#'   of the newdata rows passed into a fitted BRMS model. Each column
#'   corresponds to one outcome variable, each row to the associated row in the
#'   newdata input..
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

#' Logistic Normal Multinomial Model
#'
#' Apply a logistic normal multinomial model to jointly model a vector of count
#' responses $y$ in an outcome or mediation model. This is a common choice for
#' data where the parameter of interest is the composition across responses
#' (e.g., microbiome).
#'
#' @param ... Keyword parameters passed to lnm in the 'miniLNM' package.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a linear model.
#' @seealso model lm_model rf_model glmnet_model brms_model
#' @examples
#' m <- lnm_model()
#' mat <- data.frame(matrix(rpois(250, 10), 25, 10))
#' colnames(mat) <- paste0("y", seq_len(6))
#' fit <- estimator(m)(y1 + y2 + y3 + y4 ~ y5 + y6, mat)
#' @importFrom miniLNM lnm predict
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
#' @param ... Additional parameters passed to sample.
#' @return y_star A data.frame of samples y associated wtih the new inputs.
#' @importFrom formula.tools lhs.vars
#' @importFrom miniLNM sample
#' @examples
#' m <- lnm_model()
#' mat <- data.frame(matrix(rpois(250, 10), 25, 10))
#' colnames(mat) <- paste0("y", 1:6)
#' fit <- estimator(m)(y1 + y2 + y3 + y4 ~ y5 + y6, mat)
#' lnm_sampler(fit, depth = 10)
#' lnm_sampler(fit, depth = 100)
#' @export
lnm_sampler <- function(fit, newdata = NULL, indices = NULL, ...) {
    nm <- lhs.vars(fit@formula)
    if (is.null(indices)) {
        indices <- seq_along(nm)
    }
    sample(fit, newdata = newdata, ...)[, nm[indices], drop = FALSE]
}

#' Random Forest Model
#'
#' Apply a random forest model in parallel across a vector of responses $y$ in
#' either an outcome or mediation model. This is a natural choice when the
#' relationship between inputs and outputs is thought to be nonlinear.
#' Internally, each of the models across the response are estimated using
#' the 'ranger' package.
#'
#' @param progress A logical indicating whether to show a progress bar during
#'   estimation.
#' @param ... Keyword parameters passed to ranger() in the 'ranger' package.
#' @return model An object of class `model` with estimator, predictor, and
#'  sampler functions associated wtih a lienar model.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper, rf_model(num.trees = 10)) |>
#'     estimate(exper)
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper, rf_model(num.trees = 20, max.depth = 2)) |>
#'     estimate(exper)
#' @seealso model lm_model rf_model glmnet_model brms_model
#' @importFrom ranger ranger
#' @export
rf_model <- function(progress = TRUE, ...) {
    new(
        "model",
        estimator = parallelize(
            \(fmla, data) ranger(fmla, data, ...),
            progress
        ),
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
#' @param fits The fitted RF model from which to draw samples.
#' @param newdata A data.frame containing new inputs from which to sample
#'   responses. If NULL, defaults to the data used to estimate fit.
#' @param indices The coordinates of the response from which to draw samples.
#' @param ... Additional parameters passed to rf_model's predict method.
#' @return y_star A data.frame of samples y associated with the new inputs.
#' @importFrom dplyr bind_cols
#' @examples
#' m <- rf_model()
#' fit <- estimator(m)(mpg ~ hp + wt, data = mtcars)
#' rf_sampler(fit, mtcars)
#'
#' prf <- parallelize(ranger::ranger)
#' fit <- prf(mpg + disp ~ hp + wt, data = mtcars)
#' rf_sampler(fit, mtcars)
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

#' Estimate a Multimedia Model
#'
#' This defines a generic estimator function, which can be applied to different
#' multimedia model objects. It creates a unified interface to estimating
#' diverse mediation and outcome model families.
#' @param object A model object that we want to estimate.
#' @return A fitted version of the input model class.
#' @examples
#' m <- rf_model()
#' fit <- estimator(m)(mpg ~ hp + wt, data = mtcars)
#' @export
setGeneric("estimator", \(object) standardGeneric("estimator"))

#' Accessor for Estimators
#' @param object An object of class `model`, whose estimator function we want
#'   access to.
#' @return A fitted version of the input model class.
#' @examples
#' m <- rf_model()
#' fit <- estimator(m)(mpg ~ hp + wt, data = mtcars)
#' @export
setMethod("estimator", "model", \(object) object@estimator)
