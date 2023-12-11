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
lm_sampler <- function(fits, new_data = NULL) {
  y_hats <- list()
  for (i in seq_along(fits)) {
    sigma <- summary(fits[[i]])$sigma
    y_ <- predict(fits[[i]], newdata = new_data)
    y_hats[[i]] <- y_ + rnorm(length(y_), 0, sigma)
  }
  names(y_hats) <- names(fits)
  bind_cols(y_hats)
}

default_treatment <- function(edges, estimates) {
  treatment_names <- edges |>
    filter(node_type == "treatment") |>
    pull(name)

  model.frame(estimates[[1]]) |>
    select(any_of(treatment_names))
}

path_defaults <- function(x, t_mediator, t_outcome) {
  f_out <- x@outcome
  f_med <- x@mediation

  # fill in treatments with defaults
  if (is.null(t_mediator)) {
    t_mediator <- default_treatment(x@edges, f_med@estimates)
  }
  if (is.null(t_outcome)) {
    t_outcome <- default_treatment(x@edges, f_out@estimates)
  }
  list(f_out = f_out, f_med = f_med, t_mediator = t_mediator, t_outcome = t_outcome)
}

setMethod("sample", "multimedia", function(x, size, pretreatment = NULL, t_mediator = NULL,
                                           t_outcome = NULL, mediators = NULL, ...) {
  if (missing(size)) {
    size <- 1
  }

  # if mediators are not provided, sample them
  d <- path_defaults(x, t_mediator, t_outcome)
  if (is.null(mediators)) {
    mediator_covariates <- bind_cols(pretreatment, d$t_mediator)
    mediators <- d$f_med@sampler(d$f_med@estimates, mediator_covariates)
  }

  # sample outcome given everything else
  outcome_covariates <- bind_cols(pretreatment, d$t_outcome, mediators)
  outcomes <- d$f_out@sampler(d$f_out@estimates, outcome_covariates)
  list(mediators = mediators, outcomes = outcomes)
})

setMethod("predict", "multimedia", function(
    object, pretreatment = NULL,
    t_mediator = NULL, t_outcome = NULL, mediators = NULL, ...) {
  # if mediators are not provided, sample them
  d <- path_defaults(object, t_mediator, t_outcome)
  if (is.null(mediators)) {
    mediator_covariates <- bind_cols(pretreatment, d$t_mediator)
    mediators <- map_dfc(d$f_med@estimates, ~ predict(., mediator_covariates))
  }

  # sample outcome given everything else
  outcome_covariates <- bind_cols(pretreatment, d$t_outcome, mediators)
  outcomes <- map_dfc(d$f_out@estimates, ~ predict(., outcome_covariates))
  list(mediators = mediators, outcomes = outcomes)
})

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
  mediation_est <- model@mediation@estimator
  model@mediation@estimates <- mediation_est(
    mediation_formula(model@edges),
    exper_df(exper)
  )

  # estimate outcome model
  outcome_est <- model@outcome@estimator
  model@outcome@estimates <- outcome_est(
    outcome_formula(model@edges),
    exper_df(exper)
  )

  model
}
