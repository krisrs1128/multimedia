setClass(
  "multimedia",
  representation(
    edges = "ANY",
    outcome = "ANY",
    mediation = "ANY"
  )
)

#' @importFrom tidygraph graph_join %N>%
graph_specification <- function(outcomes, treatments, mediators, pretreatment) {
  edges <- list(
    expand_edges(treatments, mediators, "treatment", "mediator"),
    expand_edges(mediators, outcomes, "mediator", "outcome"),
    expand_edges(treatments, outcomes, "treatment", "outcome")
  )

  if (length(pretreatment) > 0) {
    new_edges <- list(
      expand_edges(pretreatment, outcomes, "pretreatment", "outcome"),
      expand_edges(pretreatment, mediators, "pretreatment", "mediator")
    )
    edges <- c(edges, new_edges)
  }

  suppressMessages(
    reduce(edges, graph_join) %N>%
      mutate(node_type = factor(node_type, levels = c("pretreatment", "treatment", "mediator", "outcome"))) |>
      arrange(node_type, name)
  )
}

#' @importFrom tidygraph %E>% as_tbl_graph
expand_edges <- function(input, output, input_name, output_name) {
  expand.grid(input, output) |>
    as_tbl_graph() |>
    mutate(
      node_type = case_when(
        name %in% input ~ input_name,
        name %in% output ~ output_name,
      )
    ) %E>%
    mutate(state = "active")
}

#' @importFrom dplyr bind_cols select
#' @importFrom SummarizedExperiment assay colData
match_names <- function(names, exper, env) {
  bind_cols(t(assay(exper)), as_tibble(colData(exper))) |>
    select(eval(names, env)) |>
    colnames()
}

#' @export
multimedia <- function(exper, outcomes, treatments, mediators = NULL,
                       pretreatment = NULL,
                       outcome_estimator = lm_model(),
                       mediation_estimator = lm_model()) {
  vars <- list(
    outcomes = quote(outcomes),
    treatments = quote(treatments),
    mediators = quote(mediators),
    pretreatment = quote(pretreatment)
  )

  for (i in seq_along(vars)) {
    vars[[i]] <- match_names(vars[[i]], exper, environment())
  }

  new(
    "multimedia",
    edges = do.call(graph_specification, vars),
    outcome = lm_model(),
    mediation = lm_model()
  )
}

print_sub <- function(x) {
  if (length(x) > 2) {
    res <- paste0(c(head(x, 1), "...", tail(x, 1)), collapse = ",")
  } else {
    res <- paste0(x, collapse = ",")
  }
  res
}

fit_type <- function(model) {
  ifelse(is.null(model@estimates), "An unfitted", "A fitted")
}

node_subset <- function(edges, condition) {
  edges |>
    filter(node_type %in% c(condition)) |>
    pull(name)
}

#' @importFrom cli col_magenta col_cyan
setMethod("show", "multimedia", function(object) {
  cat(col_magenta("[Mediation Analysis Object]"), "\n")
  cat(glue("Outcomes: {print_sub(node_subset(object@edges, 'outcome'))}"), "\n")
  cat(glue("Treatments: {print_sub(node_subset(object@edges, 'treatment'))}"), "\n")
  cat(glue("Mediators: {print_sub(node_subset(object@edges, 'mediator'))}"), "\n")

  pretreatment <- node_subset(object@edges, "pretreatment")
  if (length(pretreatment) > 0) {
    cat(glue("Pretreatment: {pretreatment}"), "\n")

  }

  cat(col_magenta("\n[Models]\n"))
  cat(glue("mediation: {fit_type(object@mediation)} {object@mediation@model_type}."), "\n")
  cat(glue("outcome: {fit_type(object@outcome)} {object@outcome@model_type}."), "\n")
})

