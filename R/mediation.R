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

#' @importFrom SummarizedExperiment assay colData
exper_df <- function(exper) {
  bind_cols(t(assay(exper)), as_tibble(colData(exper)))
}

#' @importFrom dplyr bind_cols select
match_names <- function(names, exper, env) {
  exper_df(exper) |>
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