#' @export
setClass(
  "multimedia",
  representation(
    edges = "ANY",
    outcome = "ANY",
    mediation = "ANY",
    treatments = "ANY"
  )
)

#' @export
setClass(
  "mediation_data",
  representation(
    mediators = "data.frame",
    outcomes = "data.frame",
    treatments = "data.frame",
    pretreatments = "ANY"
  )
)

#' @importFrom purrr map
bind_mediation <- function(exper) {
  map_dfc(slotNames(exper), ~ slot(exper, .))
}

#' @importFrom purrr reduce
#' @importFrom tidygraph graph_join %N>%
graph_specification <- function(outcomes, treatments, mediators, pretreatments) {
  edges <- list(
    expand_edges(c("1"), mediators, "intercept", "mediator"),
    expand_edges(c("1"), outcomes, "intercept", "outcome"),
    expand_edges(treatments, mediators, "treatment", "mediator"),
    expand_edges(mediators, outcomes, "mediator", "outcome"),
    expand_edges(treatments, outcomes, "treatment", "outcome")
  )

  if (length(pretreatments) > 0) {
    new_edges <- list(
      expand_edges(pretreatments, outcomes, "pretreatment", "outcome"),
      expand_edges(pretreatments, mediators, "pretreatment", "mediator")
    )
    edges <- c(edges, new_edges)
  }

  suppressMessages(
    purrr::reduce(edges, graph_join) %N>%
      mutate(
        node_type = factor(
          node_type,
          levels = c("intercept", "pretreatment", "treatment", "mediator", "outcome")
        )
      ) |>
      arrange(node_type, name) |>
      mutate(id = row_number())
  )
}

#' @importFrom tidygraph %E>% as_tbl_graph activate
expand_edges <- function(input, output, input_name, output_name) {
  expand.grid(input, output) |>
    as_tbl_graph(node_key = "name") |>
    mutate(
      node_type = case_when(
        name %in% input ~ input_name,
        name %in% output ~ output_name,
      )
    ) %E>%
    mutate(state = "active") |>
    activate(nodes)
}

#' @importFrom SummarizedExperiment assay colData
exper_df <- function(exper) {
  bind_cols(t(assay(exper)), as_tibble(colData(exper), .name_repair = "minimal"))
}

#' @export
mediation_data <- function(x, outcomes, treatments, mediators,
                           pretreatments = NULL) {
  result <- NULL
  if ("SummarizedExperiment" %in% class(x)) {
    result <- from_summarized_experiment(
      x,
      outcomes,
      treatments,
      mediators,
      pretreatments
    )
  }

  if ("data.frame" %in% class(x)) {
    result <- from_data_frame(
      x,
      outcomes,
      treatments,
      mediators,
      pretreatments
    )
  }

  result
}

from_summarized_experiment <- function(exper, outcomes, treatments, mediators,
                                       pretreatments) {
  exper_df(exper) |>
    from_data_frame(outcomes, treatments, mediators, pretreatments)
}

#' @importFrom dplyr bind_cols select
from_data_frame <- function(df, outcomes, treatments, mediators, 
                            pretreatments) {
  vars <- list(
    outcomes = quote(outcomes),
    treatments = quote(treatments),
    mediators = quote(mediators),
    pretreatments = quote(pretreatments)
  )

  result <- list()
  for (i in seq_along(vars)) {
    result[[names(vars)[i]]] <- select(df, eval(vars[[i]])) |>
      mutate(across(where(is.character), as.factor)) |>
      as_tibble(.name_repair = "minimal")
  }

  do.call(\(...) new("mediation_data", ...), result)
}

#' @export
multimedia <- function(mediation_data,
                       outcome_estimator = lm_model(),
                       mediation_estimator = lm_model()) {
  vars <- list(
    outcomes = colnames(mediation_data@outcomes),
    treatments = colnames(mediation_data@treatments),
    mediators = colnames(mediation_data@mediators),
    pretreatments = colnames(mediation_data@pretreatments)
  )

  new(
    "multimedia",
    edges = do.call(graph_specification, vars),
    outcome = outcome_estimator,
    mediation = mediation_estimator
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

#' @export
setGeneric("nrow", function(x) nrow(x))

#' @export
setMethod(nrow, "mediation_data", function(x) {
  nrow(x@outcomes)
})

#' @export
setMethod("[", "mediation_data",
    function(x, i, j, ..., drop=TRUE) {
      x@mediators <- x@mediators[i, ]
      x@outcomes <- x@outcomes[i, ]
      x@treatments <- x@treatments[i, ]
      x@pretreatments <- x@pretreatments[i, ]
      x
    }
)
