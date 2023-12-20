#' @importFrom dplyr row_number n
#' @importFrom tidygraph %E>%
matching_indices <- function(edges, nulls = NULL) {
  if (is.null(nulls)) {
    ids <- quote(1:n())
  }

  G <- as_tibble(edges)
  E <- edges %E>%
    as_tibble() |>
    mutate(
      edge_id = row_number(),
      from_type = G$node_type[from],
      to_type = G$node_type[to],
    )

  if (nulls == "T->Y") {
    ids <- filter(E, from_type == "treatment", to_type == "outcome")
  } else if (nulls == "T->M") {
    ids <- filter(E, from_type == "treatment", to_type == "mediator")
  } else if (nulls == "M->Y") {
    ids <- filter(E, from_type == "mediator", to_type == "outcome")
  } else {
    ids <- filter(E, nulls)
  }
  pull(ids, edge_id)
}

nullify <- function(multimedia, nulls = NULL) {
  nulls <- matching_indices(multimedia@edges, nulls)
  multimedia@edges <- multimedia@edges %E>%
    mutate(
      new_null = row_number() %in% nulls,
      state = ifelse(state == "active" & new_null, "inactive", state)
    ) |>
    select(-new_null) |>
    activate(nodes)
  multimedia
}
