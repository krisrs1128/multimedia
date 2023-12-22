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

#' Nullify Active Edges
#' @export
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

#' @importFrom progress progress_bar
#' @export
bootstrap <- function(model, exper, fs = c(direct = direct_effect), B = 100) {
  stats <- list()
  pb <- progress_bar$new(total = B, format = "[:bar] :current/:total ETA: :eta")
  if (is.null(names(fs))) {
    names(fs) <- seq_along(fs)
  }

  for (b in seq_len(B)) {
    exper_b <- exper[sample(nrow(exper), nrow(exper), replace = TRUE), ]
    model_b <- estimate(model, exper_b)
    for (f in seq_along(fs)) {
      stats[[b]] <- fs[i](model_b, exper_b) |>
        bind_rows(.id = "path") |>
        mutate(path = names(fs)[as.integer(f)])
    }
    pb$tick()
  }
  bind_rows(stats, .id = "bootstrap")
}

#' @export
null_contrast <- function(model, exper, nullification = "T->Y",
                          f = direct_effect) {
  cli_text("Fitting the nullified model...")
  altered <- model |>
    nullify(nullification) |>
    estimate(exper)

  cli_text("Generating synthetic data...")
  profile <- setup_profile(altered, exper@treatments, exper@treatments)
  synth_data <- sample(
    altered,
    profile = profile,
    pretreatment = exper@pretreatments
  )

  cli_text("Fitting the full model on synthetic data...")
  synth_model <- estimate(model, synth_data)

  cli_text("Estimating effects on real and synthetic data...")
  bind_rows(
    real = f(model, exper),
    synthetic = f(synth_model, synth_data),
    .id = "source"
  )
}

fdr_summary <- function(contrast, effect = "indirect_overall", q_value = 0.15) {
  if (effect == "indirect_overall") {
    fdr <- contrast |>
      group_by(source, outcome) |>
      summarise(indirect_effect = mean(indirect_effect), .group = "drop_last") |>
      arrange(-abs(indirect_effect)) |>
      ungroup() 
  } else if (effect == "indirect_pathwise") {
    fdr <- contrast |>
      group_by(source, outcome, mediator) |>
      summarise(indirect_effect = mean(indirect_effect), .group = "drop_last") |>
      arrange(-abs(indirect_effect)) |>
      group_by(mediator)
  } else if (effect == "direct_effect") {
    fdr <- contrast |>
      group_by(source, outcome) |>
      summarise(direct_effect = mean(direct_effect), .groups = "drop_last") |>
      arrange(-abs(direct_effect)) |>
      ungroup() 
  }

  fdr <- fdr |>
    mutate(
      rank = row_number(),
      fdr_hat = cumsum(source == "synthetic") / rank
    )

  cutoff <- fdr |>
    filter(fdr_hat < q_value) |>
    summarise(ix = max(rank))

  if (effect == "indirect_pathwise") {
    fdr <- fdr |>
      left_join(cutoff) |>
      mutate(keep = rank < ix) |>
      select(-ix)
  } else {
    fdr <- fdr |>
      mutate(keep = rank < cutoff$ix[1])
  }
  fdr
}
