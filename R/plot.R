#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom ggplot2 ggplot aes .data geom_point labs
#' @importFrom glue glue
#' @export
plot_mediators <- function(indirect_effects, exper, n_panels = NULL) {
  if (is.null(n_panels)) {
    n_panels <- min(nrow(indirect_effects), 12)
  }
  
  p <- list()
  combined <- bind_mediation(exper)
  for (i in seq_len(n_panels)) {
    m <- indirect_effects$mediator[i]
    y <- indirect_effects$outcome[i]
    effect <- indirect_effects$indirect_effect[i]
    p[[i]] <- ggplot(combined, aes(.data[[m]], .data[[y]])) +
      geom_point(aes(col = treatment), alpha = 0.7) +
      labs(title = glue("IE: {round(effect, 2)}"))
  }
  
  wrap_plots(p) +
    plot_layout(guides = "collect")
}
