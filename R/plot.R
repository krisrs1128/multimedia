#' @importFrom patchwork wrap_plots plot_layout
#' @importFrom ggplot2 ggplot aes .data geom_point labs
#' @importFrom glue glue
#' @export
plot_mediators <- function(indirect_effects, exper, n_panels = NULL) {
  if (is.null(n_panels)) {
    n_panels <- min(nrow(indirect_effects), 12)
  }
  
  combined <- bind_mediation(exper)
  for (i in seq_len(n_panels)) {
    m <- indirect$mediator[i]
    y <- indirect$outcome[i]
    effect <- indirect$indirect_effect[i]
    p[[i]] <- ggplot(combined, aes(.data[[m]], .data[[y]])) +
      geom_point(aes(col = treatment), alpha = 0.7) +
      geom_smooth(aes(col = treatment), se = FALSE, formula = y ~ x, method = "lm") +
      labs(title = glue("IE: {round(effect, 4)}"))
  }
  
  wrap_plots(p) +
    plot_layout(guides = "collect")
}
