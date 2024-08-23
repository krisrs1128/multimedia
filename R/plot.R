#' Visualize Indirect Effects
#'
#' This is a helper function to visualize the raw data responsible for the
#' largest indirect effects. It returns a faceted plot of outcome vs. mediator
#' pairs for those with high pathwise indirect effects.
#'
#' @param indirect_effects A data.frame containing estimated indirect effects
#'   for each variable, under different counterfactual settings for the "direct
#'   treatment." This is the output of `indirect_pathwise`.
#' @param exper An object of class `mediation_data` containing all mediation
#'   analysis data.
#' @param n_digit The number of digits of the indirect effects to print next to
#'   each panel. Defaults to 3.
#' @param n_panels The number of mediator-outcome pairs to show. Defaults to 12,
#'   or the number of pathways, if there are fewer than 12.
#' @param treatment What is the name of the treatment variable that we want to
#'   overlay on points? This is necessary when there are several potential
#'   treatment variables. Defaults to "treatment."
#' @param ... Further keyword arguments passed to `patchwork::wrap_plots`.
#' @examples
#' # dataset with no true effects
#' exper <- demo_joy() |>
#'   mediation_data("PHQ", "treatment", starts_with("ASV"))
#' ie <- multimedia(exper) |>
#'   estimate(exper) |>
#'   indirect_pathwise() |>
#'   effect_summary()
#' plot_mediators(ie, exper)
#'
#' # another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'   mediation_data(starts_with("outcome"), "treatment", "mediator")
#' ie <- multimedia(exper, rf_model()) |>
#'   estimate(exper) |>
#'   indirect_pathwise() |>
#'   effect_summary()
#' plot_mediators(ie, exper)
#'
#' @importFrom ggplot2 ggplot aes .data geom_point labs
#' @importFrom glue glue
#' @export
plot_mediators <- function(indirect_effects, exper, n_digit = 3, n_panels = NULL,
                           treatment = "treatment", ...) {
  check_if_installed("patchwork", "to visualize indirect effects as a multipanel plot.")
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
      geom_point(aes(col = .data[[treatment]]), alpha = 0.7) +
      labs(title = glue("IE: {round(effect, n_digit)}"))
  }

  patchwork::wrap_plots(p, ...) +
    patchwork::plot_layout(guides = "collect")
}
