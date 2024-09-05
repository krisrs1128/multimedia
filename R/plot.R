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
#' @return A patchwork-based arrangement of ggplot2 grobs.
#' @examples
#' # dataset with no true effects
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' ie <- multimedia(exper) |>
#'     estimate(exper) |>
#'     indirect_pathwise() |>
#'     effect_summary()
#' plot_mediators(ie, exper)
#'
#' # another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' ie <- multimedia(exper, rf_model()) |>
#'     estimate(exper) |>
#'     indirect_pathwise() |>
#'     effect_summary()
#' plot_mediators(ie, exper)
#'
#' @importFrom ggplot2 ggplot aes .data geom_point labs
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots plot_layout
#' @export
plot_mediators <- function(
    indirect_effects, exper, n_digit = 3, n_panels = NULL,
    treatment = "treatment", ...) {
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

    wrap_plots(p, ...) + plot_layout(guides = "collect")
}

#' Generic Sensitivity Plot
#'
#' This function draws a curve of indirect effect against the sensitivity
#' parameter, allowing users to specify the name of x and y-axis variables using
#' the x_var and y_var inputs.
#'
#' @param sensitivity_curve The output of a call to sensitivity or
#'   sensitivity_perturb. A data.frame whose columns are: outcome, \{x_var\},
#'   \{y_var\}, and \{y_var\}_standard_error, where x_var and y_var are defined
#'   in the two arguments below.
#' @param x_var The type of perturbation variable to plot along the x-axis.
#'   Defaults to "rho", following the sensitivity approach implemented in
#'   sensitivity_subset.
#' @param y_var The type of effect to plot along the y-axis. Defaults to
#'   indirect_effect.
#' @return A ggplot2 grob plotting the sensitivity parameter against the effect
#'   specified by `y_var`.
#' @examples
#' sensitivity_curve <- read.csv(url("https://go.wisc.edu/j2kvcj"))
#' plot_sensitivity(sensitivity_curve)
#' @importFrom ggplot2 geom_vline aes geom_hline geom_ribbon scale_x_continuous
#'   labs geom_line
#' @importFrom glue glue
#' @export
plot_sensitivity <- function(
    sensitivity_curve, x_var = "rho", y_var = "indirect_effect") {
    ggplot(
        sensitivity_curve,
        aes(.data[[x_var]],
            col = .data[["outcome"]],
            fill = .data[["outcome"]]
        )
    ) +
        geom_vline(xintercept = 0, linewidth = 0.5) +
        geom_hline(yintercept = 0, linewidth = 0.5) +
        geom_line(aes(y = .data[[y_var]])) +
        geom_ribbon(
            aes(
                y = .data[[y_var]],
                ymin = .data[[y_var]] -
                    2 * .data[[glue("{y_var}_standard_error")]],
                ymax = .data[[y_var]] +
                    2 * .data[[glue("{y_var}_standard_error")]],
            ),
            alpha = 0.4
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        labs(
            x = glue("Strength {x_var} of Hypothetical Confounder"),
            y = "Indirect Effect"
        )
}
