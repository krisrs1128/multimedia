#' Parse an Edgelist for Mediation or Outcome Edges
#'
#' @param edges The full edgelist associated with the current mediation analysis
#'   (mediation + outcome) models.
#' @param nulls A string specifying the indices of edges to ignore. "T->Y",
#'   "T->M", and "M->Y" will match all edges between treatment to outcome,
#'   treatment to mediator, etc. Otherwise, the vector of indices specifying
#'   which edges to ignore.
#' @return The ID of edges that match the `nulls` parameter.
#' @importFrom rlang .data
#' @importFrom dplyr row_number n pull
#' @importFrom tidygraph %E>%
#' @noRd
matching_indices <- function(edges, nulls = NULL) {
    if (is.null(nulls)) {
        ids <- quote(seq_len(n()))
    }

    G <- as.data.frame(edges)
    E <- edges %E>%
        as.data.frame() |>
        mutate(
            edge_id = row_number(),
            from_type = G$node_type[.data$from],
            to_type = G$node_type[.data$to],
        )

    if (nulls == "T->Y") {
        ids <- filter(
            E, .data$from_type == "treatment", .data$to_type == "outcome"
        )
    } else if (nulls == "T->M") {
        ids <- filter(
            E, .data$from_type == "treatment", .data$to_type == "mediator"
        )
    } else if (nulls == "M->Y") {
        ids <- filter(
            E, .data$from_type == "mediator", .data$to_type == "outcome"
        )
    } else {
        ids <- filter(E, nulls)
    }
    pull(ids, .data$edge_id)
}

#' Nullify Active Edges
#'
#' For inference, we often want to work with synthetic negative controls. One
#' way to define them is to specify submodels of the full mediation analysis
#' model. This function defines submodels by removing estimated edges according
#' to a prespecified vector of IDs. For example, setting nulls = "T -> Y" will
#' remove any direct effect when sampling or obtaining predictions for the full
#' mediation analysis model \eqn{hat{Y}}.
#'
#' @param multimedia A fitted object of class multimedia with estimates along
#'   all paths in the mediation analysis DAG.
#' @param nulls A string specifying the indices of edges to ignore. "T->Y",
#'   "T->M", and "M->Y" will match all edges between treatment to outcome,
#'   treatment to mediator, etc. Otherwise, the vector of indices specifying
#'   which edges to ignore.
#' @return multimedia A version of the input multimedia model with all edges
#'   matching `nulls` removed. Enables sampling of synthetic null controls.
#' @examples
#' # example with null data
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#'
#' nullify(fit, "T->M") |>
#'     estimate(exper) |>
#'     indirect_overall()
#' nullify(fit, "T->Y") |>
#'     estimate(exper) |>
#'     direct_effect()
#'
#' # example with another dataset
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' fit <- multimedia(exper) |>
#'     estimate(exper)
#'
#' nullify(fit, "T->M") |>
#'     estimate(exper) |>
#'     indirect_overall()
#' nullify(fit, "T->Y") |>
#'     estimate(exper) |>
#'     direct_effect()
#' @export
nullify <- function(multimedia, nulls = NULL) {
    nulls <- matching_indices(multimedia@edges, nulls)
    multimedia@edges <- multimedia@edges %E>%
        mutate(
            new_null = row_number() %in% nulls,
            state = ifelse(
                .data$state == "active" & .data$new_null,
                "inactive", .data$state
            )
        ) |>
        select(!any_of("new_null")) |>
        activate("nodes")
    multimedia
}

#' Bootstrap Distribution for Estimators
#'
#' Given a mediation model specification, estimators fs, and original dataset
#' exper, this will re-estimate the mediation model on resampled versions of
#' exper and apply each estimator in fs to construct bootstrap distributions
#' associated wtih those estimators.
#'
#' @param model An object of class multimedia with specified mediation and
#'   outcome models that we want to re-estimate across B bootstrap samples.
#' @param fs The estimators whose bootstrap samples we are interested in. These
#'   are assumed to be a vector of functions (for example, direct_effect or
#'   indirect_effect), and they will each be applied to each bootstrap resample.
#' @param exper An object of class multimedia_data containing the mediation and
#'   outcome data from which the direct effects are to be estimated.
#' @param B The number of bootstrap samples. Defaults to 1000.
#' @param progress A logical indicating whether to show a progress bar.
#' @return stats A list of length B containing the results of the fs applied on
#'   each of the B bootstrap resamples.
#' @importFrom progress progress_bar
#' @examples
#' # example with null data. We set B to 5 just to execute quickly -- it's not
#' # actually a practical choice of B
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     bootstrap(exper, B = 5)
#'
#' # example with another dataset
#' exper <- demo_spline(n_samples = 100, tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' samples <- multimedia(exper, rf_model(num.trees = 1e3)) |>
#'     bootstrap(exper, B = 5)
#' ggplot2::ggplot(samples$direct_effect) +
#'     ggplot2::geom_histogram(
#'         ggplot2::aes(direct_effect, fill = indirect_setting),
#'         bins = 15
#'     ) +
#'     ggplot2::facet_wrap(~outcome, scales = "free")
#' @export
bootstrap <- function(model, exper, fs = NULL, B = 1000, progress = TRUE) {
    if (is.null(fs)) {
        fs <- list(direct_effect = direct_effect)
    }
    if (is.null(names(fs))) {
        names(fs) <- seq_along(fs)
    }

    stats <- list()
    for (f in seq_along(fs)) {
        nf <- names(fs)[f]
        cli_text(glue("Bootstrapping {nf}"))
        stats[[nf]] <- list()
        pb <- progress_bar$new(
            total = B,
            format = "[:bar] :current/:total ETA: :eta"
        )
        for (b in seq_len(B)) {
            # resample and ensure contrast t1/t2 consistency
            exper_b <- exper[sample(nrow(exper), nrow(exper), replace = TRUE), ]
            exper_b <- exper_b[order(exper_b@treatments[[1]]), ]

            # estimate model and effects
            model_b <- estimate(model, exper_b)
            stats[[nf]][[b]] <- fs[[f]](model_b, exper_b)
            if (progress) pb$tick()
        }
        stats[[nf]] <- bind_rows(stats[[nf]], .id = "bootstrap") |>
            mutate(bootstrap = as.integer(bootstrap))
    }
    stats
}

#' Compare Effects from Experimental vs. Null Mediation Data
#'
#' One way to calibrate our conclusions from complex workflows is to see how
#' they would look on data where we know that there is no effect. This function
#' compares estimators f between real and synthetic null data, where the null
#' removes a set of edges according to the nullfication argument.
#'
#' @param model An object of class multimedia with specified mediation and
#'   outcome models that we want to re-estimate across B bootstrap samples.
#' @param exper An object of class multimedia_data containing the mediation and
#'   outcome data from which the direct effects are to be estimated.
#' @param nullification A string specifying the types of edges whose effects we
#'   want to remove in the null samples. Valid options are "T->Y" (the default),
#'   "T->M", "M->Y", which remove direct effects, treatment to mediator effects,
#'   and mediator to treatment effects, respectively.
#' @param f The estimator that we want to compare between real and null data.
#'   This is assumed to be a function taking counterfactual samples, for example
#'   `direct_effects` or `indirect_effects`.
#' @return A data.frame containing estimates on the real and synthetic data for
#'   every coordinate in the estimator f. The column `source` specifies whether
#'   the estimate was calculated using real or synthetic null data.
#' @seealso null_contrast fdr_summary
#' @examples
#' # example with null data - notice synthetic data has larger effect.
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     null_contrast(exper)
#'
#' # example with another dataset - synthetic effect is smaller.
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     null_contrast(exper)
#' @export
null_contrast <- function(
    model, exper, nullification = "T->Y", f = direct_effect) {
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

#' Calibration using Synthetic Nulls
#'
#' This function computes a threshold for indirect or direct effect estimates
#' that controls the false discovery rate according to estimates made using real
#' and synthetic null data, against the null hypotheses that effects are zero.
#' It computes the proportion of synthetic null estimates that are among the top
#' K largest effects (in magnitude) as an estimate of the FDR.
#'
#' @param contrast A data.frame summarizing the differences between outcomes
#'   across hypothetical treatments, typically as output by `null_contrast`.
#'   Each row is one outcome in one hypothetical scenario.
#' @param effect Either "indirect_overall" (the default), "indirect_pathwise",
#'   or "direct_effect" specifying the type of effect that we want to select.
#' @param q_value The target for false discovery rate control. The last time the
#'   estimated FDR is above this threshold is smallest magnitude of effect size
#'   that we will consider.
#' @return fdr A data.frame specifying, for each candidate effect, whether it
#'   should be selected.
#' @importFrom dplyr filter summarise mutate pull ungroup
#' @importFrom rlang .data
#' @examples
#' # example with null data - notice synthetic data has larger effect.
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     null_contrast(exper) |>
#'     fdr_summary("direct_effect")
#'
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     null_contrast(exper, "M->Y", indirect_overall) |>
#'     fdr_summary("indirect_overall")
#'
#' # example with another dataset - synthetic effect is smaller.
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     null_contrast(exper) |>
#'     fdr_summary("direct_effect")
#'
#' multimedia(exper) |>
#'     estimate(exper) |>
#'     null_contrast(exper, "M->Y", indirect_overall) |>
#'     fdr_summary("indirect_overall")
#' @export
fdr_summary <- function(contrast, effect = "indirect_overall", q_value = 0.15) {
    if (effect == "indirect_overall") {
        fdr <- contrast |>
            group_by(.data$source, .data$outcome) |>
            summarise(
                indirect_effect = mean(.data$indirect_effect)
            ) |>
            arrange(-abs(.data$indirect_effect))
    } else if (effect == "indirect_pathwise") {
        fdr <- contrast |>
            group_by(.data$source, .data$outcome, .data$mediator) |>
            summarise(
                indirect_effect = mean(.data$indirect_effect)
            ) |>
            arrange(-abs(.data$indirect_effect))
    } else if (effect == "direct_effect") {
        fdr <- contrast |>
            group_by(.data$source, .data$outcome) |>
            summarise(
                direct_effect = mean(.data$direct_effect)
            ) |>
            arrange(-abs(.data$direct_effect))
    }

    fdr <- fdr |>
        ungroup() |>
        mutate(
            rank = row_number(),
            fdr_hat = cumsum(.data$source == "synthetic") / rank
        )

    cutoff <- fdr |>
        filter(.data$fdr_hat < q_value) |>
        summarise(ix = max(.data$rank)) |>
        pull(.data$ix)

    if (length(cutoff) == 0) {
        fdr <- fdr |>
            mutate(keep = FALSE)
    } else {
        fdr <- fdr |>
            mutate(keep = .data$rank < cutoff & .data$source == "real")
    }
    fdr
}
