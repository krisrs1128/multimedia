setClassUnion("modelOrNull", members = c("model", "NULL"))
setClassUnion("data.frameOrNull", members = c("data.frame", "NULL"))

#' An S4 Class Representing Mediation Analysis Models
#'
#' This is the primary class exported by the multimedia package. It includes
#' both the outcome and mediation models. It also stores complementary data
#' helpful for estimating effects and carrying out inference. Most estimation
#' and inference functions expect an input model of this form.
#'
#' @slot edges A tidygraph graph specifying all edges in the mediation analysis
#'   DAG. For example, it should include all edges between treatments and
#'   outcomes if we hypothesize the presence of a direct effect.
#' @slot outcome A `model` class storing the outcome model.
#' @slot mediation A `model` class storing the mediation model.
#' @slot treatments A `treatment_profile` class describing treatment assignments
#'   across all samples.
#' @noRd
setClass(
    "multimedia",
    representation(
        edges = "ANY",
        outcome = "modelOrNull",
        mediation = "modelOrNull",
        treatments = "ANY"
    )
)

#' An S4 Class Representing a Mediation Analysis Dataset
#'
#' All the data needed for carrying out mediation analysis, encapsulated in one
#' object. Each node in the mediation analysis DAG is represented by a
#' data.frame in its own slot. Pretreatments are optional and will be left NULL
#' if not specified.
#'
#' @slot mediators A data.frame containing observed values of the mediators
#'   across all samples.
#' @slot outcomes A data.frame containing observed values of the outcomes across
#'   all samples.
#' @slot treatments A data.frame containing observed values of the treatments
#'   across all samples.
#' @slot pretreatments A data.frame containing observed values of the
#'   pretreatments across all samples.
#' @noRd
setClass(
    "mediation_data",
    representation(
        mediators = "data.frame",
        outcomes = "data.frame",
        treatments = "data.frame",
        pretreatments = "data.frameOrNull"
    )
)

#' Convert `mediation_data` to a single data.frame
#'
#' It can be helpful to combine all the data in a `mediation_data` S4 object
#' into a single data.frame. This concatenates all data sources horizontally,
#' into an N samples x (all outcomes, mediators, treatments, ...) matrix.
#'
#' @param exper An object of class `mediation_data` containing the variables
#'   that we want horizontally concatenated.
#' @return A data.frame containing all variables from the original
#'   `mediation_data` object.
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' exper
#' bind_mediation(exper)
#'
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' exper
#' bind_mediation(exper)
#' @importFrom purrr map_dfc
#' @export
bind_mediation <- function(exper) {
    map_dfc(slotNames(exper), ~ slot(exper, .))
}

#' Define all Edges in a Mediation Analysis
#'
#' This processes the variable names from a mediation analysis and builds a
#' graph representing causal relationships in the mediation analysis DAG. It is
#' helpful for automatically building formulas for the mediation and outcome
#' models, and it is used to create the `edges` slot in multimedia objects.
#'
#' @param outcomes A vector containing names of all outcome variables.
#' @param treatments A vector containing names of all treatment variables.
#' @param mediators A vector containing names of all mediators.
#' @param pretreatments A vector containing names of all pretreatment variables.
#' @return edges A tidygraph graph containing all nodes and edges in the
#'   mediation analysis. Each node is annotated with a `node_type`, describing
#'   which of the parts of the mediation analysis graph the node belongs to.
#' @importFrom purrr reduce
#' @importFrom dplyr join_by
#' @importFrom tidygraph graph_join %N>%
#' @seealso multimedia
#' @noRd
graph_specification <- function(outcomes, treatments, mediators,
                                pretreatments) {
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

    reduce(edges, graph_join, by = join_by("name", "node_type")) %N>%
        mutate(
            node_type = factor(
                .data$node_type,
                levels = c(
                    "intercept", "pretreatment", "treatment", "mediator",
                    "outcome"
                )
            )
        ) |>
        arrange(across(c("node_type", "name"))) |>
        mutate(id = row_number())
}

#' Helper for `graph_specification`
#'
#' To define the overall mediation analysis graph, it helps to have a helper
#' that learns all combinations of variables between two node types. This is
#' done simply by using expand.grid and converting into a bipartite tidygraph.
#'
#' @param input A vector of names for all variables within the input group.
#' @param output A vector of names for all variables within the output group.
#' @param input_name A node_type to associate with all inputs. For example,
#'   "treatment" or "mediator" for edges along the T->Y or X->M paths.
#' @param output_name A node_type to associate with all outputs. For example,
#'   "mediator" or "outcome" for edges along the T->M or M->Y paths.
#' @return G A tidygraph graph with bipartite structure. One set of nodes
#'   corresponds to the inputs, one to the outputs, and every pair of inputs and
#'   outputs are linked.
#' @importFrom dplyr case_when mutate %>%
#' @importFrom rlang .data
#' @importFrom tidygraph %E>% tbl_graph activate
#' @noRd
expand_edges <- function(input, output, input_name, output_name) {
    tbl_graph(edges = expand.grid(input, output), node_key = "name") |>
        mutate(
            node_type = case_when(
                name %in% input ~ input_name,
                name %in% output ~ output_name,
            )
        ) %E>%
        mutate(state = "active") |>
        activate("nodes")
}

#' Convert a Summarized Experiment to a data.frame
#'
#' This is a small helepr function to concatenate the colData and assays within
#' SummarizedExperiment objects into a single data.frame. This is not necessary
#' for the essential multimedia workflow, it is only exported for potential
#' independent interest.
#'
#' @param exper An object of class SummarizedExperiment.
#' @importFrom SummarizedExperiment assay colData
#' @return A data.frame combining all slots of a multimedia object.
#' @examples
#' demo_joy() |>
#'     exper_df()
#' @export
exper_df <- function(exper) {
    bind_cols(
        t(assay(exper)), data.frame(colData(exper)),
        .name_repair = "minimal"
    )
}

#' `mediation_data` Constructor
#'
#' Convert a SummarizedExperiment, phyloseq object, or data.frame into a
#' `mediation_data` object. This conversion helps to organize the variables that
#' lie on different parts of the mediation analysis graph, so that they are not
#' all kept in a homogeneous experiment or data.frame. It's possible to specify
#' outcome, mediator, or treatment variables using either string vectors or
#' tidyselect syntax, e.g., `starts_with("mediator_")` will match all columns of
#' the input data starting with the string mediator.
#'
#' @param x A SummarizedExperiment, phyloseq object, or data.frame whose columns
#'   contain all the variables needed for the mediation analysis.
#' @param outcomes A vector or tidyselect specification of the names of all
#'   outcome variables.
#' @param treatments A vector or tidyselect specification of the names of all
#'   treatment variables.
#' @param mediators A vector or tidyselect specification of the names of all
#'   mediators.
#' @param pretreatments A vector containing names of all pretreatment variables.
#'   Defaults to NULL, in which case the pretreatments slot will remain empty.
#' @return result An object of class `mediation_data`, with separate slots for
#'   each of the node types in a mediation analysis diagram.
#' @seealso mediation_data-class
#' @examples
#' # multiple outcomes, one mediator
#' mediation_data(
#'     demo_spline(), starts_with("outcome"), "treatment", "mediator"
#' )
#'
#' # one outcome, multiple mediators
#' mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
#' @export
mediation_data <- function(
    x, outcomes, treatments, mediators, pretreatments = NULL) {
    result <- NULL
    if ("SummarizedExperiment" %in% class(x)) {
        result <- from_summarized_experiment(
            x,
            outcomes,
            treatments,
            mediators,
            pretreatments
        )
    } else if ("phyloseq" %in% class(x)) {
        result <- from_phyloseq(
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

#' Helper to Convert SummarizedExperiment to a data.frame
#' @param x A SummarizedExperiment whose colData or assays contains all the
#'   variables needed for the mediation analysis.
#' @param outcomes A vector containing names of all outcome variables.
#' @param treatments A vector containing names of all treatment variables.
#' @param mediators A vector containing names of all mediators.
#' @param pretreatments A vector containing names of all pretreatment variables.
#'   Defaults to NULL, in which case the pretreatments slot will remain empty.
#' @return result An object of class `mediation_data`, with separate slots for
#'   each of the node types in a mediation analysis diagram.
#' @noRd
from_summarized_experiment <- function(
    exper, outcomes, treatments, mediators, pretreatments) {
    exper_df(exper) |>
        from_data_frame(outcomes, treatments, mediators, pretreatments)
}

#' Helper to Convert a phyloseq object to a data.frame
#' @param x A phyloseq object whose otu_table contains all the variables needed
#'   for the mediation analysis.
#' @param outcomes A vector containing names of all outcome variables.
#' @param treatments A vector containing names of all treatment variables.
#' @param mediators A vector containing names of all mediators.
#' @param pretreatments A vector containing names of all pretreatment variables.
#'   Defaults to NULL, in which case the pretreatments slot will remain empty.
#' @return result An object of class `mediation_data`, with separate slots for
#'   each of the node types in a mediation analysis diagram.
#' @importFrom phyloseq otu_table sample_data
#' @noRd
from_phyloseq <- function(
    exper, outcomes, treatments, mediators, pretreatments) {
    if (attr(otu_table(exper), "taxa_are_rows")) {
        counts <- t(otu_table(exper))
    } else {
        counts <- otu_table(exper)
    }

    bind_cols(counts, data.frame(sample_data(exper))) |>
        from_data_frame(outcomes, treatments, mediators, pretreatments)
}

#' `mediation_data` from a data.frame
#'
#' @param x A data.frame whose columns contain all the variables needed for the
#'   mediation analysis.
#' @param outcomes A vector containing names of all outcome variables.
#' @param treatments A vector containing names of all treatment variables.
#' @param mediators A vector containing names of all mediators.
#' @param pretreatments A vector containing names of all pretreatment variables.
#'   Defaults to NULL, in which case the pretreatments slot will remain empty.
#' @return result An object of class `mediation_data`, with separate slots for
#'   each of the node types in a mediation analysis diagram.
#' @importFrom dplyr bind_cols select across
#' @importFrom tidyselect where
#' @noRd
from_data_frame <- function(
    df, outcomes, treatments, mediators, pretreatments) {
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
            as.data.frame()

        if (ncol(result[[names(vars)[i]]]) == 0) {
            result[[names(vars)[i]]] <- NULL
        }
    }

    do.call(\(...) new("mediation_data", ...), result)
}

#' `multimedia` Constructor
#'
#' `multimedia` objects encapsulate the model and data that underlie a mediation
#' analysis, together with metadata (like graph structure) that contextualize
#' the estimation. This function can be used to construct a new multimedia
#' instances from a `mediation_data` dataset and pair of estimators.
#'
#' @param mediation_data An object of class `mediation_data`, with separate
#'   slots for each of the node types in a mediation analysis diagram.
#' @param outcome_estimator An object of class `model` that will be used to
#'   estimate the outcome model.
#' @param mediation_estimator An object of class `model` that will be used to
#'   estimate the mediation model.
#' @return An object of class `multimedia` encapsulating the full mediation
#'   model and data.
#' @seealso multimedia-class
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' multimedia(exper)
#'
#' exper <- demo_spline(tau = c(2, 1)) |>
#'     mediation_data(starts_with("outcome"), "treatment", "mediator")
#' multimedia(exper)
#'
#' # real data example with a pretreatment variable
#' data(mindfulness)
#' exper <- mediation_data(
#'     mindfulness,
#'     phyloseq::taxa_names(mindfulness),
#'     "treatment",
#'     starts_with("mediator"),
#'     "subject"
#' )
#' multimedia(exper)
#' @export
multimedia <- function(
    mediation_data, outcome_estimator = lm_model(),
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

#' Pretty printing x -> comma separated string
#' @noRd
print_sub <- function(x) {
    if (length(x) > 2) {
        res <- paste0(c(head(x, 1), "...", tail(x, 1)), collapse = ",")
    } else {
        res <- paste0(x, collapse = ",")
    }
    res
}

#' How many samples in the mediation dataset?
#' @noRd
setGeneric("nrow", function(x) nrow(x))

#' How many samples in the mediation dataset?
#' @param x The `mediation_data` object whose number of samples we want to
#'   return.
#' @return An integer giving the number of samples in the mediation object.
#' @export
setMethod(nrow, "mediation_data", function(x) {
    nrow(x@outcomes)
})

#' Subset a mediation dataset
#'
#' We can subset samples by indexing into a mediation dataset. It will subsample
#' all fields -- pretreatments, treatments, mediators, and outcomes. Note that
#' there is no way to subset columns in this way, since they would be different
#' across each source.
#'
#' @param x An object of class `mediation_data` whose samples we want to subset.
#' @param i An integer or integer/logical vector specifying the samples to
#'   subset to.
#' @param j A placeholder to argree with R's `[` function. Never used.
#' @param drop A placeholder to agree with R's `[` function. Never used.
#' @param ... A placeholder to agree with R's `[` function. Never used.
#' @return A version of the input mediation_data object whose @mediators,
#'   @outcomes, @treatments, and @pretreatments rows have all been subsetted
#'   according to `i`.
#' @export
#' @examples
#' exper <- demo_joy() |>
#'     mediation_data("PHQ", "treatment", starts_with("ASV"))
#' exper[1]
#' exper[1:10]
setMethod(
    "[", "mediation_data",
    function(x, i, j, ..., drop = TRUE) {
        x@mediators <- x@mediators[i, , drop = FALSE]
        x@outcomes <- x@outcomes[i, , drop = FALSE]
        x@treatments <- x@treatments[i, , drop = FALSE]
        x@pretreatments <- x@pretreatments[i, , drop = FALSE]
        x
    }
)
