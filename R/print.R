fit_type <- function(model) {
    ifelse(is.null(model@estimates), "An unfitted", "A fitted")
}

node_subset <- function(edges, condition) {
    edges |>
        filter(.data$node_type %in% c(condition)) |>
        pull(.data$name)
}

vec_sub <- function(names) {
    n_show <- 2
    names_sub <- head(names, n_show)
    if (length(names) > n_show) {
        names_sub <- c(names_sub, "...")
    }
    paste0(names_sub, collapse = ", ")
}

add_s <- function(df) {
    ifelse(ncol(df) > 1, "s", "")
}

setMethod("show", "mediation_data", function(object) {
    tr <- treatments(object)
    m <- mediators(object)
    o <- outcomes(object)
    pr <- pretreatments(object)

    cat(col_magenta("[Mediation Data]"), "\n")
    cat(glue("{nrow(object)} samples with measurements for,"), "\n")
    cat(glue("{ncol(tr)} treatment{add_s(tr)}: {vec_sub(colnames(tr))}"), "\n")
    cat(glue("{ncol(m)} mediator{add_s(m)}: {vec_sub(colnames(m))}"), "\n")
    cat(glue("{ncol(o)} outcome{add_s(o)}: {vec_sub(colnames(o))}"), "\n")
    if (!is.null(pr)) {
        cat(glue(
            "{ncol(pr)} pretreatment{add_s(pr)}: {vec_sub(colnames(pr))}"
        ), "\n")
    }
})

#' @importFrom cli col_magenta col_cyan
setMethod("show", "multimedia", function(object) {
    cat(col_magenta("[Multimedia Analysis]"), "\n")
    cat(glue(
        "Treatments: {vec_sub(node_subset(object@edges, 'treatment'))}"
    ), "\n")
    cat(glue("Outcomes: {vec_sub(node_subset(object@edges, 'outcome'))}"), "\n")
    cat(
        glue(
            "Mediators: {vec_sub(node_subset(object@edges, 'mediator'))}"
        ), "\n"
    )

    pretreatment <- node_subset(object@edges, "pretreatment")
    if (length(pretreatment) > 0) {
        cat(glue("Pretreatment: {pretreatment}"), "\n")
    }

    med <- object@mediation
    out <- object@outcome

    cat(" ", "\n")
    cat(col_magenta("[Models]"), "\n")
    cat(glue("mediation: {fit_type(med)} {med@model_type}."), "\n")
    cat(glue("outcome: {fit_type(out)} {out@model_type}."), "\n")
})

#' Pretty Printing
#'
#' Helper function for printing ANSI in Rmarkdown output. Use this at the start
#' of your Rmarkdown files to include colors in the printed object names in the
#' final compiled output.
#'
#' Taken from the post at
#'
#' https://blog.djnavarro.net/posts/2021-04-18_pretty-little-clis/
#'
#' @param x A character vector potentially including ANSI.
#' @param options Unused placeholder argument.
#' @return A string with HTML reformatted to ensure colors appear in printed
#'   code blocks in rmarkdown output.
#' @examples
#' knitr::knit_hooks$set(output = ansi_aware_handler)
#' options(crayon.enabled = TRUE)
#' @importFrom fansi sgr_to_html
#' @export
ansi_aware_handler <- function(x, options) {
    paste0(
        "<pre class=\"r-output\"><code>",
        sgr_to_html(x = x, warn = FALSE, term.cap = "256"),
        "</code></pre>"
    )
}

#' Print an object of class model
#' @importFrom utils head
#' @noRd
setMethod("show", "model", function(object) {
    n_show <- 2
    cat(col_magenta(glue("{fit_type(object)} {object@model_type}.")), "\n")
    if (is(object@estimates, "list")) {
        print(head(object@estimates, n_show))
        if (n_show < length(object@estimates)) {
            cat(
                glue(
                    "...and {length(object@estimates) - n_show} other estimates"
                ),
                "\n"
            )
        }
    } else {
        print(object@estimates)
    }
})
