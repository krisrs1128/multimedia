fit_type <- function(model) {
  ifelse(is.null(model@estimates), "An unfitted", "A fitted")
}

node_subset <- function(edges, condition) {
  edges |>
    filter(node_type %in% c(condition)) |>
    pull(name)
}

#' @importFrom cli col_magenta col_cyan
setMethod("show", "multimedia", function(object) {
  cat(col_magenta("[Mediation Analysis Object]"), "\n")
  cat(glue("Outcomes: {print_sub(node_subset(object@edges, 'outcome'))}"), "\n")
  cat(glue("Treatments: {print_sub(node_subset(object@edges, 'treatment'))}"), "\n")
  cat(glue("Mediators: {print_sub(node_subset(object@edges, 'mediator'))}"), "\n")

  pretreatment <- node_subset(object@edges, "pretreatment")
  if (length(pretreatment) > 0) {
    cat(glue("Pretreatment: {pretreatment}"), "\n")
  }

  cat(col_magenta("[Models]"), "\n")
  cat(glue("mediation: {fit_type(object@mediation)} {object@mediation@model_type}."), "\n")
  cat(glue("outcome: {fit_type(object@outcome)} {object@outcome@model_type}."), "\n")
})

#' Helper function for printing ANSI in Rmarkdown output
#' https://blog.djnavarro.net/posts/2021-04-18_pretty-little-clis/
#' @export
ansi_aware_handler <- function(x, options) {
  paste0(
    "<pre class=\"r-output\"><code>",
    fansi::sgr_to_html(x = x, warn = FALSE, term.cap = "256"),
    "</code></pre>"
  )
}

setMethod("show", "model", function(object) {
  print(glue("{fit_type(object)} {object@model_type}."))
})
