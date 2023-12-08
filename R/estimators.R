setClass(
  "model",
  representation(
    estimator = "ANY",
    estimates = "ANY",
    sampler = "ANY",
    model_type = "ANY"
  )
)

sub_formula <- function(formula, yj) {
  update.formula(formula, as.formula(glue("{yj} ~ .")))
}

#' @importFrom formula.tools lhs.vars
#' @examples
#' mat <- data.frame(matrix(rnorm(100), 25, 4))
#' colnames(mat) <- c("y1", "y2", "x1", "x2")
#' plm <- parallelize(lm)
#' plm(y1 + y2 ~ x1 + x2, mat)
parallelize <- function(f) {
  function(formula, ...) {
    result <- list()

    ys <- lhs.vars(formula)
    for (j in seq_along(ys)) {
      fmla <- sub_formula(formula, ys[j])
      result[[ys[j]]] <- f(fmla, ...)
    }

    result
  }
}

#' Linear Model across Responses
lm_model <- function() {
  new(
    "model",
    estimator = parallelize(lm),
    estimates = NULL,
    sampler = lm_sampler,
    model_type = "lm_model()"
  )
}

lm_sampler <- function(fits, new_data = NULL) {
  y_hats <- list()
  for (i in seq_along(fits)) {
    y_hats[[i]] <- predict(fits[[i]], newdata = new_data)
  }
}

setMethod("show", "model", function(object) {
  print(glue("{fit_type(object)} {object@model_type}."))
})
