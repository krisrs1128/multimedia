lnm <- function(formula, data, iter = 5e3, sigma_b = 1, l1 = 50, l2 = 25) {
  ys <- lhs.vars(formula)
  x_data <- model.matrix(formula, data) |>
    as_tibble() |>
    select(matches(rhs.vars(formula)))

  data_list <- list(
    y = select(data, ys),
    x = x_data,
    N = nrow(x_data),
    D = ncol(x_data),
    K = length(ys),
    sigma_b = sigma_b,
    l1 = l1,
    l2 = l2
  )

  model <- file.path(system.file(package = "multimedia"), "lnm.stan") |>
    cmdstan_model()
  model$variational(data_list, iter = iter)
}

lnm_sampler <- function(fit, newdata = NULL) {
}