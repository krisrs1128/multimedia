
void matrix_normal_lp(matrix x, int N, int M, real mu, real sigma2) {
  for (n in 1:N) {
    for (m in 1:M) {
      x[n, m] ~ normal(mu, sigma2);
    }
  }
}


void vector_inv_gamma_lp(vector x, int D, real l1, real l2) {
  for (d in 1:D) {
    x[d] ~ inv_gamma(l1, l2);
  }
}

void outcome_mu_lp(matrix mu, matrix x, matrix B, int D, vector sigmas_mu) {
  for (d in 1:D) {
    mu[d] ~ normal(B[d] * x, sigmas_mu[d]);
  }
}

void outcome_y_lp(array[,] int y, int N, matrix mu, int K) {
  vector[K + 1] p;
  for (i in 1:N) {
    p = phi_inv(mu[i]);
    y[i] ~ multinomial(to_vector(p));
  }
}

vector phi_inv(row_vector mu) {
  vector[num_elements(mu) + 1] mu_ = append_row(mu', 0.0);
  return softmax(mu_);
}
