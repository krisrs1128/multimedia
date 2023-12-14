
functions {
  #include densities.stan
}

data {
  int<lower=1> N;// number of samples
  int<lower=1> K;// number of taxa - 1
  int<lower=1> D;// number of covariates
  array[N, K + 1] int<lower=0> y;
  matrix[N, D] x;
  real sigma_b; // prior SD for B coefficients
  real l1; // inverse gamma hyperparameter for sigmas_mu
  real l2; // inverse gamma hyperparameter for sigmas_mu
}

parameters {
  matrix[D, K] B;
  matrix[N, K] mu;
  vector<lower=0>[K] sigmas_mu;// diagonal covariance for mu
}

model {
  // these calls define the prior
  matrix_normal_lp(B, D, K, 0, sigma_b);
  vector_inv_gamma_lp(sigmas_mu, K, l1, l2);

  // this defines the likelihood
  outcome_mu_lp(mu, x, B, D, sigmas_mu);
  outcome_y_lp(y, N, mu, K);
}
