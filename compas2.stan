
data {
  int<lower = 0> N; // number of observations
  int<lower = 0> K; // number of covariates
  matrix[N, K]   x; // covariates
  int<lower = 0, upper = 1> s[N]; // selection variable
  int<lower = 0> y[N]; // outcome
  int<lower = 0> f[N]; // outcome 2
  real mu_eta[K]; // Prior mean for eta_s
  real<lower = 0> sigma_eta[K]; // Prior mean for eta_s
  real mu_us; // Prior on loading of S on U
  real<lower = 0> sigma_us;
  real mu_uy; // Prior on loading of Y on U
  real<lower = 0> sigma_uy;
  real mu_y0; // Prior for the intercept on the equation for Y
  real<lower = 0> sigma_y0;
  real mu_uf; // Prior on loading of F on U
  real<lower = 0> sigma_uf;
  real mu_f0; // Prior for the intercept on the equation for F
  real<lower = 0> sigma_f0;
}

parameters {
  vector[N] u;
  vector[K] eta_s;
  real u_s;
  real y0;
  real u_y;
  real f0;
  real u_f;
}

model {
  
  eta_s ~ normal(mu_eta, sigma_eta);
  u_s ~ normal(mu_us, sigma_us);
  y0 ~ normal(mu_y0, sigma_y0);
  u_y ~ normal(mu_uy, sigma_uy);
  f0 ~ normal(mu_f0, sigma_f0);
  u_f ~ normal(mu_uf, sigma_uf);
  u ~ normal(0, 1);

  s ~ bernoulli_logit(x * eta_s + u * u_s);
  y ~ poisson(exp(y0 + u_y * u));
  f ~ bernoulli_logit(f0 + u_f * u);
  
}
