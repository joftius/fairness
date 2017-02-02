
data {
  int<lower = 0> N; // number of observations
  int<lower = 0> K; // number of covariates
  matrix[N, K]   a; // sensitive variables
  real           z[N]; // ZFYA
  int            t[N]; // LSAT
  real           g[N]; // UGPA
  vector[N]      l; // bar exam location
  int<lower = 0> y[N]; // pass the bar exam the first time
  
  //matrix[N, K]   x; // covariates
  //int<lower = 0> f[N]; // outcome 2
  //real mu_eta[K]; // Prior mean for eta_s
  //real<lower = 0> sigma_eta[K]; // Prior mean for eta_s
  //real mu_us; // Prior on loading of S on U
  //real<lower = 0> sigma_us;
  //real mu_uy; // Prior on loading of Y on U
  //real<lower = 0> sigma_uy;
  //real mu_y0; // Prior for the intercept on the equation for Y
  //real<lower = 0> sigma_y0;
  //real mu_uf; // Prior on loading of F on U
  //real<lower = 0> sigma_uf;
  //real mu_f0; // Prior for the intercept on the equation for F
  //real<lower = 0> sigma_f0;


  real mu_g0;
  real<lower = 0> sigma_g0;
  real mu_u_g;
  real<lower = 0> sigma_u_g;
  real mu_l0;
  real<lower = 0> sigma_l0;
  real mu_u_t;
  real<lower = 0> sigma_u_t;
  real mu_u_z;
  real<lower = 0> sigma_u_z;
  //real mu_yp0;
  //real<lower = 0> sigma_yp0;
  //real mu_u_yp;
  //real<lower = 0> sigma_u_yp;
  real mu_y0;
  real<lower = 0> sigma_y0;
  //real mu_yp_y;
  //real<lower = 0> sigma_yp_y;
  //real mu_a_y[K];
  //real<lower = 0> sigma_a_y[K];
  //real mu_l_y;
  //real<lower = 0> sigma_l_y;
  real mu_u_y;
  real<lower = 0> sigma_u_y;
}

parameters {
  //vector[N] u;
  //vector[K] eta_s;
  //real u_s;
  //real y0;
  //real u_y;
  //real f0;
  //real u_f;

  vector[N] u;
  real g0;
  real eta_u_g;
  real l0;
  real eta_u_t;
  real eta_u_z;
  //real yp0;
  //real eta_u_yp;
  real y0;
  //real eta_yp_y;
  //vector[K] eta_a_y;
  //real eta_l_y;
  real eta_u_y;
  
  //vector[N] yp;
  
  real<lower=0> sigma_g_Sq;
  //real<lower=0> sigma_yp_Sq;
}

transformed parameters  {
 // Population standard deviation (a positive real number)
 real<lower=0> sigma_g;
// real<lower=0> sigma_yp;
 // Standard deviation (derived from variance)
 sigma_g <- sqrt(sigma_g_Sq);
// sigma_yp <- sqrt(sigma_yp_Sq);
}

model {
  
  //eta_s ~ normal(mu_eta, sigma_eta);
  //u_s ~ normal(mu_us, sigma_us);
  //y0 ~ normal(mu_y0, sigma_y0);
  //u_y ~ normal(mu_uy, sigma_uy);
  //f0 ~ normal(mu_f0, sigma_f0);
  //u_f ~ normal(mu_uf, sigma_uf);
  //u ~ normal(0, 1);
  
  //s ~ bernoulli_logit(x * eta_s + u * u_s);
  //y ~ poisson(exp(y0 + u_y * u));
  //f ~ bernoulli_logit(f0 + u_f * u);
  
  // don't have data about this
  u ~ normal(0, 1);
  
  g0 ~ normal(mu_g0, sigma_g0);
  eta_u_g ~ normal(mu_u_g, sigma_u_g);
  l0 ~ normal(mu_l0, sigma_l0);
  eta_u_t ~ normal(mu_u_t, sigma_u_t);
  eta_u_z ~ normal(mu_u_z, sigma_u_z);
  //yp0 ~ normal(mu_yp0, sigma_yp0);
  //eta_u_yp ~ normal(mu_u_yp, sigma_u_yp);
  y0 ~ normal(mu_y0, sigma_y0);
  eta_u_y ~ normal(mu_u_y, sigma_u_y);
  //eta_yp_y ~ normal(mu_yp_y, sigma_yp_y);
  //eta_a_y ~ normal(mu_a_y, sigma_a_y);
  //eta_l_y ~ normal(mu_l_y, sigma_l_y);
  
  sigma_g_Sq ~ inv_gamma(1, 1);

  // have data about these
  g ~ normal(g0 + eta_u_g * u, sigma_g);
  t ~ poisson(exp(l0 + eta_u_t * u)); // LSAT
  z ~ normal(eta_u_z * u,1);
//  y ~ bernoulli_logit(y0 + yp * eta_yp_y + a * eta_a_y + l * eta_l_y);
  y ~ bernoulli_logit(y0 + eta_u_y * u);
  

  // don't have data about this
//  yp ~ bernoulli_logit(yp0 + eta_u_yp * u);

}
