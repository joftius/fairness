data {
  int<lower = 0> N; // number of observations
  int<lower = 0> Ds; // number of covariates
  matrix[N, Ds]  se; // sensitive variables
  int<lower = 0> Da;
  matrix[N, Da]  ap; // appearance
  int<lower = 0> ar[N]; // arrest
  int<lower = 0> sf1[N]; // searched/frisk
  int<lower = 0> sf2[N]; // searched/frisk
  int<lower = 0> we[N]; // weapons
  int<lower = 0> fo[N]; //force
  int<lower = 0> summ[N]; //summons
}

transformed data {
  
 vector[Da] zero_ap;
 vector[Da] one_ap;

 vector[Ds] zero_s;
 vector[Ds] one_s;
 
 zero_ap = rep_vector(0,Da);
 one_ap = rep_vector(1,Da);
 
 zero_s = rep_vector(0,Ds);
 one_s = rep_vector(1,Ds);
  
}

parameters {
  vector[N] c;
  vector[N] p;
  
  real ar0;    
  real w_c_ar;  
  //real w_we_ar; 
  real sf01;
  real w_c_sf1;
  real w_p_sf1;
  real sf02;
  real w_c_sf2;
  real w_p_sf2;
  real we0;
  real w_c_we;
  real fo0;
  real w_c_fo;
  real w_p_fo;
  real summ0;
  real w_c_summ;

  real p0;      
  vector[Ds] w_s_p;   
  vector[Da] w_ap_p;
  
  real<lower=0> sigma_p_Sq;
  
}


transformed parameters  {
 // Population standard deviation (a positive real number)
 real<lower=0> sigma_p;
 sigma_p <- sqrt(sigma_p_Sq);


}

model {
  
  // weights
  //ar0     ~ normal(0,1);
  //w_c_ar  ~ normal(0,1);
  //w_we_ar ~ normal(zero_we,one_we);
  //sf0     ~ normal(zero_f,one_f);
  //w_c_sf  ~ normal(zero_f,one_f);
  //w_p_sf  ~ normal(zero_f,one_f);
  //we0     ~ normal(zero_we,one_we);
  //w_c_we  ~ normal(zero_we,one_we);
  //fo0     ~ normal(zero_fo,one_fo);
  //w_c_fo  ~ normal(zero_fo,one_fo);
  //w_p_fo  ~ normal(zero_fo,one_fo);
  //p0      ~ normal(0,1);
  //w_s_p   ~ normal(zero_s,one_s);
  //w_ap_p  ~ normal(zero_ap,one_ap);
  
  ar0     ~ normal(0,1);
  w_c_ar  ~ normal(0,1);
  //w_we_ar ~ normal(0,1);
  sf01    ~ normal(0,1);
  w_c_sf1 ~ normal(0,1);
  w_p_sf1 ~ normal(0,1);
  sf02    ~ normal(0,1);
  w_c_sf2 ~ normal(0,1);
  w_p_sf2 ~ normal(0,1);
  we0     ~ normal(0,1);
  w_c_we  ~ normal(0,1);
  fo0     ~ normal(0,1);
  w_c_fo  ~ normal(0,1);
  w_p_fo  ~ normal(0,1);
  summ0   ~ normal(0,1);
  w_c_summ~ normal(0,1);
  p0      ~ normal(0,1);
  w_s_p   ~ normal(zero_s,one_s);
  w_ap_p  ~ normal(zero_ap,one_ap);

  
  // latent variable
  c ~ normal(0, 1);
  
  // have data about these
  ar   ~ bernoulli_logit(ar0 + w_c_ar * c); // ar = f(c)
  sf1  ~ bernoulli_logit(sf01 + w_c_sf1 * c + w_p_sf1  * p);  // sf = f(c,p)
  sf2  ~ bernoulli_logit(sf02 + w_c_sf2 * c + w_p_sf2  * p);  // sf = f(c,p)
  we   ~ bernoulli_logit(we0 + w_c_we * c); // we = f(c)
  fo   ~ bernoulli_logit(fo0 + w_c_fo * c + w_p_fo * p); // fo = f(c,p)
  summ ~ bernoulli_logit(summ0 + w_c_summ * c); // summ = f(c)

  // latent variable
  sigma_p_Sq ~ inv_gamma(1, 1);
  p ~ normal(p0 + se * w_s_p + ap * w_ap_p, sigma_p);
  
  
  
}