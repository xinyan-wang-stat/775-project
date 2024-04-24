data{
  int n;
  int p2;
  int p3;
  int p4;
  array[n] int<lower = 0> y;
  matrix[n,p2] X2;
  matrix[n,p3] X3;
  matrix[n,p4] X4;
  //real mu_alpha;
  //real<lower = 0> sd_alpha;
  vector[n] log_N;
}
parameters{
  real<lower = 0> sigma2;
  real mu_alpha;
  vector[p2] beta2_aux;
  vector[p3] beta3_aux;
  vector[p4] beta4_aux;

  real beta_hat2;
  real beta_hat3;
  real beta_hat4;
  real<lower = 0> tau_beta2;
  //real<lower = 0> tau_alpha2;
  
  real alpha_aux;
}

transformed parameters{
  real alpha;
  real<lower = 0> sigma;
  real<lower = 0> tau_beta;
  //real<lower = 0> tau_alpha;
  vector[n] log_lambda;
  

  vector[p2] beta2;
  vector[p3] beta3;
  vector[p4] beta4;
  
  sigma = sqrt(sigma2);
  tau_beta = sqrt(tau_beta2);
  //tau_alpha = sqrt(tau_alpha2);
  alpha = mu_alpha + sigma * alpha_aux;

  beta2 = beta_hat2 + sigma * beta2_aux;
  beta3 = beta_hat3 + sigma * beta3_aux;
  beta4 = beta_hat4 + sigma * beta4_aux;
  

  log_lambda = log_N + alpha + X2 * beta2 + X3 * beta3 + X4 * beta4;
}
model{

  sigma2 ~ inv_gamma(3, 3);
  tau_beta2 ~ inv_gamma(3, 3);
  mu_alpha ~ normal(0, tau_beta);
  beta_hat2 ~ normal(0, tau_beta);
  beta_hat3 ~ normal(0, tau_beta);
  beta_hat4 ~ normal(0, tau_beta);
  alpha_aux ~ normal(0,1);
  beta2_aux ~ normal(0,1);
  beta3_aux ~ normal(0,1);
  beta4_aux ~ normal(0,1);
  y ~ poisson_log(log_lambda);
}
generated quantities{
  array[n] int<lower = 0> pred_y;
  pred_y = poisson_log_rng(log_lambda);
  array[n] real pred_rate;
  for(i in 1:n) pred_rate[i] = pred_y[i]/exp(log_N[i]);
}
