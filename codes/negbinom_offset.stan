data{
  int n;
  int p1;
  int p2;
  int p3;
  array[n] int<lower = 0> y;
  matrix[n,p1] X1;
  matrix[n,p2] X2;
  matrix[n,p3] X3;
  vector[n] log_N; 
 
}
parameters{
  real<lower = 0> sigma2;
  real mu_alpha;
  vector[p1] beta1_aux;
  vector[p2] beta2_aux;
  vector[p3] beta3_aux;

  real beta_hat1;
  real beta_hat2;
  real beta_hat3;
  real<lower = 0> tau2;
  
  real alpha_aux;
  real<lower = 0> phi_sqrt_inv; // phi^(-1/2) where phi controls over-dispersion
  
}

transformed parameters{
  real alpha;
  real<lower = 0> sigma;
  real<lower = 0> tau_beta;
  vector[n] log_lambda;
  

  vector[p1] beta1;
  vector[p2] beta2;
  vector[p3] beta3;
  
  sigma = sqrt(sigma2);
  tau = sqrt(tau2);
  alpha = mu_alpha + sigma * alpha_aux;

  beta1 = beta_hat1 + sigma * beta1_aux;
  beta2 = beta_hat2 + sigma * beta2_aux;
  beta3 = beta_hat3 + sigma * beta3_aux;
  
  real<lower = 0> phi;
  phi = 1.0/(phi_sqrt_inv^2); 

  log_lambda = log_N + alpha + X1 * beta1 + X2 * beta2 + X3 * beta3;
}
model{
  phi_sqrt_inv ~ normal(0, 1);
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
  y ~ neg_binomial_2_log(log_lambda, phi);
}
generated quantities{
  array[n] int<lower = 0> pred_y;
  array[n] real pred_rate;
  pred_y = poisson_log_rng(log_lambda);
  for(i in 1:n) pred_rate[i] = pred_y[i]/exp(log_N[i]);
} 

