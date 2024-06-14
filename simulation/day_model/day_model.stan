data {
  int<lower=0> n_days;            // number of days
  int<lower=0> n_beeps;           // number of beeps
  vector[n_days] s;               // measurement of sleep quality
  matrix[n_days, n_beeps] m;      // measurement of beeps
}
parameters {
  vector[n_days] m_factor;      // latent day constant
  real ic_s;                    // intercept for s
  real ar_s;                    // autoregression for s
  real cr_s_mf;                 // cross-lagged regression for s
  real<lower=0> resvar_s;       // residual variance for s
  real ar_mf;                   // autoregression for mf
  real cr_mf_s;                 // crossregression for mf
  real<lower=0> resvar_mf;      // residual variance for mf
  vector[n_beeps] ic_m;               // intercepts for m
  vector<lower=0>[n_beeps] resvar_m;  // measurement error variance for m
}
transformed parameters {
  // centering
  vector[n_days] c_s;

  c_s = s - ic_s;
}
model {
  // dynamic model with imputed variables
  m_factor[2:n_days] ~ normal(ar_mf * m_factor[1:(n_days - 1)] + cr_mf_s * c_s[2:n_days], sqrt(resvar_mf));
  s[2:n_days] ~ normal(ic_s + ar_s * c_s[1:(n_days - 1)] + cr_s_mf * m_factor[1:(n_days - 1)], sqrt(resvar_s));

  for (b in 1:n_beeps) {
    m[, b] ~ normal(ic_m[b] + m_factor, sqrt(resvar_m[b]));
  }

  // priors
    // regression coefficients
  ic_s ~ normal(0, 100);
  ar_s ~ normal(0, 100);
  cr_s_mf ~ normal(0, 100);
  ic_m ~ normal(0, 100);
  ar_mf ~ normal(0, 100);
  cr_mf_s ~ normal(0, 100);
  
    // residuals
  resvar_s ~ inv_gamma(0.001, 0.001);
  resvar_mf ~ inv_gamma(0.001, 0.001);
  resvar_m ~ inv_gamma(0.001, 0.001);
}

