data {
  int<lower=0> n_days;            // number of days
  int<lower=0> n_beeps;           // number of beeps
  vector[n_days] s;               // measurement of sleep quality
  matrix[n_days, n_beeps] m;      // measurement of beeps
}
parameters {
  vector[n_days] m_factor;      // mood factor
  real ic_s;                    // intercept for s
  real ar_s;                    // autoregression for s
  real cr_s_mf;                 // cross-lagged regression of s on mf
  real cr_s_m;                  // cross-lagged regression of s om m
  real<lower=0> resvar_s;       // residual variance for s
  real ar_mf;                   // autoregression for mf
  real cr_mf_s;                 // crossregression for mf
  real<lower=0> resvar_mf;      // residual variance for mf
  real ar_m;                    // daytime autoregression for m
  real ar_night_m;              // nighttime autoregression for m
  real cr_m_s;                  // crossregression for m
  vector[n_beeps] ic_m;               // intercepts for m
  vector<lower=0>[n_beeps] resvar_m;  // residual variances for m
}
model {
  // dynamic model
  s[2:n_days] ~ normal(ic_s + ar_s * (s[1:(n_days - 1)] - ic_s) + cr_s_m * (m[1:(n_days - 1), n_beeps] - ic_m[n_beeps] - m_factor[1:(n_days - 1)]) + cr_s_mf * m_factor[1:(n_days - 1)], sqrt(resvar_s));
  m_factor[2:n_days] ~ normal(ar_mf * m_factor[1:(n_days - 1)] + cr_mf_s * (s[2:n_days] - ic_s), sqrt(resvar_mf));

  m[2:n_days, 1] ~ normal(ic_m[1] + m_factor[2:n_days] + ar_night_m * (m[1:(n_days - 1), n_beeps] - ic_m[n_beeps] - m_factor[1:(n_days - 1)]) + cr_m_s * (s[2:n_days] - ic_s), sqrt(resvar_m[1]));

  for (b in 2:n_beeps) {
    m[, b] ~ normal(ic_m[b] + m_factor + ar_m * (m[, (b - 1)] - ic_m[(b - 1)] - m_factor), sqrt(resvar_m[b]));
  }
    

  // priors
    // regression coefficients
  ic_s ~ normal(0, 100);
  ar_s ~ normal(0, 100);
  cr_s_mf ~ normal(0, 100);
  cr_s_m ~ normal(0, 100);
  ic_m ~ normal(0, 100);
  ar_mf ~ normal(0, 100);
  cr_mf_s ~ normal(0, 100);
  ar_m ~ normal(0, 100);
  ar_night_m ~ normal(0, 100);
  cr_m_s ~ normal(0, 100);
  
    // residuals-
  resvar_s ~ inv_gamma(0.001, 0.001);
  resvar_mf ~ inv_gamma(0.001, 0.001);
  resvar_m ~ inv_gamma(0.001, 0.001);
}
  