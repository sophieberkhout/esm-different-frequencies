data {
  int<lower=0> n_days;            // number of days
  int<lower=0> n_beeps;           // number of beeps
  vector[n_days] s;               // measurement of sleep quality
  matrix[n_days, n_beeps] m;      // measurement of beeps
}
parameters {
  real ic_s;                    // intercept for s
  real ar_s;                    // autoregression for s
  real cr_s_m;                  // cross-lagged regression for s
  real<lower=0> resvar_s;       // residual variance for s
  real ar_m;                    // autoregression for m
  real ar_night_m;              // night autoregression for m
  real cr_m_s;                  // crossregression for m
  real ic_m;                    // intercept for m
  real<lower=0>resvar_m1;       // residual variance for m first beep
  real<lower=0>resvar_m;        // residual variance for m
}
model {
  // dynamic model
  m[2:n_days, 1] ~ normal(ic_m + ar_night_m * (m[1:(n_days - 1), n_beeps] - ic_m) + cr_m_s * (s[2:n_days] - ic_s), sqrt(resvar_m1));
  s[2:n_days] ~ normal(ic_s + ar_s * (s[1:(n_days - 1)] - ic_s) + cr_s_m * (m[1:(n_days - 1), n_beeps] - ic_m), sqrt(resvar_s));
  
  for (b in 2:n_beeps) {
    m[, b] ~ normal(ic_m + ar_m * (m[, (b - 1)] - ic_m), sqrt(resvar_m));
  }

  // priors
    // regression coefficients
  ic_s ~ normal(0, 100);
  ar_s ~ normal(0, 100);
  cr_s_m ~ normal(0, 100);
  ic_m ~ normal(0, 100);
  ar_m ~ normal(0, 100);
  ar_night_m ~ normal(0, 100);
  cr_m_s ~ normal(0, 100);
  
    // residuals
  resvar_s ~ inv_gamma(0.001, 0.001);
  resvar_m1 ~ inv_gamma(0.001, 0.001);
  resvar_m ~ inv_gamma(0.001, 0.001);
}

