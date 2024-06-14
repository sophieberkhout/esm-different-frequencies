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
  real ar_m;                    // autoregression for df
  real ar_night_m;
  real cr_m_s;                  // crossregression for df
  vector[n_beeps] ic_m;               // intercepts for d
  vector<lower=0>[n_beeps] resvar_m;  // residual variance for d  
}
transformed parameters {
  // centering
  vector[n_days] c_s;
  matrix[n_days, n_beeps] c_m;
  
  c_s = s - ic_s;
  
  for (b in 1:n_beeps) {
    c_m[, b] = m[, b] - ic_m[b];
  }
}
model {
  // dynamic model with imputed variables
  m[2:n_days, 1] ~ normal(ic_m[1] + ar_night_m * c_m[1:(n_days - 1), n_beeps] + cr_m_s * c_s[2:n_days], sqrt(resvar_m[1]));
  s[2:n_days] ~ normal(ic_s + ar_s * c_s[1:(n_days - 1)] + cr_s_m * c_m[1:(n_days - 1), n_beeps], sqrt(resvar_s));
  
  for (b in 2:n_beeps) {
    m[, b] ~ normal(ic_m[b] + ar_m * c_m[, (b - 1)], sqrt(resvar_m[b]));
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
  resvar_m ~ inv_gamma(0.001, 0.001);
}

