readData <- function (r, filenames) {
  dat <- read.csv(paste0("example/data/datasets/", filenames[r]))
  return(dat)
}

getMplusModel <- function (r, model, dat, proc = 1, chains = 2, thin = NULL, fbiter = NULL, biter = NULL) {
  VARIABLE <- "
    USEVARIABLES = s m1-m10;
  "
    
  ANALYSIS <- sprintf("
    ESTIMATOR = BAYES;
    PROC = %1$s;
    CHAINS = %2$s;
    %4$s
    %5$s
    %3$s
  ", proc, chains,
     ifelse(is.null(thin),   "", paste0("THIN = ", thin, ";")),
     ifelse(is.null(fbiter), "", paste0("FBITER = ", fbiter, ";")),
     ifelse(is.null(biter),  "", paste0("BITER = ", biter, ";"))
  )
  
  if (model == "day") {
    MODEL <- "
      m_factor BY m1-m10@1 (&1);    ! day factor of beeps
      m_factor(resvar_mf);          ! day factor residual variance
      
      [m1-m10](ic_m1-ic_m10);       ! m beep intercepts
      m1-m10(resvar_m1-resvar_m10); ! m beep residual variances
      
      c_s BY s (&1);                ! center s
      [s](ic_s);                    ! s intercept
      s@0.01;                       ! residual variance of s close to 0
      c_s(resvar_s);                ! centered s residual variance
      
      ! day factor regressed on preceding day factor
      ! and sleep quality last night
      m_factor ON m_factor&1(ar_mf);
      m_factor ON c_s(cr_mf_s);
      
      ! sleep quality regressed on day factor of yesterday
      ! and sleep yesterday
      c_s ON c_s&1(ar_s);
      c_s ON m_factor&1(cr_s_mf);
    "
  
    TITLE <- "Day-to-Day Model"
  }
  
  if (model == "beep") {
    MODEL <- "
      ! center m
      c_m1 BY m1;
      c_m2 BY m2;
      c_m3 BY m3;
      c_m4 BY m4;
      c_m5 BY m5;
      c_m6 BY m6;
      c_m7 BY m7;
      c_m8 BY m8;
      c_m9 BY m9;
      c_m10 BY m10 (&1);        ! last beep is lagged
      m1-m10@0.01;              ! residual variance of m close to 0
      c_m1(resvar_m1);          ! residual variance for first c_m
      c_m2-c_m10(resvar_m);     ! same residual variance all other c_m
      [m1-m10](ic_m);           ! same intercept for all m
      
      c_s BY s (&1);            ! center s
      [s](ic_s);                ! s intercept
      s@0.01;                   ! residual variance of s close to 0
      c_s(resvar_s);            ! centered s residual variance
      
      c_m2-c_m10 PON c_m1-c_m9(ar_m);   ! day beep autoregression
      
      c_m1 ON c_m10&1(ar_night_m);  ! night beep autoregression
      c_m1 ON c_s(cr_m_s);          ! first beep on sleep last night
      
      c_s ON c_s&1(ar_s);           ! sleep autoregression
      c_s ON c_m10&1(cr_s_m);       ! sleep on last beep of yesterday
    "
    TITLE <- "Beep-to-Beep Model"
  }
  
  if (model == "beep_all_day") {
    MODEL <- "
      ! center m
      c_m1 BY m1;
      c_m2 BY m2;
      c_m3 BY m3;
      c_m4 BY m4;
      c_m5 BY m5;
      c_m6 BY m6;
      c_m7 BY m7;
      c_m8 BY m8;
      c_m9 BY m9;
      c_m10 BY m10 (&1);        ! last beep is lagged
      m1-m10@0.01;              ! residual variance of m close to 0
      c_m1(resvar_m1);          ! residual variance for first c_m
      c_m2-c_m10(resvar_m);      ! same residual variance all other c_m
      [m1-m10](ic_m);           ! same intercept for all m
      
      c_s BY s (&1);            ! center s
      [s](ic_s);                ! s intercept
      s@0.01;                   ! residual variance of s close to 0
      c_s(resvar_s);            ! centered s residual variance
      
      c_m2-c_m10 PON c_m1-c_m9(ar_m);   ! day beep autoregression
      
      c_m1 ON c_m10&1(ar_night_m);  ! night beep autoregression
      
      ! all self-doubt beeps on sleep last night
      c_m1-c_m10 ON c_s(cr_m_s1-cr_m_s10);
      
      c_s ON c_s&1(ar_s);           ! sleep autoregression
      c_s ON c_m10&1(cr_s_m);       ! sleep on last beep of yesterday
    "
    TITLE <- "Adjusted Beep-to-Beep Model"
  }
  
  if (model == "combi") {
    MODEL <- "
      m_factor BY m1-m10@1 (&1);     ! day factor of beeps
      m_factor(resvar_mf);           ! day factor residual variance
      
      ! residual of the mood variables
      r_m1 BY m1;
      r_m2 BY m2;
      r_m3 BY m3;
      r_m4 BY m4;
      r_m5 BY m5;
      r_m6 BY m6;
      r_m7 BY m7;
      r_m8 BY m8;
      r_m9 BY m9;
      r_m10 BY m10 (&1);            ! last beep is lagged
      m1-m10@0.01;                  ! residual variance of m close to 0
      r_m1-r_m10(resvar_m1-resvar_m10); ! residual variance of m
      [m1-m10](ic_m1-ic_m10);       ! intercepts for beeps
      
      c_s BY s (&1);                ! center s
      [s](ic_s);                    ! s intercept
      s@0.01;                       ! residual variance of s close to 0
      c_s(resvar_s);                ! centered s residual variance
      
      ! day factor regressed on preceding day factor
      ! and sleep quality last night
      m_factor ON m_factor&1(ar_mf);
      m_factor ON c_s(cr_mf_s);
      
      ! day beep autoregression
      r_m2-r_m10 PON r_m1-r_m9(ar_m);
      
      ! first beep of day regressed on last beep yesterday
      ! and sleep last night
      r_m1 ON r_m10&1(ar_night_m);
      r_m1 ON c_s(cr_m_s);
      
      ! sleep quality regressed on day factor of yesterday
      ! and sleep yesterday
      ! and residual last beep yesterday
      c_s ON c_s&1(ar_s);
      c_s ON m_factor&1(cr_s_mf);
      c_s ON r_m10&1(cr_s_m);
      
      m_factor WITH r_m10@0;        ! covariance between factors to 0
    "
    
    TITLE <- "Combination Model"
  }
    
  OUTPUT <- "
    TECH1 TECH8 STDYX;
  "
    
  PLOT <- "
    TYPE = PLOT3;
  "
  
  mod <- MplusAutomation::mplusObject(
    TITLE    = TITLE,
    rdata    = dat[[r]],
    VARIABLE = VARIABLE,
    ANALYSIS = ANALYSIS,
    MODEL    = MODEL,
    OUTPUT   = OUTPUT,
    PLOT     = PLOT
  )
}

fitMplusModel <- function (r, modelout, mod, variables) {
  out <-  tryCatch(
    MplusAutomation::mplusModeler(
      object = mod[[r]],
      modelout = paste0(modelout, variables[r], ".inp"),
      run = 1,
      hashfilename = FALSE
    ), error = function(e) e
  )
}

getStanDat <- function(dat) {
  s <- dat$s
  m <- as.matrix(dat[, grepl("m", names(dat))])
  
  i_mis_s <- which(is.na(s))
  
  mis_m <- apply(m, 2, function(x) which(is.na(x)))
  row_mis_m <- unlist(mis_m)
  col_mis_m <- rep(1:ncol(m), times = lengths(mis_m))
  
  s[i_mis_s] <- 999
  m[cbind(row_mis_m, col_mis_m)] <- 999
  
  inp <- list(
    n_days = nrow(dat), n_beeps = ncol(m), s_obs = s, m_obs = m,
    n_mis_s = length(i_mis_s), i_mis_s = i_mis_s,
    n_mis_m = length(row_mis_m), row_mis_m = row_mis_m, col_mis_m = col_mis_m
  )
  return(inp)
}

getStanModel <- function (model) {
  if (model == "day") {
    code <- "
    data {
      int<lower=1> n_days;            // number of days
      int<lower=1> n_beeps;           // number of beeps
      vector[n_days] s_obs;           // measurement of sleep quality
      matrix[n_days, n_beeps] m_obs;  // measurement of beeps
      
      // missing data
        // number of missing values in sleep variable
      int<lower=0, upper=n_days * n_beeps> n_mis_s;
        // indexes of missing values in sleep variable
      int<lower=1, upper=n_days * n_beeps> i_mis_s[n_mis_s];
        // number of missing values in mood variable
      int<lower=0, upper=n_days * n_beeps> n_mis_m;
        // row index of missing values in mood variable
      int<lower=1, upper=n_days * n_beeps> row_mis_m[n_mis_m];
        // column index of missing values in mood variable
      int<lower=1, upper=n_days * n_beeps> col_mis_m[n_mis_m];
    }
    parameters {
      vector[n_days] m_factor;      // daily mood factor
      real<lower=0> resvar_mf;      // residual variance for mf
      
      vector[n_beeps] ic_m;               // intercepts for m
      vector<lower=0>[n_beeps] resvar_m;  // residual variance for m
      
      real ic_s;                    // intercept for s
      real<lower=0> resvar_s;       // residual variance for s
      
      real ar_mf;                   // autoregression for mf
      real cr_mf_s;                 // crossregression for mf
    
      real ar_s;                    // autoregression for s
      real cr_s_mf;                 // cross-lagged regression for s
      
      // missing data
      vector[n_mis_s] mis_s;
      vector[n_mis_m] mis_m;
    }
    transformed parameters {
      vector[n_days] s;
      matrix[n_days, n_beeps] m;
      vector[n_days] c_s;
    
      // fill s with estimated missing values
      s = s_obs;
      s[i_mis_s] = mis_s;
    
      // center s
      c_s = s - ic_s;
    
      // fill m with estimated missing values
      m = m_obs;
      for (i in 1:n_mis_m) {
        m[row_mis_m[i], col_mis_m[i]] = mis_m[i];
      }
    }
    model {
      // dynamic models
      m_factor[2:n_days] ~ normal(ar_mf * m_factor[1:(n_days - 1)]
                                  + cr_mf_s * c_s[2:n_days],
                                  sqrt(resvar_mf));
                                  
      s[2:n_days] ~ normal(ic_s + ar_s * c_s[1:(n_days - 1)]
                           + cr_s_mf * m_factor[1:(n_days - 1)],
                           sqrt(resvar_s));
    
      // measurement equation
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
      
        // residual variances
      resvar_s ~ inv_gamma(0.001, 0.001);
      resvar_mf ~ inv_gamma(0.001, 0.001);
      resvar_m ~ inv_gamma(0.001, 0.001);
    }
  "
  }
  
  if (model == "beep") { 
    code <- "
    data {
      int<lower=1> n_days;            // number of days
      int<lower=1> n_beeps;           // number of beeps
      vector[n_days] s_obs;           // measurement of sleep quality
      matrix[n_days, n_beeps] m_obs;  // measurement of beeps
      
      // missing data
        // number of missing values in sleep variable
      int<lower=0, upper=n_days * n_beeps> n_mis_s;
        // indexes of missing values in sleep variable
      int<lower=1, upper=n_days * n_beeps> i_mis_s[n_mis_s];
        // number of missing values in mood variable
      int<lower=0, upper=n_days * n_beeps> n_mis_m;
        // row index of missing values in mood variable
      int<lower=1, upper=n_days * n_beeps> row_mis_m[n_mis_m];
        // column index of missing values in mood variable
      int<lower=1, upper=n_days * n_beeps> col_mis_m[n_mis_m];
    }
    parameters {
      real ic_m;                    // intercept for m
      real<lower=0> resvar_m1;      // residual variance for m 
      real<lower=0> resvar_m;       // residual variance for m 
      
      real ic_s;                    // intercept for s
      real<lower=0> resvar_s;       // residual variance for s
      
      real ar_m;                    // autoregression for m
      real ar_night_m;              // night autoregression for m
      real cr_m_s;                  // crossregression for m
    
      real ar_s;                    // autoregression for s
      real cr_s_m;                  // cross-lagged regression for s
    
      // missing data
      vector[n_mis_s] mis_s;
      vector[n_mis_m] mis_m;
    }
    transformed parameters {
      vector[n_days] s;
      matrix[n_days, n_beeps] m;
      vector[n_days] c_s;
      matrix[n_days, n_beeps] c_m;
    
      // fill s with estimated missing values
      s = s_obs;
      s[i_mis_s] = mis_s;
    
      // center s
      c_s = s - ic_s;
    
      // fill m with estimated missing values
      m = m_obs;
      for (i in 1:n_mis_m) {
        m[row_mis_m[i], col_mis_m[i]] = mis_m[i];
      }
    
      // center m
      for (b in 1:n_beeps) {
        c_m[, b] = m[, b] - ic_m;
      }
    }
    model {
      // dynamic models
      m[2:n_days, 1] ~ normal(ic_m 
                              + ar_night_m
                              * c_m[1:(n_days - 1), n_beeps]
                              + cr_m_s * c_s[2:n_days],
                              sqrt(resvar_m1));
    
      for (b in 2:n_beeps) {
        m[, b] ~ normal(ic_m + ar_m * c_m[, (b - 1)],
                        sqrt(resvar_m));
      }
                              
      s[2:n_days] ~ normal(ic_s + ar_s * c_s[1:(n_days - 1)]
                           + cr_s_m
                           * c_m[1:(n_days - 1), n_beeps],
                           sqrt(resvar_s));
    
      // priors
        // regression coefficients
      ic_s ~ normal(0, 100);
      ar_s ~ normal(0, 100);
      cr_s_m ~ normal(0, 100);
      ic_m ~ normal(0, 100);
      ar_m ~ normal(0, 100);
      ar_night_m ~ normal(0, 100);
      cr_m_s ~ normal(0, 100);
      
        // residual variances
      resvar_s ~ inv_gamma(0.001, 0.001);
      resvar_m1 ~ inv_gamma(0.001, 0.001);
      resvar_m ~ inv_gamma(0.001, 0.001);
    }
  "
  }
  
  if (model == "beep_all_day") { 
    code <- "
    data {
      int<lower=1> n_days;            // number of days
      int<lower=1> n_beeps;           // number of beeps
      vector[n_days] s_obs;           // measurement of sleep quality
      matrix[n_days, n_beeps] m_obs;  // measurement of beeps
      
      // missing data
      int<lower=0, upper=n_days * n_beeps> n_mis_s;      // number of missing values in sleep
      int<lower=1, upper=n_days * n_beeps> i_mis_s[n_mis_s];
      int<lower=0, upper=n_days * n_beeps> n_mis_m;
      int<lower=1, upper=n_days * n_beeps> row_mis_m[n_mis_m];
      int<lower=1, upper=n_days * n_beeps> col_mis_m[n_mis_m];
    }
    parameters {
      real ic_m;                    // intercept for m
      real<lower=0> resvar_m1;      // residual variance for m 
      real<lower=0> resvar_m;       // residual variance for m 
      
      real ic_s;                    // intercept for s
      real<lower=0> resvar_s;       // residual variance for s
      
      real ar_m;                    // autoregression for m
      real ar_night_m;              // night autoregression for m
      vector[n_beeps] cr_m_s;       // crossregression for m
    
      real ar_s;                    // autoregression for s
      real cr_s_m;                  // cross-lagged regression for s
    
      // missing data
      vector[n_mis_s] mis_s;
      vector[n_mis_m] mis_m;
    }
    transformed parameters {
      vector[n_days] s;
      matrix[n_days, n_beeps] m;
      vector[n_days] c_s;
      matrix[n_days, n_beeps] c_m;
    
      // fill s with estimated missing values
      s = s_obs;
      s[i_mis_s] = mis_s;
    
      // center s
      c_s = s - ic_s;
    
      // fill m with estimated missing values
      m = m_obs;
      for (i in 1:n_mis_m) {
        m[row_mis_m[i], col_mis_m[i]] = mis_m[i];
      }
    
      // center m
      for (b in 1:n_beeps) {
        c_m[, b] = m[, b] - ic_m;
      }
    }
    model {
      // dynamic models
      m[2:n_days, 1] ~ normal(ic_m 
                              + ar_night_m
                              * c_m[1:(n_days - 1), n_beeps]
                              + cr_m_s[1] * c_s[2:n_days],
                              sqrt(resvar_m1));
    
      for (b in 2:n_beeps) {
        m[, b] ~ normal(ic_m + ar_m * c_m[, (b - 1)] + cr_m_s[b] * c_s,
                        sqrt(resvar_m));
      }
                              
      s[2:n_days] ~ normal(ic_s + ar_s * c_s[1:(n_days - 1)]
                           + cr_s_m
                           * c_m[1:(n_days - 1), n_beeps],
                           sqrt(resvar_s));
    
      // priors
        // regression coefficients
      ic_s ~ normal(0, 100);
      ar_s ~ normal(0, 100);
      cr_s_m ~ normal(0, 100);
      ic_m ~ normal(0, 100);
      ar_m ~ normal(0, 100);
      ar_night_m ~ normal(0, 100);
      cr_m_s ~ normal(0, 100);
      
        // residual variances
      resvar_s ~ inv_gamma(0.001, 0.001);
      resvar_m1 ~ inv_gamma(0.001, 0.001);
      resvar_m ~ inv_gamma(0.001, 0.001);
    }
  "
  }
  
  if (model == "combi") {
    code <- "
    data {
      int<lower=1> n_days;            // number of days
      int<lower=1> n_beeps;           // number of beeps
      vector[n_days] s_obs;           // measurement of sleep quality
      matrix[n_days, n_beeps] m_obs;  // measurement of beeps
      
      // missing data
      int<lower=0, upper=n_days * n_beeps> n_mis_s;      // number of missing values in sleep
      int<lower=1, upper=n_days * n_beeps> i_mis_s[n_mis_s];
      int<lower=0, upper=n_days * n_beeps> n_mis_m;
      int<lower=1, upper=n_days * n_beeps> row_mis_m[n_mis_m];
      int<lower=1, upper=n_days * n_beeps> col_mis_m[n_mis_m];
    }
    parameters {
      vector[n_days] m_factor;      // daily mood factor
      real<lower=0> resvar_mf;      // residual variance m_factor
      
      vector[n_beeps] ic_m;               // intercepts for m
      vector<lower=0>[n_beeps] resvar_m;  // residual variances for m
      
      real ic_s;                    // intercept for s
      real ar_s;                    // autoregression for s
      
      real ar_mf;                   // autoregression for mf
      real cr_mf_s;                 // crossregresson for mf
      
      real ar_m;                    // daytime autoregression for m
      real ar_night_m;              // nighttime autoregression for m
      real cr_m_s;                  // crossregression for m
      
      real cr_s_mf;                 // cross-lagged regression s-mf
      real cr_s_m;                  // cross-lagged regression s-m
      real<lower=0> resvar_s;       // residual variance for s
      
      // missing data
      vector[n_mis_s] mis_s;
      vector[n_mis_m] mis_m;
    }
    transformed parameters {
      vector[n_days] s;
      matrix[n_days, n_beeps] m;
      vector[n_days] c_s;
      matrix[n_days, n_beeps] c_m;
    
      // fill s with estimated missing values
      s = s_obs;
      s[i_mis_s] = mis_s;
    
      // center s
      c_s = s - ic_s;
    
      // fill m with estimated missing values
      m = m_obs;
      for (i in 1:n_mis_m) {
        m[row_mis_m[i], col_mis_m[i]] = mis_m[i];
      }
      
      // center m
      for (b in 1:n_beeps) {
        c_m[, b] = m[, b] - (ic_m[b] + m_factor);
      }
    }
    model {
      // dynamic models
      m_factor[2:n_days] ~ normal(ar_mf * m_factor[1:(n_days - 1)]
                                  + cr_mf_s * c_s[2:n_days],
                                  sqrt(resvar_mf));
    
      m[2:n_days, 1] ~ normal(ic_m[1] + m_factor[2:n_days]
                              + ar_night_m
                              * c_m[1:(n_days - 1), n_beeps]
                              + cr_m_s * c_s[2:n_days],
                              sqrt(resvar_m[1]));
    
      s[2:n_days] ~ normal(ic_s + ar_s * c_s[1:(n_days - 1)]
                           + cr_s_m * c_m[1:(n_days - 1), n_beeps]
                           + cr_s_mf * m_factor[1:(n_days - 1)],
                           sqrt(resvar_s));
    
      // measurement equation
      for (b in 2:n_beeps) {
        m[, b] ~ normal(ic_m[b] + m_factor + ar_m * c_m[, (b - 1)],
                        sqrt(resvar_m[b]));
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
      
        // residual variances
      resvar_s ~ inv_gamma(0.001, 0.001);
      resvar_mf ~ inv_gamma(0.001, 0.001);
      resvar_m ~ inv_gamma(0.001, 0.001);
    }
  "
  }
  
  return(code)
}
