simulateData <- function (r, days, beeps, burnin, pars, file) {
  
  # number of days to sample with burnin
  samples <- days + burnin
  
  # create empty matrix and vectors
  s <- m_factor <- numeric(samples)

  s[1] <- m_factor[1] <- 0.001 # starting value
  
  r_m <- matrix(0.001, nrow = samples, ncol = beeps)
  m <- matrix(0.001, nrow = samples, ncol = beeps)
  
  # sample residuals
  zeta_m <- m
  for (q in 1:beeps) {
    zeta_m[, q] <- rnorm(samples, 0, sqrt(pars$resvar_m))
  }
  zeta_s <- rnorm(samples, 0, sqrt(pars$resvar_s))
  zeta_mf <- rnorm(samples, 0, sqrt(pars$resvar_mf))
  
  # simulate data
  for (d in 2:samples) {
    s[d] <- pars$ar_s * s[d - 1] + pars$cr_s_mf * m_factor[d - 1] + pars$cr_s_m * r_m[d - 1, beeps] + zeta_s[d]
    m_factor[d] <- pars$ar_mf * m_factor[d - 1] + pars$cr_mf_s * s[d] + zeta_mf[d]
    r_m[d, 1] <- pars$ar_night_m * r_m[d - 1, beeps] + pars$cr_m_s * s[d] + zeta_m[d, 1]
    
    for (b in 2:beeps) {
      r_m[d, b] <- pars$ar_m * r_m[d, b - 1] + zeta_m[d, b]
    }
  }
  
  # add intercepts / measurement error
  s <- s + pars$ic_s
  m <- pars$ic_m + m_factor + r_m
  colnames(m) <- paste0("m", 1:beeps)
  df <- data.frame(s, m)
  
  # remove burnin
  df <- df[(burnin + 1):samples, ]
  
  # save data to separate files
  saveRDS(
    df,
    file = sprintf("%sdat_days_%s_beeps_%s_r_%s.rds", file, days, beeps, r)
  )
}

fitMplusModel <- function (r, days, beeps, m_mplus, iter, chains, cores, modelout) {
  
  dat <- readRDS(
    sprintf("simulation/combi_model/data/dat_days_%s_beeps_%s_r_%s.rds",
            days, beeps, r)
  )

  model_mplus <- MplusAutomation::mplusObject(
    TITLE = "Combination Model",
    rdata = dat,
    VARIABLE = m_mplus$VARIABLE,
    ANALYSIS = m_mplus$ANALYSIS,
    MODEL    = m_mplus$MODEL,
    MODELPRIORS = m_mplus$PRIORS,
    OUTPUT   = m_mplus$OUTPUT,
    PLOT     = m_mplus$PLOT
  )
  
  fit_mplus <- tryCatch(
    MplusAutomation::mplusModeler(
    model_mplus,
    modelout = sprintf("%s/mplus/fit_days_%s_beeps_%s_r_%s.inp",
                       modelout, days, beeps, r),
    run = 1,
    hashfilename = FALSE
    ), error = function(e) e
  )
  
}

fitStanModel <- function (r, days, beeps, m_stan, iter, chains, cores, modelout) {
  
  dat <- readRDS(
    sprintf("simulation/combi_model/data/dat_days_%s_beeps_%s_r_%s.rds",
            days, beeps, r)
  )
  
  dat_stan <- list(n_days = days, n_beeps = beeps,
                   s = dat$s, m = dat[, grepl("m", names(dat))])
  
  fit_stan <- tryCatch(
    rstan::sampling(m_stan, data = dat_stan, seed = 13,
                    warmup = iter / 2, iter = iter, save_warmup = FALSE,
                    chains = chains, cores = cores,
                    pars = c("ar_mf", "ar_m", "ar_night_m", "cr_mf_s", "cr_m_s",
                             "ar_s", "cr_s_mf", "cr_s_m",
                             "ic_m", "ic_s", "resvar_m",
                             "resvar_mf", "resvar_s")
    ), error = function(e) e
  )
  
  saveRDS(fit_stan, file = sprintf("%s/stan/fit_days_%s_beeps_%s_r_%s.rds",
                                   modelout, days, beeps, r))
}


.mplusSpecification <- function(beeps, proc, chains, fbiter = NULL, biter = NULL, thin = NULL, defaultPriors = TRUE) {
  
  VARIABLE <- sprintf("
  USEVARIABLES = s m1-m%1$s;
  ", beeps)
  
  ANALYSIS <- sprintf("
  ESTIMATOR = BAYES;
  PROC = %1$s;
  CHAINS = %2$s;
  %4$s
  %5$s
  %3$s
  ", proc, chains,
     ifelse(is.null(thin), "", paste0("THIN = ", thin, ";")),
     ifelse(is.null(fbiter), "", paste0("FBITER = ", fbiter, ";")),
     ifelse(is.null(biter), "", paste0("BITER = ", biter, ";")))
  
  MODEL <- sprintf("
    m_factor BY m1-m%1$s@1 (&1); ! day factor of beeps
    m_factor(resvar_mf);         ! day factor residual variance
  
    ! residual of the mood variables
    %3$s \n
    r_m%1$s BY m%1$s (&1);        ! last beep is lagged
    m1-m%1$s@0.01;                ! set residual variances close to zero (0.01)
    r_m1-r_m%1$s(resvar_m1-resvar_m%1$s); ! residual variances for beeps
    [m1-m%1$s](ic_m1-ic_m%1$s);   ! intercepts for beeps
    
    c_s BY s (&1);                ! center s
    [s](ic_s);                    ! s intercept
    s@0.01;                       ! residual variance of s close to zero
    c_s(resvar_s);                ! centered s residual variance
  
    ! day factor regressed on preceding day factor
    ! and sleep quality last night
    m_factor ON m_factor&1(ar_df);
    m_factor ON c_s(cr_df_s);
  
    ! day beep autoregression
    r_m2-r_m%1$s PON r_m1-r_m%2$s(ar_m);
    
    ! first beep of day regressed on last beep yesterday
    ! and sleep last night
    r_m1 ON r_m%1$s&1(ar_night_m);
    r_m1 ON c_s(cr_m_s);
    
    ! sleep quality regressed on day factor of yesterday
    ! and sleep yesterday
    ! and residual last beep yesterday
    c_s ON c_s&1(ar_s);
    c_s ON m_factor&1(cr_s_mf);
    c_s ON r_m%1$s&1(cr_s_m);
  
    m_factor WITH r_m%1$s@0;        ! covariance between factors to 0
  ", beeps, beeps - 1,
     as.character(paste(sprintf("r_m%1$s BY m%1$s;", 1:(beeps - 1)),
                        collapse = " \n ")))
  
  PRIORS <- NULL
  if (!defaultPriors) {
    PRIORS <- sprintf("
    DO (1,%1$s) resvar_m#~IG(0.001,0.001);
    DO (1,%1$s) ic_m#~N(0.000,100);
    ar_mf~N(0.000,100);
    ar_m~N(0.000,100);
    ar_night_m~N(0.000,100);
    cr_mf_s~N(0.000,100);
    cr_m_s~N(0.000,100);
    ar_s~N(0.000,100);
    cr_s_mf~N(0.000,100);
    cr_s_m~N(0.000,100);
    ic_s~N(0.000,100);
    resvar_mf~IG(0.001,0.001);
    resvar_s~IG(0.001,0.001);
    ", beeps)
  }
  
  OUTPUT <- "
  TECH1 TECH8 STDYX;
  "
  
  PLOT <- "
  TYPE = PLOT3;
  FACTOR = ALL;
  "
  
  return(
    list(
      VARIABLE = VARIABLE,
      ANALYSIS = ANALYSIS,
      MODEL    = MODEL,
      PRIORS   = PRIORS,
      OUTPUT   = OUTPUT,
      PLOT     = NULL
    )
  )
  
}

readMplusResults <- function (r, days, beeps, modelout, true) {
  out <- tryCatch({
    fit <- MplusAutomation::readModels(
      sprintf("%s/mplus/fit_days_%s_beeps_%s_r_%s.out",
              modelout, days, beeps, r))

    df <- subset(fit$parameters$unstandardized,
                 select = c("est", "posterior_sd",
                            "lower_2.5ci", "upper_2.5ci"))
    
    # awkward code to get parameters in right order
    if (beeps == 3) idx <- c(8, 15, 13, 9, 14, 10:12, 19:21, 18, 27:29, 26, 30)
    if (beeps == 5) idx <- c(12, 19, 17, 13, 18, 14:16, 25:29, 24, 37:41, 36, 42)
    if (beeps == 9) idx <- c(20, 27, 25, 21, 26, 22:24, 37:45, 36, 57:65, 56, 66)
    df  <- df[idx, ]
    names(df) <- c("median", "sd", "lower", "upper")
    
    df$true <- true
    df$parameter <- names(true)
    df
  }, error = function(e) return(NULL))
  
  return(out)
}

readStanResults <- function (r, days, beeps, modelout, true) {
  out <- tryCatch({
    fit <- readRDS(sprintf("%s/stan/fit_days_%s_beeps_%s_r_%s.rds",
                           modelout, days, beeps, r))
    
    sfit <- rstan::summary(fit)$summary
    df <- sfit[-nrow(sfit), c("50%", "sd", "2.5%", "97.5%")]
    df <- as.data.frame(df)
    names(df) <- c("median", "sd", "lower", "upper")
    
    df$true <- true
    df$parameter <- names(true)
    df
  }, error = function(e) return(NULL))
  
  return(out)
}

diagnostics <- function (reps, res, true) {
  
  deviance <- sapply(1:reps, .deviance, res = res, true = true)
  
  bias <- apply(deviance, 1, mean)
  
  mae <- apply(abs(deviance), 1, mean)
  
  coverage_total <- sapply(1:reps, .coverage, res = res, true = true)
  coverage <- apply(coverage_total, 1, .proportion)
  
  df <- data.frame(par = names(true), bias = bias,
                   mae = mae, coverage = coverage)
  
  return(df)
  
}

.deviance <- function (r, res, true) {
  out <- res[[r]]$median - true
  return(out)
}

.coverage <- function (r, res, true) {
  out <- tryCatch({
    coverage <- res[[r]]$lower <= true & true <= res[[r]]$upper
  }, error = function(e) return(NA))
  
  return(out)
}

.proportion <- function (x) {
  out <- tryCatch({
    sum(x) / length(x)
  }, error = function(e) return(NA))
  
  return(out)
}
