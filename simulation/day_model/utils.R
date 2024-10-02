simulateData <- function (r, days, beeps, burnin, pars, file) {
  
  # number of days to sample with burnin
  samples <- days + burnin
  
  # create empty matrix and vectors
  epsilon <- matrix(NA, nrow = samples, ncol = beeps)
  s <- m_factor <- numeric(samples)

  s[1] <- m_factor[1] <- 0.001 # starting value
  
  # sample residuals
  for (q in 1:beeps) {
    epsilon[, q] <- rnorm(samples, 0, sqrt(pars$resvar_m))
  }
  zeta_s <- rnorm(samples, 0, sqrt(pars$resvar_s))
  zeta_mf <- rnorm(samples, 0, sqrt(pars$resvar_mf))
  
  # simulate data
  for (d in 2:samples) {
    s[d] <- pars$ar_s * s[d - 1] + pars$cr_s_mf * m_factor[d - 1] + zeta_s[d]
    m_factor[d] <- pars$ar_mf * m_factor[d - 1] + pars$cr_mf_s * s[d] + zeta_mf[d]
  }
  
  # add intercepts / measurement error
  s <- s + pars$ic_s
  m <- pars$ic_m + m_factor + epsilon
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
    sprintf("simulation/day_model/data/dat_days_%s_beeps_%s_r_%s.rds",
            days, beeps, r)
  )

  model_mplus <- MplusAutomation::mplusObject(
    TITLE = "Day-to-Day Model",
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
    sprintf("simulation/day_model/data/dat_days_%s_beeps_%s_r_%s.rds",
            days, beeps, r)
  )
  
  dat_stan <- list(n_days = days, n_beeps = beeps,
                   s = dat$s, m = dat[, grepl("m", names(dat))])
  
  fit_stan <- tryCatch(
    rstan::sampling(m_stan, data = dat_stan, seed = 13,
                    warmup = iter / 2, iter = iter, save_warmup = FALSE,
                    chains = chains, cores = cores,
                    pars = c("ar_mf", "cr_mf_s", "ar_s", "cr_s_mf",
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
  ! latent structure beeps
  m_factor BY m1-m%1$s@1 (&1);      ! day factor of beeps
  [m1-m%1$s](ic_m1-ic_m%1$s);       ! intercepts of beeps
  m1-m%1$s(resvar_m1-resvar_m%1$s); ! residual variances of beeps
  m_factor(resvar_mf);              ! residual variance of day factor
  
  ! center s
  c_s BY s (&1); ! centered variable s
  [s](ic_s);     ! intercept of s
  s@0.01;        ! residual variance of s set to zero (0.01)
  c_s(resvar_s); ! residual variance of cs

  ! day factor regressed on preceding day factor and sleep quality last night
  m_factor ON m_factor&1(ar_mf);
  m_factor ON c_s(cr_mf_s);
  
  ! sleep quality regressed on day factor yesterday and sleep yesterday
  c_s ON c_s&1(ar_s);
  c_s ON m_factor&1(cr_s_mf);
  ", beeps)
  
  PRIORS <- NULL
  if (!defaultPriors) {
    PRIORS <- sprintf("
    DO (1,%1$s) resvar_m#~IG(0.001,0.001);
    DO (1,%1$s) ic_m#~N(0.000,100);
    ar_mf~N(0.000,100);
    cr_mf_s~N(0.000,100);
    ar_s~N(0.000,100);
    cr_s_mf~N(0.000,100);
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
      PLOT     = PLOT
    )
  )
  
}

readMplusResults <- function (r, days, beeps, modelout, true) {
  out <- tryCatch({
    fit <- MplusAutomation::readModels(
      sprintf("%s/mplus/fit_days_%s_beeps_%s_r_%s.out",
              modelout, days, beeps, r))

    df <- subset(fit$parameters$unstandardized,
                 paramHeader == "M_FACTOR.ON" | paramHeader == "C_S.ON" | paramHeader == "Intercepts" | (paramHeader == "Residual.Variances" & param != "S"), 
                 select = c("est", "posterior_sd", "lower_2.5ci", "upper_2.5ci"))
    
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
