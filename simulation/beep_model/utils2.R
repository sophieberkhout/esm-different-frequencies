simulateData <- function (r, days, beeps, burnin, pars, file) {
  
  # number of days to sample with burnin
  samples <- days + burnin
  
  # create empty vector
  s <- numeric(samples)

  # set starting values
  s[1] <- 0.001
  m <- matrix(0.001, nrow = samples, ncol = beeps)
  
  # sample residuals
  zeta_m <- m
  for (q in 1:beeps) {
    zeta_m[, q] <- rnorm(samples, 0, sqrt(pars$resvar_m))
  }
  zeta_s <- rnorm(samples, 0, sqrt(pars$resvar_s))

  # simulate data
  for (d in 2:samples) {
    s[d] <- pars$ic_s + pars$ar_s * (s[d - 1] - pars$ic_s) + pars$cr_s_m * (m[d - 1, beeps] - pars$ic_m) + zeta_s[d]
    m[d, 1] <- pars$ic_m + pars$ar_night_m * (m[d - 1, beeps] - pars$ic_m) + pars$cr_m_s * (s[d] - pars$ic_s) + zeta_m[d, 1]
    
    for (b in 2:beeps) {
      m[d, b] <- pars$ic_m + pars$ar_m * (m[d, b - 1] - pars$ic_m) + zeta_m[d, b]
    }  
  }
  
  # add intercepts / measurement error
  # s <- s + pars$ic_s
  # m <- pars$ic_m + m
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

# stanData <- function (s_obs, m_obs) {
#   s_miss_i <- which(is.na(s_obs))
#   m_miss_i <- which(is.na(m_obs), arr.ind = TRUE)
#   s_miss_n <- length(s_miss_i)
#   m_miss_n <- nrow(m_miss_i)
# 
#   s_obs[is.na(s_obs)] <- -99
#   m_obs[is.na(m_obs)] <- -99
# 
#   inp <- list(D = nrow(m_obs), B = ncol(m_obs),
#               s_obs = s_obs, m_obs = m_obs,
#               s_miss_n = s_miss_n, s_miss_i = s_miss_i,
#               m_miss_n = m_miss_n,
#               m_miss_row = m_miss_i[, 1], m_miss_col = m_miss_i[, 2])
# 
#   return(inp)
# }

fitMplusModel <- function (r, days, beeps, m_mplus, iter, chains, cores, modelout) {
  
  dat <- readRDS(
    sprintf("simulation/beep_model/data2/dat_days_%s_beeps_%s_r_%s.rds",
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
    PLOT     = NULL
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
    sprintf("simulation/beep_model/data2/dat_days_%s_beeps_%s_r_%s.rds",
            days, beeps, r)
  )
  
  dat_stan <- list(n_days = days, n_beeps = beeps,
                   s = dat$s, m = dat[, grepl("m", names(dat))])
  
  fit_stan <- tryCatch(
    rstan::sampling(m_stan, data = dat_stan, seed = 13,
                    warmup = iter / 2, iter = iter, save_warmup = FALSE,
                    chains = chains, cores = cores,
                    pars = c("ar_m", "ar_night_m", "cr_m_s", "ar_s", "cr_s_m",
                             "ic_m", "ic_s", "resvar_m1", "resvar_m", "resvar_s")
    ), error = function(e) e
  )
  
  saveRDS(fit_stan, file = sprintf("%s/stan/fit_days_%s_beeps_%s_r_%s.rds",
                                   modelout, days, beeps, r))
}


.mplusSpecification <- function(beeps, proc, chains, fbiter = NULL, biter = NULL, thin = NULL, defaultPriors = TRUE) {
  
  # VARIABLE <- sprintf("
  # USEVARIABLES = s m1-m%1$s;
  # LAGGED = s(1) m%1$s(1);
  # ", beeps)
  
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
  ! center m
  %3$s \n
  c_m%1$s BY m%1$s (&1);        ! last beep is lagged
  m1-m%1$s@0.01;                ! residual variance of m close to zero
  c_m1(resvar_m1);              ! residual variance for first c_m
  c_m2-c_m%1$s(resvar_m);       ! same residual variance for all other c_m
  [m1-m%1$s](ic_m);             ! same intercept for all m
  
  c_s BY s (&1);                ! center s
  [s](ic_s);                    ! s intercept
  s@0.01;                       ! residual variance of s close to zero
  c_s(resvar_s);                ! centered s residual variance

  c_m2-c_m%1$s PON c_m1-c_m%2$s(ar_m);   ! day beep autoregression

  c_m1 ON c_m%1$s&1(ar_night_m);! night beep autoregression
  c_m1 ON c_s(cr_m_s);          ! first beep of the day on sleep last night
  
  c_s ON c_s&1(ar_s);           ! sleep autoregression
  c_s ON c_m%1$s&1(cr_s_m);     ! sleep on last beep of yesterday
  ", beeps, beeps - 1,
     as.character(paste(sprintf("c_m%1$s BY m%1$s;", 1:(beeps - 1)), collapse = " \n ")))
  
  # MODEL <- sprintf("
  #   m1-m%1$s(resvar_m);
  #   [m1-m%1$s](ic_m);
  # 
  #   [s](ic_s);                    ! s intercept
  #   s(resvar_s);                  ! s residual variance
  # 
  #   m2-m%1$s PON m1-m%2$s(ar_m);  ! day beep autoregression
  # 
  #   m1 ON m%1$s&1(ar_night_m);    ! night beep autoregression
  #   m1 ON s(cr_m_s);              ! first beep of the day on sleep last night
  # 
  #   s ON s&1(ar_s);               ! sleep autoregression
  #   s ON m%1$s&1(cr_s_m);         ! sleep on last beep of yesterday
  # ", beeps, beeps - 1)
  
  PRIORS <- NULL
  if (!defaultPriors) {
    PRIORS <- sprintf("
    resvar_m~IG(0.001,0.001);
    ic_m~N(0.000,100);
    ar_m~N(0.000,100);
    ar_night_m~N(0.000,100);
    cr_m_s~N(0.000,100);
    ar_s~N(0.000,100);
    cr_s_m~N(0.000,100);
    ic_s~N(0.000,100);
    resvar_s~IG(0.001,0.001);
    ", beeps)
  }
  
  OUTPUT <- "
  TECH1 TECH8 STDYX;
  "
  
  PLOT <- "
  TYPE = PLOT3;
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
    fit <- MplusAutomation::readModels(sprintf("%s/mplus/fit_days_%s_beeps_%s_r_%s.out",
                                               modelout, days, beeps, r))

    df <- subset(fit$parameters$unstandardized,
                 # paramHeader == "C_M.ON" | paramHeader == "C_S.ON" | paramHeader == "Intercepts" | (paramHeader == "Residual.Variances" & param != "S"), 
                 select = c("est", "posterior_sd", "lower_2.5ci", "upper_2.5ci"))
    idx <- c(5, 1:4, 13, 14, 24, 23)
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
    # df <- sfit[-nrow(sfit), c("50%", "sd", "2.5%", "97.5%")]
    
    # have to use annoying idx to get same order of pars...
    # if (beeps == 3) idx <- c(3:4, 1:2, 5:8, 11:13, 10, 9)
    # if (beeps == 5) idx <- c(3:4, 1:2, 5:10, 13:17, 12, 11)
    # if (beeps == 9) idx <- c(3:4, 1:2, 5:14, 17:25, 16, 15)
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
  
  df <- data.frame(par = names(true), bias = bias, mae = mae, coverage = coverage)
  
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
