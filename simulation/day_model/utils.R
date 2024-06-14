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
    rstan::sampling(m_stan, data = dat_stan,
                    warmup = iter / 2, iter = iter, save_warmup = FALSE,
                    chains = chains, cores = cores,
                    pars = c("ar_s", "cr_s_mf", "ar_mf", "cr_mf_s",
                             "ic_s", "ic_m", "resvar_s",
                             "resvar_mf", "resvar_m")
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
  m_factor BY m1-m%1$s@1 (&1);      ! day mean of beeps
  [m1-m%1$s](ic_m1-ic_m%1$s);       ! intercepts of beeps
  m1-m%1$s(resvar_m1-resvar_m%1$s); ! residual variances of beeps
  m_factor(resvar_mf);              ! residual variance of day factor
  
  ! center s
  c_s BY s (&1); ! centered variable s
  [s](ic_s);    ! intercept of s
  s@0.01;          ! residual variance of s set to zero (0.001)
  c_s(resvar_s); ! residual variance of cs

  ! day constant regressed on preceding day constant and sleep quality last night
  m_factor ON m_factor&1(ar_mf);
  m_factor ON c_s(cr_mf_s);
  
  ! sleep quality regressed on day mean yesterday and sleep yesterday
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
    fit <- MplusAutomation::readModels(sprintf("%s/mplus/fit_days_%s_beeps_%s_r_%s.out",
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
    # df <- sfit[-nrow(sfit), c("50%", "sd", "2.5%", "97.5%")]
    
    # have to use annoying idx to get same order of pars...
    if (beeps == 3) idx <- c(3:4, 1:2, 5:8, 11:13, 10, 9)
    if (beeps == 5) idx <- c(3:4, 1:2, 5:10, 13:17, 12, 11)
    if (beeps == 9) idx <- c(3:4, 1:2, 5:14, 17:25, 16, 15)
    df <- sfit[idx, c("50%", "sd", "2.5%", "97.5%")]
    df <- as.data.frame(df)
    names(df) <- c("median", "sd", "lower", "upper")
    
    df$true <- true
    df$parameter <- names(true)
    df
  }, error = function(e) return(NULL))
  
  return(out)
}

diagnostics <- function (clus, r, res, true) {
  bias <- parallel::parSapply(cl = clus, 1:r, .bias, res = res, true = true)
  
  bias_average <- parallel::parApply(cl = clus, abs(bias), 1, mean)
  
  coverage <- parallel::parSapply(cl = clus, 1:r, .coverage, res = res, true = true)
  
  coverage_rate <- parallel::parApply(cl = clus, coverage, 1, .proportion)

  return(data.frame(bias = bias_average, coverage = coverage_rate))
}

diagnosticsNoPar <- function (reps, res, true) {
  deviance <- sapply(1:reps, .deviance, res = res, true = true)
  
  bias <- apply(deviance, 1, mean)
  
  mae <- apply(abs(deviance), 1, mean)
  
  coverage_total <- sapply(1:reps, .coverage, res = res, true = true)
  coverage <- apply(coverage_total, 1, .proportion)
  
  df <- data.frame(par = names(true), bias = bias, mae = mae, coverage = coverage)
  
  return(df)
  
  
  # bias <- sapply(1:r, .bias, res = res, true = true)
  # 
  # bias_average <- apply(bias, 1, mean)
  # bias_average_abs <- apply(abs(bias), 1, mean)
  # 
  # coverage <- sapply(1:r, .coverage, res = res, true = true)
  # 
  # coverage_rate <- apply(coverage, 1, .proportion)
  # 
  # return(data.frame(bias = bias_average, bias_abs = bias_average_abs, coverage = coverage_rate))
}

.deviance <- function (r, res, true) {
  out <- res[[r]]$median - true
  return(out)
}

# .bias <- function (r, res, true) {
#   out <- tryCatch({
#     bias <- res[[r]]$median - true
#   }, error = function(e) return(NA))
#   
#   return(out)
# }

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

.deviancePlot <- function (res, true, pars) {

  df_deviance <- data.frame(matrix(NA, nrow = length(res), ncol = nrow(res[[1]])))

  for (i in 1:length(res)) {
      df_deviance[i, ] <- res[[i]][, "median"]
  }
  names(df_deviance) <- names(true)

  df_long <- tidyr::pivot_longer(df_deviance, cols = tidyselect::all_of(pars), names_to = "parameter", values_to = "deviance")
  df_vline <- data.frame(parameter = pars,
                         true = true[pars],
                         median = apply(df_deviance[, pars], 2, median))
  
  label_names <- ggplot2::as_labeller(c('ar_eta' = "Autoregression eta",
                                        'ar_s'= "Autoregression s",
                                        'cr_eta_s'= "Crossregression eta <- s",
                                        'cr_s_eta' = "Cross-lagged regression s <- eta"))
  
  p <- ggplot2::ggplot(df_long) +
    ggplot2::geom_density(ggplot2::aes(x = deviance), colour = "red", fill = "red", alpha = 0.2) +
    ggplot2::facet_wrap(~ parameter, nrow = 1, labeller = label_names) +
    ggplot2::geom_vline(data = df_vline, mapping = ggplot2::aes(xintercept = true)) +
    ggplot2::geom_vline(data = df_vline, mapping = ggplot2::aes(xintercept = median), colour = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line()) +
    ggplot2::labs(x = "Estimated", y = "Density")
  
  return(p)

}

.deviancePlotBoth <- function (res_mplus, res_stan, true, pars) {
  
  reps <- length(res_mplus)
  n_pars <- nrow(res_mplus[[1]])
  deviance_mplus <- data.frame(matrix(NA, nrow = reps, ncol = n_pars))
  deviance_stan <- data.frame(matrix(NA, nrow = reps, ncol = n_pars))
  
  for (i in 1:reps) {
    deviance_mplus[i, ] <- res_mplus[[i]][, "median"]
    deviance_stan[i, ] <- res_stan[[i]][, "median"]
  }
  names(deviance_mplus) <- names(true)
  names(deviance_stan) <- names(true)
  
  df_long_mplus <- tidyr::pivot_longer(deviance_mplus, cols = tidyselect::all_of(pars), names_to = "parameter", values_to = "deviance")
  df_long_stan <- tidyr::pivot_longer(deviance_stan, cols = tidyselect::all_of(pars), names_to = "parameter", values_to = "deviance")
  df_long_mplus$software <- "mplus"
  df_long_stan$software <- "stan"
  
  df_merge <- rbind(df_long_mplus, df_long_stan)
  
  df_vline <- data.frame(parameter = pars,
                         true = true[pars],
                         median_mplus = apply(deviance_mplus[, pars], 2, median),
                         median_stan = apply(deviance_stan[, pars], 2, median))
  
  label_names <- ggplot2::as_labeller(c('ar_eta' = "Autoregression eta",
                                        'ar_s'= "Autoregression s",
                                        'cr_eta_s'= "Crossregression eta <- s",
                                        'cr_s_eta' = "Cross-lagged regression s <- eta"))
  
  p <- ggplot2::ggplot(df_merge) +
    ggplot2::geom_density(ggplot2::aes(x = deviance, colour = software, fill = software), alpha = 0.2) +
    ggplot2::scale_colour_manual(values = c("blue", "red")) +
    ggplot2::scale_fill_manual(values = c("blue", "red")) +
    ggplot2::facet_wrap(~ parameter, nrow = 1, labeller = label_names) +
    ggplot2::geom_vline(data = df_vline, mapping = ggplot2::aes(xintercept = true)) +
    ggplot2::geom_vline(data = df_vline, mapping = ggplot2::aes(xintercept = median_mplus), colour = "blue", linetype = "dashed") +
    ggplot2::geom_vline(data = df_vline, mapping = ggplot2::aes(xintercept = median_stan), colour = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_line()) +
    ggplot2::labs(x = "Estimated", y = "Density", colour = "Software", fill = "Software")
  
  return(p)
  
}
