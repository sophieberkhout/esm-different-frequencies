source("simulation/day_model/utils.R")
load("simulation/day_model/simulation_settings.RData")
modelout <- paste0("simulation/day_model/modelout")

# create empty data frame with 7 columns
df_diagnostics <- data.frame(matrix(NA, nrow = 0, ncol = 7))

# compute bias, mean absolute error, and coverage rate for each scenario
for (days in n_days) {
  for (beeps in n_beeps) {
    t_read <- system.time({
      
      # get true values used to simulate data
      true <- pars
      true$ic_m <- rep(pars$ic_m, beeps) # intercept per beep
      true$resvar_m <- rep(pars$resvar_m, beeps) # residual variance per beep
      true <- unlist(true)
      
      # read Mplus results
      res_mplus <- lapply(1:reps, readMplusResults, days = days, beeps = beeps,
                            modelout = modelout, true = true)
      
      # compute Mplus diagnostics and make data frame
      diag_mplus <- diagnostics(reps, res_mplus, true)
      diag_mplus$days <- days
      diag_mplus$beeps <- beeps
      diag_mplus$software <- "Mplus"
      
      # read Stan results
      res_stan <- lapply(1:reps, readStanResults, days = days, beeps = beeps,
                         modelout = modelout, true = true)
      
      # compute Stan diagnostics and make data frame
      diag_stan <- diagnostics(reps, res_stan, true)
      diag_stan$days <- days
      diag_stan$beeps <- beeps
      diag_stan$software <- "Stan"
      
      # combine data frames
      df_diagnostics <- rbind(df_diagnostics, diag_mplus, diag_stan)
      
    })
    # keep track of duration and iteration
    print(sprintf("%s min elapsed", round(t_fit[3] / 60, digits = 1)))
  }
}

# save diagnostics
saveRDS(df_diagnostics, file = "simulation/day_model/simulation_results.rds")
