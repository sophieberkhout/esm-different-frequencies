setwd("C:/Users/Berkh011/Documents/GitHub/different-timescales")
source("simulation/beep_model/utils.R")

load("simulation/beep_model/simulation_settings.RData")

modelout <- paste0("simulation/beep_model/modelout")

df_diagnostics <- data.frame(matrix(NA, nrow = 0, ncol = 7))
for (days in n_days) {
  for (beeps in n_beeps) {
    t_read <- system.time({
      
      true <- pars
      true$ic_m <- rep(pars$ic_m, beeps)
      true$resvar_m <- rep(pars$resvar_m, beeps)
      true <- unlist(true)
      
      res_mplus <- lapply(1:reps, readMplusResults, days = days, beeps = beeps,
                            modelout = modelout, true = true)
      
      diag_mplus <- diagnosticsNoPar(reps, res_mplus, true)
      diag_mplus$days <- days
      diag_mplus$beeps <- beeps
      diag_mplus$software <- "Mplus"
      
      res_stan <- lapply(1:reps, readStanResults, days = days, beeps = beeps,
                         modelout = modelout, true = true)
      
      diag_stan <- diagnostics(reps, res_stan, true)
      diag_stan$days <- days
      diag_stan$beeps <- beeps
      diag_stan$software <- "Stan"
      
      df_diagnostics <- rbind(df_diagnostics, diag_mplus, diag_stan)
      
    })
    print(sprintf("%s min elapsed", round(t_fit[3] / 60, digits = 1)))
  }
}

saveRDS(df_diagnostics, file = "simulation/beep_model/simulation_results.rds")
