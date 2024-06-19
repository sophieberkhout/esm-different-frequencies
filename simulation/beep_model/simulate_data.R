reps <- 1000

n_threads <- 25

clus <- parallel::makeCluster(n_threads)

source("simulation/beep_model/utils.R")

n_days <- c(14, 28, 56, 112)
n_beeps <- c(3, 5, 9)

pars <- list(ar_m = 0.5,
             ar_night_m = 0.8,
             cr_m_s = -0.1,
             ar_s = -0.2,
             cr_s_m = -0.3,
             ic_s = 6,
             ic_m = 1,
             resvar_m = 0.75,
             resvar_s = 1)

save(reps, n_days, n_beeps, pars,
     file = "simulation/beep_model/simulation_settings.RData")


for (days in n_days) {
  for (beeps in n_beeps) {
    
    set.seed(13)
    parallel::parLapplyLB(cl = clus,
                          1:reps, simulateData,
                          days = days, beeps = beeps,
                          burnin = 50, pars = pars,
                          file = "simulation/beep_model/data/")
    
  }
}

parallel::stopCluster(clus)
