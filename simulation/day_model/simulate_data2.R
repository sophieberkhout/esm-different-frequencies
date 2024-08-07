reps <- 1000

n_threads <- 25

clus <- parallel::makeCluster(n_threads)

source("simulation/day_model/utils.R")

# out <- parallel::clusterEvalQ(clus, source("simulation/day_model/utils.R"))

n_days <- c(14, 28, 56, 112)
n_beeps <- c(3, 5, 9)

pars <- list(ar_mf = 0.4,
             cr_mf_s = -0.2,
             ar_s = 0.4,
             cr_s_mf = -0.2,
             ic_m = 3,
             ic_s = 3,
             resvar_m = 1,
             resvar_mf = 0.2,
             resvar_s = 1)

save(reps, n_days, n_beeps, pars,
     file = "simulation/day_model/simulation_settings2.RData")


for (days in n_days) {
  for (beeps in n_beeps) {
    
    set.seed(13)
    parallel::parLapplyLB(cl = clus,
                          1:reps, simulateData,
                          days = days, beeps = beeps,
                          burnin = 50, pars = pars,
                          file = "simulation/day_model/data2/")
    
  }
}

parallel::stopCluster(clus)
