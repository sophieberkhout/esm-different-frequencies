iter <- 20000
chains <- 4
cores <- 1
thin <- 10
defaultPriors <- TRUE

n_threads <- 20

clus <- parallel::makeCluster(n_threads)

source("simulation/beep_model/utils2.R")

evq <- parallel::clusterEvalQ(clus, source("simulation/beep_model/utils2.R"))

modelout <- paste0("simulation/beep_model/modelout2")

load("simulation/beep_model/simulation_settings2.RData")

days <- n_days[1]
# for (days in n_days) {
  for(beeps in n_beeps) {

    m_mplus <- .mplusSpecification(beeps = beeps, proc = cores, chains = chains,
                                   fbiter = iter, thin = thin,
                                   defaultPriors = defaultPriors)

    t_fit_mplus <- system.time(
      parallel::parLapplyLB(cl = clus,
                            1:reps, fitMplusModel,
                            days = days, beeps = beeps,
                            m_mplus = m_mplus,
                            iter = iter, chains = chains, cores = cores,
                            modelout = modelout)
    )

  }
# }

# m_stan <- rstan::stan_model("simulation/beep_model/beep_model_centering.stan")
# for (days in n_days) {
#   for(beeps in n_beeps) {
# 
#     t_fit_stan <- system.time(
#       parallel::parLapplyLB(cl = clus,
#                             1:reps, fitStanModel,
#                             days = days, beeps = beeps,
#                             m_stan = m_stan,
#                             iter = iter, chains = chains, cores = cores,
#                             modelout = modelout)
#     )
# 
#   }
# }

parallel::stopCluster(clus)
