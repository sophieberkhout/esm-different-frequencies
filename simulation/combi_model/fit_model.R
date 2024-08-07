iter <- 20000
chains <- 4
cores <- 1
thin <- 10
defaultPriors <- TRUE

n_threads <- 36

clus <- parallel::makeCluster(n_threads)

source("simulation/combi_model/utils.R")

evq <- parallel::clusterEvalQ(clus, source("simulation/combi_model/utils.R"))

modelout <- paste0("simulation/combi_model/modelout")

load("simulation/combi_model/simulation_settings.RData")

# for (days in n_days) {
#   for(beeps in n_beeps) {
# 
#     m_mplus <- .mplusSpecification(beeps = beeps, proc = cores, chains = chains,
#                                    fbiter = iter, thin = thin,
#                                    defaultPriors = defaultPriors)
# 
#     t_fit <- system.time(
#       parallel::parLapplyLB(cl = clus,
#                             1:reps, fitMplusModel,
#                             days = days, beeps = beeps,
#                             m_mplus = m_mplus,
#                             iter = iter, chains = chains, cores = cores,
#                             modelout = modelout)
#     )
# 
#   }
# }

m_stan <- rstan::stan_model("simulation/combi_model/combi_model.stan")
for (days in n_days) {
  for(beeps in n_beeps) {
    
    t_fit <- system.time(
      parallel::parLapplyLB(cl = clus,
                            1:reps, fitStanModel,
                            days = days, beeps = beeps,
                            m_stan = m_stan,
                            iter = iter, chains = chains, cores = cores,
                            modelout = modelout)
    )
    
  }
}

parallel::stopCluster(clus)
