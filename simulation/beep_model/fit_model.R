# estimation settings
iter <- 20000 # number of iterations
chains <- 4   # number of Markov chains
cores <- 1    # number of cores
thin <- 10    # thinning
defaultPriors <- TRUE # use Mplus default priors

# parallelize
n_threads <- 36
clus <- parallel::makeCluster(n_threads)
source("simulation/beep_model/utils.R")
evq <- parallel::clusterEvalQ(clus, source("simulation/beep_model/utils.R"))

# where to store output
modelout <- paste0("simulation/beep_model/modelout")
# create modelout folder if does not exist
dir.create(modelout)
dir.create(paste0(modelout, "/mplus/"))
dir.create(paste0(modelout, "/stan/"))

# load simulation settings
load("simulation/beep_model/simulation_settings.RData")

# fit Mplus
for (days in n_days) {
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
}

# compile Stan model
m_stan <- rstan::stan_model("simulation/beep_model/beep_model.stan")

# fit Stan
for (days in n_days) {
  for(beeps in n_beeps) {

    t_fit_stan <- system.time(
      parallel::parLapplyLB(cl = clus,
                            1:reps, fitStanModel,
                            days = days, beeps = beeps,
                            m_stan = m_stan,
                            iter = iter, chains = chains, cores = cores,
                            modelout = modelout)
    )

  }
}

# end parallelization
parallel::stopCluster(clus)
