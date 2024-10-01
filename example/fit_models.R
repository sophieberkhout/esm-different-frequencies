# set working directory and create folders if they do not exist
setwd("D:/Users/berkh011/Documents/GitHub/esm-different-frequencies")
dir.create("example/results/")
dir.create("example/results/beep_model/")
dir.create("example/results/beep_model/modelout/")
dir.create("example/results/day_model/")
dir.create("example/results/day_model/modelout/")
dir.create("example/results/combi_model/")
dir.create("example/results/combi_model/modelout/")

# get list of data set per vraiable
datasets <- list.files("example/data/datasets/")
# extract variable names
variables <- substr(datasets, 1, nchar(datasets) - 4)
# number of variables
n <- length(datasets)

# parallellize
clus <- parallel::makeCluster(n)

source("example/utils.R")

# read data sets
dat <- parallel::parLapplyLB(cl = clus, 1:n, readData, datasets)

# estimation settings
proc <- 1       # number of cores
chains <- 4     # number of Markov chains
thin <- 10      # thinning
fbiter <- 20000 # number of iterations

# Day-to-Day Model
## Mplus

# set up Mplus model code per variable
mod_mplus <- parallel::parLapplyLB(cl = clus, 1:n, getMplusModel,
                                   model = "day", dat = dat,
                                   proc = proc, chains = chains,
                                   thin = thin, fbiter = fbiter)

# fit Mplus per variable
t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:n, fitMplusModel,
                        modelout = "example/results/day_model/modelout/",
                        mod = mod_mplus, variables = variables)
)

## Stan

# compile Stan model
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "day"))

# read self-doubt data and put in Stan format
dat_selfdoub <- read.csv(paste0("example/data/datasets/", "se_selfdoub.csv"))
dat_selfdoub_stan <- getStanDat(dat_selfdoub)

# fit Stan
t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = fbiter, warmup = fbiter / 2, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_s", "cr_s_mf", "ar_mf", "cr_mf_s",
                                  "ic_s", "ic_m", "resvar_s",
                                  "resvar_mf", "resvar_m"))
)

# save results
saveRDS(out, "example/results/day_model/stan_fit.rds")

# Beep-to-Beep Model
## Mplus

# set up Mplus model code per variable
mod <- parallel::parLapplyLB(cl = clus, 1:n, getMplusModel,
                             model = "beep", dat = dat,
                             proc = proc, chains = chains,
                             thin = thin, fbiter = fbiter)

# fit Mplus per variable
t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:n, fitMplusModel,
                        modelout = "example/results/beep_model/modelout/",
                        mod = mod, variables = variables)
)

## Stan

# compile Stan model
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "beep"))

# fit Stan
t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = fbiter, warmup = fbiter / 2, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_m", "ar_night_m", "cr_m_s",
                                  "ar_s", "cr_s_m",
                                  "ic_s", "ic_m", "resvar_m1", "resvar_m",
                                  "resvar_s"))
)

# save results
saveRDS(out, "example/results/beep_model/stan_fit.rds")

###### Beep-to-Beep Model Extra Analysis
###### cr_m_s for all beeps (self-doubt beep regressed on morning sleep quality)

## Mplus
# which is self-doubt
r_selfdoub <- which(variables == "se_selfdoub")

# set up Mplus model code
mod_all_day <- getMplusModel(r_selfdoub,
                     model = "beep_all_day", dat = dat,
                     proc = proc, chains = chains,
                     thin = thin, fbiter = fbiter)

# fit Mplus
t_fit <- system.time(
  MplusAutomation::mplusModeler(
    object = mod_all_day,
    modelout = "example/results/beep_model/modelout/se_selfdoub_all_day.inp",
    run = 1,
    hashfilename = FALSE
  )
)

## Stan

# compile Stan model
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "beep_all_day"))

# fit Stan
t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = fbiter, warmup = fbiter / 2, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_m", "ar_night_m", "cr_m_s",
                                  "ar_s", "cr_s_m",
                                  "ic_m", "ic_s", "resvar_m1", "resvar_m",
                                  "resvar_s"))
)

# save results
saveRDS(out, "example/results/beep_model/stan_fit_all_day.rds")

# Combination Model
## Mplus

# set up Mplus model code per variable
mod <- parallel::parLapplyLB(cl = clus, 1:n, getMplusModel,
                             model = "combi", dat = dat,
                             proc = proc, chains = chains,
                             thin = thin, fbiter = fbiter)

# fit Mplus per variable
t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:n, fitMplusModel,
                        modelout = "example/results/combi_model/modelout/",
                        mod = mod, variables = variables)
)

## Stan

# compile Stan model
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "combi"))

# fit Stan
t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = fbiter, warmup = fbiter / 2, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_s", "cr_s_mf", "cr_s_m",
                                  "ar_mf", "cr_mf_s",
                                  "ar_m", "ar_night_m", "cr_m_s",
                                  "ic_s", "ic_m", "resvar_s", "resvar_mf",
                                  "resvar_m"))
)

# save results
saveRDS(out, "example/results/combi_model/stan_fit.rds")

# end parallelization
parallel::stopCluster(clus)
