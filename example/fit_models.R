setwd("D:/Users/berkh011/Documents/GitHub/esm-different-frequencies")

datasets <- list.files("example/data/datasets/")
variables <- substr(datasets, 1, nchar(datasets) - 4)
n <- length(datasets)

clus <- parallel::makeCluster(n)

source("example/utils.R")

dat <- parallel::parLapplyLB(cl = clus, 1:n, readData, datasets)

proc <- 1
chains <- 4
thin <- 10
fbiter <- 20000

# Day-to-Day Model
## Mplus
mod_mplus <- parallel::parLapplyLB(cl = clus, 1:n, getMplusModel,
                                   model = "day", dat = dat,
                                   proc = proc, chains = chains,
                                   thin = thin, fbiter = fbiter)

t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:n, fitMplusModel,
                        modelout = "example/results/day_model/modelout/",
                        mod = mod_mplus, variables = variables)
)

# code_stan <- getStanModel(model = "day")
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "day"))

getStanDat <- function(dat) {
  s <- dat$s
  m <- as.matrix(dat[, grepl("m", names(dat))])
  
  i_mis_s <- which(is.na(s))

  mis_m <- apply(m, 2, function(x) which(is.na(x)))
  row_mis_m <- unlist(mis_m)
  col_mis_m <- rep(1:ncol(m), times = lengths(mis_m))
  
  s[i_mis_s] <- 999
  m[cbind(row_mis_m, col_mis_m)] <- 999
  
  inp <- list(
    n_days = nrow(dat), n_beeps = ncol(m), s_obs = s, m_obs = m,
    n_mis_s = length(i_mis_s), i_mis_s = i_mis_s,
    n_mis_m = length(row_mis_m), row_mis_m = row_mis_m, col_mis_m = col_mis_m
  )
  return(inp)
}


dat_selfdoub <- dat[[which(variables == "se_selfdoub")]]
dat_selfdoub_stan <- getStanDat(dat_selfdoub)

t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = fbiter, warmup = fbiter / 2, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_s", "cr_s_mf", "ar_mf", "cr_mf_s",
                                  "ic_s", "ic_m", "resvar_s",
                                  "resvar_mf", "resvar_m"))
)

# Beep-to-Beep Model
## Mplus
mod <- parallel::parLapplyLB(cl = clus, 1:n, getMplusModel,
                             model = "beep", dat = dat,
                             proc = proc, chains = chains,
                             thin = thin, fbiter = fbiter)

t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:n, fitMplusModel,
                        modelout = "example/results/beep_model/modelout/",
                        mod = mod, variables = variables)
)

## Stan
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "beep"))

t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = fbiter, warmup = fbiter / 2, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_s", "cr_s_m",
                                  "ar_m", "ar_night_m", "cr_m_s",
                                  "ic_s", "ic_m", "resvar_s",
                                  "resvar_m"))
)

# Combination Model
## Mplus
mod <- parallel::parLapplyLB(cl = clus, 1:n, getMplusModel,
                             model = "combi", dat = dat,
                             proc = proc, chains = chains,
                             thin = thin, fbiter = fbiter)

t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:n, fitMplusModel,
                        modelout = "example/results/combi_model/modelout/",
                        mod = mod, variables = variables)
)

## Stan
mod_stan <- rstan::stan_model(model_code = getStanModel(model = "combi"))

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

t_fit <- system.time(
  out <- rstan::sampling(mod_stan, data = dat_selfdoub_stan, seed = 13,
                         iter = 50000, warmup = 25000, thin = thin,
                         save_warmup = FALSE,
                         pars = c("ar_s", "cr_s_mf", "cr_s_m",
                                  "ar_mf", "cr_mf_s",
                                  "ar_m", "ar_night_m", "cr_m_s",
                                  "ic_s", "ic_m", "resvar_s", "resvar_mf",
                                  "resvar_m"))
)

parallel::stopCluster(clus)
