datasets <- list.files("example/data/datasets/")
variables <- substr(datasets, 1, nchar(datasets) - 4)
n <- length(datasets)

sig <- matrix(nrow = n, ncol = 4)
for (i in 1:n) {
  out <- MplusAutomation::readModels(
    paste0("example/results/day_model/modelout/", variables[i], ".out")
  )
  reg_pars <- grepl("ON", out$parameters$unstandardized$paramHeader)
  p <- out$parameters$unstandardized[reg_pars, "sig"]
  if(!is.null(p)) sig[i, ] <- p
}

n_sig <- rowSums(sig)

variables[is.na(n_sig)] # "phy_dizzy"    "phy_drymouth" "phy_nauseous"
variables[ifelse(is.na(n_sig), FALSE, n_sig > 2)] # "mood_cheerf"
variables[ifelse(is.na(n_sig), FALSE, n_sig > 1)] # "mood_doubt"  "mood_down"
# "mood_strong" "mood_suspic" "pat_restl"  "pat_worry" "phy_tired" "se_selfdoub"


out <- MplusAutomation::readModels(
  paste0("example/results/day_model/modelout/", "se_selfdoub", ".out")
)

# mplus_resvars <- subset(out$parameters$unstandardized,
#                    paramHeader == "Residual.Variances" & grepl("M", out$parameters$unstandardized$param), 
#                    select = "est")
# mplus_resvars <- mplus_resvars[-nrow(mplus_resvars), ]
# margin <- qt(0.975, df = 141 - 1) * sqrt(mplus_resvars) / sqrt(141)

mplus_df <- subset(out$parameters$unstandardized,
                   paramHeader == "Intercepts"& grepl("M", out$parameters$unstandardized$param), 
                   select = c("paramHeader", "est", "posterior_sd", "lower_2.5ci", "upper_2.5ci"))
# mplus_df$lower <- mplus_df$est - margin
# mplus_df$upper <- mplus_df$est + margin

ggplot2::ggplot(mplus_df) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_2.5ci, ymax = upper_2.5ci, x = 1:10), fill = "grey75") +
  ggplot2::geom_line(ggplot2::aes(x = 1:10, y = est), linewidth = 1) +
  ggplot2::geom_point(ggplot2::aes(x = 1:10, y = est), shape = 21, fill = "white", size = 2, stroke = 1)

ggplot2::ggplot(mplus_df) +
  # ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_2.5ci, ymax = upper_2.5ci, x = 1:10), fill = "grey75") +
  ggplot2::geom_line(ggplot2::aes(x = 1:10, y = est), linewidth = 1) +
  ggplot2::geom_pointrange(ggplot2::aes(x = 1:10, y = est, ymin = lower_2.5ci, ymax = upper_2.5ci), shape = 21, fill = "white", size = 1, stroke = 2, linewidth = 1)


out_stan <- readRDS("example/results/day_model/stan_fit.rds")
df_stan <- rstan::summary(out_stan)$summary
df_stan <- as.data.frame(df_stan[grepl("ic_m", row.names(df_stan)), c("50%", "2.5%", "97.5%")])
names(df_stan) <- c("median", "lower", "upper")
ggplot2::ggplot(df_stan) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, x = 1:10), fill = "red", alpha = 0.2) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_2.5ci, ymax = upper_2.5ci, x = 1:10), data = mplus_df, fill = "blue", alpha = 0.2) +
  # ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, x = 1:10), fill = "red") +
  ggplot2::geom_line(ggplot2::aes(x = 1:10, y = median), colour = "red") +
  ggplot2::geom_point(ggplot2::aes(x = 1:10, y = median), shape = 21, fill = "red", size = 3) +
  ggplot2::geom_point(ggplot2::aes(x = 1:10, y = est), data = mplus_df, shape = 21, fill = "blue", size = 3) +
  ggplot2::geom_line(ggplot2::aes(x = 1:10, y = est), data = mplus_df, colour = "blue")



sig <- matrix(nrow = n, ncol = 13)
for (i in 1:n) {
  out <- MplusAutomation::readModels(
    paste0("example/results/beep_model/modelout/", variables[i], ".out")
  )
  reg_pars <- grepl("ON", out$parameters$unstandardized$paramHeader)
  p <- out$parameters$unstandardized[reg_pars, "sig"]
  if(!is.null(p)) sig[i, ] <- p
}
sig <- sig[, 1:5]
n_sig <- rowSums(sig)

variables[is.na(n_sig)] # "phy_dizzy"    "phy_drymouth" "phy_nauseous"
variables[ifelse(is.na(n_sig), FALSE, n_sig > 2)] # "mood_doubt"   "mood_enthus"
# "mood_lonely"  "mood_satisfi" "mood_strong"  "pat_restl"    "pat_worry"
# "se_ashamed"   "se_handle"    "se_selfdoub"  "se_selflike" 

out_stan <- readRDS("example/results/beep_model/stan_fit.rds")
df_stan <- rstan::summary(out_stan)$summary


sig <- matrix(nrow = n, ncol = 16)
for (i in 1:n) {
  out <- MplusAutomation::readModels(
    paste0("example/results/combi_model/modelout/", variables[i], ".out")
  )
  reg_pars <- grepl("ON", out$parameters$unstandardized$paramHeader)
  p <- out$parameters$unstandardized[reg_pars, "sig"]
  if(!is.null(p)) sig[i, ] <- p
}
sig <- sig[, 1:8]
n_sig <- rowSums(sig)

variables[is.na(n_sig)] # "phy_dizzy"    "phy_drymouth" "phy_nauseous"
variables[ifelse(is.na(n_sig), FALSE, n_sig > 3)] # "mood_cheerf"  "mood_down"
# "mood_enthus" "mood_satisfi" "mood_strong" "pat_worry" 
# "se_ashamed" "se_selfdoub"

out_stan <- readRDS("example/results/combi_model/stan_fit.rds")
df_stan <- rstan::summary(out_stan)$summary
