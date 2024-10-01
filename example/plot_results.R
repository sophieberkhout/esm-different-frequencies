# plot colors for Stan and Mplus
cols <- c("#1E88E5", "#D81B60")

# day-to-day model

# read results
out_stan <- readRDS("example/results/day_model/stan_fit.rds")
out_mplus <- MplusAutomation::readModels(
  paste0("example/results/day_model/modelout/", "se_selfdoub", ".out")
)

# autoregressive and cross(-lagged) effects
# get Stan estimates of median and CI and put in data frame for plotting
res_stan <- rstan::summary(out_stan)$summary
df_stan <- res_stan[, c("2.5%", "50%", "97.5%")]
df_stan <- df_stan[c(3:4, 1:2), ]
colnames(df_stan) <- c("lower", "median", "upper")
pars <- rownames(df_stan)
pars_df <- data.frame(pars = pars)
df_stan <- cbind(df_stan, pars_df)
df_stan$pars <- factor(df_stan$pars, levels = pars)
df_stan$model <- "Stan"

# get Mplus estimates of median an dCI and put in data frame for plotting
res_mplus <- out_mplus$parameters$unstandardized
df_mplus <- res_mplus[, c("lower_2.5ci", "est", "upper_2.5ci")]
df_mplus <- df_mplus[c(12, 13, 14, 15), ]
colnames(df_mplus) <- c("lower", "median", "upper")
df_mplus$pars <- factor(pars, levels = pars)
df_mplus$model <- "Mplus"

# combine data frames
df <- rbind(df_stan, df_mplus)

# plot estimates
p_day <- ggplot2::ggplot(df) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey75") + 
  ggplot2::geom_pointrange(ggplot2::aes(x = pars, y = median,
                                        ymin = lower, ymax = upper,
                                        colour = model),
                           position = ggplot2::position_dodge2(width = .5),
                           shape = 21, fill = "white", size = 0.5, stroke = 1,
                           linewidth = 1) +
  ggplot2::scale_x_discrete(labels = c(expression(phi["MF"]),
                                       expression(beta["MF"]),
                                       expression(phi["S"]),
                                       expression(beta["S"]))) +
  ggplot2::scale_y_continuous(breaks = seq(-2, 2, 1), name = "Estimate") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Parameter", colour = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    legend.position = "bottom",
  ) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -Inf, y = -2, yend = 2),
                        linewidth = 0.5, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 4, y = -Inf, yend = -Inf,
                        linewidth = 0.5, lineend = "square")

ggplot2::ggsave("example/results/day_model/estimates_day.pdf", p_day, 
                width = 5, height = 3)

#### day-to-day model beep mean estimates
# get Stan estimates of median and CI and put in data frame for plotting
df_stan <- res_stan[, c("2.5%", "50%", "97.5%")]
df_stan <- df_stan[c(6:15), ]
colnames(df_stan) <- c("lower", "median", "upper")
pars <- rownames(df_stan)
pars_df <- data.frame(pars = pars)
df_stan <- cbind(df_stan, pars_df)
df_stan$pars <- factor(df_stan$pars, levels = pars)
df_stan$model <- "Stan"

# get Mplus estimates of median and CI and put in data frame for plotting
df_mplus <- res_mplus[, c("lower_2.5ci", "est", "upper_2.5ci")]
df_mplus <- df_mplus[c(17:26), ]
colnames(df_mplus) <- c("lower", "median", "upper")
df_mplus$pars <- factor(pars, levels = pars)
df_mplus$model <- "Mplus"

# combine data frames
df <- rbind(df_stan, df_mplus)

# plot beep means
p_day_mu <- ggplot2::ggplot(df) +
  ggplot2::geom_pointrange(ggplot2::aes(x = pars, y = median,
                                        ymin = lower, ymax = upper,
                                        colour = model),
                           position = ggplot2::position_dodge2(width = .5),
                           shape = 21, fill = "white", size = 0.5, stroke = 1,
                           linewidth = 1) +
  ggplot2::scale_x_discrete(labels = c(expression(mu["M,1"]),
                                       expression(mu["M,2"]),
                                       expression(mu["M,3"]),
                                       expression(mu["M,4"]),
                                       expression(mu["M,5"]),
                                       expression(mu["M,6"]),
                                       expression(mu["M,7"]),
                                       expression(mu["M,8"]),
                                       expression(mu["M,9"]),
                                       expression(mu["M,10"]))) +
  ggplot2::scale_y_continuous(breaks = seq(1, 3, 0.5), name = "Estimate") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Parameter", colour = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    legend.position = "bottom",
  ) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -Inf, y = 1, yend = 3),
                        linewidth = 0.5, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 10, y = -Inf, yend = -Inf,
                        linewidth = 0.5, lineend = "square")

ggplot2::ggsave("example/results/day_model/beep_means_day.pdf", p_day_mu, 
                width = 5, height = 3)

#### day-to-day model beep variance estimates
# get Stan estimates of median and CI and put in data frame for plotting
df_stan <- res_stan[, c("2.5%", "50%", "97.5%")]
df_stan <- df_stan[c(18:27), ]
colnames(df_stan) <- c("lower", "median", "upper")
pars <- rownames(df_stan)
pars_df <- data.frame(pars = pars)
df_stan <- cbind(df_stan, pars_df)
df_stan$pars <- factor(df_stan$pars, levels = pars)
df_stan$model <- "Stan"

# get Mplus estimates of median and CI and put in data frame for plotting
df_mplus <- res_mplus[, c("lower_2.5ci", "est", "upper_2.5ci")]
df_mplus <- df_mplus[c(28:37), ]
colnames(df_mplus) <- c("lower", "median", "upper")
df_mplus$pars <- factor(pars, levels = pars)
df_mplus$model <- "Mplus"

# combine data frames
df <- rbind(df_stan, df_mplus)

# plot variance estimates
p_day_var <- ggplot2::ggplot(df) +
  ggplot2::geom_pointrange(ggplot2::aes(x = pars, y = median,
                                        ymin = lower, ymax = upper,
                                        colour = model),
                           position = ggplot2::position_dodge2(width = .5),
                           shape = 21, fill = "white", size = 0.5, stroke = 1,
                           linewidth = 1) +
  ggplot2::scale_x_discrete(labels = c(expression(sigma[epsilon][",1"]^"2"),
                                       expression(sigma[epsilon][",2"]^"2"),
                                       expression(sigma[epsilon][",3"]^"2"),
                                       expression(sigma[epsilon][",4"]^"2"),
                                       expression(sigma[epsilon][",5"]^"2"),
                                       expression(sigma[epsilon][",6"]^"2"),
                                       expression(sigma[epsilon][",7"]^"2"),
                                       expression(sigma[epsilon][",8"]^"2"),
                                       expression(sigma[epsilon][",9"]^"2"),
                                       expression(sigma[epsilon][",10"]^"2"))) +
  ggplot2::scale_y_continuous(breaks = seq(0, 1.5, 0.5), name = "Estimate") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Parameter", colour = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    legend.position = "bottom",
  ) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -Inf, y = 0, yend = 1.5),
                        linewidth = 0.5, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 10, y = -Inf, yend = -Inf,
                        linewidth = 0.5, lineend = "square")

ggplot2::ggsave("example/results/day_model/beep_vars_day.pdf", p_day_var, 
                width = 5, height = 3)

# beep-to-beep model

# read results
out_stan <- readRDS("example/results/beep_model/stan_fit.rds")
out_mplus <- MplusAutomation::readModels(
  paste0("example/results/beep_model/modelout/", "se_selfdoub", ".out")
)

# get Stan estimates of median and CI and put in data frame for plotting
res_stan <- rstan::summary(out_stan)$summary
df_stan <- res_stan[, c("2.5%", "50%", "97.5%")]
df_stan <- df_stan[c(1:3, 4:5), ]
colnames(df_stan) <- c("lower", "median", "upper")
pars <- rownames(df_stan)
pars_df <- data.frame(pars = pars)
df_stan <- cbind(df_stan, pars_df)
df_stan$pars <- factor(df_stan$pars, levels = pars)
df_stan$model <- "Stan"

# get Mplus estimates of median and CI and put in data frame for plotting
res_mplus <- out_mplus$parameters$unstandardized
df_mplus <- res_mplus[, c("lower_2.5ci", "est", "upper_2.5ci")]
df_mplus <- df_mplus[c(16, 12, 13, 14, 15), ]
colnames(df_mplus) <- c("lower", "median", "upper")
df_mplus$pars <- factor(pars, levels = pars)
df_mplus$model <- "Mplus"

# combine data frames
df <- rbind(df_stan, df_mplus)

# plot estimates
p_beep <- ggplot2::ggplot(df) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey75") + 
  ggplot2::geom_pointrange(ggplot2::aes(x = pars, y = median,
                                        ymin = lower, ymax = upper,
                                        colour = model),
                           position = ggplot2::position_dodge2(width = .5),
                           shape = 21, fill = "white", size = 0.5, stroke = 1,
                           linewidth = 1) +
  ggplot2::scale_x_discrete(labels = c(expression(phi["M"]),
                                       expression(gamma["M"]),
                                       expression(delta["M"]),
                                       expression(phi["S"]),
                                       expression(delta["S"]))) +
  ggplot2::scale_y_continuous(breaks = seq(-1, 1, 0.5), name = "Estimate") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Parameter", colour = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    legend.position = "bottom",
  ) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -Inf, y = -1, yend = 1),
                        linewidth = 0.5, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 5, y = -Inf, yend = -Inf,
                        linewidth = 0.5, lineend = "square")

ggplot2::ggsave("example/results/beep_model/estimates_beep.pdf", p_beep, 
                width = 5, height = 3)

#### beep-to-beep model all day crossregressions

# read results
out_stan <- readRDS("example/results/beep_model/stan_fit_all_day.rds")
out_mplus <- MplusAutomation::readModels(
  paste0("example/results/beep_model/", "se_selfdoub_all_day", ".out")
)

# get Stan estimates of median and CI and put in data frame for plotting
res_stan <- rstan::summary(out_stan)$summary
df_stan <- res_stan[, c("2.5%", "50%", "97.5%")]
df_stan <- df_stan[c(3:12), ]
colnames(df_stan) <- c("lower", "median", "upper")
pars <- rownames(df_stan)
pars_df <- data.frame(pars = pars)
df_stan <- cbind(df_stan, pars_df)
df_stan$pars <- factor(df_stan$pars, levels = pars)
df_stan$model <- "Stan"

# get Mplus estimates of median and CI and put in data frame for plotting
res_mplus <- out_mplus$parameters$unstandardized
df_mplus <- res_mplus[, c("lower_2.5ci", "est", "upper_2.5ci")]
df_mplus <- df_mplus[seq(13, 31, 2), ]
colnames(df_mplus) <- c("lower", "median", "upper")
df_mplus$pars <- factor(pars, levels = pars)
df_mplus$model <- "Mplus"

# combine data frames
df <- rbind(df_stan, df_mplus)

# plot crossregression estimates
p_beep_all_day <- ggplot2::ggplot(df) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey75") + 
  ggplot2::geom_pointrange(ggplot2::aes(x = pars, y = median,
                                        ymin = lower, ymax = upper,
                                        colour = model),
                           position = ggplot2::position_dodge2(width = .5),
                           shape = 21, fill = "white", size = 0.5, stroke = 1,
                           linewidth = 1) +
  ggplot2::scale_x_discrete(labels = c(expression(delta["M,1"]),
                                       expression(delta["M,2"]),
                                       expression(delta["M,3"]),
                                       expression(delta["M,4"]),
                                       expression(delta["M,5"]),
                                       expression(delta["M,6"]),
                                       expression(delta["M,7"]),
                                       expression(delta["M,8"]),
                                       expression(delta["M,9"]),
                                       expression(delta["M,10"]))) +
  ggplot2::scale_y_continuous(breaks = seq(-0.8, 0.4, .2), name = "Estimate") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Parameter", colour = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    legend.position = "bottom",
  ) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -Inf,
                                     y = -0.8, yend = 0.4),
                        linewidth = 0.5, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 10, y = -Inf, yend = -Inf,
                        linewidth = 0.5, lineend = "square")

ggplot2::ggsave("example/results/beep_model/estimates_beep_cross.pdf",
                p_beep_all_day, width = 5, height = 3)

# combi model

# read results
out_stan <- readRDS("example/results/combi_model/stan_fit.rds")
out_mplus <- MplusAutomation::readModels(
  paste0("example/results/combi_model/modelout/", "se_selfdoub", ".out")
)

# get Stan estimates of median and CI and put in data frame for plotting
res_stan <- rstan::summary(out_stan)$summary
df_stan <- res_stan[, c("2.5%", "50%", "97.5%")]
df_stan <- df_stan[c(4:5, 6:8, 1:3), ]
colnames(df_stan) <- c("lower", "median", "upper")
pars <- rownames(df_stan)
pars_df <- data.frame(pars = pars)
df_stan <- cbind(df_stan, pars_df)
df_stan$pars <- factor(df_stan$pars, levels = pars)
df_stan$model <- "Stan"

# get Mplus estimates of median and CI and put in data frame for plotting
res_mplus <- out_mplus$parameters$unstandardized
df_mplus <- res_mplus[, c("lower_2.5ci", "est", "upper_2.5ci")]
df_mplus <- df_mplus[c(22, 23, 29, 27, 28, 24:26), ]
colnames(df_mplus) <- c("lower", "median", "upper")
df_mplus$pars <- factor(pars, levels = pars)
df_mplus$model <- "Mplus"

# combine data frames
df <- rbind(df_stan, df_mplus)

# plot estimates
p_combi <- ggplot2::ggplot(df) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey75") + 
  ggplot2::geom_pointrange(ggplot2::aes(x = pars, y = median,
                                        ymin = lower, ymax = upper,
                                        colour = model),
                           position = ggplot2::position_dodge2(width = .5),
                           shape = 21, fill = "white", size = 0.5, stroke = 1,
                           linewidth = 1) +
  ggplot2::scale_x_discrete(labels = c(expression(phi["MF"]),
                                       expression(beta["MF"]),
                                       expression(phi["M"]),
                                       expression(gamma["M"]),
                                       expression(delta["M"]),
                                       expression(phi["S"]),
                                       expression(beta["S"]),
                                       expression(delta["S"]))) +
  ggplot2::scale_y_continuous(breaks = seq(-5, 2, 1), name = "Estimate") +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::labs(x = "Parameter", colour = "Software") +
  ggplot2::theme_void() +
  ggplot2::theme(
    text = ggplot2::element_text(family = "sans", size = 12),
    axis.title = ggplot2::element_text(size = 12),
    axis.title.y = ggplot2::element_text(angle = 90),
    axis.text = ggplot2::element_text(margin = ggplot2::margin(5, 5, 5, 5)),
    axis.text.y = ggplot2::element_text(hjust = 1),
    axis.ticks = ggplot2::element_line(lineend = "butt", linewidth = 0.5),
    axis.ticks.length = ggplot2::unit(2.5, "pt"),
    legend.position = "bottom",
  ) +
  ggplot2::geom_segment(ggplot2::aes(x = -Inf, xend = -Inf, y = -5, yend = 2),
                        linewidth = 0.5, lineend = "square") +
  ggplot2::geom_segment(x = 1, xend = 8, y = -Inf, yend = -Inf,
                        linewidth = 0.5, lineend = "square")

ggplot2::ggsave("example/results/combi_model/estimates_combi.pdf", p_combi, 
                width = 5, height = 3)
