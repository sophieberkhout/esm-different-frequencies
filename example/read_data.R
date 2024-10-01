# data available at https://osf.io/j4fg8/
# Kossakowski, J. J., Groot, P. C., Haslbeck, J. M. B., Borsboom, D., & Wichers, M. (2017).
# Data from ‘Critical Slowing Down as a Personalized Early Warning Signal for Depression.’
# Journal of Open Psychology Data, 5, 1. https://doi.org/10.5334/jopd.29

dir.create("example/data/")
dir.create("example/data/datasets")

# to run code below, add data file named 'ESMdata.csv' to folder "example/data/"

dat_raw <- read.csv("example/data/ESMdata.csv")

# keep get only last phase of the study (post treatment)
dat_post <- dat_raw[dat_raw$phase %in% c(4, 5), ]

# get sequence of all dates in the range of dates
d <- as.Date(dat_post$date, format = "%d/%m/%y")
dat_post$date <- d
date_range <- seq(min(d), max(d), by = 1) 

# get sleep quality observation per day
sleep <- numeric()
for (i in 1:length(date_range)) {
  df_day <- dat_post[dat_post$date == date_range[i], ]
  if (nrow(df_day) == 0) {
    sleep[i] <- NA
  } else {
    s <- df_day$mor_qualsleep
    if(length(unique(s)) != 1) warning(paste("no unique s in", date_range[i]))
    sleep[i] <- unique(s)
  }
}

# create data frame with all dates and all beeps
df_beepday <- data.frame(beepno = rep(1:10, length(date_range)),
                         date = rep(date_range, each = 10),
                         s = rep(sleep, each = 10))

# merge dat_post with df_beepday to add missings for missing dates and beeps
dat <- merge(df_beepday, dat_post, by = c("date", "beepno"), all = TRUE)

# likert scale momentary variables
variables <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

# divide wide format data into separate data files per variable
for (i in variables) {
  
  # keep only variable of interest and change column names
  df <- dat[, c("date", "beepno", "resptime_s", "s", i)]
  names(df) <- c("date", "beep", "time", "s", "m")

  # create wide format data with beep numbers
  df_wide <- tidyr::pivot_wider(df, id_cols = c(date, s),
                                names_from = beep, values_from = m,
                                names_prefix = "m", names_sort = TRUE)
  
  write.csv(df_wide, row.names = FALSE,
            file = paste0("example/data/datasets/", i, ".csv"))
  
}

# number of observations in sleep variable
length(sleep) - sum(is.na(sleep))
# number of observations in self-doubt variable
length(dat$se_selfdoub) - sum(is.na(dat$se_selfdoub))

# average number of observations per day in self-doubt variable
obs_selfdoub <- numeric()
for (i in 1:length(date_range)) {
  sd_day <- dat[dat$date == date_range[i], "se_selfdoub"]
  if (length(sd_day) == 0) {
    obs_selfdoub[i] <- NA
  } else {
    obs_selfdoub[i] <- length(sd_day) - sum(is.na(sd_day))
  }
}
mean(obs_selfdoub, na.rm = TRUE)
