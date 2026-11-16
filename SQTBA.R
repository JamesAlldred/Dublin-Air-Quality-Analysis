# SQTBA and trajectory frequency maps for PM2.5, 2022 ------------------------------

library(tidyverse)
library(openair)

## Data ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds") %>%
  select(1:8, wd, ws) %>%
  mutate(
    across(c(pm10, pm25), ~ if_else(.x < -50, NA_real_, .x)),
    so2 = if_else(so2 > 1000, NA_real_, so2)
  ) %>%
  rename(lat = lat.x)

## 2022 back trajectories (72 hours, 500 m, from the local HYSPLIT run), joined to PM2.5 at arrival time
sqtba_data <- readRDS("hysplit/sqtba/TrajData/traj_2022.rds") %>%
  left_join(select(rathAQMET, date, pm25), by = "date") %>%
  drop_na(pm25)

## Pull the ggplot out of an openair object 
as_gg <- function(x) if (inherits(x, "openair")) x$plot else x

## Maps ---------------------------------
p_sqtba <- as_gg(trajLevel(sqtba_data, pollutant = "pm25", statistic = "sqtba",
                           lat.inc = 0.5, lon.inc = 0.5, map.fill = FALSE, plot = FALSE))

p_freq  <- as_gg(trajLevel(sqtba_data, statistic = "frequency",
                           lat.inc = 0.5, lon.inc = 0.5, map.fill = FALSE, plot = FALSE))

class(p_sqtba)
class(p_freq)

## Save ---------------------------------
ggsave("outputs/4 - sqtba_pm25_2022.png", p_sqtba, width = 8, height = 6, dpi = 600)
ggsave("outputs/5 - trajectory_frequency_2022.png", p_freq, width = 8, height = 6, dpi = 600)
