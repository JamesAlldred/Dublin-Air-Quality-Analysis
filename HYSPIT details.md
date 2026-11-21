# HYSPLIT back Trajectories: how They Were Made

_Dublin air quality project (Rathmines), November 2026. Paths are relative to the `dublin/` project folder, and `hysplit/` is gitignored as it contains setup data, not scripts._

This covers the three trajectory products on the poster: (1) two case study runs on the NOAA website, (2) a full year of local runs for 2022 that feeds the SQTBA map, and (3) the trajectory frequency map made from the same 2022 file. All times are UTC.

## 1. Software

| Item                                                          | Used for        | Notes                                                                                                                                                                                                                                                                                        |
| ------------------------------------------------------------- | --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| NOAA READY HYSPLIT trajectory page                            | Case studies    | https://www.ready.noaa.gov/HYSPLIT_traj.php, nothing to install                                                                                                                                                                                                                              |
| HYSPLIT for Windows - Public (unregistered) Version(NOAA ARL) | 2022 local run  | Installed to `C:/HYSPLIT`. The trajectory program is `C:/HYSPLIT/exec/hyts_std.exe`. Requires other software to run, details available at: [https://www.ready.noaa.gov/HYSPLIT_hytrial.php](https://www.ready.noaa.gov/HYSPLIT_hytrial.php)<br><br>Version: **April 2025 Revision (v5.4.0)** |
| R 4.6.1 and RStudio                                           | All analysis    |                                                                                                                                                                                                                                                                                              |
| R packages                                                    | Trajectory work | `tidyverse`, `openair`, `devtools` (for `source_gist()`); figures also use `patchwork` and `ragg`                                                                                                                                                                                            |
| `run_hysplit()`                                               | Local runs      | From the openair book, Appendix C. Loaded from David Carslaw's gist: https://gist.github.com/davidcarslaw/c67e33a04ff6e1be0cd7357796e4bdf5                                                                                                                                                   |

## 2. Data

| Data                                                                               | Source                                                                                        | Stored in                                                                                                                                                         |
| ---------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Hourly PM2.5 and NO2 (also O3, SO2, PM10) at Rathmines, 30 Jun 2020 to 30 Sep 2026 | 13 csv files downloaded from [https://airquality.ie/readings](https://airquality.ie/readings) | `data/split_data/`, merged into `data/rathAQMET.rds` by `data_setup.R`<br><br>Unlimited downloads, but has to be downloaded in 6 month intervals and merged later |
| Hourly wind direction and speed, Dublin Airport                                    | GHCN station EII0000EIDW, `worldmet::import_ghcn_hourly()`                                    | Merged into `rathAQMET.rds.`<br><br>At time of download, only available up to 30/04/2026                                                                          |
| Meteorology for HYSPLIT                                                            | NCEP/NCAR global reanalysis, one file per month (`RPYYYYMM.gbl`), 2.5 degree grid             | Web runs: chosen in the form. Local run: `hysplit/sqtba/TrajData/`                                                                                                |
| Trajectory files                                                                   | Made in this workflow                                                                         | `hysplit/clean_tdump.txt`, `hysplit/polluted_tdump.txt`, `hysplit/sqtba/TrajData/traj_2022.rds`                                                                   |

## 3. Case Study Trajectories (NOAA Website)

### 3.1 Choosing the Two Cases (R)

```R
# Daily means (wind direction is vector averaged), days with at least 75% data capture
rath_daily <- timeAverage(rathAQMET, avg.time = 'day', data.thresh = 75, interval = 'hour')

pm25_p95 <- quantile(rath_daily$pm25, 0.95, na.rm = TRUE)
pm25_p25 <- quantile(rath_daily$pm25, 0.25, na.rm = TRUE)

# High PM2.5 days with easterly wind (45 to 135 degrees), grouped into episodes
high_east <- rath_daily %>%
  filter(pm25 > pm25_p95, wd >= 45, wd <= 135) %>%
  arrange(date) %>%
  mutate(gap = as.numeric(difftime(date, lag(date), units = 'days')),
         episode = cumsum(is.na(gap) | gap > 1))

# Clean comparison days: low PM2.5 with south westerly wind (180 to 270 degrees)
low_sw <- rath_daily %>%
  filter(pm25 < pm25_p25, wd >= 180, wd <= 270) %>%
  arrange(date)
```

- **Dirty case:** episode 7, 8 to 11 Mar 2024 (daily mean PM2.5 29.1 and NO2 19.9 ug/m3, wind 6.5 m/s from the east). It had the lowest NO2 and highest wind speed of the strong candidates.
- **Clean case:** 21 Mar 2023 (daily mean PM2.5 2.16 ug/m3, wind from 192 degrees at 5.8 m/s). The first choice, 24 Mar 2026, could not be used because the web form only offered 2026 meteorology up to day 17.
- **Windows shown on the poster:** 21 Mar 2023 03:00 to 22 Mar 2023 00:00 and 9 Mar 2024 03:00 to 10 Mar 2024 00:00 (8 arrivals each). The mean PM2.5 of the dirty window is 39.4 ug/m3.

### 3.2 Running HYSPLIT on the Website

1. Open https://www.ready.noaa.gov/HYSPLIT_traj.php, choose **Compute archive trajectories**, and keep the defaults (1 starting location, normal trajectory).
2. Meteorology: **REANALYSIS (global, 1948 to present)**. Enter the location in decimal degrees (longitude west). The saved files show 53.332 N, 6.267 W. The station is at 53.322 N, 6.2672 W, about 1 km away, which is small next to the 2.5 degree grid.
3. Archive file: `RP202303.gbl` (clean) or `RP202403.gbl` (dirty).
4. Trajectory direction: **Backward**. Set this before entering the start time, because the form asks you to change the default start time after choosing it. One early run came out forward by mistake.
5. Start time (UTC): 2023, 03, 22, hour 00 (clean) or 2024, 03, 10, hour 00 (dirty). This is the last arrival. Further trajectories start earlier.
6. Total run time 72 hours. Start a new trajectory every 3 hours. Maximum number of trajectories 8. Start height 500 m above ground. Vertical motion: model vertical velocity (the default).
7. Request the trajectory, wait for the figure to generate.
8. For further figure customisation, the endpoints text file. Save the text files as `hysplit/clean_tdump.txt` and `hysplit/polluted_tdump.txt`.
9. Check the header of each file: `readLines('hysplit/clean_tdump.txt', 12)`. It should say BACKWARD, list 8 start times (22 00, then 21 21, down to 21 03), and give 500.0 as the height.

Web limits: 500 trajectories per day, and in practice a run returned at most 105 trajectories, with the earliest 23 truncated at the start of the run window. That is fine for 8 trajectories but not for a year, which is why section 4 runs locally.

### 3.3 Reading the Files into R

`read_tdump()` is defined in `fig2_trajectories.R`. It takes each arrival time from the start times in the file header (not the age column, which is counted from the first start) and returns the columns `date`, `date2`, `hour.inc`, `lat`, `lon` and `height`.

```r
traj <- bind_rows(read_tdump('hysplit/clean_tdump.txt'),
                  read_tdump('hysplit/polluted_tdump.txt')) %>%
  left_join(select(rathAQMET, date, pm25), by = 'date')

traj %>% group_by(date) %>% summarise(n = n(), start = min(hour.inc))   # 16 arrivals, 73 rows each, start -72
trajPlot(traj, pollutant = 'pm25', cols = c('royalblue', 'firebrick'), linewidth = 1)
```

## 4. Full Year of Trajectories for SQTBA (Local HYSPLIT, 2022)

### 4.1 Choosing the Year

A year needs all 12 monthly meteorology files plus the previous December, and good PM2.5 coverage.

```r
list_met <- function(year) {   # which monthly files exist on NOAA's server?
  url <- sprintf('https://noaa-oar-arl-hysplit-pds.s3.amazonaws.com/?prefix=reanalysis/%d/', year)
  x <- paste(readLines(url, warn = FALSE), collapse = '')
  sort(unique(regmatches(x, gregexpr(sprintf('RP%d[0-9]{2}[.]gbl', year), x))[[1]]))
}

rathAQMET %>%   # valid hourly PM2.5 per month (a full month is about 720 to 744)
  filter(year(date) %in% 2021:2024) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(valid_pm25_hours = sum(!is.na(pm25)), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = valid_pm25_hours)
```

2025 was rejected because `RP202506.gbl` does not exist on the server (HTTP 404). 2022 was chosen: all met files exist and its weakest month (April) still had 553 valid hours. 2021 has no August data, and 2023 and 2024 have weak May and June.

### 4.2 Install HYSPLIT

Download the Windows version from NOAA ARL's HYSPLIT page (ready.noaa.gov) and install it to `C:/HYSPLIT`. Check: `list.files('C:/HYSPLIT/exec', pattern = 'hyts')` should show `hyts_std.exe`. Check their page for the supporting software needed.

### 4.3 Download the Meteorology (About 1.5 GB)

Each monthly file is about 110 to 125 MB. A failed download leaves a 0 byte file, so this version deletes failures.

```r
getMet <- function(year, month, path_met) {
  base_url <- 'https://noaa-oar-arl-hysplit-pds.s3.amazonaws.com/reanalysis'
  for (yr_i in year) for (mo in month) {
    filename <- sprintf('RP%d%02d.gbl', yr_i, mo)
    destfile <- file.path(path_met, filename)
    if (file.exists(destfile) && file.size(destfile) > 0) next
    message('Downloading ', filename)
    try(suppressWarnings(download.file(paste0(base_url, '/', yr_i, '/', filename),
                                       destfile, mode = 'wb', quiet = FALSE)), silent = TRUE)
    if (file.exists(destfile) && file.size(destfile) == 0) {
      file.remove(destfile)
      message('  FAILED (file not available): ', filename)
    }
  }
}

yr <- 2022
met_dir <- normalizePath('hysplit/sqtba/TrajData', winslash = '/')
getMet(yr - 1, 12, met_dir)     # previous December, for arrivals in early January
getMet(yr, 1:12, met_dir)
```

### 4.4 Run the Trajectories (About 28 Minutes)

```r
library(devtools)
source_gist('https://gist.github.com/davidcarslaw/c67e33a04ff6e1be0cd7357796e4bdf5',
            filename = 'run_hysplit.R')

temp_dir <- normalizePath('hysplit/sqtba/TrajTemp', winslash = '/')

year_traj <- run_hysplit(
  latitude = 53.322, longitude = -6.2672,   # Rathmines station
  runtime = -72,                            # 72 hours back
  start_height = 500, model_height = 10000,
  start = yr, end = paste0(yr, '-12-31'),   # one trajectory every 3 hours
  hysplit_exec = 'C:/HYSPLIT/exec',
  hysplit_input = met_dir, hysplit_output = temp_dir,
  site = 'rathmines')

saveRDS(year_traj, 'hysplit/sqtba/TrajData/traj_2022.rds')
```

Skim the gist before running it, since `source_gist()` runs whatever it contains.

### 4.5 Checks

```r
year_traj %>% count(date, name = 'rows') %>% count(rows)    # every arrival has 73 rows
year_traj %>% distinct(date) %>% summarise(arrivals = n(), first = min(date), last = max(date))

expected <- seq(as.POSIXct('2022-01-01 00:00', tz = 'UTC'),
                as.POSIXct('2022-12-30 21:00', tz = 'UTC'), by = '3 hours')
sum(!(as.numeric(expected) %in% as.numeric(unique(year_traj$date))))   # 0 gaps
```

Result: 2,912 arrivals (1 Jan 00:00 to 30 Dec 21:00), 73 rows each (hourly points from 0 to -72 hours), no gaps. The run does not produce an arrival for 31 Dec.

### 4.6 Join PM2.5 at the Arrival Time

```r
sqtba_data <- readRDS('hysplit/sqtba/TrajData/traj_2022.rds') %>%
  left_join(select(rathAQMET, date, pm25), by = 'date') %>%
  drop_na(pm25)

n_distinct(sqtba_data$date)   # 2,711 trajectories used
nrow(sqtba_data)              # 197,903 points (2,711 x 73)
```

## 5. SQTBA Map

```r
trajLevel(sqtba_data, pollutant = 'pm25', statistic = 'sqtba',
          lat.inc = 0.5, lon.inc = 0.5, map.fill = FALSE)
```

SQTBA is simplified quantitative transport bias analysis (openair book, section 10.4). Every point on a trajectory carries the PM2.5 measured when the air arrived at Rathmines, so the map shows where the air that arrived with high PM2.5 had been. At 0.5 degrees it took about 9 seconds. The hot spot off Norway comes from a single event (10 to 11 Dec 2022) and is treated as an outlier. Script: `fig4_sqtba.R`.

## 6. Trajectory Frequency Map

```r
freq_breaks <- c(0, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100)    # log spaced steps, % of the busiest cell
freq_cols   <- hcl.colors(length(freq_breaks), 'Plasma', rev = TRUE)[-1]

trajLevel(sqtba_data, statistic = 'frequency', lat.inc = 0.5, lon.inc = 0.5,
          breaks = freq_breaks, cols = freq_cols, map.fill = FALSE)
```

**What a cell shows:** the number of hourly trajectory points in the cell, as a percentage of the busiest cell. This was checked, not assumed: the cell counts add up to 197,903 (the number of rows in `sqtba_data`), the Dublin cell holds 3,893 (more than the 2,711 trajectories), and every colour band matches `count / max(count) x 100`. The `trajLevel()` reference page describes frequency as the number of back trajectory points in a grid square. The openair book wording (unique trajectories) is looser, so cite the reference page. Script: `fig5_trajectory_frequency.R`.

## 7. Problems Met and Fixes

| Problem                                                  | Cause                                                                       | Fix                                                                          |
| -------------------------------------------------------- | --------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| Arrivals missing for 31 May to 31 Jul 2025               | `RP202506.gbl` is not on the server (404) and an empty file was left behind | `list_met()` before choosing a year. `getMet()` now deletes failed downloads |
| Web run capped at 105 trajectories, early ones truncated | Limit of the web form                                                       | Run locally                                                                  |