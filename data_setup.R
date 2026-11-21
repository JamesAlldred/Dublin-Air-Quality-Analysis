library(tidyverse)
library(leaflet)
library(htmltools)
library(openair)
library(openairmaps)
library(patchwork)
library(webshot2)
library(png)
library(htmlwidgets)
library(worldmet)

## AQ data (Rathmines - 30th Jun 2020 to 30th Sept 2026) --------------------------------

### Load and list CSVs
data_dir <- "C:/CodingProjects/RProjects/AirPoll/dublin/data/split_data"
file_list <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)

### read and combine CSVs into a single data frame, rename columns and add lat/lon
rathAQ <- file_list %>%
  map_dfr(~read.csv(.x, 
                    fill = TRUE, 
                    na.strings = c("", "NA", "No Data", "N/A", "-"))) %>% ### Make NA values explicit 
  rename(
    date = Date.and.Time,
    no2 = NO.sub.2..sub.,
    o3 = O.sub.3..sub.,
    so2 = SO.sub.2..sub.,
    pm10 = PM.sub.10..sub.,
    pm25 = PM.sub.2.5..sub.
  ) %>%
  mutate(
    date = ymd_hms(date, tz = "UTC"),
    lat = 53.3220,
    lon = -6.2672
  ) %>%
  arrange(date) %>% ###sort by date for calculations
  distinct(date, .keep_all = TRUE) ### remove duplicates

## worldmet ---------------------------------

### Check station locations on map, download & filter met data --> met data only goes to 04/30/26
rathAQ %>%
  slice(1) %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = "Rathmines Station")
import_ghcn_stations(name = "Dublin", return="map")

rathMET <- import_ghcn_hourly(station = "EII0000EIDW", year = 2020:2026) %>%
  filter(
    date >= ymd_hms("2020-06-30 01:00:00", tz = "UTC"), 
    date <= ymd_hms("2026-04-30 23:00:00", tz = "UTC")
  )

## Merge AQ & MET data, cleanup ---------------------------------

rathAQMET <- rathAQ %>%
  left_join(rathMET, by = "date")

summary(rathAQMET)

### check for negative values <-10
rathAQMET %>%
  filter(if_any(c(no2, o3, so2, pm10, pm25), ~ .x < -10)) %>%
  View()


## HYSPLIT back trajectories (2022) ---------------------------------

### 72 hour back trajectories, 500 m, arriving every 3 hours at rathmines
yr       <- 2022
site_lat <- rathAQ$lat[1]
site_lon <- rathAQ$lon[1]

### Folders 
dir.create("hysplit/sqtba/TrajData", recursive = TRUE, showWarnings = FALSE)
dir.create("hysplit/sqtba/TrajTemp", recursive = TRUE, showWarnings = FALSE)
met_dir  <- normalizePath("hysplit/sqtba/TrajData", winslash = "/")
temp_dir <- normalizePath("hysplit/sqtba/TrajTemp", winslash = "/")

### Download monthly reanalysis met files
getMet <- function(year, month, path_met) {
  base_url <- "https://noaa-oar-arl-hysplit-pds.s3.amazonaws.com/reanalysis"
  for (yr_i in year) for (mo in month) {
    filename <- sprintf("RP%d%02d.gbl", yr_i, mo)
    destfile <- file.path(path_met, filename)
    if (file.exists(destfile) && file.size(destfile) > 0) next
    message("Downloading ", filename, " ...")
    try(suppressWarnings(download.file(paste0(base_url, "/", yr_i, "/", filename),
                                       destfile, mode = "wb", quiet = FALSE)), silent = TRUE)
    if (file.exists(destfile) && file.size(destfile) == 0) {
      file.remove(destfile)
      message("  FAILED (file not available): ", filename)
    }
  }
}

getMet(yr - 1, 12,   met_dir)   # previous December, for trajectories arriving in early January
getMet(yr,     1:12, met_dir)

### Check: files ~ 110-125 MB
file.info(list.files(met_dir, pattern = sprintf("^RP(%d12|%d)", yr - 1, yr), full.names = TRUE)) %>%
  tibble::rownames_to_column("file") %>%
  transmute(file = basename(file), size_MB = round(size / 1e6))

### Load run_hysplit() (openair book, Appendix C)
devtools::source_gist("https://gist.github.com/davidcarslaw/c67e33a04ff6e1be0cd7357796e4bdf5",
                      filename = "run_hysplit.R")

### Run the full year (takes ~28 min --> check progress in file explorer), or load the saved file if it exists
traj_file <- sprintf("hysplit/sqtba/TrajData/traj_%d.rds", yr)

if (file.exists(traj_file)) {
  year_traj <- readRDS(traj_file)
} else {
  year_traj <- run_hysplit(
    latitude       = site_lat,
    longitude      = site_lon,
    runtime        = -72,
    start_height   = 500,
    model_height   = 10000,
    start          = yr,
    end            = paste0(yr, "-12-31"),
    hysplit_exec   = "C:/HYSPLIT/exec",
    hysplit_input  = met_dir,
    hysplit_output = temp_dir,
    site           = "rathmines"
  )
  saveRDS(year_traj, traj_file)
}

### Checks: 73 rows per arrival, 2912 arrivals, no gaps
year_traj %>% count(date, name = "rows") %>% count(rows)
year_traj %>% distinct(date) %>% summarise(arrivals = n(), first = min(date), last = max(date))

expected <- seq(as.POSIXct(paste0(yr, "-01-01 00:00"), tz = "UTC"),
                as.POSIXct(paste0(yr, "-12-30 21:00"), tz = "UTC"), by = "3 hours")
sum(!(as.numeric(expected) %in% as.numeric(unique(year_traj$date))))   # should be 0