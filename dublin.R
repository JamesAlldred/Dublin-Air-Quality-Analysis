# Setup ---------------------------------

library(tidyverse)
library(leaflet)
library(htmltools)
library(openair)
library(openairmaps)
library(patchwork)
library(webshot2)
library(png)
library(htmlwidgets)

## Load merged AQ & MET data (30th Jun 2020 to 30th Sept 2026) ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds")
rathAQMET <- rathAQMET %>%
  select(1:8, wd, ws) %>%
  mutate(
    across(c(pm10, pm25), ~ if_else(.x < -50, NA_real_, .x))
  ) %>%
  rename(lat = lat.x)


# First look at data
timePlot(rathAQMET, pollutant = c("no2", "o3", "so2", "pm10", "pm25"), group = TRUE, avg.time = "hour", title = "Hourly Air Quality at Rathmines Station")

# Map title box, reused by every map
map_label <- function(map, label, position = "bottomleft") {
  addControl(
    map,
    html = tags$div(
      style = "background: rgba(255, 255, 255, 0.85); padding: 6px 12px; border-radius: 4px; font-family: sans-serif; font-weight: bold; font-size: 16px;",
      label
    ),
    position = position
  )
}

# Wind rose and polar maps: combined figure ------------------------------

## Convert to ggplot to make conversion to single plot easier
as_gg <- function(x) if (inherits(x, "openair")) x$plot else x

## Making the map static and standardising the zoom
map_zoom <- 8
map_to_gg <- function(map, zoom = map_zoom, width = 750, height = 700) {
  html <- tempfile(fileext = ".html")
  png  <- tempfile(fileext = ".png")
  
  map %>%
    leaflet::setView(lng = -6.2672, lat = 53.322, zoom = zoom) %>%
    htmlwidgets::saveWidget(html, selfcontained = TRUE)
  
  webshot2::webshot(html, png, vwidth = width, vheight = height, delay = 2)
  
  patchwork::wrap_elements(full = grid::rasterGrob(png::readPNG(png), interpolate = TRUE))
}

## Polar map function to call later
make_full_polar_map <- function(df, pol, label) {
  df %>%
    polarMap(pollutant = pol, legend = TRUE, legend.position = "topright") %>%
    map_label(label)
}

## Plotting
p_wind <- as_gg(windRose(rathAQMET, main = "Wind Direction in Dublin", plot = FALSE))
p_pm25 <- map_to_gg(make_full_polar_map(rathAQMET, "pm25", "PM2.5"))
p_no2  <- map_to_gg(make_full_polar_map(rathAQMET, "no2",  "Nitrogen Dioxide (NO2)"))

layout <- "
#AA#
BBCC
BBCC
"

wind_polar <- p_wind + p_pm25 + p_no2 +
  plot_layout(design = layout, heights = c(2, 1.5, 1.5)) +
  plot_annotation(tag_levels = "A")

## Save to outputs folder
ggsave("outputs/1 - wind_rose_polar_maps.png", wind_polar,
       width = 10, height = 8.07, dpi = 600)   


# Back trajectories  ------------------------------

## Read a HYSPLIT endpoints file for trajPlot()
read_tdump <- function(file) {
  lines  <- readLines(file)
  n_met  <- as.integer(strsplit(trimws(lines[1]), "\\s+")[[1]][1])          # number of met grids
  n_traj <- as.integer(strsplit(trimws(lines[n_met + 2]), "\\s+")[[1]][1])  # number of trajectories
  first  <- n_met + n_traj + 4                                              # first data row
  
  ## Arrival times
  starts  <- read.table(text = lines[(n_met + 3):(n_met + 2 + n_traj)])
  arrival <- data.frame(
    traj = seq_len(n_traj),
    date = as.POSIXct(sprintf("%04d-%02d-%02d %02d:00",
                              2000 + starts$V1, starts$V2, starts$V3, starts$V4),
                      format = "%Y-%m-%d %H:%M", tz = "UTC")
  )
  
  ## Points along each path
  df <- read.table(text = lines[first:length(lines)])
  names(df)[1:12] <- c("traj", "grid", "yr", "mo", "dy", "hr", "min",
                       "fcst", "age", "lat", "lon", "height")
  
  df %>%
    mutate(date2 = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d",
                                      2000 + yr, mo, dy, hr, min),
                              format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
    left_join(arrival, by = "traj") %>%
    mutate(hour.inc = as.numeric(difftime(date2, date, units = "hours"))) %>%
    select(date, date2, hour.inc, lat, lon, height)
}

## timeSeries() functon to call later
make_timeseries <- function(start, end, title, pm_col) {
  rathAQMET %>%
    filter(date >= as.POSIXct(start, tz = "UTC"),
           date <= as.POSIXct(end,   tz = "UTC")) %>%
    timePlot(pollutant = c("pm25", "no2"),
             avg.time  = "hour",
             ylim      = c(0, 55),
             cols      = c(pm_col, "grey40"),
             ylab      = "Concentration (ug/m3)",
             plot = FALSE) %>%
    as_gg() +
    scale_y_continuous(breaks = c(0, 25, 50), limits = c(0, 55)) +
    scale_x_datetime(
      breaks      = seq(as.POSIXct(start, tz = "UTC"),
                        as.POSIXct(end,   tz = "UTC"), by = "9 hours"),
      date_labels = "%H:%M"
    ) +
    labs(title = title, x = "Time (UTC)") +
    theme(plot.title = element_text(size = 10))
}

## Maps coloured by arrival time PM2.5
traj <- bind_rows(
  read_tdump("hysplit/clean_tdump.txt"),
  read_tdump("hysplit/polluted_tdump.txt")
) %>%
  left_join(select(rathAQMET, date, pm25), by = "date")

p_map <- as_gg(trajPlot(traj,
                        pollutant = "pm25",
                        cols      = c("royalblue", "firebrick"),
                        linewidth = 1,
                        plot = FALSE))

## Plotting timesSeries()
p_clean <- make_timeseries("2023-03-21 03:00", "2023-03-22 00:00",
                           "Atlantic air, 21 to 22 Mar 2023", "royalblue")
p_dirty <- make_timeseries("2024-03-09 03:00", "2024-03-10 00:00",
                           "European air, 9 to 10 Mar 2024", "firebrick")

## Combine and save
combined <- p_map / (p_clean | p_dirty) +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title      = "Air mass origin and PM2.5 at Rathmines, Dublin",
    tag_levels = "A"
  )

dir.create("dublin/outputs", showWarnings = FALSE)
ggsave("outputs/2 - combined_traj.png", combined, width = 10, height = 9, dpi = 600)


# PM2.5 vs NO2 scatter ------------------------------

min_ws <- 0   # Change later if needed

air_levels <- c("Atlantic (wind from 180 to 270 degrees)",
                "European (wind from 45 to 135 degrees)")

scatter_all <- rathAQMET %>%
  filter(ws >= min_ws) %>%
  mutate(air = case_when(
    wd >= 180 & wd <= 270 ~ air_levels[1],
    wd >= 45  & wd <= 135 ~ air_levels[2]
  )) %>%
  select(air, pm25, no2) %>%
  drop_na() %>%                       # Also drops hours outside both sectors
  mutate(air = factor(air, levels = air_levels))

## R2, slope, intercept and n per sector
fit_labels <- scatter_all %>%
  group_by(air) %>%
  summarise(
    r2        = summary(lm(pm25 ~ no2))$r.squared,
    slope     = coef(lm(pm25 ~ no2))[2],
    intercept = coef(lm(pm25 ~ no2))[1],
    n_obs     = n(),
    label     = sprintf("R\u00b2 = %.2f\nn = %d", r2, n_obs)
  )
fit_labels

## Plotting
p_scatter_all <- ggplot(scatter_all, aes(x = no2, y = pm25, colour = air)) +
  geom_point(alpha = 0.25, size = 1, show.legend = FALSE) +
  geom_text(data = fit_labels, aes(x = -Inf, y = Inf, label = label),
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, size = 3.5) +
  facet_wrap(~ air) +
  scale_colour_manual(values = setNames(c("royalblue", "firebrick"), air_levels)) +
  labs(x = "NO2 (ug/m3)", y = "PM2.5 (ug/m3)") +
  theme_bw()

ggsave("outputs/3 - pm25_no2_scatter.png", p_scatter_all,
       width = 10, height = 5, dpi = 600)