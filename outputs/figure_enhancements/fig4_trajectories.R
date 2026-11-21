# Figure 4 (back trajectories and PM2.5 / NO2 time series): poster version ------------------------------


library(tidyverse)
library(openair)
library(patchwork)

## Settings ---------------------------------
fig_dir     <- "outputs/figure_enhancements"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

title_size  <- 15    # panel titles (pt, bold)
text_size   <- 14    # axis, strip and legend text (pt)
line_width  <- 1.1     # time series line width (passed to timePlot as lwd)
fig_w       <- 15.2    # saved width (in). Poster box is ~1.59 : 1 (w : h)
fig_h       <- 9.55    # saved height (in) = fig_w / 1.59

## Data ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds") %>%
  select(1:8, wd, ws) %>%
  mutate(
    across(c(pm10, pm25), ~ if_else(.x < -50, NA_real_, .x)),
    so2 = if_else(so2 > 1000, NA_real_, so2)
  ) %>%
  rename(lat = lat.x)

## Helpers ---------------------------------

### Convert to ggplot
as_gg <- function(x) if (inherits(x, "openair")) x$plot else x

### Read a HYSPLIT endpoints file for trajPlot()
read_tdump <- function(file) {
  lines  <- readLines(file)
  n_met  <- as.integer(strsplit(trimws(lines[1]), "\\s+")[[1]][1])
  n_traj <- as.integer(strsplit(trimws(lines[n_met + 2]), "\\s+")[[1]][1])
  first  <- n_met + n_traj + 4
  
  starts  <- read.table(text = lines[(n_met + 3):(n_met + 2 + n_traj)])
  arrival <- data.frame(
    traj = seq_len(n_traj),
    date = as.POSIXct(sprintf("%04d-%02d-%02d %02d:00",
                              2000 + starts$V1, starts$V2, starts$V3, starts$V4),
                      format = "%Y-%m-%d %H:%M", tz = "UTC")
  )
  
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

### Period means used to label panels B and C
period_means <- function(start, end) {
  rathAQMET %>%
    filter(date >= as.POSIXct(start, tz = "UTC"),
           date <= as.POSIXct(end, tz = "UTC")) %>%
    summarise(pm25 = mean(pm25, na.rm = TRUE),
              no2 = mean(no2, na.rm = TRUE))
}

### PM2.5 and NO2 time series for one window (UTC), with period-average guides
make_timeseries <- function(start, end, title, pm_col, averages,
                            show_ylab = TRUE) {
  start_t <- as.POSIXct(start, tz = "UTC")
  end_t   <- as.POSIXct(end,   tz = "UTC")
  
  p <- rathAQMET %>%
    filter(date >= start_t, date <= end_t) %>%
    timePlot(pollutant = c("pm25", "no2"),
             avg.time  = "hour",
             ylim      = c(0, 55),
             cols      = c(pm_col, "grey40"),
             lwd       = line_width,
             ylab      = "Concentration (µg/m3)",
             plot      = FALSE) %>%
    as_gg()
  
  average_lines <- tibble(variable = c("pm25", "no2"),
                          average = c(averages$pm25, averages$no2))
  
  p <- p +
    geom_hline(data = average_lines, aes(yintercept = average),
               inherit.aes = FALSE, linetype = "dashed", colour = "black",
               linewidth = 0.5) +
    scale_y_continuous(breaks = c(0, 25, 50), limits = c(0, 55)) +
    scale_x_datetime(
      breaks      = seq(start_t + 3 * 3600, end_t, by = "6 hours"),
      date_labels = "%d %b\n%H:%M"
    ) +
    labs(title = title, x = "Time (UTC)") +
    theme(legend.position = "none",
          text = element_text(size = text_size))
  
  if (!show_ylab) p <- p + labs(y = NULL)
  p
}

## Panels ---------------------------------
dublin_marker <- list(
  geom_point(data = tibble(lon = -6.2672, lat = 53.322, site = "Rathmines Station, Dublin"),
             aes(x = lon, y = lat, shape = site),
             inherit.aes = FALSE, size = 3.5, colour = "black"),
  scale_shape_manual(name = NULL, values = 16),
  guides(shape = guide_legend(order = 2,
                              theme = theme(legend.text = element_text(size = text_size - 3),
                                            legend.key  = element_blank())))
)

traj <- bind_rows(
  read_tdump("hysplit/clean_tdump.txt"),
  read_tdump("hysplit/polluted_tdump.txt")
) %>%
  left_join(select(rathAQMET, date, pm25), by = "date")

p_map <- as_gg(trajPlot(traj,
                        pollutant = "pm25",
                        cols      = c("royalblue", "firebrick"),
                        linewidth = 1,
                        plot      = FALSE)) +
  labs(title    = expression(bold("72 hour back trajectories coloured by PM"[2.5]*" at arrival")),
       subtitle = paste0("HYSPLIT, arrival height 500 m, one trajectory every 3 hours\n",
                         "Blue: Atlantic air arriving 21 to 22 Mar 2023. Red: European air arriving 9 to 10 Mar 2024"),
       colour   = expression(PM[2.5]~"("*mu*"g m"^-3*")")) +
  dublin_marker +
  theme(text          = element_text(size = text_size),
        plot.subtitle = element_text(hjust = 0.5, size = text_size - 2),
        axis.text     = element_text(colour = "grey20", size = text_size),
        panel.grid    = element_line(colour = "grey85", linewidth = 0.2))

period_b <- period_means("2023-03-21 03:00", "2023-03-22 00:00")
period_c <- period_means("2024-03-09 03:00", "2024-03-10 00:00")
relative_change <- 100 * (period_c - period_b) / period_b

p_clean <- make_timeseries("2023-03-21 03:00", "2023-03-22 00:00",
                           "Atlantic air, 21 to 22 Mar 2023", "royalblue",
                           period_b)
p_dirty <- make_timeseries("2024-03-09 03:00", "2024-03-10 00:00",
                           "European air, 9 to 10 Mar 2024", "firebrick",
                           period_c, show_ylab = FALSE)

### Labels
p_map   <- p_map   + labs(tag = "4A")
p_clean <- p_clean + labs(tag = "4B")
p_dirty <- p_dirty + labs(tag = "4C")

## Key and period-means table for panels B and C ---------------------------------

key_size   <- (text_size - 2) / ggplot2::.pt
table_size <- (text_size - 2) / ggplot2::.pt

key_bc <- ggplot() +
  annotate("text", x = 0.04, y = 0.92, label = "Key for panels 4B and 4C",
           hjust = 0, fontface = "bold", size = key_size) +
  annotate("segment", x = c(0.04, 0.12, 0.20), xend = c(0.12, 0.20, 0.28), y = 0.78, yend = 0.78,
           colour = c("royalblue", "firebrick", "grey40"),
           linewidth = line_width + 0.3, lineend = "butt") +
  annotate("text", x = 0.32, y = 0.78, label = "Hourly concentration",
           hjust = 0, size = key_size) +
  annotate("segment", x = 0.04, xend = 0.28, y = 0.65, yend = 0.65,
           linetype = "dashed", colour = "black", linewidth = 0.6) +
  annotate("text", x = 0.32, y = 0.65, label = "Mean concentration",
           hjust = 0, size = key_size) +
  annotate("segment", x = 0.04, xend = 0.96, y = 0.54, yend = 0.54,
           colour = "grey70", linewidth = 0.4) +
  annotate("text", x = 0.04, y = 0.44,
           label = "Mean (µg/m³)", hjust = 0,
           fontface = "bold", size = table_size) +
  annotate("text", x = c(0.52, 0.68, 0.87), y = 0.44,
           label = c("B", "C", "Change vs B"), hjust = 0.5,
           fontface = "bold", size = table_size) +
  annotate("text", x = 0.04, y = c(0.30, 0.16),
           label = c("PM₂.₅", "NO₂"), hjust = 0,
           size = table_size) +
  annotate("text", x = c(0.52, 0.68, 0.87), y = 0.30,
           label = c(sprintf("%.1f", period_b$pm25), sprintf("%.1f", period_c$pm25),
                     sprintf("%+.1f%%", relative_change$pm25)), hjust = 0.5,
           size = table_size) +
  annotate("text", x = c(0.52, 0.68, 0.87), y = 0.16,
           label = c(sprintf("%.1f", period_b$no2), sprintf("%.1f", period_c$no2),
                     sprintf("%+.1f%%", relative_change$no2)), hjust = 0.5,
           size = table_size) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "grey70", linewidth = 0.4),
        plot.margin = margin(4, 4, 4, 4))

## Layout ---------------------------------

top_row    <- (p_map | key_bc) + plot_layout(widths = c(2.4, 1))
bottom_row <- p_clean | p_dirty

combined <- ((top_row / bottom_row) +
               plot_layout(heights = c(1.1, 1))) &
  theme(plot.title        = element_text(hjust = 0.5, size = title_size, face = "bold"),
        plot.tag          = element_text(size = 18, face = "bold"),
        plot.tag.position = "topleft")

print(combined)

## Save as png ------
save_fig <- TRUE
if (save_fig) {
  ggsave(file.path(fig_dir, "fig4_rearranged.png"), combined,
         width = fig_w, height = fig_h, dpi = 600)
}