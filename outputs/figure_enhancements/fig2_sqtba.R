# Figure 2 (SQTBA source map for PM2.5, 2022) ------------------------------

library(tidyverse)
library(openair)

## Settings ---------------------------------
fig_dir    <- "outputs/figure_enhancements"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

title_size <- 18
text_size  <- 17
x_lim      <- c(-62, 25)   # longitude range shown
y_lim      <- c(33, 72)    # latitude range shown
grid_deg   <- 0.5          # SQTBA grid size, as in SQTBA.R
note_event <- TRUE         # label the Norway hot spot as a single event

## Data ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds") %>%
  select(date, pm25) %>%
  mutate(pm25 = if_else(pm25 < -50, NA_real_, pm25))

### 2022 back trajectories (72 hours, 500 m, local HYSPLIT run), joined to PM2.5 at arrival time
sqtba_data <- readRDS("hysplit/sqtba/TrajData/traj_2022.rds") %>%
  left_join(rathAQMET, by = "date") %>%
  drop_na(pm25)

## Helpers ---------------------------------
as_gg <- function(x) if (inherits(x, "openair")) x$plot else x

### Rathmines marker with key
dublin_marker <- list(
  geom_point(data = tibble(lon = -6.2672, lat = 53.322, site = "Rathmines Station, Dublin"),
             aes(x = lon, y = lat, shape = site),
             inherit.aes = FALSE, size = 3.5, colour = "black"),
  scale_shape_manual(name = NULL, values = 16),
  guides(shape = guide_legend(order = 2,
                              theme = theme(legend.text = element_text(size = text_size - 3),
                                            legend.key  = element_blank())))
)

## Map ---------------------------------
p_sqtba <- as_gg(trajLevel(sqtba_data, pollutant = "pm25", statistic = "sqtba",
                           lat.inc = grid_deg, lon.inc = grid_deg,
                           map.fill = TRUE, map.cols = "grey40",
                           map.alpha = 0.4, plot = FALSE))

p_sqtba <- p_sqtba +
  coord_sf(xlim = x_lim, ylim = y_lim, expand = FALSE) +
  dublin_marker +
  labs(title    = expression(bold("SQTBA source regions for PM"[2.5]*" at Rathmines, Dublin, 2022")),
       subtitle = "72 hour HYSPLIT back trajectories, arrival height 500 m, one trajectory every 3 hours",
       fill     = expression(atop("SQTBA", PM[2.5]~"("*mu*"g m"^-3*")"))) +
  theme(text            = element_text(size = text_size),
        plot.title      = element_text(face = "bold", hjust = 0.5, size = title_size),
        plot.subtitle   = element_text(hjust = 0.5, size = text_size - 1),
        axis.text       = element_text(colour = "grey20", size = text_size),
        legend.title    = element_text(size = text_size),
        legend.text     = element_text(size = text_size),
        panel.grid      = element_line(colour = "grey85", linewidth = 0.2),
        plot.background = element_rect(fill = "white", colour = NA))

if (note_event) {
  p_sqtba <- p_sqtba +
    annotate("segment", x = 8.4, y = 67.4, xend = 12, yend = 66.2,
             arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.9) +
    annotate("label", x = 2, y = 68.3, label = "Single event\n10 to 11 Dec 2022",
             size = 5, fontface = "bold", lineheight = 0.95,
             fill = alpha("white", 0.9), linewidth = 0)
}

print(p_sqtba)


## Save png ------
save_fig <- TRUE
if (save_fig) {
  ggsave(file.path(fig_dir, "fig2_sqtba.png"), p_sqtba,
         width = 13, height = 7.5, dpi = 600, bg = "white",
         device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")
}
