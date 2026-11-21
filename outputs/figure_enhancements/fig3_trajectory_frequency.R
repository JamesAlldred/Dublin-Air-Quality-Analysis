# Figure 3 (back trajectory frequency, 2022): basic version ------------------------------


library(tidyverse)
library(openair)

text_size <- 17
x_lim <- c(-62, 25)   # same as Figure 2
y_lim <- c(33, 72)

fig_dir <- "outputs/figure_enhancements"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

## Data ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds") %>%
  select(date, pm25) %>%
  mutate(pm25 = if_else(pm25 < -50, NA_real_, pm25))

sqtba_data <- readRDS("hysplit/sqtba/TrajData/traj_2022.rds") %>%
  left_join(rathAQMET, by = "date") %>%
  drop_na(pm25)


## Rathmines marker with a key ----
dublin_marker <- list(
  geom_point(data = tibble(lon = -6.2672, lat = 53.322,
                           site = "Rathmines Station, Dublin"),
             aes(x = lon, y = lat, shape = site),
             inherit.aes = FALSE, size = 3.5, colour = "black"),
  scale_shape_manual(name = NULL, values = 16),
  guides(shape = guide_legend(order = 1,
                              theme = theme(legend.text = element_text(size = text_size - 3),
                                            legend.key = element_blank())),
         fill = guide_legend(order = 2))
)

## Helpers ----
as_gg <- function(x) if (inherits(x, "openair")) x$plot else x

### Number of trajectories behind the map (arrivals with PM2.5 data)
n_traj_used <- n_distinct(sqtba_data$date)
n_traj_used

## Map ----

### Log spaced colour steps 
freq_palette <- "YlGnBu"   
freq_breaks  <- c(0, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100)
freq_cols    <- hcl.colors(length(freq_breaks), freq_palette, rev = TRUE)[-1]

p_freq <- as_gg(trajLevel(sqtba_data, statistic = "frequency",
                          lat.inc = 0.5, lon.inc = 0.5,
                          breaks = freq_breaks, cols = freq_cols,
                          map.fill = TRUE, map.cols = "grey40",
                          map.alpha = 0.4, plot = FALSE)) +
  coord_sf(xlim = x_lim, ylim = y_lim, expand = FALSE) +
  dublin_marker +
  labs(title    = "Back trajectory frequency at Rathmines, Dublin, 2022",
       subtitle = "72 hour HYSPLIT back trajectories, arrival height 500 m, one trajectory every 3 hours",
       caption  = stringr::str_wrap(
         paste0("Each cell shows how many hourly points of the ", format(n_traj_used, big.mark = ","),
                " trajectories (2022, hours with PM2.5 data) fall in it, as a percentage of the busiest cell. ",
                "Colour steps are log spaced. Cells outside the map area are not shown."),
         width = 120),
       fill     = "Trajectory points per cell\n(% of busiest cell)") +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle   = element_text(hjust = 0.5, size = 16),
        plot.caption    = element_text(hjust = 0.5, size = text_size - 3),
        axis.text       = element_text(colour = "grey20"),
        legend.box      = "vertical",
        legend.box.just = "left",
        panel.grid      = element_line(colour = "grey85", linewidth = 0.2),
        plot.background = element_rect(fill = "white", colour = NA)
  )

class(p_freq)   # should include "ggplot"
print(p_freq)

## Save png ----
save_fig <- TRUE
if (save_fig) {
  ggsave(file.path(fig_dir, "fig3_trajectory_frequency.png"), p_freq,
         width = 13, height = 7.5, dpi = 600, bg = "white",
         device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png")
}
