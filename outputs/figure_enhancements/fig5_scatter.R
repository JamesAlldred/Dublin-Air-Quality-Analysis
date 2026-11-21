# Figure 5 (PM2.5 against NO2 by wind sector): poster version ------------------------------

library(tidyverse)

## Settings ---------------------------------
fig_dir     <- "outputs/figure_enhancements"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

title_size  <- 15     # figure title (pt, bold)
text_size   <- 14     # axis and strip text (pt)
stat_size   <- 5      # R2 and n text (mm, about 14 pt)
pt_alpha    <- 0.12   # lower = see density inside the dense core
pt_size     <- 0.9
view_max    <- 100    # axes show 0 to this value; points beyond it stay in the R2, just off screen
min_ws      <- 0      # raise (for example to 3) to drop calm hours
show_fit    <- TRUE  # TRUE adds a linear fit line (it is fitted to ALL the data, outliers included)
fit_lwd     <- 1                  # trend line width (the halo underneath is 1.5 wider)
fit_q       <- c(0.005, 0.995)    # the line spans this share of the NO2 values in each sector

## Data ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds") %>%
  select(1:8, wd, ws) %>%
  mutate(
    across(c(pm10, pm25), ~ if_else(.x < -50, NA_real_, .x)),
    so2 = if_else(so2 > 1000, NA_real_, so2)
  ) %>%
  rename(lat = lat.x)

## Wind sectors ---------------------------------
air_levels <- c("Atlantic (wind from 180\u00b0 to 270\u00b0)",
                "European (wind from 45\u00b0 to 135\u00b0)")

scatter_all <- rathAQMET %>%
  filter(ws >= min_ws) %>%
  mutate(air = case_when(
    wd >= 180 & wd <= 270 ~ air_levels[1],
    wd >= 45  & wd <= 135 ~ air_levels[2]
  )) %>%
  select(air, pm25, no2) %>%
  drop_na() %>%                       # also drops hours outside both sectors
  mutate(air = factor(air, levels = air_levels))

## Linear fit per sector ----- 
fits <- scatter_all %>%
  group_by(air) %>%
  summarise(
    r         = cor(no2, pm25),
    slope     = r * sd(pm25) / sd(no2),
    intercept = mean(pm25) - slope * mean(no2),
    r2        = r^2,
    n_obs     = n(),
    x_start   = quantile(no2, fit_q[1]),
    x_end     = quantile(no2, fit_q[2]),
    .groups   = "drop"
  ) %>%
  mutate(
    y_start = intercept + slope * x_start,
    y_end   = intercept + slope * x_end,
    label   = sprintf("y = %.2fx %s %.1f\nR\u00b2 = %.2f\nn = %s",
                      slope, if_else(intercept < 0, "-", "+"), abs(intercept),
                      r2, format(n_obs, big.mark = ","))
  )
fits

## Handle off-screen points for readability -----
scatter_all %>%
  mutate(in_view = pm25 <= view_max & no2 <= view_max) %>%
  group_by(air) %>%
  summarise(
    slope_all  = cor(no2, pm25) * sd(pm25) / sd(no2),
    r2_all     = cor(no2, pm25)^2,
    slope_view = cor(no2[in_view], pm25[in_view]) * sd(pm25[in_view]) / sd(no2[in_view]),
    r2_view    = cor(no2[in_view], pm25[in_view])^2,
    .groups    = "drop"
  )

scatter_all %>%
  group_by(air) %>%
  summarise(n_off_screen = sum(pm25 > view_max | no2 > view_max), .groups = "drop")

## Plot ---------------------------------

fit_layers <- if (show_fit) list(
  geom_segment(data = fits, aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
               inherit.aes = FALSE, colour = alpha("white", 0.7),
               linewidth = fit_lwd + 1.5, lineend = "round"),
  geom_segment(data = fits, aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
               inherit.aes = FALSE, colour = "grey15",
               linetype = "dashed", linewidth = fit_lwd)
) else NULL

p_scatter <- ggplot(scatter_all, aes(x = no2, y = pm25, colour = air)) +
  geom_point(alpha = pt_alpha, size = pt_size, show.legend = FALSE) +
  fit_layers +
  geom_text(data = fits, aes(x = -Inf, y = Inf, label = label),
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.3, size = stat_size, lineheight = 1) +
  facet_wrap(~ air) +
  scale_colour_manual(values = setNames(c("royalblue", "firebrick"), air_levels)) +
  coord_cartesian(xlim = c(-5, view_max), ylim = c(-5, view_max)) +   # zooms the view only
  labs(title = expression(bold("PM"[2.5]*" against NO"[2]*" by wind sector at Rathmines, Dublin")),
       x = expression(NO[2]~"("*mu*"g m"^-3*")"),
       y = expression(PM[2.5]~"("*mu*"g m"^-3*")")) +
  theme_bw(base_size = text_size) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = title_size),
        strip.text = element_text(face = "bold", size = text_size + 1))

print(p_scatter)

## Save as png ---------------------------------
save_fig <- TRUE
if (save_fig) {
  ggsave(file.path(fig_dir, "fig5_scatter.png"), p_scatter,
         width = 10, height = 5.5, dpi = 600)
}
