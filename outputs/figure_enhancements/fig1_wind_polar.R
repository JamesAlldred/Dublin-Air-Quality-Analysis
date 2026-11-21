# Figure 1 (wind rose and polar maps) ------------------------------

library(tidyverse)
library(openair)
library(openairmaps)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(webshot2)
library(png)
library(patchwork)

## Output folder ---------------------------------
fig_dir <- "outputs/figure_enhancements"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

## Data ---------------------------------
rathAQMET <- readRDS("data/rathAQMET.rds") %>%
  select(1:8, wd, ws) %>%
  mutate(
    across(c(pm10, pm25), ~ if_else(.x < -50, NA_real_, .x)),
    so2 = if_else(so2 > 1000, NA_real_, so2)
  ) %>%
  rename(lat = lat.x)

## Map settings ---------------------------------
ref_zoom    <- 8     # zoom at which d.icon = 200 px looked right
map_zoom    <- 9
k           <- 2^(map_zoom - ref_zoom)
shot_scale  <- 2

## Text settings ---------------------------------
title_size  <- 15    # panel titles (pt, bold)
text_size   <- 14    # wind rose text (pt)
legend_zoom <- 1.5   # scales the whole map legend, colour bar and labels together
fig_shrink  <- 0.7   # below 1 makes the text inside the polar plots relatively bigger
ring_angle  <- 45    # direction along which the wind rose ring labels sit

## Helpers ---------------------------------

### Convert to ggplot
as_gg <- function(x) if (inherits(x, "openair")) x$plot else x

### Polar map with ledgend, scale bar
make_full_polar_map <- function(df, pol, legend_title = NULL) {
  df %>%
    polarMap(pollutant = pol, limits = "fixed",
             legend = TRUE, legend.position = "topleft",
             legend.title = legend_title,
             d.icon = 200 * k, d.fig = 3.5 * k * fig_shrink) %>%
    addScaleBar(position = "bottomleft",
                options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE)) %>%
    addControl(
      html = tags$div(
        style = "font-size: 36px; font-weight: bold; line-height: 1; text-align: center; background: rgba(255,255,255,0.85); padding: 4px 10px; border-radius: 4px;",
        HTML("N<br>&#8593;")),
      position = "topright")
}

### Centre and freeze map
map_to_gg <- function(map, title, zoom = map_zoom, width = 700, height = 700) {
  html <- tempfile(fileext = ".html")
  png  <- tempfile(fileext = ".png")
  
  map %>%
    leaflet::setView(lng = -6.2672, lat = 53.322, zoom = zoom) %>%
    htmlwidgets::prependContent(
      tags$style(paste0(
        ".leaflet-control-zoom { display: none; } ",
        ".info.legend { font-size: 16px; zoom: ", legend_zoom, "; } ",
        ".leaflet-control-scale-line { font-size: 26px; line-height: 1.3; }"))
    ) %>%
    htmlwidgets::saveWidget(html, selfcontained = TRUE)
  
  webshot2::webshot(html, png, vwidth = width, vheight = height,
                    zoom = shot_scale, delay = 2)
  
  ggplot() +
    annotation_custom(grid::rasterGrob(png::readPNG(png), interpolate = TRUE),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    labs(title = title) +
    theme_void() +
    theme(aspect.ratio = 1)
}

### Make ggplot text layers bigger
enlarge_text_layers <- function(p, size = 5) {
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomText")) p$layers[[i]]$aes_params$size <- size
  }
  p
}

## Panels (slow --> run once per session) ---------------------------------
p_wind <- as_gg(windRose(rathAQMET, annotate = FALSE, angle.scale = ring_angle, plot = FALSE))
p_wind <- enlarge_text_layers(p_wind, 5) +
  theme(text = element_text(size = text_size)) +
  labs(title = expression(bold("Wind rose: speed and direction")))

p_pm25 <- map_to_gg(make_full_polar_map(rathAQMET, "pm25", "PM2.5 (ug/m3)"),
                    title = expression(bold("PM"[2.5]*" by wind speed and direction")))
p_no2  <- map_to_gg(make_full_polar_map(rathAQMET, "no2", "NO2 (ug/m3)"),
                    title = expression(bold("NO"[2]*" by wind speed and direction")))

## Layout ---------------------------------
wind_polar <- ((p_wind | p_pm25 | p_no2) &
                 theme(plot.title = element_text(hjust = 0.5, size = title_size, face = "bold"))) +
  plot_annotation(
    tag_levels = list(c("1A", "1B", "1C")),
    theme = theme(plot.tag          = element_text(size = 18, face = "bold"),
                  plot.tag.position = "topleft")
  )

print(wind_polar)

## Wind rose stats ---------
round(mean(rathAQMET$ws, na.rm = TRUE), 2)
round(100 * mean(rathAQMET$ws == 0, na.rm = TRUE), 1)

## Save png -----
save_fig <- TRUE
if (save_fig) {
  ggsave(file.path(fig_dir, "fig1_wind_polar.png"), wind_polar,
         width = 15, height = 7.9, dpi = 600)
}