library(tidyverse)
library(terra)
library(tigris)
library(stars)
library(ggtext)
library(sf)

nys <- states(cb = TRUE) |>
  filter(NAME == 'New York') |>
  st_transform(26918)

rast(nys,
     res = 1000,
     crs = crs(nys),
     vals = 0) |>
  crop(nys, mask = TRUE) |>
  project('EPSG: 4326') |>
  as.data.frame(xy = TRUE) |>
  as_tibble() -> nys_pts

map(seq(now(), now()+days(27), '1 day') %>% as.Date, \(doy) {
  nys_pts |>  mutate(
    sunset = suncalc::getSunlightTimes(data = tibble(
      date = doy,
      lat = y,
      lon = x,
      tz = 'America/New_York'
    ))$sunset %>% with_tz('America/New_York'),
    doh = hour(sunset) + (minute(sunset) / 60)
  ) |>
    transmute(x, y, z = doh) |>
    rast(type = 'xyz', crs = 'EPSG: 4326') |>
    st_as_stars() |>
    st_contour(breaks = seq(0, 24, 5/60)) |>
    st_transform(26918) |>
    st_make_valid() |>
    st_intersection(nys) |>
    rmapshaper::ms_simplify() |>
    transmute(Time = ifelse(Min == -Inf, Max, Min),
              Month = factor(format(doy, '%a %b %-d')))

}) |>  bind_rows() -> month_contours


month_contours |>
  mutate(
    time_fmt = as_datetime(str_c(
      'January 1, 2026 ', floor(Time), ':', round((Time - floor(Time)) * 60)
    ), format = '%B %d, %Y %H:%M') %>%
      format('%-I:%M %p') |> factor(
        seq(
          as_datetime('2026-01-01'),
          as_datetime('2026-01-01') + 86399,
          '5 min'
        ) |> format('%-I:%M %p')
      )
  ) %>%
  ggplot() +
  geom_sf(aes(fill = time_fmt), linewidth = 0) +
  facet_wrap( ~ Month, ncol=7) +
  scale_fill_discrete(palette = colorRampPalette(
    c('navy', 'blue', 'red', 'orange', 'yellow', 'lightyellow')
  )) +
  coord_sf(crs = 3857, expand = FALSE) +
  theme_void() +
  labs(
    title = str_c('Sunset Time</span>'),
    tag = str_c(
      'Andy Arthur, ',
      format(Sys.Date(), format = "%-m/%-d/%y"),
      '\nSource: rSunCalc'
    ),
    fill = ""
  )  +
  theme(
    text = element_text(family = 'Routed Gothic', size = 14),
    strip.text = element_text(size = 16, margin = margin(t = 15, b = 15)),
    plot.title = element_textbox(
      family = 'Routed Gothic Wide',
      halign = 0.5,
      hjust = 0.5,
      face = 'bold',
      size = 30,
      margin = unit(c(0, 0, 5, 0), 'pt')
    ),
    plot.background = element_rect(fill = '#f0eee9', color = NA),
    plot.tag = element_text(
      size = 10,
      hjust = 0,
      color = '#555555'
    ),
    plot.margin = unit(c(1, 1, 1, 1), 'lines'),
    plot.tag.position = 'bottom',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.position = 'top',
  ) +
  guides(fill = guide_legend(nrow = 1))

ow <- 1920
oh <- 1300
fn <- 'Sunset Time Over Next 30 Days'
source('create-svg.R', local = TRUE)
