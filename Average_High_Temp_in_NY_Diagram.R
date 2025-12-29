library(tidyverse)
library(sf)
library(terra)
library(tigris)
library(stars)
library(ggtext)

nys <- counties('ny', cb = TRUE) |> st_transform(4269) |> group_by() |> summarise()

map(seq(1, 12) %>% str_pad('0', side = 'left', width = 2),
    \(x)
    rast(
      str_c(
        '/vsizip//vsicurl/https://ftp.prism.oregonstate.edu/normals/us/4km/tmax/monthly/prism_tmax_us_25m_2020',
        x,
        '_avg_30y.zip/prism_tmax_us_25m_2020',
        x,
        '_avg_30y.tif'
      )
    ) |>
      crop(nys)) -> year_high

for (i in seq(1,12)) {
  names(year_high[[i]]) <- month.name[i]
}

year_high <- map(year_high, \(x) (x*9/5)+32)

map(year_high,
    \(x) x |>
      st_as_stars() |>
      st_contour(contour_lines = FALSE, breaks = seq(-30,120,5)) |>
      st_intersection(nys) |>
      st_make_valid() |>
      rmapshaper::ms_simplify(keep = 0.08) |>
      mutate(month = factor(names(x), levels = month.name)) %>%
      select(month, Max)
    ) |>
  bind_rows() -> contours

contours |>
  ggplot() +
  geom_sf(aes(fill = Max), linewidth=0) +
  geom_sf(data=nys, linewidth=0.1, color='gray20') +
  scale_fill_gradientn(name = "", colors = c("blue", "cyan", "green", "yellow", "orange"), space = "Lab",
                       na.value = "grey50", guide = "colourbar", aesthetics = "fill", limits = c(20,90),  breaks=seq(-30,115,5)) +
  facet_wrap( ~ month) +
  coord_sf( crs=3857, expand=FALSE) +
  theme_void() +
  labs(title = str_c('Average High Temperature, 1991-2020</span>'),
       tag=str_c('Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: Prism Group, Oregon State. prism.oregonstate.edu/mtd/\n',
                 'Copyright (c) PRISM Climate Group, Oregon State University'),
       fill = "")  +
  theme(
    text= element_text(family='Routed Gothic',size=14),
    strip.text = element_text(size=16, margin = margin(t=15, b=15)),
    plot.title=element_textbox(family='Routed Gothic Wide', halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(20,0,5,0),'pt')),
    plot.background = element_rect(fill = '#f0eee9', color=NA),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = 'bottom',
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(4.2,'cm'),
    legend.position = 'top'
  ) +
  guides(fill = guide_colorsteps(ticks=FALSE))


ow <- 1920
oh <- 1600
fn <- 'Average High Temperature, 1991-2020'
source('create-svg.R', local=TRUE)
