library(tidyverse)
library(sf)

read_sf('/vsizip/vsicurl/https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/610temp_latest.zip') |>
  st_transform(5070) |>
  rmapshaper::ms_simplify() |>
  mutate(outlook = str_c('6-10 Day — ', format(Start_Date, '%a %-m/%-d'), ' to ',  format(End_Date, '%a %-m/%-d') )) ->
  outlook

read_sf('/vsizip/vsicurl/https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/814temp_latest.zip') |>
  st_transform(5070) |>
  rmapshaper::ms_simplify() |>
  mutate(outlook = str_c('8-14 Day — ', format(Start_Date, '%a %-m/%-d'), ' to ',  format(End_Date, '%a %-m/%-d') )) |>
  bind_rows(outlook) -> outlook

read_sf('/vsizip/vsicurl/https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/wk34temp_latest.zip') |>
  st_transform(5070) |>
  rmapshaper::ms_simplify() |>
  mutate(outlook = str_c('Week 3 to 4 — ', format(Start_Date, '%a %-m/%-d'), ' to ',  format(End_Date, '%a %-m/%-d') )) |>
  bind_rows(outlook) -> outlook

read_sf('/vsizip/vsicurl/https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/seastemp_202601.zip/lead1_FMA_temp.shp') |>
  st_transform(5070) |>
  rmapshaper::ms_simplify() |>
  mutate(outlook = Valid_Seas) |>
  bind_rows(outlook) -> outlook

usst <- states(cb=TRUE) |>
  filter(GEOID < 57) |>
  st_transform(5070) |>
  rmapshaper::ms_simplify()

cpc_palette <- c(
  # Below Normal (Blues)
  "Below 33" = "#C6DBEF", "Below 40" = "#9ECAE1", "Below 50" = "#6BAED6",
  "Below 60" = "#4292C6", "Below 70" = "#2171B5", "Below 80" = "#08519C", "Below 90" = "#08306B",

  # Above Normal (Oranges/Reds)
  "Above 33" = "#FEE6CE", "Above 40" = "#FDD0A2", "Above 50" = "#FDAE6B",
  "Above 60" = "#FD8D3C", "Above 70" = "#E6550D", "Above 80" = "#A63603", "Above 90" = "#7F2704",

  # Near Normal
  "Normal 36" = "#D9D9D9", "EC 33" = "White"
)

cpc_order <- c(
  "Below 90", "Below 80", "Below 70", "Below 60", "Below 50", "Below 40", "Below 33",
  "Normal 36", "EC 33",
  "Above 33", "Above 40", "Above 50", "Above 60", "Above 70", "Above 80", "Above 90"
)

outlook_order <- rev(unique(outlook$outlook))

outlook |>
  st_make_valid() |>
  st_intersection(usst) |>
  rmapshaper::ms_simplify() |>
  tigris::shift_geometry() |>
  mutate(FillKey = paste(Cat, Prob),
         FillKey = factor(FillKey, levels = cpc_order),
         outlook = factor(outlook, levels= outlook_order )
         ) |>
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = FillKey), color = "white", size = 0.1) +
  scale_fill_manual(values = cpc_palette, labels = \(x) str_c(x,'%')) +
  facet_wrap(~outlook, nrow=2) +
  theme_void() +
  coord_sf(expand=FALSE) +
  labs(title = 'NOAA Temperature Outlooks',
  tag=str_c('Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: National Weather Service Climate Predicton Center'),
  fill = "")  +
  theme(
    text= element_text(family='Routed Gothic',size=14),
    strip.text = element_text(size=13, margin = margin(t=15, b=15)),
    plot.title=element_textbox(family='Routed Gothic Wide', halign = 0.5, hjust=0.5, face='bold',size=30, margin=unit(c(10,0,15,0),'pt')),
    plot.background = element_rect(fill = '#f0eee9', color=NA),
    plot.tag=element_text(size=10,hjust=0, color='#555555',
                          margin=unit(c(7,0,0,0),'pt')),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = 'bottom',
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = 'top',
  ) +
  guides(fill = guide_legend(nrow=2))


ow <- 1920
oh <- 1550
fn <- 'NOAA Temperature Outlook'
dpi <- 150
source('create-svg.R', local=TRUE)
source('upload-svg.R')
