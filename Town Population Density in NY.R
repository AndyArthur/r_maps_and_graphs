library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)
library(ggtext)
library(units)

pop <- get_decennial(geography = "county subdivision", state='ny',variables = "P1_001N",
                     year = 2020, cache=TRUE, geometry = TRUE, cb=TRUE)


pop |>
  filter(value > 0) %>% # neccessary for placeholder beow
  mutate(psqmi = value/(st_area(.) |> set_units('mi^2') |> drop_units()),
         less100 = psqmi < 100
  ) -> pop_dense

pop_dense |>
  ggplot() +
  geom_sf(aes(fill=psqmi)) +
  scale_fill_stepsn(colours = rev(c('red', 'yellow', 'navy')), n.breaks=10, labels=scales::label_comma(), trans='log10') +
  coord_sf(crs=3857, expand=FALSE) +
  theme_void() +
  labs(title = '<b>Town Population Density</b> Persons per Square Mile',
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: 2020 US Census' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.75,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Routed Gothic',size=14, color='gray99'),
    plot.title=element_textbox(family='Routed Gothic', halign = 0.5, hjust=0, size=50, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "gray20", color=NA),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  )

ow <- 1920
oh <- 1400
fn <- 'Town Population Density'
source('create-svg.R', local = TRUE)


pop_dense |>
  ggplot() +
  geom_sf(aes(fill=less100)) +
  scale_fill_manual(values=c(`TRUE`='orange',`FALSE`='blue'), name='', labels=\(x) str_to_title(x)) +
  coord_sf(crs=3857, expand=FALSE) +
  theme_void() +
  labs(title = '<b>Town Population Density',
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: 2020 US Census' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.75,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Routed Gothic',size=14, color='gray99'),
    plot.title=element_textbox(family='Routed Gothic', halign = 0.5, hjust=0, size=48, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "gray20", color=NA),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  )

ow <- 1920
oh <- 1400
fn <- 'Town Population Density Less then 100/per sq mile'
source('create-svg.R', local = TRUE)


