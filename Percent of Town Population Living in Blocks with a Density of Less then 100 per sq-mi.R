library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)
library(ggtext)
library(units)

pop <- get_decennial(geography = "block", state='ny',variables = "P1_001N",
                     year = 2020, cache=TRUE, geometry = TRUE) %>% st_transform(26918)

nycos <- county_subdivisions('ny', cb=TRUE) |> st_transform(26918)

pop |>
  filter(value > 0) %>% # neccessary for placeholder beow
  mutate(psqmi = value/(st_area(.) |> set_units('mi^2') |> drop_units()),
         less100 = psqmi < 100
         ) |>
 st_centroid() -> pop_dense

pop_dense |>
  st_intersection(nycos) |>
  st_drop_geometry() |>
  group_by(GEOID.1, NAMELSAD) |>
  summarise(percent = sum(less100)/n()) %>%
  left_join(nycos, join_by(GEOID.1 == GEOID)) %>% st_drop_geometry() %>% View
  st_set_geometry('geometry') %>%
  ggplot() +
  geom_sf(aes(fill=percent), linewidth=0.1) +
  scale_fill_stepsn(colours = c('red', 'yellow', 'navy'), n.breaks=10, labels=scales::label_percent()) +
  coord_sf(crs=3857, expand=FALSE) +
  theme_void() +
  labs(title = 'Percent of Town Population Living in Blocks with a <b>Density of Less then 100 per sq/mi</b>',
       tag=str_c(
         'Andy Arthur, ', format(Sys.Date(), format="%-m/%-d/%y"),
         '\nData Source: 2020 US Census' ),
       fill = "") +
  theme(
    legend.key.height = unit(0.75,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.position = c(0.25,0.17),
    text= element_text(family='Routed Gothic',size=14, color='gray99'),
    plot.title=element_textbox(family='Routed Gothic', halign = 0.5, hjust=0, size=38, margin=unit(c(15,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "gray20", color=NA),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
    legend.direction = 'horizontal'
  )

ow <- 1920
oh <- 1400
fn <- 'Percent of Town Population Living in Blocks with a Density of Less then 100 People Per Square Mile'
source('create-svg.R', local = TRUE)

