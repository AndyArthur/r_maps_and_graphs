library(tidycensus)
library(tidyverse)
library(tigris)

pop_est <- get_estimates(geography = "county",
                         year = 2024, output = 'wide', geometry = T, resolution='20m') %>%
  shift_geometry()

usst <- states(cb=T, resolution='20m') %>%
  filter(GEOID < 57) %>%
  shift_geometry()

library(sysfonts)
font_add_google('lexend')

pop_est %>%
  mutate(More = str_c('More\n',ifelse(BIRTHS > DEATHS,'Births', 'Deaths'))) %>%
  ggplot() +
  geom_sf(aes(fill=More), linewidth=0.05, color='white') +
  geom_sf(data=usst, fill=NA) +
  scale_fill_manual(values = c('#5ab4ac','#d8b365')) +
  theme_void() +
  coord_sf(expand=F) +
  labs(title = str_c('<span style="font-size: 40pt; font-weight: bold"><b>More Births or Deaths in 2024</b></span><br />',
                     'A look at which counties had more births then deaths last year according to 2024 US Census Population Estimates progam.'
  ),
  tag=str_c(
    '<b>Data Source:</b> 2024 US Census Population Estimates<br />',
    'Andy Arthur - ', format(Sys.Date(), format="%-m.%-d.%y"),
    ''),
  fill = "")  +
  theme(
    text= element_text(family='Roboto Condensed',size=20),
    plot.title=ggtext::element_textbox_simple(hjust=0, halign=0, size=18, margin=margin(5,0,20,0)),
    plot.background = element_rect(fill = "snow", color="snow"),
    plot.tag=ggtext::element_textbox(size=14,hjust=1, color='#555555', maxheight=0, halign = 1, valign=0),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0),
    legend.key.height = unit(6,'cm'),
    legend.key.width = unit(0.35,'cm'),
    legend.text = element_text(margin = margin(t = 30, l=5, unit = "pt")),
    legend.position = c(0,0.6),
    legend.justification = c(0,0.5)
  )

fn <- str_c('More Births or Deaths in 2024')
oh <- 1200
source('create-svg.R')
source('upload-svg.R')
