library(tidyverse)
library(duckdb)
library(tigris)

duckdb() |>
  dbConnect() |>
  dbGetQuery(r"(SELECT County, "Model Year", count(*) as Count,
             count(*)/sum(count(*)) OVER (PARTITION BY County) AS Percent
             FROM '~/Downloads/vehregdec25.parquet'
             WHERE "Record Type"='VEH' AND County != 'OUT-OF-STATE'
             GROUP BY County, "Model Year";)") |>
  as_tibble() |>
  janitor::clean_names() -> myv

myv %>%
  mutate(county = str_to_title(county)) %>%
  ggplot() +
  geom_col(aes(x=model_year, y=percent, fill=percent)) +
  coord_cartesian(xlim=c(2005,2026), expand=FALSE) +
  facet_wrap(~county) +
  scale_y_continuous(labels=scales::label_percent()) +
  scale_fill_stepsn(colors=c('blue','yellow','red'), n.breaks=10) +
  theme_minimal() +
  labs(
    title = str_c('<span style="color: blue; font-size: 26pt">Model Year</span> of Registered Motor Vehicles in New York, December 2025'),
    x='',y='',
    tag = paste("Andy Arthur,", format(Sys.Date(), format = "%-m/%-d/%y"), "<em>Data Source: NYS DMV Public File - data.ny.gov"),
    fill = ""
  ) +
  theme(
    text = element_text(family = "Routed Gothic", size = 9),
    panel.spacing.x = unit(0.5,'cm'),
    axis.text.x = element_text(angle=45, hjust = 1),
    plot.title = ggtext::element_textbox_simple(hjust = 0, size = 20, face = "bold", halign = 0.5),
    plot.background = element_rect(fill = "#FFFCFF", color = "#FFFCFF"),
    plot.tag = ggtext::element_textbox(size = 10, hjust = 1, color = "#555555", halign = 1, valign = 0),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.tag.position = c(1, 0),
    legend.position = "none"
  )

fn <- 'Model Year of Registered Motor Vehicles in New York, December 2025'
oh <- 1400
source('create-svg.R')
