
library(readr)
library(tidytuesdayR)
library(skimr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(glue)
library(lubridate)
library(here)


tuesdata <- tidytuesdayR::tt_load('2023-04-25')

winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon

skim(winners)
skim(london_marathon)

country_codes <- readr::read_csv('https://raw.githubusercontent.com/johnashu/datacamp/master/medals/Summer%20Olympic%20medalists%201896%20to%202008%20-%20IOC%20COUNTRY%20CODES.csv')


### Which results is the fastest, per category?
winners_edit <- winners %>%
  group_by( Category ) %>%
  mutate( category_rank = rank(Time)) %>%
  mutate( highlight_category = ifelse(
    category_rank < 2,
    "highlight",
    "other" ) )  %>%
  left_join( country_codes %>% select(Country,NOC),
             by=c("Nationality"="Country")) %>%
  mutate( winner_label = glue::glue("{Athlete} ({NOC}), {Year}")) %>%
  mutate( time_as_duration = as.duration(Time) )

highlight_alpha = c("highlight"=1, "other"=0.3)
highlight_size = c("highlight"=3, "other"=3)

labelbox_x <- c( 1995, 2020 )
labelbox_y <- c( as.duration(as.difftime("03:00:00","%H:%M:%S")), Inf )

ggplot( winners_edit,
        aes(x=Year, y=Time, col=Category, label=winner_label)) +
  geom_point( aes(alpha=highlight_category, size=highlight_category) ) +
  geom_line( alpha=0.3,size=5,lineend = "round" ) +
  geom_point( data=winners_edit %>%
                filter( highlight_category=="highlight"),
              shape=1, size=5, show.legend=FALSE ) +
  scale_alpha_manual(values=highlight_alpha) +
  scale_size_manual(values=highlight_size) +
  geom_text_repel(data=winners_edit %>%
                    filter( highlight_category=="highlight"),
                  size=3,
                  force      = 50,
                  force_pull = 0, # do not pull toward data points
                  nudge_y    = 0.05,
                  direction  = "x",
                  angle      = 90,
                  hjust      = 0,
                  xlim       = labelbox_x,
                  ylim       = labelbox_y,
                  show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  scale_colour_brewer( palette="Set1" ) +
  guides( alpha = FALSE, size=FALSE ) +
  guides( label = guide_legend(override.aes = aes(label = NA))) +
  labs( title="London Marathon Winners") +
  theme_minimal()

ggsave(here("projects","2023-04-25 London Marathon", "fig", "winning_times.png"), width=8, height=4)
