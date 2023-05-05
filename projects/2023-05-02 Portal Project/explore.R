
library(readr)
library(tidytuesdayR)
library(skimr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(glue)
library(lubridate)
library(here)


tuesdata <- tidytuesdayR::tt_load('2023-05-02')

plots <- tuesdata$plots
species <- tuesdata$species
surveys <- tuesdata$surveys

species_by_plot_by_year_data = surveys %>%
  group_by( plot, species, year ) %>%
  summarise(count=n()) %>%
  filter( !is.na(species)) %>%
  mutate( count = replace_na( count, 0 ) )

# species_by_plot_by_year_data %>%
#   ggplot(aes(x=year,y=count, group_by=species, colour=species)) +
#   facet_grid(species~plot,scales="free_y") +
#   geom_point() +
#   geom_line() +
#   theme_bw() 
# 
# ggsave(here("projects","2023-05-02 Portal Project", "fig", "species_plot.png"), width=8, height=8)

library(corrplot)

for ( this_plot in species_by_plot_by_year_data %>% pull( plot ) %>% unique() ) {
  
  this_plot_correlation = species_by_plot_by_year_data %>%
    filter( plot == this_plot ) %>%
    pivot_wider( names_from = species, values_from=count, values_fill = 0 ) %>%
    ungroup() %>%
    select (-plot)
  
  this_plot_species_correlation = cor(this_plot_correlation %>% select(-year))
  
  shrink_factor = 0.5
  png(height=1024*shrink_factor, width=1024*shrink_factor,
      file=here("projects","2023-05-02 Portal Project", "fig", glue("plot{this_plot}_species_correlations.png")),
      type = "cairo")
  
  corrplot(this_plot_species_correlation, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  dev.off()
}