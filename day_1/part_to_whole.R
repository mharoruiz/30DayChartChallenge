##-----------------------------------------------------------------------------
##
## This script visualizes the share of renewable energy generated in a series 
## of EU countries. It was created for day 1 of the #30DayChartChallege, whose 
## theme is part-to-whole.
##
## Author: Miguel Haro Ruiz  
## Date : April 1, 2023
##
##-----------------------------------------------------------------------------

rm(list = ls())

### Load required packages
library(tidyverse)
library(camcorder)
library(rstudioapi)

### Obtain working directory of this script and import data
dir = getSourceEditorContext()$path |> str_remove("/day_1.*")
d.raw = read_csv(paste0(dir, "/data/renew_share.csv"))

### This allows for plots to be auto-saved
#gg_record(dir = "plots",
#          device = "png",
#          width = 8,
#          height = 5,
#          units = "in",
#          dpi = 300)

### Manipulate data 
d = d.raw |>
  filter(ym >= as.Date("2018-01-01") & 
           ym <= as.Date("2022-12-01") &
           (ccode == "DE" |
              ccode == "FR" |
              ccode == "IT" | 
              ccode == "ES" |
              ccode == "NL" |
              ccode == "SE" |
              ccode == "PL" |
              ccode == "BE" |
              ccode == "AT") ) |>
  mutate(country = case_when(
            ccode == "AT" ~ "Austria",
            ccode == "BE" ~ "Belgium",
            ccode == "DE" ~ "Germany",
            ccode == "ES" ~ "Spain", 
            ccode == "FR" ~ "France",
            ccode == "IT" ~ "Italy",
            ccode == "NL" ~ "Netherlands",
            ccode == "PL" ~ "Poland",
            ccode == "SE" ~ "Sweden",
            #ccode == "BG" ~ "Bulgaria",
            #ccode == "CZ" ~ "Czechia",
            #ccode == "DK" ~ "Denmark",
            #ccode == "FI" ~ "Finland",
            #ccode == "GR" ~ "Greece",
            #ccode == "HU" ~ "Hungary",
            #ccode == "LT" ~ "Lithuania",
            #ccode == "LU" ~ "Luxemburg",
            #ccode == "NO" ~ "Norway",
            #ccode == "PT" ~ "Portugal",
            #ccode == "SI" ~ "Slovenia"
         )) |>
  select(country, ym, gen_share) |>
  group_by(country) |>
  mutate(mean = mean(gen_share, na.rm = TRUE)) |>
  arrange(desc(mean))

labels = data.frame(text = NA,
                    country = NA,
                    date = NA,
                    value = NA)

### Create a data frame for plot labels 
r = 1
for (c in unique(d$country)) {
  m = format(round(unique(d$mean[d$country == c]), 4)*1000, nsmall = 1)
  if (c == "Austria") {
    labels[r, ] = c(paste0("Average: ", m, "%"), 
                    c,
                    "2022-11-01",
                    y = unique(d$mean[d$country == c])
                    )
  } else {
    labels[r, ] = c(paste0(m, "%"), 
                    c,
                    "2022-11-01",
                    y = unique(d$mean[d$country == c])
                    )
  }
  r = r+1
}

### Create a data frame for source and author 
source = data.frame(text = c("Source: energy-charts.info",
                             "Visualization: Miguel Haro Ruiz"),
                    country = rep("Netherlands", 2),
                    date = rep("2019-06-01", 2),
                    value = rep(0, 2))

### Create the visualisation
d |>
  ggplot() + 
  geom_line(aes(ym, mean),
            color = "#af2316",
            linewidth = 0.5) +
  geom_area(aes(ym, gen_share),
            fill = "#56c866",
            alpha = 2/3) + 
  geom_hline(yintercept = 0, 
             linewidth = 0.25, 
             color = "black") +
  geom_text(data = labels,
            aes(x = as.Date(date), 
                y = as.numeric(value),
                label = text),
            color = "#8f1b11",
            hjust = 1,
            vjust = -.9,
            family = "Avenir Heavy",
            size = 3.25
            ) +
  geom_text(data = source,
            aes(x = as.Date(date), 
                y = as.numeric(value),
                label = text),
            color = "grey33",
            hjust = 0,
            vjust = c(6, 8),
            family = "Avenir Light",
            size = 3
            ) +
  coord_cartesian(clip = "off") + 
  facet_wrap(~factor(country,
                     levels = unique(d$country))) + 
  scale_x_date(breaks = c(as.Date("2018-01-01"),
                          as.Date("2020-01-01"),
                          as.Date("2022-01-01")),
               limits = c(as.Date("2018-01-01"),
                           as.Date("2022-12-01")),
               labels = c(2018, 2020, 2022),
               expand = c(0.01, 0) ) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1),
                     limits = c(0, 0.1),
                     labels = c("0%", "50%", "100%")) + 
  xlab("") + 
  ylab("") +
  labs(title = "Renewable share of generated electricity",
       subtitle = "Selected EU countries, Jan 2018 - Dec 2022") + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9,
                                   hjust = 0.1,
                                   family = "Avenir Light"),
        axis.text.y = element_text(size = 9,
                                   family = "Avenir Light"),
        plot.title = element_text(size = 18,
                                  hjust = -.15,
                                  vjust = 4,
                                  family = "Charter Roman"),
        plot.subtitle = element_text(size = 10, 
                                     hjust = -.06,
                                     vjust = 5.5,
                                     family = "Charter Roman"
                                     ),
        plot.background = element_rect(fill = "#fcf8f4",
                                       color = "#fcf8f4"
                                         ),
        plot.margin = margin(t = 1,  
                             r = 0.5,  
                             b = 1,  
                             l = 0.5,  
                             unit = "cm"),
        panel.spacing.x = unit(2/3, "cm"),
        panel.spacing.y = unit(0.75, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey33",
                                          linewidth = 0.1),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(hjust = 0,
                                  size = 12,
                                  vjust = 2,
                                  family = "Canela Text Regular")
        )

### This creates an gif showing the progress of the different auto-saved plots
#gg_playback(name = file.path("plots", "vignette_gif.gif"),
#            first_image_duration = 5,
#            last_image_duration = 15,
#            frame_duration = .4,
#            image_resize = 800 )
