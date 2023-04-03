##-----------------------------------------------------------------------------
##
## This script visualizes global plant extinction by climate.
## It was created for day 3 of the #30DayChartChallege, whose theme is flora
## and fauna.
##
## Author: Miguel Haro Ruiz  
## Date : April 3, 2023
##
##-----------------------------------------------------------------------------

rm(list = ls())

### Load required packages
library(tidyverse)
library(readxl)
library(rstudioapi)
library(camcorder)
library(treemap)
library(ggtext)

### Obtain working directory of this script and import data
dir = getSourceEditorContext()$path |> str_remove("/day_3.*")
d.raw = read_excel(paste0(dir, "/data/plant_extinction.xlsx"),
                   sheet = "Supplementary_Data_1")

### This allows for plots to be auto-saved
#gg_record(dir = "plots",
#          device = "png",
#          width = 9,
#          height = 6,
#          units = "in",
#          dpi = 300)

### Wrangle data
d = 
  d.raw |>
  filter((Extinction.Risk == "EX" |
           Extinction.Risk == "EW" |
           Extinction.Risk == "CR" |
           Extinction.Risk == "EN" |
           Extinction.Risk == "VU") &
           (Climate != "TA" &
              Climate != "A")) |> 
  mutate(risk = 
           case_when(
             Extinction.Risk == "EX" ~ "a",
             Extinction.Risk == "EW" ~ "b",
             Extinction.Risk == "CR" ~ "c",
             Extinction.Risk == "EN" ~ "d",
             Extinction.Risk == "VU" ~ "e"
             ),
         climate = 
           case_when(
             Climate == "S" ~ "Subtropical",
             Climate == "WT" | Climate == "MT" | Climate == "T" ~ "Tropical",
             Climate == "DT" ~ "Desert",
             Climate == "G" ~ "Temperate")
         ) |>
  count(risk, climate) |>
  mutate(color_f = 
           case_when(
             risk == "a" ~ "black" ,
             risk == "b" ~ "#971d16",
             risk == "c" ~ "#e03329",
             risk == "d" ~ "#e97069",
             risk == "e" ~ "#f2aca8"  ) )

### Create dataframe for treemap chart and manipulate it
tm = treemap(dtf = d,
             index = c("climate", "risk"),
              vSize = "n",
              vColor = "color_f",
              type = 'color')

tm_plot_data = tm$tm |>
  mutate(x1 = x0 + w,
         y1 = y0 + h) |>
  mutate(x = (x0+x1)/2,
         y = (y0+y1)/2) |> 
  mutate(primary_group = ifelse(is.na(risk), 7.5, .5)) |>
  mutate(color = ifelse(is.na(risk), NA, color))

### Create dataframe for main text
text_box = data.frame(
  label = "Most global analyses of biodiversity loss have overlooked plant 
  extinction. A 2019 study* broke this trend by compiling a comprehensive 
  dataset of modern plant extinctions. It documented **376 extinct plant 
  species** -- most of which were native to tropical climates -- and classified 
  about 600 species worldwide as 'vulnerable' to extinction or worse.",
  x = 0, 
  y = 1
)

### Create dataframe to label sub treemaps
headings = data.frame(x = c(0.565, 0.015, 0.828, 0.565),
                      y = c(1, 1, 0, 0),
                      text = c("Subtropical",
                               "Tropical Climates",
                               "Desert",
                               "Temperate") )

### Create dataframe to label extinction levels
text_labels = data.frame(x = c(sum(tm_plot_data$x0[tm_plot_data$risk == "a" & 
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) +0.025,
                               sum(tm_plot_data$x0[tm_plot_data$risk == "b" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) +0.008,
                               sum(tm_plot_data$x0[tm_plot_data$risk == "b" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) +0.008,
                               sum(tm_plot_data$x0[tm_plot_data$risk == "c" & 
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) +0.023,
                               sum(tm_plot_data$x0[tm_plot_data$risk == "d" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) +0.0075,
                               sum(tm_plot_data$x0[tm_plot_data$risk == "e" & 
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) +0.0075),
                         
                         y = c(sum(tm_plot_data$y1[tm_plot_data$risk == "a" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) -0.055,
                               sum(tm_plot_data$y1[tm_plot_data$risk == "b" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) -0.05,
                               sum(tm_plot_data$y1[tm_plot_data$risk == "b" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) -0.125,
                               sum(tm_plot_data$y1[tm_plot_data$risk == "c" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) -0.03,
                               sum(tm_plot_data$y1[tm_plot_data$risk == "d" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) -0.02,
                               sum(tm_plot_data$y1[tm_plot_data$risk == "e" &
                                                     tm_plot_data$climate == "Tropical"], na.rm = T) -0.02 ),
                         text = c("Extinct", 
                                  "Extinct",
                                  "in the wild", 
                                  "Critically endangered",
                                  "Endangered",
                                  "Vulnerable") )

### Create dataframe to label number of extinct/endangered species in each climate
num_labels = tm_plot_data |>
  filter(!is.na(risk)) |>
  mutate(x1 = 
           case_when(
             (climate == "Tropical" & 
                (risk == "b" | 
                   risk == "d" |
                   risk == "e") ) |
               (climate == "Subtropical" &
                  (risk == "b" |
                     risk == "d" |
                     risk == "e") ) |
             (climate == "Temperate" &
                (risk == "c" |
                   risk == "d" |
                   risk == "e") ) |
               (climate == "Desert" &
                  (risk == "b" |
                     risk == "e") ) ~ x1 -0.019, 
             TRUE ~ x1 -0.006),
         
         y0 = case_when(
           (climate == "Tropical" & 
              (risk == "c" | 
                 risk == "d") ) |
             (climate == "Temperate" &
                (risk == "b" | 
                   risk == "d") ) |
           (climate == "Subtropical" &
              (risk == "c" |
                 risk == "d" ) ) |
             (climate == "Desert" &
                (risk == "e" |
                   risk == "c") ) ~ y0 +0.05,
           TRUE ~ y0 +0.02),
         color = 
           case_when(
             risk == "a" ~ "white",
             risk == "b" ~ "white",
             risk == "c" ~ "white",
             risk == "d" ~ "black",
             risk == "e" ~ "black"),
         vSize = 
           case_when(
             vSize < 10 ~ NA,
             TRUE ~ vSize
           )
         ) |>
  select(x1, y0, vSize, color) 

### Create dataframe for source and authorship
source = data.frame(text = c("*Humphreys, A. M., Govaerts, R., Ficinski, S. Z., Nic Lughadha, E., & Vorontsova, M. S. (2019). Global dataset shows geography and life form predict modern plant extinction and rediscovery. Nature ecology & evolution, 3(7), 1043-1047.",
                             "Visualization: Miguel Haro Ruiz"),
                    x =  c(1, 1),
                    y = c(0, -.125))

### Create visualization
tm_plot_data |>
  ggplot() +
  geom_rect(aes(xmin = x0, xmax = x1,
                ymin = y0, ymax = y1,
                fill = color,
                linewidth = primary_group,
                ),
            show.legend = FALSE, color = "white") +
  
  geom_text(data = headings,
            aes(x = x, 
                y = y,
                label = text),
            size = c(8, 9, 6, 6.66),
            hjust = c(0, 0, 0, 0),
            vjust = c(0, 0, .85, .85),
            family = "DIN Alternate Bold") +
  
  geom_text(data = text_labels,
            aes(x = x,
                y = y,
                label = text),
            hjust = rep(0, 6),
            vjust = rep(1, 6),
            color = c("white", "white","white", "white", "black", "black"),
            size = c(7, 6.25, 6.25, 5.5, 3.75, 4.5),
            family = "Avenir Next Medium") +
  
  geom_text(data = num_labels,
            aes(x = x1, y = y0,
                label = vSize,
                color = color),
            hjust = 1,
            vjust = 0,
            size = 3,
            family = "Avenir Next Medium") +
  geom_textbox(data = text_box, 
               aes(x = x, y = y, 
                   label = label),
               hjust = -0.0075, vjust = -.65,
               orientation = "upright", 
               fill = NA,
               box.colour = NA,
               lineheight = 1.15,
               halign = 0,
               size = 4,
               width = unit(0.89, "npc"),
               family = "Gill Sans") +
  
  geom_textbox(data = source, 
               aes(x = x, y = y, 
                   label = text),
               hjust = 1, vjust = 2,
               orientation = "upright", 
               fill = NA,
               box.colour = NA,
               lineheight = 1,
               halign = 1,
               size = 3,
               width = unit(0.82, "npc"),
               family = "Avenir Light") +
  
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  scale_linewidth(range = range(tm_plot_data$primary_group)) +
  labs(title = "Global plant extinction") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.11,
                                  vjust = 14.5, 
                                  size = 30,
                                  family = "DIN Alternate Bold"),
        plot.background = element_rect(fill =  "white",
                                       color = "white"),
        plot.margin = margin(t = 3.5,
                             r = 0,  
                             b = 1.1,
                             l = 0.25,  
                             unit = "cm"))

### This creates an gif showing the progress of the different auto-saved plots
#gg_playback(name = file.path("plots", "fauna_flora.gif"),
#            first_image_duration = 3,
#            last_image_duration = 7,
#            frame_duration = .2,
#            image_resize = 800 )
