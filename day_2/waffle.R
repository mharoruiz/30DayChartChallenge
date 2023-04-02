##-----------------------------------------------------------------------------
##
## This script visualizes waste management in Spain between 2010 and 2020
## It was created for day 2 of the #30DayChartChallege, whose theme is waffle.
##
## Author: Miguel Haro Ruiz  
## Date : April 2, 2023
##
##-----------------------------------------------------------------------------

rm(list = ls())

### Load required packages
library(tidyverse)
library(camcorder)
library(waffle)
library(ggtext)

### Obtain working directory of this script and import data
dir = getSourceEditorContext()$path |> str_remove("/day_2.*")
d.raw = read_csv2(paste0(dir, "/data/residuos2.csv"))

### This allows for plots to be auto-saved
#gg_record(dir = "plots",
#          device = "png",
#          width = 8,
#          height = 5,
#          units = "in",
#          dpi = 300)

### Manipulate data 
d = d.raw |> 
  filter(`Tipo de residuo` == "TOTAL" & 
           `Tipo de tratamiento` != "Total" & 
           `Clase de peligrosidad` != "TOTAL GENERAL") |>
  mutate(tonnes = as.numeric(gsub("\\.", "", Total))) |>
  group_by(`Tipo de tratamiento`, `Clase de peligrosidad`) |>
  summarise_at("tonnes", sum) |>
  ungroup() |>
  arrange(`Clase de peligrosidad`) |>
  mutate(type = paste(`Tipo de tratamiento`, `Clase de peligrosidad`),
         total = sum(tonnes), 
         share = (tonnes/total)*100,
         share_r = round(share, 0) ) |>
  select(type, tonnes, total, share, share_r) |>
  arrange(desc(share_r)) |>
  slice(1:2, 4, 5, 6, 3)

### Create a data frame for plot headings 
headings = data.frame(text = c("Hazardous (2%)",
                               "Non-Hazardous (98%)"),
                      x = c(-6.25, 3),
                      y = c(12.6, 12.6))

### Create a data frame for plot labels
labels = data.frame(text = c("45% is dumped in", 
                             "landfills or stored permanently",
                             
                             "42% is recycled,",
                             "composted or regenerated",
                             
                             "8% is repurposed as mining",
                             "and landscaping material",
                             
                             "3% is incinerated",
                             
                             "1% is repurposed",
                             
                             "1% is dumped",
                             "Although small in relative terms,",
                             "this amounts to 600,000 tonnes/year"),
                    
                    x = c(1.6, 1.6, 
                          2.6, 2.6, 
                          3.6, 3.6, 
                          10.5,
                          0.9,
                          0.1, 0.1, 0.1),
                    
                    y = c(2.8, 2.2, 
                          7.8, 7.2,
                          11.4, 10.9,
                          9,
                          11.85,
                          11, 10.5, 10.1),
                    
                    h = c(0, 0,
                          0, 0,
                          0, 0,
                          0,
                          1,
                          1, 1, 1)
                    )

### Create a data frame for plot text 
text_box = data.frame(
  label = "Every year, more than a **100 million tonnes** of waste are generated in Spain.
  Most of this waste is non-hazardous, whereas a small portion is considered 
  hazardous. This includes flammable, corossive and carcinogenic substances, as well as
  infectious microorganisms and explosives.",
  x = -.4,
  y = 8.6
)

### Create a data frame for source and author 
source = data.frame(text = c("Source: ine.es",
                             "Visualization: Miguel Haro Ruiz"),
                    x =  c(13.5, 13.5),
                    y = c(0, -.5))

### Create the visualisation
d |>
  ggplot() +
  
  geom_waffle(aes(fill = factor(type,
                                unique(type)), 
                  values = as.numeric(share_r)),
              flip = TRUE,
              n_rows = 10, 
              size = 1, 
              color = "#f2eded") +
  
  geom_segment(aes(x = 2.5, y = 9.5, 
                   xend = -6.5, yend = 9.5),
               linetype = "dashed",
               linewidth = 0.15) +
  geom_segment(aes(x = 2.5, y = 9.5, 
                   xend = 2.5, yend = 13.5),
               linetype = "dashed",
               linewidth = 0.15) +
  
  geom_richtext(data = labels,
                aes(x = x, y = y , 
                    label = text,
                    hjust = h),
                size = c(4.25, 4.25,
                         4.25, 4.25, 
                         3.8, 3.8,
                         3.8,
                         3.8, 
                         3.8, 3, 3),
                vjust = 0.5,
                fill = c("#c4bced", "#c4bced",
                         "#eebece", "#eebece",
                         rep(NA, 7)),
                alpha = c(rep(.8, 4), rep(1, 7)),
                label.size = NA,
                family = "Proxima Nova"
                ) +
  
  geom_curve(aes(xend = 2 , yend = 10.55, 
                 x = 0.9, y = 11.85), 
             color = "grey33", 
             linewidth = 0.15, 
             curvature = -0.45,
             arrow = arrow(length = unit(0.0115, "npc"), type = "closed")) +
  
  geom_curve(aes(xend = 1 , yend = 10.6, 
                 x = 0.1, y = 11.05), 
             color = "grey33", 
             linewidth = 0.15, 
             curvature = -0.35,
             arrow = arrow(length = unit(0.0115, "npc"), type = "closed")) +
  
  geom_richtext(data = headings,
                aes(x = x, y = y,
                    label = text),
                hjust = 0, 
                vjust = 0, 
                fill =  NA, 
                label.size = NA, 
                size = 5.25,
                family = "Proxima Nova Semibold"
                ) +
  
  scale_fill_manual(values = c("#a599e4",
                               "#e499b2", 
                               "#d7e499",
                               "#533dc4",
                               "#38b58b",
                               "#99e4cb")
                               ) +
  labs(title = "How is waste processed in Spain?",
       subtitle = "Data ranges from 2010 to 2020") +
  
  
  geom_textbox(data = text_box, 
               aes(x = x, y = y, 
                   label = label),
               hjust = 1, vjust = 1,
               orientation = "upright", 
               fill = NA,
               box.colour = NA,
               lineheight = 1.4,
               halign = 1,
               size = 3.95,
               width = unit(0.275, "npc"),
               family = "Proxima Nova") +
  
  geom_text(data = source,
            aes(x = x,
                y = y,
                label = text),
            color = "grey33",
            hjust = 1,
            vjust = c(0 , 0),
            family = "Avenir Light",
            size = 3
  ) +
  
  coord_equal(clip = "off") + 
  theme_void() + 
  theme(
    plot.title = element_text(hjust = 0.05,
                              vjust = 2,
                              size = 27.5,
                              family = "Founders Grotesk Medium"),
    plot.subtitle = element_text(hjust = 0.05,
                                 vjust = 2.5, 
                                 size = 11, 
                                 family = "Founders Grotesk"), 
    legend.position = "none",
    plot.background = element_rect(fill =  "#f2eded",
                                   color = "#f2eded"),
    plot.margin = margin(t = 0.75,
                         r = 0,  
                         b = 0,
                         l = 0.5,  
                         unit = "cm")
  ) 

### This creates an gif showing the progress of the different auto-saved plots
#gg_playback(name = file.path("plots", "waffle.gif"),
#            first_image_duration = 5,
#            last_image_duration = 15,
#            frame_duration = .4,
#            image_resize = 800 )
