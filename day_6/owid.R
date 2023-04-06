##-----------------------------------------------------------------------------
##
## This script visualizes the global land use in 1900 and 2015. 
## It was created for day 1 of the #30DayChartChallege, whose 
## theme is owid.
##
## Author: Miguel Haro Ruiz  
## Date : April 6, 2023
##
##-----------------------------------------------------------------------------

rm(list = ls())

### Load required packages
library(tidyverse)
library(rstudioapi)
library(owidR)
library(camcorder)
library(ggtext)


### This allows for plots to be auto-saved
#dir = getSourceEditorContext()$path |> str_remove("/day_6.*")
#gg_record(dir = file.path(dir, "plots"),
#          device = "png",
#          width = 9,
#          height = 6,
#          units = "in",
#          dpi = 300)

### Import and wranggle data
d.raw = owid(chart_id = "global-land-use-since-10000bc")

d = d.raw |>
  filter((year == 1900 |
           year == 2015) ) |>
  mutate(area = `Global land use`,
         use = entity,
         use = 
           case_when(
             use == "Urban" ~ "Cities",
             use == "Semi-natural land" ~ "Woodlands",
             TRUE ~ use)
         ) |>
  group_by(year) |>
  mutate(total = sum(area)) |>
  ungroup() |>
  mutate(share = area/total*100) |>
  select(use, year, area, share, total) 
  

labs = d |>
    select(use, year, area) |>
    pivot_wider(names_from = year,
                values_from = area) |>
    mutate(total = sum(`1900`),
           share = `2015`/total *100,
      diff = (`2015`-`1900`)/`1900` *100,
      labs = 
        ifelse(sign(diff) == 1, 
               paste0(use, " **+", round(diff, 0), "%**"),
               paste0(use, " **", round(diff, 0), "%**") ),
      y = 
        case_when(
          use == "Pasture" ~ share + 0.85,
          use == "Woodlands" ~ share - 1.75,
          TRUE ~ share) 
      )

d.plot = d |>
  inner_join(labs |> select(use, labs, y), by = "use") |>
  filter(use != "Wild woodlands" &
           use != "Wild barren land" &
           use != "Permanent ice" ) |>
  mutate(color = 
           case_when(use == "Woodlands" ~ "a",
                     use == "Pasture" ~ "b",
                     use == "Cropland" ~ "c",
                     use == "Villages" |
                       use == "Cities" ~ "d"),
         color = factor(color),
         series = 
           case_when(use == "Pasture" ~ "a",
                     use == "Cropland" ~ "b",
                     use == "Villages" ~ "c",
                     use == "Cities" ~ "d",
                     use == "Woodlands" ~ "e"))

### Create dataframe for main text
text_box = data.frame(x = 2015,
                      y =  40,
                      text = "Between 1900 and 2015, the world’s population 
                      grew from 1.65 to 7.43 billion people. 
                      The most direct implication of this demographic explosion 
                      is that humans needed more space to live; during this time, 
                      the **land taken by cities and villages grew** by about 200%. 
                      More people also means more mouths to feed; over the last 100 
                      years, the **surface used for crops and pasture has increased** 
                      by 68 and 14%, respectively. Dedicating more land to human 
                      needs makes less of it available for everything else. In 
                      Particular, **woodlands are the most affected by this shift 
                      in land use**. In the 1960s, the total area used for pasture 
                      surpassed woodlands for the first time ever. Since then, 
                      the trend has only gotten worse. ")

### Create dataframe for source and authorship
source = data.frame(text = c("Source: Our World in Data",
                             "Visualization: Miguel Haro Ruiz"),
                    x =  c(2015, 2015),
                    y = c(1.75, 0))

### Create visualization
d.plot |>
  ggplot() +
  geom_line(aes(year, share,
                group = series,
                color = color),
            linewidth = 0.66) +
  geom_point(aes(year, share,
                 group = series,
                 color = color),
             size = 2) + 
  scale_y_continuous(labels = c("0%", 
                                #"10%",
                                "20%",
                                #"30%", 
                                "40%"),
                     breaks = seq(0, 40, by =20)) +
  scale_color_manual(values = 
                       c("#e23e3e",
                         #"#d7b9b9",
                         "#88aaf8", 
                         "#326aea",
                         "#103895")) +
  geom_textbox(data = subset(d.plot, year == 2015),
            aes(year +1, y,
                label = labs,
                color = color),
            hjust = 0,
            vjust = 0.5,
            orientation = "upright",
            fill = NA,
            box.colour = NA,
            lineheight = 1.1,
            halign = 0.5,
            size = 4,
            width = unit(2.5, "cm"),
            family = "Gill Sans") + 
  
  geom_textbox(data = text_box,
               aes(x, y,
                   label = text),
               hjust = -0.41,
               vjust = .975,
               orientation = "upright",
               fill = NA,
               box.colour = NA,
               lineheight = 1.65,
               halign = 0,
               size = 3.5,
               width = unit(7.75, "cm"),
               family = "Proxima Nova") + 
  
  geom_textbox(data = source, 
               aes(x = x, y = y, 
                   label = text),
               hjust = -0.75, 
               vjust = 2.35,
               orientation = "upright", 
               fill = NA,
               box.colour = NA,
               lineheight = 0,
               halign = 1,
               size = 2.5,
               width = unit(0.82, "npc"),
               family = "Avenir Light") +
  
  ylab("") + 
  xlab("") +
  scale_x_continuous(breaks = c(1900, 2015)) +
  labs(title = "Population is growing. Land does not",
       subtitle = "Global land use, 1900-2015") +
  coord_cartesian(clip = "off") + 
  theme_minimal(base_size = 12,
                base_family = "Proxima Nova") + 
  theme(plot.background = element_rect(fill =  "whitesmoke",
                                      color = "whitesmoke"),
        plot.margin = margin(t = 1,
                             b = 0.4,
                             l = 0.4,
                             r = 11.5,
                             unit = "cm"),
        plot.title = element_text(vjust = 3.5,
                                  hjust = .15,
                                  size = 25,
                                  family = "DIN Alternate Bold"),
        plot.subtitle = element_text(vjust = 4.5, 
                                     hjust = -.1,
                                     size = 10),
        panel.grid.major.y = element_line(linewidth = 0.075,
                                          color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

### This creates an gif showing the progress of the different auto-saved plots
#gg_playback(name = file.path("plots", "owid.gif"),
#            first_image_duration = 3,
#            last_image_duration = 10,
#            frame_duration = .1,
#            image_resize = 800 )
