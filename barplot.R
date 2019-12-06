library(tidyverse)
library(cowplot)
library(magick)
library(showtext)

font_add_google(name = "UnifrakturMaguntia", family = "uni")
font_add_google(name = "Roboto", family = "roboto")
font_add_google(name = "Cousine", family = "cousine")

showtext_auto()

csv <- "Cause of death,incidence
Drops dead,1
Snakebite,1
Ripped apart by Mob,1
Blinded,1
Disappears,1
Stabbed,30
Poisoned,4
Broken heart,1
Stabbed and poisoned,3
Beheaded,5
Drowned,1
Smothered by pillow,1
Lack of sleep,1
Shame,1
Hanging,2
Grief,1
Dismemberment then fire,1
Baked into pie,2
Indigestion,1
Buried to neck starvation,1
Throws oneself away,1
Pursued by bear,1"

df <- read_csv(csv) %>%
  janitor::clean_names() %>%
  arrange(incidence, desc(cause_of_death)) %>%
  mutate(
    cause_of_death = fct_inorder(cause_of_death, ordered = TRUE)
  )

shakespeare_img <- here::here("cropped_Title_page_William_Shakespeare_First_Folio_1623.jpg") %>%
  image_read() %>%
  image_colorize(80, "white")

plot_title <- "Causes of Death\nin Shakespeare plays"
plot_subtitle <- "A reinterpretation of\nhttp://bit.ly/charactersdieshakespeare"

barplot <- ggplot(df, aes(x = cause_of_death, y = incidence,
               fill = cause_of_death)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = incidence), nudge_y = 1,
             fill = "white",
             label.size = 0,
             show.legend = FALSE) +
  annotate(
    geom = "text",
    label = plot_title,
    x = 12,
    y = 20,
    size = 12,
    family = "uni"
  ) +
  annotate(
    geom = "text",
    label = plot_subtitle,
    x = 8,
    y = 20,
    size = 5,
    family = "roboto"
  ) +
  annotate(
    geom = "text",
    label = "Shakespeare image from:\nhttp://bit.ly/firstfolioshakespeare",
    x = 3,
    y = 30,
    hjust = 1,
    size = 3,
    family = "cousine"
  ) +
  annotate(
    geom = "text",
    label = "@jmcastagnetto, Jesus M. Castagnetto, 2019-12-06\nCode: https://github.com/jmcastagnetto/shakespeare_deaths",
    x = 2,
    y = 30,
    hjust = 1,
    size = 3,
    family = "cousine"
  ) +
  scale_fill_viridis_d() +
  coord_flip() +
  ggthemes::theme_tufte(18) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  theme(
   # plot.margin = unit(rep(1, 4), "cm"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold"),
    axis.ticks.length.y = unit(.5, "cm"),
    axis.ticks.x = element_blank()
  )

ggdraw() +
  draw_image(shakespeare_img) +
  draw_plot(barplot, width = 0.63, hjust = -.31)

ggsave(
  filename = here::here("20191206-deaths-shakespeare-plays.png")
)

# crop the image to remove extra space on both sides
img <- here::here("20191206-deaths-shakespeare-plays.png") %>%
  image_read() %>%
  image_crop(geometry = "898x942+190+0")

image_write(
  img,
  path = here::here("20191206-deaths-shakespeare-plays.png"),
  format = "png"
)
