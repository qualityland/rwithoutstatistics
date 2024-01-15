library(tidyverse)

gapminder_10_rows <- read_csv("https://data.rwithoutstatistics.com/gapminder_10_rows.csv")


## First Layer: Mapping Data to Aesthetic Properties

ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
)



## Second Layer: Choosing the geoms

# scatter plot
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_point()

# line plot
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_line()

# line & points
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_point() +
  geom_line()

# column chart
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp
  )
) +
  geom_col()



## Third Layer: Altering Aesthetic Properties

# fill columns
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col()

# alter fill
ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col() +
  scale_fill_viridis_c()



## Fourth Layer: Setting a Theme

ggplot(
  data = gapminder_10_rows,
  mapping = aes(
    x = year,
    y = lifeExp,
    fill = year
  )
) +
  geom_col() +
  scale_fill_viridis_c() +
  theme_minimal()



## Drought Visualization

library(rio)

dm_perc_cat_hubs_raw <- import("https://data.rwithoutstatistics.com/dm_export_20000101_20210909_perc_cat_hubs.json")

# wrangle
hubs_order <- c("Northwest", "California", "Southwest", "Northern Plains", 
                "Southern Plains", "Midwest", "Southeast", "Northeast")


dm_perc_cat_hubs <-
  dm_perc_cat_hubs_raw %>%
  ## Remove Northern Forest as it combines Midwest + Northeast
  filter(Name != "Northern Forests\\n") %>%
  ## Remove Carribean which shows no distinct drought patterns anyway
  filter(Name != "Caribbean") %>%
  mutate(
    across(c(MapDate, ValidStart, ValidEnd), as_date),
    across(None:D4, ~as.numeric(.x) / 100),
    Name = stringr::str_remove(Name, "\\\\n"),
    Name = str_replace(Name, "Nothern", "Northern")
  ) %>%
  rename("date" = "MapDate", "hub" = "Name") %>%
  pivot_longer(
    cols = c(None:D4),
    names_to = "category",
    values_to = "percentage"
  ) %>%
  filter(category != "None") %>%
  mutate(category = factor(category)) %>%
  dplyr::select(-ValidStart, -ValidEnd, -StatisticFormatID) %>%
  mutate(
    year = year(date),
    week = week(date),
    hub = factor(hub, levels = hubs_order, labels = hubs_order)
  ) %>%
  group_by(year) %>%
  mutate(max_week = max(week)) %>% ## for var
  ungroup() %>% 
  filter(percentage > 0)


southwest_2003 <- dm_perc_cat_hubs %>%
  filter(hub == "Southwest") %>%
  filter(year == 2003)

# visualize southwest for 2003
ggplot(
  data = southwest_2003,
  aes(
    x = week,
    y = percentage,
    fill = category
  )
) +
  geom_col()

# changing aesthetic properties (rocket palette)
ggplot(
  data = southwest_2003,
  aes(
    x = week,
    y = percentage,
    fill = category
  )
) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  )


# tweak x and y axes
ggplot(
  data = southwest_2003,
  aes(
    x = week,
    y = percentage,
    fill = category
  )
) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  ) +
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  )


# facetting the plot
dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage,
    fill = category
  )) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  ) +
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  )


# applying small polishes
dm_perc_cat_hubs %>%
  filter(hub %in% c(
    "Northwest",
    "California",
    "Southwest",
    "Northern Plains"
  )) %>%
  ggplot(aes(
    x = week,
    y = percentage,
    fill = category
  )) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4
  ) +
  geom_col() +
  scale_fill_viridis_d(
    option = "rocket",
    direction = -1
  ) +
  scale_x_continuous(
    name = NULL,
    guide = "none"
  ) +
  scale_y_continuous(
    name = NULL,
    labels = NULL,
    position = "right"
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  theme_light(base_family = "Roboto") +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      face = "bold"
    ),
    legend.text = element_text(color = "#2DAADA"),
    strip.text.x = element_text(
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )


# final version
library(shades)
library(colorspace)
ggplot(dm_perc_cat_hubs, aes(week, percentage)) +
  geom_rect(
    aes(
      xmin = .5,
      xmax = max_week + .5,
      ymin = -0.005,
      ymax = 1
    ),
    fill = "#f4f4f9",
    color = NA,
    size = 0.4,
    show.legend = FALSE
  ) +
  geom_col(
    aes(
      fill = category,
      # fill = after_scale(addmix(
      #   darken(
      #     fill,
      #     .05,
      #     space = "HLS"
      #   ),
      #   "#d8005a",
      #   .15
      # )),
      color = after_scale(darken(
        fill,
        .2,
        space = "HLS"
      ))
    ),
    width = .9,
    size = 0.12
  ) +
  facet_grid(
    rows = vars(year),
    cols = vars(hub),
    switch = "y"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(.02, .02),
    guide = "none",
    name = NULL
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    position = "right",
    labels = NULL,
    name = NULL
  ) +
  scale_fill_viridis_d(
    option = "rocket",
    name = "Category:",
    direction = -1,
    begin = .17,
    end = .97,
    labels = c(
      "Abnormally Dry",
      "Moderate Drought",
      "Severe Drought",
      "Extreme Drought",
      "Exceptional Drought"
    )
  ) +
  guides(fill = guide_legend(
    nrow = 2,
    override.aes = list(size = 1)
  )) +
  theme_light(
    base_size = 18,
    base_family = "Roboto"
  ) +
  theme(
    axis.title = element_text(
      size = 14,
      color = "black"
    ),
    axis.text = element_text(
      family = "Roboto Mono",
      size = 11
    ),
    axis.line.x = element_blank(),
    axis.line.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.y = element_line(
      color = "black",
      size = .2
    ),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(
      color = "#2DAADA",
      size = 18,
      face = "bold"
    ),
    legend.text = element_text(
      color = "#2DAADA",
      size = 16
    ),
    strip.text.x = element_text(
      size = 16,
      hjust = .5,
      face = "plain",
      color = "black",
      margin = margin(t = 20, b = 5)
    ),
    strip.text.y.left = element_text(
      size = 18,
      angle = 0,
      vjust = .5,
      face = "plain",
      color = "black"
    ),
    strip.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(
      fill = "transparent",
      color = "transparent"
    ),
    panel.border = element_rect(
      color = "transparent",
      size = 0
    ),
    plot.background = element_rect(
      fill = "transparent",
      color = "transparent",
      size = .4
    ),
    plot.margin = margin(rep(18, 4))
  )


# 2nd try
dm_perc_cat_hubs %>% 
  ggplot(aes(x = week, y = percentage)) +
  geom_rect(aes(xmin = .5, 
                xmax = max_week + .5,
                ymin = -0.005, 
                ymax = 1),
            fill = "#f4f4f9", 
            color = NA, 
            size = 0.4, 
            show.legend = FALSE  #9d9ca7, 99a4be, 8696bd
  ) + 
  geom_col(
    aes(fill = category, 
        #fill = after_scale(addmix(darken(fill, .05, space = "HLS"), "#d8005a", .15)), 
        color = after_scale(darken(fill, .2, space = "HLS"))),
    width = .9, size = 0.12
  ) + 
  facet_grid(rows = vars(year), cols = vars(hub), switch = "y") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(.02, .02), guide = "none", name = NULL) +
  scale_y_continuous(expand = c(0, 0), position = "right", labels = NULL, name = NULL) + 
  scale_fill_viridis_d(
    option = "rocket", name = "Category:", 
    direction = -1, begin = .17, end = .97,
    labels = c("Abnormally Dry", "Moderate Drought", "Severe Drought", 
               "Extreme Drought", "Exceptional Drought")
  ) +
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  theme_light(base_size = 12, base_family = "Roboto") +
  theme(
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(family = "Roboto Mono", size = 11),
    axis.line.x = element_blank(),
    axis.line.y = element_line(color = "black", size = .2),
    axis.ticks.y = element_line(color = "black", size = .2),
    axis.ticks.length.y = unit(2, "mm"),
    legend.position = "top",
    legend.title = element_text(color = "#2DAADA", size = 18, face = "bold"),
    legend.text = element_text(color = "#2DAADA", size = 16),
    strip.text.x = element_text(size = 16, hjust = .5, face = "plain", color = "black", margin = margin(t = 20, b = 5)),
    strip.text.y.left = element_text(size = 18, angle = 0, vjust = .5, face = "plain", color = "black"),
    strip.background = element_rect(fill = "transparent", color = "transparent"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.spacing.x = unit(0.3, "lines"),
    panel.spacing.y = unit(0.25, "lines"),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    panel.border = element_rect(color = "transparent", size = 0),
    plot.background = element_rect(fill = "transparent", color = "transparent", size = .4),
    plot.margin = margin(rep(18, 4))
  )
