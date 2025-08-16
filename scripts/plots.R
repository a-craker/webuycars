library(tmap)
library(sf)
library(ggridges)
library(cowplot)
library(tidyverse)

cars <- read_csv("output/webuycars.csv")
car_dates <- read_csv("output/webuycars_dates.csv")

provinces <- st_read("data/za.json")


# market composition ------------------------------------------------------

# make prettier and make sure labels look good
top_makes <- cars %>% 
  count(make, sort = TRUE) %>% 
  top_n(10) %>% 
  pull(make)

# df of top makes
cars_top_10 <- cars %>% 
  filter(make %in% top_makes)

# reusable theme
theme_clean_sans <- theme_void() +
  theme(
    axis.text.y = element_text(size = 11, hjust = 1, family = "sans"),
    axis.text.x = element_text(size = 10, family = "sans"),
    axis.title.x = element_text(size = 11, family = "sans"),
    plot.title = element_text(size = 13, family = "sans", face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

most_listed <- ggplot(cars_top_10,
       aes(fct_infreq(make))) + 
  geom_bar(fill = "#8b0e3a", show.legend = FALSE) +
  coord_flip() +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Top 10 Most Listed Cars Brands",
       x = "Make", 
       y = "Number of Listings") +
  theme_clean_sans +
  theme(plot.title = element_text(hjust = 0.5, size = 13, family = "sans", face = "bold"))

ggsave("plots/most_listed.png", most_listed, width = 14, height = 10)

# geographic dist ---------------------------------------------------------

tmap_mode("plot")

province_count <- cars %>%
  group_by(province) %>%
  summarise(n_cars = n_distinct(stock_number), .groups = "drop") %>% 
  mutate(
    pct = n_cars / sum(n_cars) * 100,
    label = paste0(round(pct, 0), "%")
  ) %>% arrange(n_cars)

map_car <- provinces %>%
  left_join(province_count, by = c("name" = "province")) %>% 
  filter(!name %in% c("Northern Cape", "Free State"))

plum <- rev(c("#8b0e3a", "#9d3850", "#ad5466", "#bb6d7b", "#c7858f", "#d29ba3"))

map_car$label_colour <- ifelse(map_car$n_car >= 15000, "white", "black")


tm_shape(map_car) +
  tm_fill("n_cars",
    palette = plum,
    colorNa = "grey",
    title = "Distribution Of Car Listings",
    # textNa = "Missing",
    legend.show = FALSE
  ) + 
  tm_borders(col = "white",
             lwd = 0.5) +
  tm_text("label",
          col = "label_colour",
          size = 1.2, 
          fontface = "bold",
          fontfamily = "Open Sans") +
  tm_layout(legend.show = FALSE)
  

# price and mileage dist --------------------------------------------------

# df top makes and other category
top_cars <- cars %>% 
  mutate(make_top = if_else(make %in% top_makes, make, "Other"),
         make_top = fct_rev(fct_infreq(make_top)))

# THEME
theme_clean_sans <- theme_void() +
  theme(
    axis.text.y = element_text(size = 11, hjust = 1, family = "sans"),
    axis.text.x = element_text(size = 10, family = "sans"),
    axis.title.x = element_text(size = 11, family = "sans"),
    plot.title = element_text(size = 13, family = "sans", face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )



mileage <- ggplot(top_cars, 
                  aes(x = mileage, y = make_top)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, color = "white", fill = "#9d3850") +
  scale_x_continuous(labels = scales::label_comma(suffix = " K", scale = 1e-3),
                     limits = c(0, 600000)) +
  labs(title = "Mileage (km)", 
       x = "", y = "Make") +
  theme_clean_sans +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 13, family = "sans", face = "bold"))

price <- ggplot(top_cars, 
                aes(x = price, y = make_top)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, color = "white", fill = "#8b0e3a") +
  scale_x_continuous(labels = scales::label_comma(suffix = " K", scale = 1e-3),
                     limits = c(0, 1000000)) +
  labs(title = "Price (ZAR)", 
       x = "", y = "") +
  theme_clean_sans +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 13, family = "sans", face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


price_milage_dis <- plot_grid(mileage, price, ncol = 2, rel_widths = c(1, 1), align = "h")

ggsave("plots/price_milage_dis.png", price_milage_dis, width = 14, height = 10)

# Price vs manufacture year -------------------------------------------------------------------------



