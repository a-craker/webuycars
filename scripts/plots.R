library(tmap)
library(sf)
library(ggridges)
library(cowplot)
library(patchwork)
library(tidyverse)

cars <- read_csv("output/webuycars.csv")
car_dates <- read_csv("output/webuycars_dates.csv")

# -------------------------------------------------------------------------

provinces <- st_read("data/za.json")



# market composition ------------------------------------------------------

# make prettier and make sure labels look good
top_makes <- cars %>% 
  count(make, sort = TRUE) %>% 
  top_n(10) %>% 
  pull(make)

cars_top_10 <- cars %>% 
  filter(make %in% top_makes)

ggplot(cars_top_10,
       aes(fct_infreq(make))) + 
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Listed Cars Brands",
       x = "Make", 
       y = "Number of Listings")


# geographic dist ---------------------------------------------------------

tmap_mode("plot")

province_count <- cars %>%
  group_by(province) %>% 
  summarise(n_cars = n_distinct(stock_number))
  
map_car <- provinces %>% 
  left_join(province_count, by = c("name" = "province"))
  
tm_shape(map_car) +
  tm_polygons("n_cars",
              palette = "Blues", 
              title = "Number of Car Listings") + 
  tm_layout(title = "Cars Listed by Province")


# price and mileage dist --------------------------------------------------

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
                  aes(x = mileage, y = make_top, fill = make_top)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, color = "white") +
  scale_x_continuous(labels = scales::label_comma(suffix = " K", scale = 1e-3),
                     limits = c(0, 600000)) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Mileage (km)", 
       x = "", y = "Make") +
  theme_clean_sans +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 13, family = "sans", face = "bold"))


price <- ggplot(top_cars, 
                  aes(x = price, y = make_top, fill = make_top)) +
  geom_density_ridges(scale = 1.5, alpha = 0.7, color = "white") +
  scale_x_continuous(labels = scales::label_comma(suffix = " K", scale = 1e-3),
                     limits = c(0, 1000000)) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Price (ZAR)", 
       x = "", y = "") +
  theme_clean_sans +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 13, family = "sans", face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 


plot_grid(mileage, price, ncol = 2, rel_widths = c(1, 1), align = "h")


# Pice vs manufacture year -------------------------------------------------------------------------




  