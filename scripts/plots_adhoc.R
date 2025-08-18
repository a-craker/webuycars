library(tidyverse)
library(hexbin)
library(scales)

cars <- read_csv("output/webuycars.csv")

# -------------------------------------------------------------------------
# setting theme
theme_clean_sans <- theme_void() +
  theme(
    axis.text.y = element_text(size = 11, hjust = 1, family = "sans"),
    axis.text.x = element_text(size = 10, family = "sans"),
    axis.title.x = element_text(size = 11, family = "sans"),
    plot.title = element_text(size = 13, family = "sans", face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )
# -------------------------------------------------------------------------

# removing outliers - making plots more legible
cars_trim <- cars %>%
  filter(price > 0, mileage > 0) %>%
  filter(price < quantile(price, 0.995, na.rm = TRUE),
         mileage < quantile(mileage, 0.995, na.rm = TRUE))

# plum <- rev(c("#8b0e3a", "#9d3850", "#ad5466", "#c7858f"))
plum <- rev(c("#6f0b2e", "#8b0e3a", "#9d3850", "#bb6d7b", "#d29ba3"))
# plum <- rev(c("#8b0e3a", "#9d3850", "#ad5466", "#bb6d7b", "#c7858f", "#d29ba3"))

hex_price_mileage <- ggplot(cars_trim, aes(x = mileage, y = price)) +
  geom_hex(bins = 55, color = "white", size = 0.09, show.legend = TRUE) +
  scale_fill_gradientn(colors = plum, name = "Listings") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Price vs Mileage",
    x = "Mileage (km)",
    y = "Price (ZAR)"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 13,
      family = "sans",
      face = "bold"
    ),
    legend.position = "right"
  )

ggsave("plots/price_vs_mileage_hexbin.png", hex_price_mileage, width = 14, height = 10, dpi = 300)


# Boxplots -------------------------------------------------------------------------
top_cars <- cars %>% 
  mutate(make_top = if_else(make %in% top_makes, make, "Other"),
         make_top = fct_rev(fct_infreq(make_top)))

top_makes <- cars %>% 
  count(make, sort = TRUE) %>% 
  top_n(15) %>% 
  pull(make)

mileage_make_boxplot <- cars_trim %>% 
  mutate(make_top = if_else(make %in% top_makes, make, "Other"),
         make_top = fct_rev(fct_infreq(make_top))) %>% 
  filter(make_top != "Other") %>%
  ggplot(aes(x = mileage, y = make_top)) +
  geom_dotplot(
    binaxis = "x", stackdir = "down",
    binwidth = 2000,            # mileage bin
    dotsize = 0.45,             
    color = "#8b0e3a", fill = "#8b0e3a", alpha = 0.9
  ) +
  geom_boxplot(
    width = 0.22, fill = "orange", color = "#8b0e3a",
    outlier.shape = NA, alpha = 0.9
  ) +
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(title = "Mileage Variation Across Make", y = "Make", x = "Mileage (km)") +
  theme_clean_sans +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),        # remove horizontal grid
    panel.grid.major.x = element_line(size = 0.1, color = "darkgrey"),  # keep vertical grid (as in ref)
    panel.grid.minor = element_blank()
  )

ggsave("plots/mileage_make_boxplot.png", mileage_make_boxplot, width = 14, height = 15, dpi = 300)

# feature premiums --------------------------------------------------------

feature_premium <- cars %>%
  filter(!is.na(price), price > 0, !is.na(service_history)) %>%
  group_by(service_history) %>%
  summarise(
    median_price = median(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # Order by descending median price
  mutate(service_history = reorder(service_history, -median_price))

service_price <- ggplot(feature_premium, aes(x = service_history, y = median_price)) +
  geom_col(fill = "#8b0e3a", alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Median Price by Service History",
    x = "",
    y = "Median Price (ZAR)"
  ) +
  # theme_clean_sans +
  theme_minimal(base_size = 12) +  # clean background
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1.3),
    panel.grid.major.x = element_blank(), # remove vertical gridlines
    panel.grid.minor = element_blank()    # remove minor gridlines
  )

ggsave("plots/service_price.png", service_price, width = 14, height = 12, dpi = 300)

# Feature premiums -------------------------------------------------------------------------

# 1) Year vs Price (Year vertical)
year_price <- cars_trim %>%
  group_by(year) %>%
  summarise(
    median_price = median(price, na.rm = TRUE),
    n_cars = n()
  ) %>%
  ggplot(aes(x = median_price, y = factor(year))) +
  geom_col(fill = "#8b0e3a", alpha = 0.8) +
  geom_text(aes(label = n_cars),
            hjust = -0.3,            # push labels slightly right of bar
            color = "black",
            size = 3.5) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.02, 0.1))) +
  labs(title = "Car Year vs Median Price",
       x = "Median Price (ZAR)", y = "Year") +
  theme_clean_sans +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 9))

ggsave("plots/year_price.png", year_price, width = 14, height = 12, dpi = 300)


# 3) GVM vs Price (binned median)
cars_gvm <- cars_trim %>%
  mutate(gvm_bin = cut(gvm, breaks = seq(min(gvm, na.rm = TRUE),
                                         max(gvm, na.rm = TRUE),
                                         by = 250))) %>%
  group_by(gvm_bin) %>%
  summarise(median_price = median(price, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(gvm_bin))

cars_trim %>%
  mutate(
    # Create 10 equal-width bins for GVM
    gvm_bin = cut(gvm, breaks = 10, include.lowest = TRUE)
  ) %>%
  group_by(gvm_bin) %>%
  summarise(
    median_price = median(price, na.rm = TRUE),
    n_cars = n(),
    .groups = "drop"
  ) %>%
  # Order bins by GVM range (factor ordering)
  mutate(gvm_bin = factor(gvm_bin, levels = levels(gvm_bin))) %>%
  ggplot(aes(x = gvm_bin, y = median_price)) +
  geom_col(fill = "#8b0e3a") +
  geom_text(aes(label = n_cars),
            vjust = -0.2, size = 3.5) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(title = "GVM vs Price",
       x = "GVM (kg) bins",
       y = "Median Price (ZAR)") +
  theme_clean_sans +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


# 4) Service History vs Price
p4 <- cars_trim %>%
  filter(!is.na(service_history)) %>%
  group_by(service_history) %>%
  summarise(median_price = median(price, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_price) %>% 
  ggplot(aes(y = service_history, x = median_price)) +
  geom_col(fill = "#8b0e3a") +
  geom_text(aes(label = comma(median_price)), vjust = -0.5, family = "sans") +
  scale_x_continuous(labels = comma) +
  labs(title = "Service History vs Price", x = "Has Service History", y = "Median Price (ZAR)") +
  theme_clean_sans


# Linkedin - Chinese Entrants Donut ---------------------------------------

cars <- read_csv("output/webuycars.csv")

new_cn_entrants <- c(
  "Chery", 
  "Omoda", 
  "Jaecoo",
  "Baic", "Beijing",     
  "MG", 
  "Geely",
  "Changan", "Chana",
  "Haval", 
  "GWM",                
  "JMC",                
  "Foton",
  "King Long",
  "JAC",                
  "B.a.w"               
)


cars_cn <- cars %>%
  mutate(
    chinese_entrant = as.integer(
      str_detect(make, regex(paste(new_cn_entrants, collapse = "|"), ignore_case = TRUE))
    )
  )

composition <- cars_cn %>%
  group_by(chinese_entrant) %>%
  summarise(total = n_distinct(stock_number), .groups = "drop") %>%
  mutate(
    group = if_else(chinese_entrant == 1, "New Chinese entrants", "Other makes"),
    share = total / sum(total)
  )


# ---- chart in the style of your mileage plot (palette, grids, theme) ----
donut_plot <- composition %>%
  mutate(label = percent(share, accuracy = 0.1)) %>%
  ggplot(aes(x = 2, y = share, fill = group)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +   # add donut hole
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 12, fontface = "bold") +
  scale_fill_manual(values = c(
    "New Chinese entrants" = "orange",
    "Other makes" = "#8b0e3a"
  )) +
  labs(
    title = "Second-Hand Market Composition on Webuycars",
    subtitle = "Share of listings: New Chinese Entrants vs Other Makes",
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 25),
    legend.title  = element_text(size = 25, face = "bold"),
    legend.text   = element_text(size = 25),
    legend.position = "bottom"
  )

ggsave("plots/donut_plot.png", donut_plot, width = 14, height = 15, dpi = 300)


