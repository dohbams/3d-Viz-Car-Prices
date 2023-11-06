# Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(rayshader)   # For creating 2D and 3D plots
library(janitor)     # For data cleaning
library(extrafont)   # For additional fonts in plots
library(ggsci)       # For additional color palettes
library(rgl)         # For 3D visualization
library(webshot2)    # For capturing web screenshots
library(webshot)     # For capturing web screenshots

# Set options to display large numbers in non-scientific notation
options(scipen = 999)

# Read the CSV file containing car prices data
data <- read.csv("car_prices.csv", header = TRUE)

# Clean column names using the janitor package
data <- data %>% clean_names()

# Select specific car makes for analysis
makes2 <- c("Toyota", "Mercedes-Benz", "Kia", "Lexus", "Ford", "Audi", "Hyundai", "Honda", "BMW")

# Filter the data based on selected car makes
data2 <- data %>% filter(make %in% makes2)

# Create a ggplot for visualizing car prices and mileage among selected car brands in Nigeria
p <- ggplot(data2) +
  geom_point(aes(x = make, y = price, color = mileage)) + 
  scale_color_gradient(name = "Mileage", low = "#dab8ff", high = "#ff0000",
                       breaks = c(0, 500000, 1000000, 1500000, 2000000),
                       labels = c('', '500 k', '1 M', '1.5 M', '2 M'),
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5, ticks = FALSE, reverse = FALSE)) +
  coord_flip() +
  theme_classic() +
  scale_y_continuous(labels = c("0", '20 M', '40 M', '60 M')) +
  ylab("Price (NGN)") + xlab("") +
  ggtitle("Comparing Car Prices and Mileage Among Top Nigerian Car Brands") +
  labs(caption = '@DOh_Bams | R {tidyverse, rayshader} | Data: Kaggle & Cars45') +
  theme(plot.caption = element_text(size = 11, hjust = 0.5, vjust = -5),
        plot.title = element_text(size = 22, face = 'bold', family = "Tw Cen MT", hjust = 0.5),
        text = element_text(family = "Tw Cen MT"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 8, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(family = "Tw Cen MT", size = 14, face = "bold"),
        axis.text.x = element_text(family = "Tw Cen MT", size = 14),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(2, 4, 2, 2, 'cm'),
        plot.background = element_rect(fill = 'grey95'),
        panel.background = element_rect(fill = 'grey95'),
        legend.background = element_rect(fill = 'grey95'))

# Display the ggplot
p

# Set up a layout for plotting 3D visualization using rayshader
par(mfrow = c(1, 2))

# Plot the ggplot in 3D using rayshader
plot_gg(p, width = 7, height = 9, multicore = TRUE, windowsize = c(1600, 1600), 
        zoom = 0.89, phi = 35, theta = 30, sunangle = 225)
