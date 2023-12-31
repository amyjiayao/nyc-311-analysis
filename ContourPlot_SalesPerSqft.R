# Load packages
library(ggmap)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Read data
file_path <- "data/NYC_Citywide_Sales_2022.csv"
data <- read.csv(file_path)

# Process data
colnames(data) <- gsub(" ", ".", colnames(data)) # Replace space with period in column names
data <- data[complete.cases(data$LAND.SQUARE.FEET, data$SALE.PRICE, data$Longitude, data$Latitude), ] # Delete rows with missing data
data <- data[data$LAND.SQUARE.FEET != 0 & data$SALE.PRICE != 0, ] # Filter rows with 0 value

# Calculate Sales Price Per Sqft
data$SALE.PRICE <- as.numeric(gsub("[^0-9.]", "", data$SALE.PRICE)) 
data$LAND.SQUARE.FEET <- as.numeric(gsub("[^0-9.]", "", data$LAND.SQUARE.FEET))
data$Sales.Price.Per.Sqft <- data$SALE.PRICE / data$LAND.SQUARE.FEET

# Create NYC map with adjusted bounding box
ny_map <- get_stamenmap(bbox = c(left = -74.5, bottom = 40.55, right = -73.75, top = 40.85), maptype = "terrain-background")

# Create a contour plot
ny_contour_plot <- ggmap(ny_map) +
  stat_density_2d(data = data, aes(x = Longitude, y = Latitude, fill = ..level..), geom = "polygon", alpha = 0.5) +
  scale_fill_gradient(name = "Density", low = "white", high = "darkred") +
  labs(title = "New York City Property Value Contour Plot",
       subtitle = "Density of Sales Price/Sqft for Citywide Sales in 2022",
       theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)))

# Save the image
ggsave("ContourPlot_SalesPerSqft.png", plot = ny_contour_plot, width = 12, height = 10, dpi = 300)
