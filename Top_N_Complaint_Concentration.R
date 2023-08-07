# Load packages
library(ggmap)
library(ggplot2)
library(dplyr)

# Read data
file_path <- "data/311_Service_Requests_from_2022.csv"
data <- read.csv(file_path)

# Process data
colnames(data) <- gsub(" ", ".", colnames(data)) # Replace space with period in column names
data <- data %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) # Remove rows with null values

# Set the number of top complaints to plot
num_top_complaints <- 30

# Create a data frame to plot complaint type counts
complaint_counts <- data %>%
  group_by(Complaint.Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(num_top_complaints)

# Filter the data for the top N complaint types
top_complaint_data <- data %>%
  filter(Complaint.Type %in% complaint_counts$Complaint.Type)

# Remove special characters from name
remove_special_chars <- function(x) { gsub("[[:punct:]]", "_", x) }
top_complaint_data$Complaint.Type <- remove_special_chars(top_complaint_data$Complaint.Type)

# Calculate the total count for each complaint type
complaint_total_count <- top_complaint_data %>%
  group_by(Complaint.Type) %>%
  summarise(Total_Count = n())  # Change Count to n

# Create NYC Map
ny_map <- get_stamenmap(bbox = c(left = -74.3, bottom = 40.4, right = -73.6, top = 40.95), maptype = "terrain-background")

# Create plot for each complaint type
for (complaint_type in unique(top_complaint_data$Complaint.Type)) {
  plot_data <- top_complaint_data %>%
    filter(Complaint.Type == complaint_type)
  
  if (nrow(plot_data) > 0) {
    ny_plot <- ggmap(ny_map) +
      geom_density_2d_filled(data = plot_data, aes(x = Longitude, y = Latitude, fill = factor(after_stat(level))), alpha = 0.7) +
      scale_fill_viridis_d() +  # Using a discrete fill scale
      labs(title = paste("Geo Concentration Map for Complaint Type:", complaint_type),
           subtitle = paste("Total Count:", complaint_total_count$Total_Count[complaint_total_count$Complaint.Type == complaint_type]),
           fill = "Density Level") +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Save image
    ggsave(paste("Complaint_Concentration_", complaint_type, ".png", sep = ""), plot = ny_plot, width = 12, height = 10, dpi = 300)
  } else {
    cat("No data available for Complaint Type:", complaint_type, "\n")
  }
}
