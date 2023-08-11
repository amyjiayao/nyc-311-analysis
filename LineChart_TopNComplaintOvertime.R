# Load packages
library(ggplot2)

# Read data
file_path <- "data/311_Service_Requests_from_2022.csv"
data <- read.csv(file_path)

# Process data
colnames(data) <- gsub(" ", ".", colnames(data)) # Replace space with period in column names
data$Created.Date <- as.Date(data$Created.Date, format = "%m/%d/%Y") # Format date

# Set the number of top complaint types to plot
N <- 30
top_complaints <- names(sort(table(data$Complaint.Type), decreasing = TRUE))[1:N]

# Create plot for each of the top N complaint types
for (complaint_type in top_complaints) {
  filtered_data <- subset(data, Complaint.Type == complaint_type)
  complaint_counts <- aggregate(Complaint.Type ~ Created.Date, data = filtered_data, FUN = length)
  
  plot <- ggplot(complaint_counts, aes(x = Created.Date, y = Complaint.Type)) +
    geom_line() +
    geom_point(color = "black", alpha = 0.5) +  # Add scatter plot layer
    labs(title = paste("Complaint Type Count Over Time -", complaint_type),
         x = "Created Date", 
         y = "Complaint Type Count") +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
    theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
  
  cleaned_complaint_name <- gsub("[[:punct:]]", "_", complaint_type) # Remove special characters from name
  
  # Save image
  file_name <- paste("Complaint_Type_Count_", cleaned_complaint_name, ".png", sep = "")
  ggsave(file_name, plot, width = 10, height = 6, dpi = 300)
}
