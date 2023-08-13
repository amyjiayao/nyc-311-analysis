# Load required library
library(ggplot2)
library(dplyr)

# Read data
file_path <- "data/311_Service_Requests_from_2022.csv"
data <- read.csv(file_path)

# Process data
colnames(data) <- gsub(" ", ".", colnames(data)) # Replace space with period in column names

# Calculate the frequency of each complaint type
complaint_counts <- data %>%
  group_by(Complaint.Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Set the number of top complaint types to plot
top_n_complaints <- 30
top_n_complaints_data <- head(complaint_counts, top_n_complaints)

# Create bar chart
chart <- ggplot(data = top_n_complaints_data, aes(x = reorder(Complaint.Type, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = paste("Top", top_n_complaints, "Complaint Types Citywide"),
       x = "Complaint Type",
       y = "Number of Complaints") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the plot title

# Save image
ggsave(paste("Top_", top_n_complaints, "_Complaints_citywide.png", sep = ""), plot = chart, width = 8, height = 6, dpi = 300)
