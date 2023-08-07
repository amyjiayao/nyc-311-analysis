# Load required library
library(ggplot2)
library(dplyr)

# Read data
file_path <- "data/311_Service_Requests_from_2022.csv"
data <- read.csv(file_path)

# Process data
colnames(data) <- gsub(" ", ".", colnames(data)) # Replace space with period in column names
filtered_data <- data %>%
  filter(Borough != "Unspecified") # Filter data to exclude "Unspecified" borough

# Set the number of top complaint types to plot
top_n_complaints <-30 

# Calculate the frequency of each complaint type per borough
complaint_counts_by_borough <- filtered_data %>%
  group_by(Borough, Complaint.Type) %>%
  summarise(Count = n()) %>%
  arrange(Borough, desc(Count))

# Get the top N complaint types per borough
top_n_complaints_data <- complaint_counts_by_borough %>%
  group_by(Borough) %>%
  top_n(top_n_complaints, Count)

# Create bar plots for the top N complaint types per borough
chart <- ggplot(data = top_n_complaints_data, aes(x = reorder(Complaint.Type, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = paste("Top", top_n_complaints, "Complaint Types in Each Borough"),
       x = "Complaint Type",
       y = "Number of Complaints") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~ Borough, ncol = 2) 

for (borough in unique(top_n_complaints_data$Borough)) {
  plot_data <- filter(top_n_complaints_data, Borough == borough)
  plot <- ggplot(data = plot_data, aes(x = reorder(Complaint.Type, -Count), y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Top", top_n_complaints, "Complaint Types in", borough),
         x = "Complaint Type", y = "Number of Complaints") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))  # Center the plot title
  
  # Save image
  ggsave(paste("Top_", top_n_complaints, "_Complaints_", gsub(" ", "_", tolower(borough)), ".png", sep = ""), plot = plot, width = 8, height = 6, dpi = 300)
}
