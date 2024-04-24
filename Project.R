# Jaineel Pravin Parmar
# SUID: 216033843
# IST 719: POSTER R CODE


## STEP 1 - LOADING THE REQUIRED LIBRARIES


# 'dplyr' is used for data manipulation and transformation, providing a set of verbs like filter, arrange, and mutate
library(dplyr)

# 'ggplot2' is a plotting system for R, based on the grammar of graphics, which allows to create complex multi-layered graphics
library(ggplot2)

# 'tidyr' is used for data tidying, helping to create tidy data, which is easier to manipulate, model and visualize
library(tidyr) 

# 'readr' package in R is primarily used for importing data into R.
library(readr)

# 'viridisLite' package in R enables use of viridis color scales.
library(viridisLite)

# 'forcats' package in R facilitates handling of categorical data.
library(forcats)

# 'scales' package in R provides functions for customizing plot scales.
library(scales)

# 'gridExtra' package in R enhances layout options for grid graphics.
library(gridExtra)

# 'png' package in R allows reading and writing PNG images.
library(png)

# 'grid' package in R offers low-level control over grid graphics.
library(grid)



## STEP 2 - LOADING THE DATA 


# Table 1
table <- file.choose()
tb <- read.csv(table)
View(tb)
unique(tb$Team)

# Table 2
players <- file.choose()
plyrs <- read.csv(players)
View(plyrs)

# Table 3
goalkeepers <- file.choose()
gk <- read.csv(goalkeepers)
View(gk)



## STEP 3 - CREATING PLOTS


# PLOT 1 - MAIN PLOT
# convert column into numeric form. 
tb$Pts <- as.numeric(as.character(tb$Pts))

# Sum total points for each team and identify the top 10 teams
team_points <- tb %>%
  group_by(Club) %>%
  summarize(Total_Points = sum(Pts, na.rm = TRUE)) %>%
  arrange(desc(Total_Points)) %>%
  slice_head(n = 10) %>%
  ungroup()

# Find the top scorer for each top team
top_scorers <- plyrs %>%
  group_by(Club) %>%
  summarize(Top_Scorer_Goals = max(G, na.rm = TRUE)) %>%
  filter(Club %in% team_points$Club) %>%
  ungroup()

# Find the top goalkeeper for each top team
top_goalkeepers <- gk %>%
  group_by(Club) %>%
  summarize(Top_Goalkeeper_Saves = max(SV, na.rm = TRUE)) %>%
  filter(Club %in% team_points$Club) %>%
  ungroup()

# Merge all data
combined_data <- team_points %>%
  left_join(top_scorers, by = "Club") %>%
  left_join(top_goalkeepers, by = "Club")

# Define the light golden color for the background
light_golden <- "#fff3cc"

# Define a vector of custom dark colors for the clubs
dark_colors <- c("#00008B", "#006400", "#8B0000", "#FF8C00", "#800080", "#A9A9A9", "#008B8B", "#8B008B", "#B8860B", "#A52A2A")

# Create the plot with the desired adjustments
ggplot(combined_data, aes(x = Top_Scorer_Goals, y = Top_Goalkeeper_Saves, size = Total_Points, color = Club)) +
  geom_point(alpha = 0.7) +  # Apply alpha for point transparency
  scale_size(range = c(6, 12)) +  # Adjust the range to increase the size of the points
  scale_color_manual(values = dark_colors) +  # Use the custom dark colors
  labs(title = "Top 10 MLS Teams: Scorer and Goalkeeper Impact on Points",
       subtitle = "Scatter plot of goals by top scorers and saves by top goalkeepers against total team points",
       x = "Top Scorer Goals",
       y = "Top Goalkeeper Saves") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),  # Bold and larger plot title
    plot.subtitle = element_text(face = "bold"),  # Bold plot subtitle
    axis.text = element_text(face = "bold", size = 12),  # Bold axis text
    axis.title = element_text(face = "bold", size = 12),  # Bold axis titles
    panel.grid.major = element_line(linewidth = 1, color = "grey30"),  # Darker and thicker major grid lines
    panel.grid.minor = element_line(linewidth = 0.5, color = "grey30"),  # Darker and thicker minor grid lines
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(face = "bold"),   # Bold legend text
    plot.background = element_rect(fill = light_golden, color = NA), # Set light golden as the background color
    panel.background = element_rect(fill = light_golden, color = NA) # Set light golden as the panel color
  ) +
  guides(color = guide_legend(title = "Club"), size = guide_legend(title = "Total Points"))  # Add legends for color and size



# Define a light color palette
light_palette <- c("#fff3cc", "#ffdf80", "#ffb84d", "#ff9933", "#e67300")



# PLOT 2 -	Top 5 Goal Scorers                      
# Calculate the top 5 goal scorers and reorder them
top_scorers <- plyrs %>%
  group_by(Player) %>%
  summarize(Total_Goals = sum(G, na.rm = TRUE)) %>%
  arrange(desc(Total_Goals)) %>%
  slice_head(n = 5) %>%
  mutate(Player = fct_reorder(Player, Total_Goals)) %>% # Order players by ascending goals
  ungroup()

# Create a curved bar plot for the top scorers with light colors
scorers_plot <- ggplot(top_scorers, aes(x = Player, y = Total_Goals, fill = Player)) +
  geom_bar(stat = "identity", width = 0.95) +
  coord_polar(theta = "y") +
  ylim(c(0, max(top_scorers$Total_Goals) * 1.1)) + # Extend the y-limits to fit the text
  xlab("") + ylab("") +
  geom_text(aes(label = Total_Goals), position = position_stack(vjust = 1.05),
            size = 6, color = "black", fontface = "bold") + # Increased size, bold font
  scale_fill_manual(values = light_palette) + # Use the light color palette
  theme_void() +
  labs(title = "Top 5 Goal Scorers in MLS") +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        legend.position = "right", # Move legend to the right of the plot
        legend.text = element_text(size = 12, face = "bold"), # Increase size and bold the legend text
        legend.title = element_text(size = 12, face = "bold")) # Increase size and bold the legend title

# Print the plot for Plot 2
print(scorers_plot)



# PLOT 3 -	Top 5 Saves by GoalKeepers
# Calculate the top 5 goalkeepers with most saves and reorder them
top_goalkeepers <- gk %>%
  group_by(Player) %>%
  summarize(Total_Saves = sum(SV, na.rm = TRUE)) %>%
  arrange(desc(Total_Saves)) %>%
  slice_head(n = 5) %>%
  mutate(Player = fct_reorder(Player, Total_Saves)) %>% # Order by ascending Total_Saves
  ungroup()

# Create a curved bar plot for the top goalkeepers
goalkeepers_plot <- ggplot(top_goalkeepers, aes(x = Player, y = Total_Saves, fill = Player)) +
  geom_bar(stat = "identity", width = 0.95) + 
  coord_polar(theta = "y") + 
  ylim(c(0, max(top_goalkeepers$Total_Saves) * 1.1)) +
  xlab("") + ylab("") +
  geom_text(aes(label = Total_Saves), position = position_stack(vjust = 1.05),
            size = 6, color = "black", fontface = "bold") +
  scale_fill_manual(values = light_palette) + # light_palette should be defined previously
  theme_void() + 
  labs(title = "Top 5 Goalkeepers by Saves in MLS") +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        legend.position = "right", # Move legend to the right of the plot
        legend.text = element_text(size = 12, face = "bold"), # Increase size and bold the legend text
        legend.title = element_text(size = 12, face = "bold")) # Increase size and bold the legend title

# Print the plot for Plot 3
print(goalkeepers_plot)



# Plot 4 -	Teams with Highest Red Cards and Yellow Cards 
# Remove rows with blank or NA Club names
clean_data <- plyrs %>%
  filter(Club != "" & !is.na(Club))

# Prepare summaries for yellow and red cards
yellow_cards_summary <- clean_data %>%
  group_by(Club) %>%
  summarise(Yellow_Cards = sum(YC, na.rm = TRUE)) %>%
  arrange(desc(Yellow_Cards)) %>%
  slice_head(n = 5)  # Select the top 5

red_cards_summary <- clean_data %>%
  group_by(Club) %>%
  summarise(Red_Cards = sum(RC, na.rm = TRUE)) %>%
  arrange(desc(Red_Cards)) %>%
  slice_head(n = 5)  # Select the top 5

# Split the data into two plots
yellow_data <- yellow_cards_summary 
yellow_data$Type = c("Yellow", "Yellow", "Yellow", "Yellow", "Yellow")

red_data <- red_cards_summary
red_data$Type = c("Red", "Red", "Red", "Red", "Red")

# Define color palette
my.cols <- c("#ffdf80", "#e67300")

# Plot for yellow cards with labels
p_yellow <- ggplot(yellow_data, aes(x = Club, y = Yellow_Cards, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Yellow_Cards), vjust = -0.5, size = 3, color = "black") + # Add labels
  scale_fill_manual(values = my.cols[1]) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) + # Remove minor grid lines
  labs(y = "Number of Yellow Cards") +
  guides(fill=FALSE) + 
  labs(title = "Top 5 Teams with the Highest Yellow and Red Cards") +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold")) # Bold title

# Plot for red cards with labels
p_red <- ggplot(red_data, aes(x = Club, y = Red_Cards, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Red_Cards), vjust = -0.5, size = 3, color = "black") + # Add labels
  scale_fill_manual(values = my.cols[2]) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) + # Remove minor grid lines
  labs(y = "Number of Red Cards") +
  guides(fill=FALSE)

# Arrange the plots 
grid.arrange(p_yellow, p_red, ncol = 1, heights = c(3.2, 2.6))



# Plot 5 - Players with Highest Red Cards and Yellow Cards and their clubs
# Prepare summaries for yellow and red cards by both player and club
yellow_data <- clean_data %>%
  group_by(Player, Club) %>%
  summarise(Yellow_Cards = sum(YC, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Yellow_Cards)) %>%
  slice_head(n = 5) %>%
  mutate(Type = "Yellow")

red_data <- clean_data %>%
  group_by(Player, Club) %>%
  summarise(Red_Cards = sum(RC, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Red_Cards)) %>%
  slice_head(n = 5) %>%
  mutate(Type = "Red")

# Define color palette
my.cols <- c("#ffdf80", "#e67300")

# Plot for yellow cards with labels
p_yellow <- ggplot(yellow_data, aes(x = Player, y = Yellow_Cards, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Yellow_Cards), vjust = -0.5, size = 3, color = "black") + # Add labels
  scale_fill_manual(values = my.cols[1]) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),  # Adjust text angle and alignment
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) + # Remove minor grid lines
  labs(y = "Number of Yellow Cards") + 
  labs(title = "Top 5 Players with the Highest Yellow and Red Cards") +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold")) # Bold title

# Plot for red cards with labels
p_red <- ggplot(red_data, aes(x = Player, y = Red_Cards, fill = Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Red_Cards), vjust = -0.5, size = 3, color = "black") + # Add labels
  scale_fill_manual(values = my.cols[2]) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),  # Adjust text angle and alignment
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) + # Remove minor grid lines
  labs(y = "Number of Red Cards")

# Arrange the plots 
grid.arrange(p_yellow, p_red, ncol = 1, heights = c(3.2, 2.6))



# Plot 6 - Clubs with the Highest Titles and their years 
# Filter for overall champions and count titles for each team
df <- tb[tb$Pos == 1 & tb$Conference == 'Overall',]
title_counts <- table(df$Team)

# Get the top 5 teams
top_teams <- names(sort(title_counts, decreasing = TRUE)[1:5])

# Filter the data to include only the top 5 teams
df <- df[df$Team %in% top_teams,]

# Read the trophy image
trophy_image <- readPNG("/Users/jaineelparmar/Desktop/SU/IST 719 Info Viz/Project/trophy.png")


trophy_image_dark <- trophy_image
trophy_image_dark[,,4] <- trophy_image_dark[,,4] * 0.5  # Reduce the alpha channel by half

# Define a light golden color
light_golden <- "#fff3cc"

# Create a base ggplot with a light golden background color
p <- ggplot(df, aes(x = Year, y = Team)) +
  geom_blank() + # This is to set up the plot space
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) + # Set x breaks every 5 years
  theme_minimal() +
  labs(title = 'Annual MLS Championship Titles for the 5 Most Successful Teams') +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), # Bold title 
    axis.text = element_text(face = "bold", size = 12), # Bold axis text
    axis.title = element_text(face = "bold", size = 12), # Bold axis title
    panel.grid.major = element_line(color = "grey20", size = 1), # Bold grid lines
    panel.grid.minor = element_line(color = "grey20", size = 0.5), # Minor grid lines if needed
    panel.background = element_rect(fill = light_golden, colour = light_golden), # Panel background color
  )

p <- p + theme(
  plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), # Bold title 
  axis.text = element_text(face = "bold", size = 12), # Bold axis text
  axis.title = element_text(face = "bold", size = 12), # Bold axis title
  panel.grid.major = element_line(color = "grey20", size = 1), # Bold grid lines
  panel.grid.minor = element_line(color = "grey20", size = 0.5), # Minor grid lines if needed
  panel.background = element_rect(fill = light_golden, colour = light_golden), # Set the panel background color
  plot.background = element_rect(fill = light_golden, colour = NA) # Remove dark plot background color
)

# Calculate the y positions for the teams
team_levels <- as.numeric(factor(df$Team, levels = rev(top_teams)))

# Adjust the size of the trophy images and make them darker
# Here we are increasing the size of the bounding box for the images to make them larger
for(i in 1:nrow(df)) {
  p <- p + annotation_custom(
    rasterGrob(trophy_image, interpolate = TRUE), 
    xmin = df$Year[i] - 0.5, xmax = df$Year[i] + 0.5, # Increase the size of the image horizontally
    ymin = team_levels[i] - 0.5, ymax = team_levels[i] + 0.5) # Increase the size of the image vertically
}

# Print the plot
print(p)



