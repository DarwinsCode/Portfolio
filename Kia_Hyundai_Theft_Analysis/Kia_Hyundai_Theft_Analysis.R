# Assignment: ASSIGNMENT 5
# Class: DSC 640
# Name: Saenz, Nicolas
# Date: 2025-07-07

## Kia and Hyundai theft data
install.packages("readxl")
install.packages("treemapify")
library(ggplot2)
library(dplyr)
library(purrr)
library(foreign)
library(readxl)
library(treemapify)
library(RColorBrewer)




setwd("C:/Users/Nick/Desktop/DSC 640")

#Read in data sets
df1 <- read.csv("carTheftsMap.csv")
df2 <- read.csv("KiaHyundaiMilwaukeeData.csv")
df3 <- read.csv("kiaHyundaiThefts.csv")
df4 <- read_excel("Motherboard VICE News Kia Hyundai Theft Data.xlsx")


Pieselected <- df1 %>% select(countCarThefts2019, countCarThefts2020, countCarThefts2021, countCarThefts2022) %>% mutate(across(everything(), as.integer))

sum_of_2019 <- sum(Pieselected$countCarThefts2019, na.rm = TRUE)
sum_of_2020 <- sum(Pieselected$countCarThefts2020, na.rm = TRUE)
sum_of_2021 <- sum(Pieselected$countCarThefts2021, na.rm = TRUE)
sum_of_2022 <- sum(Pieselected$countCarThefts2022, na.rm = TRUE)

# Create a new data frame suitable for the pie chart 
# A pie chart needs a column for the categories (Year) and a column for the values (Thefts).
theft_data <- data.frame(
  Year = c("2019", "2020", "2021", "2022"),
  Thefts = c(sum_of_2019, sum_of_2020, sum_of_2021, sum_of_2022)
)

#Add percentage and label position columns
theft_data <- theft_data %>%
  mutate(
    Percentage = Thefts / sum(Thefts),
    label_text = paste0(round(Percentage * 100), "%"),
    label_pos = cumsum(Percentage) - 0.5 * Percentage
  )


#Create the Pie Chart
ggplot(theft_data, aes(x = "", y = Percentage, fill = Year)) +
  # Create a stacked bar chart. width=1 makes it a full circle.
  geom_bar(width = 1, stat = "identity", color = "white") +
  
  # Convert the bar chart to polar coordinates to make it a pie chart.
  coord_polar("y", start = 0) +
  
  # Add the percentage labels to the chart
  geom_text(aes(y = label_pos, label = label_text), color = "white", size = 5) +
  
  # Use a minimal theme that's perfect for pie charts
  theme_void() +
  
  # Add titles and customize the legend
  labs(
    title = "Distribution of Car Thefts by Year",
    subtitle = "2019-2022",
    fill = "Year"
  ) +
  
  # Optional: Use a nice color palette
  scale_fill_brewer(palette = "Pastel1")






#alter theft data using df source and create a donut chart
theft_data3 <- theft_data %>%
  mutate(
    Percentage = Thefts / sum(Thefts),
    label_text = paste0(round(Percentage * 100), "%")
  )


# Create the Donut Chart with ggplot2
# We use geom_bar and coord_polar. The "hole" is created by setting the x-axis limits.
ggplot(theft_data3, aes(x = 2, y = Percentage, fill = Year)) +
  # Create the bar. `stat="identity"` means use the values from the 'y' aesthetic.
  geom_bar(stat = "identity", color = "white") +
  
  # Convert the bar chart to polar coordinates to make it a pie/donut.
  coord_polar(theta = "y", start = 0) +
  
  # Add the percentage labels to the chart
  # We adjust the `x` aesthetic to place the labels inside the donut ring.
  geom_text(aes(y = label_pos, label = label_text), color = "white", size = 5, x = 2) +
  
  # Use a minimal theme that's perfect for this kind of chart
  theme_void() +
  
  # Set the x-axis limits to create the "hole" in the middle.
  # The space between 0.5 and 2.5 defines the donut's ring.
  xlim(0.5, 2.5) +
  
  # Add titles and customize the legend, as in the user's example
  labs(
    title = "Distribution of Car Thefts by Year",
    subtitle = "2019-2022",
    fill = "Year"
  ) +
  
  # Center the plot title
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  
  # Use a nice color palette
  scale_fill_brewer(palette = "Pastel1")




donut_data <- df2 %>%
  # Ensure the 'year' column is treated as a factor for grouping
  mutate(year = as.factor(year)) %>%
  # Group by the 'year' column
  group_by(year) %>%
  # Calculate the total thefts for each year
  summarise(total_thefts = sum(countKiaHyundaiThefts, na.rm = TRUE)) %>%
  # Ungroup to prevent issues in subsequent steps
  ungroup() %>%
  # Arrange the data by year for consistent plotting
  arrange(year) %>%
  # Add helper columns for the ggplot donut chart
  mutate(
    fraction = total_thefts / sum(total_thefts),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_position = (ymax + ymin) / 2,
    label_text = paste0(year, "\n", format(total_thefts, big.mark = ","))
  )

# Create the Donut Chart with ggplot2
ggplot(donut_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = year)) +
  # Create the rectangular segments that will form the donut
  geom_rect() +
  # Add the text labels inside each segment of the donut chart
  geom_text(aes(x = 3.5, y = label_position, label = label_text), color = "white", size = 4) +
  # Convert the bar chart to polar coordinates to create a pie/donut
  coord_polar(theta = "y") +

  xlim(c(2, 4.5)) +
  # Apply a color scheme. You can change this to other Brewer palettes like "Set2", "Pastel1", etc.
  scale_fill_brewer(palette = "Pastel2") +
  # Use a minimal theme to remove background, axes, and gridlines for a cleaner look
  theme_void() +
  # Customize the theme elements
  theme(
    legend.position = "none",
    # Add a title and center it
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  # Add the main title for the chart
  labs(title = "Total Kia & Hyundai Thefts in Milwaukee by Year")


# Make a tree map of all hyundai and kia thefts by state

state_theft_summary <- df3 %>%
  group_by(state) %>%
  summarise(total_thefts = sum(countKiaHyundaiThefts, na.rm = TRUE)) %>%
  # It's good practice to remove any states with zero thefts to keep the chart clean.
  filter(total_thefts > 0) %>%
  # Arrange the data from most to least thefts for clarity, though not required for the plot.
  arrange(desc(total_thefts))

# Create the Treemap
ggplot(state_theft_summary, aes(area = total_thefts, fill = total_thefts, label = state)) +
  # geom_treemap creates the rectangles. The 'area' aesthetic determines their size.
  geom_treemap() +
  
  # geom_treemap_text adds the state labels inside the rectangles.
  # We'll make the text bold, white, and ensure it grows to fit the space.
  geom_treemap_text(
    fontface = "bold",
    color = "white",
    place = "centre", # Center the text
    grow = TRUE      # Allow text to grow with the rectangle size
  ) +
  
  # Customize the color scale for the fill.
  # A gradient from a light blue to a dark blue looks professional.
  scale_fill_gradient(low = "#a3d0e5", high = "#1c4966", name = "Total Thefts") +
  
  # Add informative titles and a subtitle.
  labs(
    title = "Kia & Hyundai Thefts by State",
    subtitle = "The size of each rectangle represents the total number of reported thefts",
    caption = "Data source: kiaHyundaiThefts.csv"
  ) +
  
  # Apply a minimal theme to remove distracting background elements.
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9))



# Stacked Bar chart of all hyundai and kia thefts by state


top_10_states <- df3 %>%
  group_by(state) %>%
  summarise(grand_total = sum(countKiaHyundaiThefts, na.rm = TRUE)) %>%
  top_n(10, grand_total) %>%
  arrange(desc(grand_total)) %>%
  pull(state) # pull() extracts the 'state' column as a vector

# Now, prepare the data for the stacked bar chart.
# We will group all other states into an "Other" category.
yearly_summary <- df3 %>%
  mutate(
    # Create a new column for plotting: if the state is in our top 10, use its name, otherwise use "Other".
    state_plot = factor(
      ifelse(state %in% top_10_states, as.character(state), "Other"),
      # Set the order of the levels for a more organized legend.
      levels = c(top_10_states, "Other")
    )
  ) %>%
  # Group by year and our new state category.
  group_by(year, state_plot) %>%
  # Calculate the total thefts for each group.
  summarise(total_thefts = sum(countKiaHyundaiThefts, na.rm = TRUE), .groups = 'drop') %>%
  # Remove any rows where theft count is zero.
  filter(total_thefts > 0)


# Create the Stacked Bar Chart

# We need 11 colors (10 for states + 1 for "Other").
color_palette <- c(brewer.pal(10, "Paired"), "grey70")

ggplot(yearly_summary, aes(x = factor(year), y = total_thefts, fill = state_plot)) +
  # geom_col() creates the bars. position="stack" is the default.
  geom_col(position = "stack", color = "white", linewidth = 0.5) +
  
  # Apply our custom color palette.
  scale_fill_manual(values = color_palette, name = "State") +
  
  # Add informative labels and titles.
  labs(
    title = "Annual Kia & Hyundai Thefts by State",
    subtitle = "Showing the top 10 states; all others are grouped into 'Other'",
    x = "Year",
    y = "Total Number of Thefts",
    caption = "Data source: kiaHyundaiThefts.csv"
  ) +
  
  # Apply a clean, professional theme.
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1) # Angle x-axis labels for readability
  )


#Stacked Area chart using the same data

# Prepare the data for the chart, grouping smaller states into "Other"
yearly_summary <- df3 %>%
  mutate(
    state_plot = factor(
      ifelse(state %in% top_10_states, as.character(state), "Other"),
      levels = c(top_10_states, "Other")
    )
  ) %>%
  group_by(year, state_plot) %>%
  summarise(total_thefts = sum(countKiaHyundaiThefts, na.rm = TRUE), .groups = 'drop') %>%
  filter(total_thefts > 0)


#Create the Stacked Area Chart

# Define a color palette (10 for states + 1 for "Other").
color_palette <- c(brewer.pal(10, "Paired"), "grey70")

ggplot(yearly_summary, aes(x = year, y = total_thefts, fill = state_plot, group = state_plot)) +
  geom_area(position = "stack", color = "white", linewidth = 0.5) +

  scale_fill_manual(values = color_palette, name = "State") +

  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(yearly_summary$year)))) +

  labs(
    title = "Annual Kia & Hyundai Thefts by State",
    subtitle = "Showing the top 10 states; all others are grouped into 'Other'",
    x = "Year",
    y = "Total Number of Thefts",
    caption = "Data source: kiaHyundaiThefts.csv"
  ) +
  
  # Apply a clean, professional theme.
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
