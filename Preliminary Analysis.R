install.packages("tidyverse") 
library(tidyverse)
df <- read.csv("/Users/arundhathiroy/Desktop/vgsales-12-4-2019.csv", stringsAsFactors = FALSE)
colSums(is.na(df))
df_clean <- na.omit(df)
df$Global_Sales[is.na(df$Global_Sales)] <- 0
df$NA_Sales[is.na(df$NA_Sales)] <- 0
df$PAL_Sales[is.na(df$PAL_Sales)] <- 0
df$JP_Sales[is.na(df$JP_Sales)] <- 0
df$Other_Sales[is.na(df$Other_Sales)] <- 0
df$Publisher[is.na(df$Publisher)] <- "Unknown"
df$Genre[is.na(df$Genre)] <- "Unknown"
df$Global_Sales[is.na(df$Global_Sales)] <- mean(df$Global_Sales, na.rm = TRUE)
df$Critic_Score[is.na(df$Critic_Score)] <- mean(df$Critic_Score, na.rm = TRUE)
df$User_Score[is.na(df$User_Score)] <- mean(df$User_Score, na.rm = TRUE)
colSums(is.na(df))

df <- df %>% select(-VGChartz_Score, -Vgchartzscore)
#year
df$Year[is.na(df$Year)] <- median(df$Year, na.rm = TRUE)  # Replace with median year
df$Total_Shipped[is.na(df$Total_Shipped)] <- 0  # Assuming missing means no shipment reported
colSums(is.na(df))

write.csv(df, "Cleaned_VG_Sales.csv", row.names = FALSE)
getwd()  

df_check <- read.csv("Cleaned_VG_Sales.csv")
head(df_check)  # View the first few rows

# Visulaizations

install.packages("tidyverse")  # For data manipulation & visualization
install.packages("ggplot2")    # For visualization
install.packages("dplyr")      # For data handling
install.packages("readr")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

# Load the cleaned dataset
df <- read_csv("/Users/arundhathiroy/Desktop/CapstoneProject/Cleaned_VG_Sales.csv")

# Filter for handheld platforms
handheld_platforms <- c("3DS", "DS", "GBA", "PSP", "PS Vita", "Switch")
df_handheld <- df %>% filter(Platform %in% handheld_platforms)

# Summarize total sales per publisher
handheld_sales <- df_handheld %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

# Select top 10 publishers in handheld sales
top_publishers <- handheld_sales %>% top_n(10, Total_Sales)


#visualization 1 : Handheld sales by publisher
ggplot(top_publishers, aes(x = reorder(Publisher, Total_Sales), y = Total_Sales, fill = Publisher)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Handheld Gaming Publishers by Sales",
       x = "Publisher", y = "Total Global Sales (in millions)") +
  theme_minimal()

#Visualization 2: Pie Chart - Market Share of Handheld Publishers


# Filter for handheld platforms
handheld_platforms <- c("3DS", "DS", "GBA", "PSP", "PS Vita", "Switch")
df_handheld <- df %>% filter(Platform %in% handheld_platforms)

# Summarize total sales per publisher
publisher_sales <- df_handheld %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)  # Select Top 10 Publishers

# Create a percentage column for labels
ggplot(publisher_sales, aes(x = "", y = Total_Sales, fill = Publisher)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to pie chart
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4) + 
  labs(title = "Market Share of Top Handheld Game Publishers") +
  theme_void() +  # Remove unnecessary grid lines
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  scale_fill_brewer(palette = "Paired")  # Use a nice color scheme

# Visualization 3 : Stacked Barchart - Sles by Platform for Top Publishers
# Filter handheld data for top publishers
df_top_handheld <- df_handheld %>% filter(Publisher %in% top_publishers$Publisher)

ggplot(df_top_handheld, aes(x = Publisher, y = Global_Sales, fill = Platform)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Sales by Platform for Top Handheld Publishers",
       x = "Publisher", y = "Total Global Sales (in millions)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


#visualization 4 : trend analysis
ggplot(df_handheld_trend, aes(x = Year, y = Total_Sales, color = Publisher, group = Publisher)) +
  geom_line(size = 1.2) +        # Line for each publisher
  geom_point(size = 3) +         # Points for each data point
  labs(title = "Handheld Gaming Sales Trend by Top Publishers",
       x = "Year",
       y = "Total Global Sales (in millions)",
       color = "Publisher") +
  theme_minimal() +              # Clean visualization style
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "top") +
  scale_y_continuous(labels = scales::comma)  # Format Y-axis for readability

#Table 1 Yearly Sales Trends for Handheld Devices

top_games <- df_handheld %>%
  select(Name, Platform, Publisher, Global_Sales) %>%
  arrange(desc(Global_Sales)) %>%
  head(10)

# Display table
print(top_games)
write_csv(top_games, "Top_Handheld_Games.csv")

#Table 2: Sales per Handheld Platform
platform_sales <- df_handheld %>%
  group_by(Platform) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

# Display table
print(platform_sales)
write_csv(platform_sales, "Handheld_Platform_Sales.csv")

#Total sales per publisher
publisher_sales <- df_handheld %>%
  group_by(Publisher) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

# Display table
print(publisher_sales)

write_csv(publisher_sales, "Top_Handheld_Publishers.csv")

