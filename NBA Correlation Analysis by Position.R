# Load necessary libraries
library(readr)
library(dplyr)

# Step 1: Set the Test variable, MinGames variable, and output path
Test <- TRUE  # Set to TRUE to filter data for GSW, FALSE for the full dataset
MinGames <- 5  # Set the minimum number of games that players in a position must play together to be included
output_path <- "C:/Users/Luis Rodriguez/Desktop/Beat the HOUSE"  # Set the output path

# Step 2: Load the data
url <- "https://github.com/noah-po/Personal-Stuff/raw/main/Sports%20Stuff.csv"
sports_data <- read_csv(url, col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# Step 3: If Test is TRUE, filter the data for only the GSW team
if (Test) {
  sports_data <- sports_data %>% filter(Team == "GSW")
}

# Step 4: Data Preprocessing - Rename and select relevant columns
filtered_data <- sports_data %>%
  rename(PTS = `PTS...3`) %>%  # Rename PTS...3 to PTS
  select(Date, Pos., MP, PTS, TRB, AST, FG, FGA, STL, BLK) %>%
  filter(MP >= 10) # Filter to include only players who played at least 10 minutes

# Step 5: Aggregate Data by Position
position_data <- filtered_data %>%
  group_by(Date, Pos.) %>%
  summarise(
    PTS = sum(PTS, na.rm = TRUE),
    TRB = sum(TRB, na.rm = TRUE),
    AST = sum(AST, na.rm = TRUE),
    FG = sum(FG, na.rm = TRUE),
    FGA = sum(FGA, na.rm = TRUE),
    STL = sum(STL, na.rm = TRUE),
    BLK = sum(BLK, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 6: Calculate Correlations Across Positions and All Stat Combinations
calculate_position_correlations <- function(data) {
  results <- data.frame(Position1 = character(), Stat1 = character(), Position2 = character(), Stat2 = character(), Correlation = numeric(), n = integer())
  
  positions <- unique(data$Pos.)
  stats <- c("PTS", "TRB", "AST", "FG", "FGA", "STL", "BLK")
  
  # Compare each pair of positions
  for (i in 1:(length(positions) - 1)) {
    for (j in (i + 1):length(positions)) {
      position1 <- positions[i]
      position2 <- positions[j]
      
      # Filter data for the two positions
      position1_data <- data %>% filter(Pos. == position1)
      position2_data <- data %>% filter(Pos. == position2)
      
      # Merge data by Date to ensure they played in the same games
      merged_data <- merge(position1_data, position2_data, by = "Date", suffixes = c("_1", "_2"))
      n <- nrow(merged_data)  # Count the number of shared games
      
      # Calculate correlations for each combination of stats if they meet the minimum game threshold
      if (n >= MinGames) {
        for (stat1 in stats) {
          for (stat2 in stats) {
            pos1_stat <- merged_data[[paste0(stat1, "_1")]]
            pos2_stat <- merged_data[[paste0(stat2, "_2")]]
            
            # Calculate correlation if there are enough data points
            if (length(unique(c(pos1_stat, pos2_stat))) > 1) {
              corr_value <- cor(pos1_stat, pos2_stat, method = "spearman", use = "complete.obs")
            } else {
              corr_value <- NA
            }
            
            # Add results to the data frame
            results <- rbind(results, data.frame(Position1 = position1, Stat1 = stat1, Position2 = position2, Stat2 = stat2, Correlation = corr_value, n = n))
          }
        }
      }
    }
  }
  
  return(results)
}

# Step 7: Calculate Adjusted R^2 Across Positions and All Stat Combinations
calculate_position_adjusted_r_squared <- function(data) {
  results <- data.frame(Position1 = character(), Stat1 = character(), Position2 = character(), Stat2 = character(), R_squared = numeric(), Adjusted_R_squared = numeric(), n = integer())
  
  positions <- unique(data$Pos.)
  stats <- c("PTS", "TRB", "AST", "FG", "FGA", "STL", "BLK")
  
  # Compare each pair of positions
  for (i in 1:(length(positions) - 1)) {
    for (j in (i + 1):length(positions)) {
      position1 <- positions[i]
      position2 <- positions[j]
      
      # Filter data for the two positions
      position1_data <- data %>% filter(Pos. == position1)
      position2_data <- data %>% filter(Pos. == position2)
      
      # Merge data by Date to ensure they played in the same games
      merged_data <- merge(position1_data, position2_data, by = "Date", suffixes = c("_1", "_2"))
      n <- nrow(merged_data)  # Count the number of shared games
      
      # Calculate Adjusted R^2 for each combination of stats if they meet the minimum game threshold
      if (n >= MinGames) {
        for (stat1 in stats) {
          for (stat2 in stats) {
            pos1_stat <- merged_data[[paste0(stat1, "_1")]]
            pos2_stat <- merged_data[[paste0(stat2, "_2")]]
            
            # Calculate Adjusted R^2 if there are enough data points
            if (length(unique(c(pos1_stat, pos2_stat))) > 1) {
              model <- lm(pos1_stat ~ pos2_stat)
              r_squared <- summary(model)$r.squared
              adj_r_squared <- summary(model)$adj.r.squared
            } else {
              r_squared <- NA
              adj_r_squared <- NA
            }
            
            # Add results to the data frame
            results <- rbind(results, data.frame(Position1 = position1, Stat1 = stat1, Position2 = position2, Stat2 = stat2, R_squared = r_squared, Adjusted_R_squared = adj_r_squared, n = n))
          }
        }
      }
    }
  }
  
  return(results)
}

# Step 8: Apply the functions to calculate correlations and Adjusted R^2 across all games and all stat combinations
position_correlations <- calculate_position_correlations(position_data)
position_adjusted_r_squared <- calculate_position_adjusted_r_squared(position_data)

# Step 9: Output Minimum and Maximum Dates
min_date <- min(filtered_data$Date, na.rm = TRUE)
max_date <- max(filtered_data$Date, na.rm = TRUE)

cat("Minimum Date:", as.character(min_date), "\n")
cat("Maximum Date:", as.character(max_date), "\n")

# Step 10: Identify Most Positive and Negative Correlations/Adjusted R^2
# For Correlations
position_correlations <- position_correlations %>%
  filter(!is.na(Correlation)) %>%
  arrange(desc(Correlation))

# Top Positive Correlations
top_positive_correlations <- head(position_correlations, 10)

# Top Negative Correlations
top_negative_correlations <- head(position_correlations %>% arrange(Correlation), 10)

# For Adjusted R^2
position_adjusted_r_squared <- position_adjusted_r_squared %>%
  filter(!is.na(Adjusted_R_squared)) %>%
  arrange(desc(Adjusted_R_squared))

# Top Positive Adjusted R_squared
top_positive_adjusted_r_squared <- head(position_adjusted_r_squared, 10)

# Top Negative Adjusted R_squared (even though Adjusted R_squared can't be negative, it could be useful to see the lowest values)
top_negative_adjusted_r_squared <- head(position_adjusted_r_squared %>% arrange(Adjusted_R_squared), 10)

# Output the results
cat("\nTop Positive Position Correlations:\n")
print(top_positive_correlations)

cat("\nTop Negative Position Correlations:\n")
print(top_negative_correlations)

cat("\nTop Positive Adjusted R_squared for Positions:\n")
print(top_positive_adjusted_r_squared)

cat("\nLowest Adjusted R_squared for Positions:\n")
print(top_negative_adjusted_r_squared)

# Step 11: Save the filtered data and results to CSV files
write.csv(filtered_data, file = paste0(output_path, "/position_filtered_data.csv"), row.names = FALSE)
write.csv(position_correlations, file = paste0(output_path, "/position_correlation_results.csv"), row.names = FALSE)
write.csv(position_adjusted_r_squared, file = paste0(output_path, "/position_adjusted_r_squared_results.csv"), row.names = FALSE)
