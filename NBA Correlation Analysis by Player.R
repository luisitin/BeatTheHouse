# Load necessary libraries
library(readr)
library(dplyr)

# Step 1: Set the Test variable, MinGames variable, and output path
Test <- TRUE  # Set to TRUE to filter data for GSW, FALSE for the full dataset
MinGames <- 5  # Set the minimum number of games players must play together to be included
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
  select(Player, Date, Team, Opp, MP, PTS, TRB, AST, FG, FGA, STL, BLK) %>%
  filter(MP >= 10) # Filter to include only players who played at least 10 minutes

# Step 5: Standardize the data (Z-score normalization)
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Apply standardization
standardized_data <- filtered_data %>%
  mutate(across(c(PTS, TRB, AST, FG, FGA, STL, BLK), standardize))

# Step 6: Calculate Spearman Rank Correlations Across All Shared Games and All Stat Combinations
calculate_spearman_correlation <- function(data) {
  results <- data.frame(Player1 = character(), Stat1 = character(), Player2 = character(), Stat2 = character(), SpearmanCorrelation = numeric(), n = integer())
  
  players <- unique(data$Player)
  stats <- c("PTS", "TRB", "AST", "FG", "FGA", "STL", "BLK")
  
  # Compare each pair of players
  for (i in 1:(length(players) - 1)) {
    for (j in (i + 1):length(players)) {
      player1 <- players[i]
      player2 <- players[j]
      
      # Filter data for the two players
      player1_data <- data %>% filter(Player == player1)
      player2_data <- data %>% filter(Player == player2)
      
      # Merge data by Date to ensure they played in the same games
      merged_data <- merge(player1_data, player2_data, by = "Date", suffixes = c("_1", "_2"))
      n <- nrow(merged_data)  # Count the number of shared games
      
      # Calculate Spearman correlations for each combination of stats if they meet the minimum game threshold
      if (n >= MinGames) {
        for (stat1 in stats) {
          for (stat2 in stats) {
            player1_stat <- merged_data[[paste0(stat1, "_1")]]
            player2_stat <- merged_data[[paste0(stat2, "_2")]]
            
            # Calculate Spearman correlation if there are enough data points
            if (length(unique(c(player1_stat, player2_stat))) > 1) {
              corr_value <- cor(player1_stat, player2_stat, method = "spearman", use = "complete.obs")
            } else {
              corr_value <- NA
            }
            
            # Add results to the data frame
            results <- rbind(results, data.frame(Player1 = player1, Stat1 = stat1, Player2 = player2, Stat2 = stat2, SpearmanCorrelation = corr_value, n = n))
          }
        }
      }
    }
  }
  
  return(results)
}

# Step 7: Calculate Adjusted R^2 Across All Shared Games and All Stat Combinations
calculate_adjusted_r_squared <- function(data) {
  results <- data.frame(Player1 = character(), Stat1 = character(), Player2 = character(), Stat2 = character(), R_squared = numeric(), Adjusted_R_squared = numeric(), n = integer())
  
  players <- unique(data$Player)
  stats <- c("PTS", "TRB", "AST", "FG", "FGA", "STL", "BLK")
  
  # Compare each pair of players
  for (i in 1:(length(players) - 1)) {
    for (j in (i + 1):length(players)) {
      player1 <- players[i]
      player2 <- players[j]
      
      # Filter data for the two players
      player1_data <- data %>% filter(Player == player1)
      player2_data <- data %>% filter(Player == player2)
      
      # Merge data by Date to ensure they played in the same games
      merged_data <- merge(player1_data, player2_data, by = "Date", suffixes = c("_1", "_2"))
      n <- nrow(merged_data)  # Count the number of shared games
      
      # Calculate Adjusted R^2 for each combination of stats if they meet the minimum game threshold
      if (n >= MinGames) {
        for (stat1 in stats) {
          for (stat2 in stats) {
            player1_stat <- merged_data[[paste0(stat1, "_1")]]
            player2_stat <- merged_data[[paste0(stat2, "_2")]]
            
            # Calculate Adjusted R^2 if there are enough data points
            if (length(unique(c(player1_stat, player2_stat))) > 1) {
              model <- lm(player1_stat ~ player2_stat)
              r_squared <- summary(model)$r.squared
              adj_r_squared <- summary(model)$adj.r.squared
            } else {
              r_squared <- NA
              adj_r_squared <- NA
            }
            
            # Add results to the data frame
            results <- rbind(results, data.frame(Player1 = player1, Stat1 = stat1, Player2 = player2, Stat2 = stat2, R_squared = r_squared, Adjusted_R_squared = adj_r_squared, n = n))
          }
        }
      }
    }
  }
  
  return(results)
}

# Step 8: Apply the functions to calculate Spearman correlations and Adjusted R^2 across all games and all stat combinations
spearman_results <- calculate_spearman_correlation(standardized_data)
adjusted_r_squared_results <- calculate_adjusted_r_squared(filtered_data)

# Step 9: Output Minimum and Maximum Dates
min_date <- min(filtered_data$Date, na.rm = TRUE)
max_date <- max(filtered_data$Date, na.rm = TRUE)

cat("Minimum Date:", as.character(min_date), "\n")
cat("Maximum Date:", as.character(max_date), "\n")

# Step 10: Identify Most Positive and Negative Correlations/Adjusted R^2
# For Spearman Correlations
spearman_results <- spearman_results %>%
  filter(!is.na(SpearmanCorrelation)) %>%
  arrange(desc(SpearmanCorrelation))

# Top Positive Spearman Correlations
top_positive_spearman <- head(spearman_results, 10)

# Top Negative Spearman Correlations
top_negative_spearman <- head(spearman_results %>% arrange(SpearmanCorrelation), 10)

# For Adjusted R^2
adjusted_r_squared_results <- adjusted_r_squared_results %>%
  filter(!is.na(Adjusted_R_squared)) %>%
  arrange(desc(Adjusted_R_squared))

# Top Positive Adjusted R_squared
top_positive_adjusted_r_squared <- head(adjusted_r_squared_results, 10)

# Top Negative Adjusted R_squared (even though Adjusted R_squared can't be negative, it could be useful to see the lowest values)
top_negative_adjusted_r_squared <- head(adjusted_r_squared_results %>% arrange(Adjusted_R_squared), 10)

# Output the results
cat("\nTop Positive Spearman Correlations:\n")
print(top_positive_spearman)

cat("\nTop Negative Spearman Correlations:\n")
print(top_negative_spearman)

cat("\nTop Positive Adjusted R_squared:\n")
print(top_positive_adjusted_r_squared)

cat("\nLowest Adjusted R_squared:\n")
print(top_negative_adjusted_r_squared)

# Step 11: Save the filtered data and results to CSV files
write.csv(filtered_data, file = paste0(output_path, "/filtered_data.csv"), row.names = FALSE)
write.csv(spearman_results, file = paste0(output_path, "/spearman_correlation_results.csv"), row.names = FALSE)
write.csv(adjusted_r_squared_results, file = paste0(output_path, "/adjusted_r_squared_results.csv"), row.names = FALSE)
