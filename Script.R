library(SparkR)
library(sparklyr)
library(tidyverse)
library(rJava)



# CREATE FIFA ULTIMATE TEAM

# 1/ Data overview -- Overall analysis: Distribution of ratings. 
# Ratings by age
# Ratings by club (Focus on 5 major leagues i.e. Premiere League, Liga, Bundesliga, 
#Serie A, Ligue 1)
# Who are the best strikers/midfielders/defenders/GKs?
# Who has the greatest potential?
# Best free agents?
# Best quality/price ratio?
# Who can you buy with your budget? How can you maximize your chances to win the league?







# Import data

data <- readRDS("data.RDS")

# Cleaning steps

# 1. Character to numeric

# A - Translate character 'M' into millions and 'K' into thousands for the following columns: Value, Wage, Release.Clause.

data <- data %>%
  dplyr::select(-X) %>%
  dplyr::mutate(value_multiplier = ifelse(str_detect(Value, "M"), 1000000, ifelse(str_detect(Value, "K"), 1000, 1))) %>%
  dplyr::mutate(wage_multiplier = ifelse(str_detect(Wage, "M"), 1000000, ifelse(str_detect(Wage, "K"), 1000, 1))) %>%
  mutate(clause_multiplier = ifelse(str_detect(Release.Clause, "M"), 1000000, ifelse(str_detect(Release.Clause, "K"), 1000, 1)))

# B - Convert to numeric

data <- data %>%
  dplyr::mutate(Value = as.numeric(str_extract_all(Value, "[[:digit:]]+\\.*[[:digit:]]*")) * value_multiplier) %>%
  mutate(Wage = as.numeric(str_extract_all(Wage, "[[:digit:]]+\\.*[[:digit:]]*")) * wage_multiplier) %>%
  mutate(Release.Clause = as.numeric(str_extract_all(Release.Clause, "[[:digit:]]+\\.*[[:digit:]]*"))* clause_multiplier)

# 2. Filter by 7 major European Leagues (Premiere League, Liga, Bundesliga, Serie A, Ligue 1, Portuguese Liga, Russian League) + MLS

premier_league <- c("Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley", "Cardiff City", "Chelsea", "Crystal Palace",
                    "Everton", "Fulham", "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City", "Manchester United",
                    "Newcastle United", "Southampton", "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers") 

liga_teams <- c("Deportivo Alavés", "Athletic Club de Bilbao", "Atlético Madrid", "FC Barcelona", "RC Celta", "SD Eibar", "RCD Espanyol",
                "Getafe CF", "Girona FC", "SD Huesca", "CD Leganés", "Levante UD", "Rayo Vallecano", "Real Betis", "Real Madrid",
                "Real Sociedad", "Sevilla FC", "Valencia CF", "Real Valladolid CF", "Villareal CF")

bundesliga <- c("FC Augsburg", "Hertha BSC", "SV Werder Bremen", "Borussia Dortmund", "Fortuna Düsseldorf", "Eintracht Frankfurt",
                "SC Freiburg", "TSG 1899 Hoffenheim", "Hannover 96", "RB Leipzig", "Bayer 04 Leverkusen", "1. FSV Mainz 05",
                "Borussia Mönchengladbach", "FC Bayern München", "1. FC Nürnberg", "FC Schalke 04", "VfB Stuttgart", "VfL Wolfsburg")


# Connect to Spark Cluster

sc <- spark_connect("local")

# Copy data to Spark Cluster 

data_tbl <- copy_to(sc, data, overwrite = T)

# 
