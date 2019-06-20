library(SparkR)
library(sparklyr)
library(tidyverse)
library(rJava)
library(naniar)
library(ggplot2)
library(ggthemes)
library(fmsb)



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





data2 <- read.csv("data.csv", stringsAsFactors = F, na.strings = c("", " "))

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

# 2. Filter by 7 major European Leagues (Premiere League, Liga, Bundesliga, Serie A, Ligue 1, Primeira Liga, Russian League) + MLS

premier_league <- c("Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley", "Cardiff City", "Chelsea", "Crystal Palace",
                    "Everton", "Fulham", "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City", "Manchester United",
                    "Newcastle United", "Southampton", "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers") 

liga <- c("Deportivo Alavés", "Athletic Club de Bilbao", "Atlético Madrid", "FC Barcelona", "RC Celta", "SD Eibar", "RCD Espanyol",
                "Getafe CF", "Girona FC", "SD Huesca", "CD Leganés", "Levante UD", "Rayo Vallecano", "Real Betis", "Real Madrid",
                "Real Sociedad", "Sevilla FC", "Valencia CF", "Real Valladolid CF", "Villarreal CF")

bundesliga <- c("FC Augsburg", "Hertha BSC", "SV Werder Bremen", "Borussia Dortmund", "Fortuna Düsseldorf", "Eintracht Frankfurt",
                "SC Freiburg", "TSG 1899 Hoffenheim", "Hannover 96", "RB Leipzig", "Bayer 04 Leverkusen", "1. FSV Mainz 05",
                "Borussia Mönchengladbach", "FC Bayern München", "1. FC Nürnberg", "FC Schalke 04", "VfB Stuttgart", "VfL Wolfsburg")

serie_A <- c("Atalanta", "Bologna", "Cagliari", "Chievo Verona", "Empoli", "Fiorentina", "Frosinone", "Genoa", "Inter", "Juventus",
             "Lazio", "Milan", "Napoli", "Parma", "Roma", "Sampdoria", "Sassuolo", "SPAL", "Torino", "Udinese")

primeira_liga <- c("Os Belenenses", "SL Benfica", "Boavista FC", "SC Braga", "GD Chaves", "CD Aves", "CD Feirense", "Clube Sport Marítimo",
                   "Moreirense FC", "CD Nacional", "Portimonense SC", "FC Porto", "Rio Ave FC", "Santa Clara", "Sporting CP",
                   "CD Tondela", "Vitória Guimarães", "Vitória de Setúbal")

ligue_1 <- c("Amiens SC", "Angers SCO", "FC Girondins de Bordeaux", "Stade Malherbe Caen", "Dijon FCO", "En Avant de Guigamps", "LOSC Lille",
             "Olympique Lyonnais", "Olympique de Marseille", "AS Monaco", "Montpellier HSC", "FC Nantes", "OGC Nice", "Nîmes Olympique",
             "Paris Saint-Germain", "Stade de Reims", "Stade Rennais FC", "AS Saint-Étienne", "RC Strasbourg Alsace", "Toulouse Football Club")

russian_premier_league <- c("PFC CSKA Moscow", "Spartak Moscow", "PFC CSKA Moscow", "Lokomotiv Moscow")

brazilian_serie_A <- c("América FC (Minas Gerais)","Atlético Mineiro", "Atlético Paranaense", "Bahia", "Ceará Sporting Club", "Chapecoense", 
                       "Corinthians", "Cruzeiro", "Fluminense", "Grêmio", "Internacional", "Paraná", "Santos", "Sport Club do Recife")


data <- data %>%
  mutate(League = ifelse(Club %in% premier_league, "Premier League", 
                         ifelse(Club %in% liga, "Liga", 
                                ifelse(Club %in% bundesliga, "Bundesliga",
                                       ifelse(Club %in% serie_A, "Serie A",
                                              ifelse(Club %in% primeira_liga, "Primeira Liga",
                                                     ifelse(Club %in% ligue_1, "Ligue 1",
                                                            ifelse(Club %in% russian_premier_league, "Russian Premier League",
                                                                   ifelse(Club %in% brazilian_serie_A, "Brazilian Serie A", "Other"))))))))) %>%
  select(ID:Club, League, Club.Logo:Release.Clause)

# 3. Positions

defense <- c("CB", "RWB", "LWB", "LB", "RB", "LCB", "RCB")
midfield <- c("CAM","CDM", "CM", "LAM", "LDM", "LCM", "LM", "RAM", "RDM", "RCM", "RM")
attack <- c("CF", "LF", "RF", "ST", "LW", "RW", "LS", "RS")

data <- data %>%
  mutate(Pitch_Position = ifelse(Position %in% defense, "Defender",
                                 ifelse(Position %in% midfield, "Midfielder",
                                        ifelse(Position %in% attack, "Striker",
                                               ifelse(Position == "GK", "Goalkeeper", "Other"))))) %>%
  select(ID:Wage, Position, Pitch_Position, Special:Release.Clause)


# 4. Drop Variables with too many missing values

missing_vals <- naniar::gg_miss_var(data, show_pct = T)

data <- data %>%
  select(-Loaned.From) %>%
  filter(League != 'Other')


### GRAPHS

## OVERALL STATS

# Distribution of ratings 

overall_ratings <- ggplot(data, aes(Overall, fill = League)) + geom_histogram(stat = 'count') + facet_wrap(~League) + theme_gdocs()
overall_ratings2 <- ggplot(data, aes(x = League, y = Overall, fill = League)) + geom_boxplot() + geom_point() + theme_fivethirtyeight()

ggRadar(data[,c(10,15,55:58)], aes(group = Pitch_Position)) + facet_wrap(~Pitch_Position) + scale_y_discrete(breaks = NULL)

## Distribution of Age relative to player rating

by_age <- data %>%
  group_by(Age, Pitch_Position, League) %>%
  summarise(rating = mean(Overall))




# Connect to Spark Cluster

sc <- spark_connect("local")

# Copy data to Spark Cluster 

data_tbl <- copy_to(sc, data, overwrite = T)



