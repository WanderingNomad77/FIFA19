library(SparkR)
library(sparklyr)
library(tidyverse)
library(rJava)
library(naniar)
library(ggplot2)
library(ggthemes)
library(ggiraphExtra)
library(gridExtra)
library(knitr)
library(kableExtra)
library(ggrepel)


# CREATE FIFA ULTIMATE TEAM

# 1/ Data overview -- Overall analysis: Distribution of ratings. 
# Ratings by age
# Ratings by club (Focus on major leagues i.e. Premiere League, Liga, Bundesliga, 
#Serie A, Ligue 1, Primeira Liga, Russian Premier League, MLS, Brazilian Serie A)
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

# C - Player Heights and Weights

data$Weight <- strsplit(data$Weight, 'lbs') %>%
  as.numeric()

data$Height <- gsub("'", ".", data$Height) %>%
  as.numeric() 
data$Height <- round(data$Height * 30.48, 0)

# D - Dummify Preferred.Foot

data <- data %>%
  mutate(Foot = ifelse(Preferred.Foot == "Left", 0, 1))

# Better player photos

data$Photo <- paste(
  'https://www.fifaindex.com/static/FIFA19/images/players/10/',data$ID,'@2x.png', sep = "")

# Better club logos


data$Club.Logo <- paste("https://www.fifaindex.com/static/FIFA19/images/crest/10/light/",
                        sub( "\\.png.*" ,"", sub(".*.light/", "", data$Club.Logo)),
                        "@2x.png",
                        sep = "")


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

ligue_1 <- c("Amiens SC", "Angers SCO", "FC Girondins de Bordeaux", "Stade Malherbe Caen", "Dijon FCO", "En Avant de Guingamp", "LOSC Lille",
             "Olympique Lyonnais", "Olympique de Marseille", "AS Monaco", "Montpellier HSC", "FC Nantes", "OGC Nice", "Nîmes Olympique",
             "Paris Saint-Germain", "Stade de Reims", "Stade Rennais FC", "AS Saint-Étienne", "RC Strasbourg Alsace", "Toulouse Football Club")

russian_premier_league <- c("PFC CSKA Moscow", "Spartak Moscow", "PFC CSKA Moscow", "Lokomotiv Moscow")

brazilian_serie_A <- c("América FC (Minas Gerais)","Atlético Mineiro", "Atlético Paranaense", "Bahia","Botafogo", "Ceará Sporting Club", "Chapecoense", 
                       "Vitória", "Cruzeiro", "Fluminense", "Grêmio", "Internacional", "Paraná", "Santos", "Sport Club do Recife")

MLS_West <- c("Colorado Rapids", "FC Dallas", "Houston Dynamo", "LA Galaxy", "Los Angeles FC", "Minnesota United FC", "Portland Timbers", "Real Salt Lake",
         "San Jose Earthquakes", "Seattle Sounders FC", "Sporting Kansas City", "Vancouver Whitecaps FC")

MLS_East <- c("Atlanta United", "Chicago Fire", "Columbus Crew SC", "DC United", "Montreal Impact", "New England Revolution", "New York City FC",
              "New York Red Bulls", "Orlando City SC", "Philadelphia Union", "Toronto FC")

eredivisie <- c("ADO Den Haag", "Ajax", "AZ Alkmaar", "De Graafschap", "FC Emmen", "Excelsior", "Feyenoord", "Fortuna Sittard", "FC Groningen",
                "SC Heerenveen", "Heracles Almelo", "NAC Breda", "PEC Zwolle", "PSV", "FC Utrecht", "Vitesse", "VVV-Venlo", "Willem II")

superlig <- c("Akhisar Belediyespor", "Alanyaspor", "MKE Ankaragücü", "Antalyaspor", "Beşiktaş JK", "Bursaspor", "Çaykur Rizespor", "BB Erzurumspor",
              "Fenerbahçe SK", "Galatasaray SK", "Göztepe SK", "Medipol Başakşehir FK", "Kasimpaşa SK", "Kayserispor", "Atiker Konyaspor",
              "Sivasspor", "Trabzonspor", "Yeni Malatyaspor")

primera_division <- c("Club Atlético Aldosivi", "Argentinos Juniors", "Atlético Tucumán", "Club Atlético Banfield", "Belgrano de Córdoba",
                     "Boca Juniors", "Club Atlético Colón", "Defensa y Justicia", "Estudiantes de La Plata", "Gimnasia y Esgrima La Plata",
                     "Godoy Cruz", "Club Atlético Huracán", "Independiente", "Club Atlético Lanús", "Newell's Old Boys", "Patronato",
                     "Racing Club", "River Plate", "Rosario Central", "San Lorenzo de Almagro", "San Martin de Tucumán", "San Martín de San Juan",
                     "Club Atlético Talleres", "Club Atlético Tigre", "Unión de Santa Fe", "Vélez Sarsfield")


data <- data %>%
  mutate(League = ifelse(Club %in% premier_league, "English Premier League", 
                         ifelse(Club %in% liga, "Spanish Liga", 
                                ifelse(Club %in% bundesliga, "German Bundesliga",
                                       ifelse(Club %in% serie_A, "Italian Serie A",
                                              ifelse(Club %in% primeira_liga, "Portuguese Primeira Liga",
                                                     ifelse(Club %in% ligue_1, "French Ligue 1",
                                                                   ifelse(Club %in% brazilian_serie_A, "Brazilian Série A",
                                                                          ifelse(Club %in% MLS_West, "MLS Western Conference",
                                                                                 ifelse(Club %in% MLS_East, "MLS Eastern Conference",
                                                                                        ifelse(Club %in% eredivisie, "Dutch Eredivisie",
                                                                                               ifelse(Club %in% superlig, "Turkish Süper Lig",
                                                                                                      ifelse(Club %in% primera_division, "Argentinian Primera División", "Other")))))))))))))%>%
  select(ID:Club, League, Club.Logo:Release.Clause)

# 3. Positions

defense <- c("CB", "RWB", "LWB", "LB", "RB", "LCB", "RCB")
midfield <- c("CAM","CDM", "CM", "LAM", "LDM", "LCM", "LM", "RAM", "RDM", "RCM", "RM")
attack <- c("CF", "LF", "RF", "ST", "LW", "RW", "LS", "RS")

data <- data %>%
  dplyr::mutate(Pitch_Position = ifelse(Position %in% defense, "Defender",
                                 ifelse(Position %in% midfield, "Midfielder",
                                        ifelse(Position %in% attack, "Striker",
                                               ifelse(Position == "GK", "Goalkeeper", "Other"))))) %>%
  select(ID:Wage, Position, Pitch_Position, Special:Release.Clause) %>%
  filter(!is.na(Pitch_Position))




# 4. Drop Variables with too many missing values

missing_vals <- naniar::gg_miss_var(data, show_pct = T)

top_leagues <- data %>%
  select(-Loaned.From) %>%
  filter(League != "Other") %>%
  mutate(wage_bracket =
           ifelse(Wage %in% c(0, 3000), "1,000 € - 3,000 €",
                  ifelse(Wage %in% c(3001, 9000), "3,001 € - 9,000 €",
                         ifelse(Wage %in% c(9001, 23000), "9,001 € - 23,000 €","+ 23,001 €"))))

top_leagues$wage_bracket <- factor(top_leagues$wage_bracket, levels = c("1,000 € - 3,000 €","3,001 € - 9,000 €","9,001 € - 23,000 €","+ 23,001 €" ),ordered = T)
top_leagues$Pitch_Position <- factor(top_leagues$Pitch_Position, levels = c("Goalkeeper", "Defender", "Midfielder", "Striker"), ordered= T)

other_leagues <- data %>%
  select(-Loaned.From) %>%
  filter(League == "Other")



# Connect to Spark Cluster

sc <- spark_connect("local")

# Copy data to Spark Cluster 

data_tbl <- copy_to(sc, data, overwrite = T)




scores <- list("Messi" = data[,c("Crossing", "Penalties", "ShortPassing")][1:3,])


scores <- data.frame("Label"=names(data[c("Crossing", "Penalties", "ShortPassing")]),
                     "Messi" = as.numeric(data[,c("Crossing", "Penalties", "ShortPassing")][1,]))

chartJSRadar(scores, maxScale = 100, showToolTipLabel=TRUE)

############

labs <- c("Crossing", "Penalties", "ShortPassing")

scores <- list(as.numeric(data[,c("Crossing", "Penalties", "ShortPassing")][1,]))


chartJSRadar(scores = scores, labs = labs, maxScale = 100)