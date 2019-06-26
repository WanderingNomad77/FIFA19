source("Script.R")

### GRAPHS

## OVERALL STATS

# Distribution of ratings 

per_l <- data %>%
  group_by(League) %>%
  summarise(`Number of Players` = n(),
            `Number of Teams` = n_distinct(Club),
            `Average Player Rating` = round(mean(Overall),2),
            `Median Player Rating` = median(Overall),
            `Minimum Player Rating` = min(Overall),
            `Maximum Player Rating` = max(Overall),
            `Average Player Potential` = round(mean(Potential),2)) %>%
  arrange(`Average Player Rating`)

per_l %>%
  kable("html", caption = "League Summary Table", align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "responsive"), full_width = T)


major_league_ratings <- ggplot(top_leagues, aes(Overall, fill = League)) + geom_histogram(stat = 'count') + theme_fivethirtyeight() +
  scale_fill_manual(values = c("dodgerblue4","dodgerblue1","firebrick4","firebrick1","goldenrod4","goldenrod1",
                               "darkolivegreen4","darkolivegreen1","antiquewhite3","antiquewhite","coral3","coral")) +
  theme(legend.position = "top") + labs(caption = "Major Leagues")

other_league_ratings <- ggplot(other_leagues, aes(Overall)) + geom_histogram(stat = 'count', fill = 'black') + theme_fivethirtyeight() + labs(caption = "Other Leagues")

gridExtra::grid.arrange(major_league_ratings, other_league_ratings, ncol = 2)
overall_ratings2 <- ggplot(data, aes(x = reorder(League, -Overall), y = Overall, fill = League)) + geom_boxplot() + theme_fivethirtyeight() +
  coord_flip()

ggRadar(data[,c(10,15,55:58)], aes(group = Pitch_Position)) + facet_wrap(~Pitch_Position) + scale_y_discrete(breaks = NULL)

## Distribution of Age relative to player rating

by_age <- data %>%
  group_by(Age, Pitch_Position, League) %>%
  summarise(rating = mean(Overall))

