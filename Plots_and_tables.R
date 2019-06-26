source("Script.R")

# League summary stats -

options(scipen = 999)
league_summary <- data %>%
  group_by(League) %>%
  summarise(`Number of Players` = n(),
            `Number of Teams` = n_distinct(Club),
            `Average Player Rating` = round(mean(Overall),2),
            `Median Player Rating` = median(Overall),
            `Minimum Player Rating` = min(Overall),
            `Maximum Player Rating` = max(Overall),
            `Average Player Age` = mean(Age),
            `Average Player Potential` = mean(Potential),
            `Average Player Potential` = round(mean(Potential),2),
            `Average Player Value (€)` = mean(Value),
            `Median Player Value (€)` = median(Value)) %>%
  arrange(desc(`Average Player Rating`))

league_summary_table <- league_summary%>%
  kable("html", caption = "League Summary Table", align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "responsive"), full_width = T)


### GRAPHS

## OVERALL STATS

# Distribution of ratings 


major_league_ratings <- ggplot(top_leagues, aes(Overall, fill = League)) + geom_histogram(stat = 'count') + 
  theme_fivethirtyeight() +
  scale_fill_manual(values = c("dodgerblue4","dodgerblue2","dodgerblue","firebrick2","purple","palevioletred2",
                               "chartreuse3","darkolivegreen1","yellow","orange","coral3","coral")) +
  theme(legend.position = "top") + 
  labs(caption = "Major Leagues") + 
  geom_vline(aes(xintercept = median(Overall), col = 'Median'), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean(Overall), col = 'Mean'), linetype= 'dashed', size = 1) + 
  scale_color_manual(name = "Stats", values = c(Median = "blue", Mean = "red"))



other_league_ratings <- ggplot(other_leagues, aes(Overall)) + 
  geom_histogram(stat = 'count', fill = 'black') + 
  geom_vline(aes(xintercept = median(Overall), col = 'Median'), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = mean(Overall), col = 'Mean'), linetype = "dashed", size = 1) +
  scale_color_manual(name = "Stats", values = c(Median = "blue", Mean = "red")) +
  theme_fivethirtyeight() +
  labs(caption = "Other Leagues") +
  theme(legend.position = 'top')


overall_ratings2 <- ggplot(data, aes(x = reorder(League, -Overall), y = Overall, fill = League)) + 
  geom_boxplot() + 
  theme_fivethirtyeight() +
  coord_flip() + 
  theme(legend.position = 'none')

gridExtra::grid.arrange(overall_ratings2, gridExtra::arrangeGrob(major_league_ratings, other_league_ratings, ncol = 2), nrow = 2)



## Club ratings

by_team <- data %>% 
  group_by(Club) %>%
  summarize(`Overall Player Rating` = mean(Overall),
            `Total Team Value` = sum(Value)) %>%
  top_n(10, `Total Team Value`) %>%
  arrange(desc(`Total Team Value`))

ggRadar(data[,c(10,15,55:58)], aes(group = League, facet = Pitch_Position)) + scale_y_discrete(breaks = NULL)


## Top teams

top_teams <- top_leagues %>%
  filter(!is.na(Club)) %>%
  group_by(Club) %>%
  summarise(`Number of Players` = n(),
            `Average Player Rating` = round(mean(Overall),2),
            `Median Player Rating` = median(Overall),
            `Minimum Player Rating` = min(Overall),
            `Maximum Player Rating` = max(Overall),
            `Average Player Age` = mean(Age),
            `Average Player Potential` = mean(Potential),
            `Average Player Potential` = round(mean(Potential),2),
            `Average Player Value (€)` = mean(Value),
            `Median Player Value (€)` = median(Value))

## Distribution of Age relative to player rating

by_age <- data %>%
  group_by(Age, Pitch_Position, League) %>%
  summarise(rating = mean(Overall))

# Ratings by nationality 

by_nationality <- top_leagues %>%
  group_by(Nationality) %>%
  summarize(players = n(),
            avg_rating = mean(Overall)) 

by_nationality%>%
  filter(players >= quantile(players, 0.90)) %>%
  arrange(desc(players)) %>%
  top_n(wt = players, 10)

# Rating by position

by_position <- top_leagues %>%
  group_by(League, Pitch_Position) %>%
  summarise(players = n(),
            avg = mean(Overall),
            med = median(Overall),
            val = mean(Value)) %>%
  filter(!is.na(Pitch_Position)) %>%
  arrange(Pitch_Position, desc(avg), val)

position_plot <- top_leagues %>%
  filter(!is.na(Pitch_Position)) %>%
  ggplot(aes(reorder(League, -Overall), Overall, fill = Pitch_Position)) +
  geom_boxplot() +
  theme_stata() +
  facet_grid(vars(Pitch_Position), vars(wage_bracket)) +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'none') +
  xlab("League")

  ggplot(top_leagues, aes(reorder(League, - Overall), Overall)) + geom_boxplot() + facet_grid(~Pitch_Position) + coord_flip()
  
  ggplot(top_leagues, aes(Overall, Value, col = League)) + geom_point() + facet_wrap(~League)
  

  
