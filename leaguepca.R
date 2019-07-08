

######## PCA #################

# We will analyze X characteristics collected for each player. Because some of these indicators are hightly correlated, we will use Principal
# Component Analysis to reduce redundancies and highlight patterns that may not be obvious in the raw data. 
# We will then cluster the 

### Dimension Reduction 

library(FactoMineR)

# Matrix for PCA 

league_matrix <- league_summary %>%
  filter(League != 'Other') %>%
  select(-League) %>%
  as.matrix() 

# Apply PCA and print the results

(league_pca <- PCA(league_matrix, scale.unit = T))

# Variable components

library(factoextra)

league_pca_var_plot <- fviz_pca_var(league_pca)


# Sum the variance preserved by the first two components 

variance_first_two_pca <- fifa_pca$eig[1,2] + fifa_pca$eig[2,2]

### KMEANS CLUSTERING

# Create intermediate data frame with first two pcas

league_comps <- tibble(pca_1 = league_pca$ind$coord[,1],
                    pca_2 = league_pca$ind$coord[,2])

# Cluster observations using the first two components

league_km <- kmeans(league_comps, centers = 4, nstart = 20, iter.max = 50)

# Converst assigned cluster to factor

clusters_as_factors <- factor(league_km$cluster)

# plot by colored clusters



a <- fviz_pca_var(league_pca, repel = T, pointsize = 'cos2', pointshape = 21, fill = "#E7B800" ,geom.var = c("point", "text", "arrow")) + theme_fivethirtyeight()
b <- fviz_pca_ind(league_pca, geom.ind = "point",fill.ind ="#E7B800",  repel = T, geom.var = c("point", "text")) + geom_text_repel(label = league_summary$League[-13], aes(color = clusters_as_factors))

grid.arrange(a,b, ncol =2 )

abc <- fviz_pca_biplot(league_pca, geom = "point", repel = T, pointsize = 'cos2', pointshape = 21, col.var  = 'black', col.ind = clusters_as_factors) + 
  geom_text_repel(label = league_summary$League[-13], aes(col = clusters_as_factors)) +
  labs(col = "League", size = "Cos2") + theme_fivethirtyeight()
