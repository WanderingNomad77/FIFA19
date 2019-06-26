

######## PCA #################

# We will analyze X characteristics collected for each player. Because some of these indicators are hightly correlated, we will use Principal
# Component Analysis to reduce redundancies and highlight patterns that may not be obvious in the raw data. 
# We will then cluster the 

### Dimension Reduction 

library(FactoMineR)

# Matrix for PCA 

fifa_matrix <- data %>%
  filter(League == "English Premier League", Potential >= 80, Age <= 21) %>%
  select_if(is.numeric) %>%
  select(-ID, -Special, -Jersey.Number) %>%
  as.matrix() 

# Apply PCA and print the results

(fifa_pca <- PCA(fifa_matrix, scale.unit = T))

# Variable components

library(factoextra)

pca_var_plot <- fviz_pca_var(fifa_pca)


# Sum the variance preserved by the first two components 

variance_first_two_pca <- fifa_pca$eig[1,2] + fifa_pca$eig[2,2]

### KMEANS CLUSTERING

# Create intermediate data frame with first two pcas

mls_comps <- tibble(pca_1 = fifa_pca$ind$coord[,1],
                    pca_2 = fifa_pca$ind$coord[,2])

# Cluster observations using the first two components

mls_km <- kmeans(mls_comps, centers = 6, nstart = 20, iter.max = 50)

# Converst assigned cluster to factor

clusters_as_factors <- factor(mls_km$cluster)

# plot by colored clusters


MLS <- data %>%
  filter(League %in% c("MLS Western Conference", "MLS Eastern Conference" ), Potential >= 80) 

premier <- data %>%
  filter(League == "English Premier League", Potential >= 80, Age <= 21)



a <- fviz_pca_var(fifa_pca, repel = T, col.var = "cos2", pointsize = 'cos2',geom.var = c("point", "text"))
b <- fviz_pca_biplot(fifa_pca, geom.ind = "point",fill.ind ="#E7B800",  repel = T, geom.var = c("point", "text")) + geom_text_repel(label = premier[,2], aes(color = clusters_as_factors))

grid.arrange(a,b, ncol =2 )

abc <- fviz_pca_ind(fifa_pca, geom = "point", repel = T, pointsize = 'cos2', pointshape = 21, fill = "#E7B800", col.ind  = clusters_as_factors) + 
  geom_text_repel(label = data[,2], aes(col = clusters_as_factors)) +
  labs(col = "Position", size = "Cos2")







