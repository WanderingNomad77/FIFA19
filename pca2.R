library(factoextra)
library(FactoMineR)

player_matrix <- data %>%
  filter(!is.na(Pitch_Position)) %>%
  select(Crossing:GKReflexes) %>%
  as.matrix()

player_pca <- PCA(player_matrix, scale.unit = T)


players_comps <- tibble(pca_1 = player_pca$ind$coord[,1],
                        pca_2 = player_pca$ind$coord[,2])

filtered_positions <- data %>%
  filter(!is.na(Pitch_Position))

profile_ind_plot <- fviz_pca_ind(player_pca, geom.ind = "point",col.ind = filtered_positions$Pitch_Position,  repel = T, 
                                 geom.var = c("point", "text"), addEllipses = T) 

profile_var_plot <- fviz_pca_var(player_pca, geom.var = c("point", "text", "arrow"), repel = T)