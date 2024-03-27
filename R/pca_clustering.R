summary(df)
summary(df_num_sel)

# 1. Standardize the Data
df_num_standardized <- scale(df_num_sel)

# 2. Perform PCA
pca_result <- prcomp(df_num_standardized, scale. = TRUE)

# 3. Scree Plot
screeplot(pca_result, type = "line", main = "Scree Plot")

# 4. Cumulative Explained Variance Plot
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
plot(cumulative_variance, type = "b", xlab = "Number of Components", ylab = "Cumulative Explained Variance",
     main = "Cumulative Explained Variance Plot")

# Determine the number of components to retain based on the scree plot and cumulative explained variance plot.

# 5. Perform Clustering (e.g., K-means)
# Use the selected number of components for clustering
num_components <- 5  # Adjust this based on your analysis
pca_data <- pca_result$x[, 1:num_components]  # Subset the principal components
kmeans_result <- kmeans(pca_data, centers = 3)  # Example: Perform K-means clustering with 3 clusters

# Visualize the clusters
plot(pca_data, col = kmeans_result$cluster)

# Extract the loadings
loadings <- pca_result$rotation[, 1:num_components]

# Visualize loadings
library(ggplot2)
library(reshape2)

loadings_df <- as.data.frame(loadings)
loadings_df$Variable <- rownames(loadings_df)
loadings_df_long <- melt(loadings_df, id.vars = "Variable", variable.name = "Component", value.name = "Loading")

# Plot loadings
ggplot(loadings_df_long, aes(x = Variable, y = Loading, fill = Component)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Loadings of Principal Components", x = "Variable", y = "Loading") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#---------------------------
head(df_num_sel)
#calculate principal components
results <- prcomp(df_num_sel, scale = TRUE)
#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

biplot(results, scale = 0)
# Plot the biplot with arrows and labels
biplot(results, scale = 0)


#calculate total variance explained by each principal component
results$sdev^2 / sum(results$sdev^2)

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:10), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# HA SENSO CHE VENGA TUTTO MALE,
# DALLA CORRELATION MATRIX SI VEDE CHE NON SONO CORRELLATE