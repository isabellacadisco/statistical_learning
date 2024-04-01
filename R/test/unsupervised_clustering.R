source("R/packages.R")

colnames(df_reordered)

data <- subset(df_reordered, select=-c(yob, age_arrival_ita, citizenship_code))

colnames(data)

summary(data)

library(FactoMineR)

data.mfa <- MFA(data, 
                group = c(5, 3, 5, 3, 6, 6),
                
                type = c(
                  "m",
                  "m",
                  "m",
                  "c",
                  "m",
                  "n"
                ),
               
                name.group = c("general info", 
                               "migration and legal status", 
                               "education", 
                               "economic well being",
                               "family and relationships",
                               "health"),
                graph = FALSE)

type = c("c", "n", "c", "c", "c",
         "n", "c", "c",
         "c", "n", "c", "c", "c",
         "n", "n", "n",
         "c", "c", "n", "n", "n", "n",
         "c", "c", "c", "c", "c", "c"),

library(FactoMineR)
data(wine)


# MFA

res <- MFA(wine, group=c(2,5,3,10,9,2), type=c("n",rep("s",5)),
           ncp=5, name.group=c("orig","smell","vis","smellAf","tasting","pref"),
           num.group.sup=c(1,6))


# K-MEANS WITH NUMERICAL
df_num <- data %>% select(where(is.numeric))

library(factoextra)
library(cluster)

#scale each variable to have a mean of 0 and sd of 1
df_num_scaled <- scale(df_num)

head(df_num_scaled)

fviz_nbclust(df_num_scaled, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df_num_scaled,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 6,
                    B = 30)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
# sta roba non converge

#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(df_num_scaled, centers = 4, nstart = 25)
km

#plot results of final k-means model
fviz_cluster(km, data = df_num_scaled)

aggregate(df_num_scaled, by=list(cluster=km$cluster), mean)

#add cluster assigment to original data
final_data <- cbind(df_num, cluster = km$cluster)
head(final_data)

#--------------------------------------------
# MIXED TYPE DATA CLUSTERING

library(clustMixType)

set.seed(1)
lambda <- lambdaest(data, num.method = 1,fac.method = 2)
mixed_data_clust <- kproto(df_reordered, 4, lambda = lambda)
summary(mixed_data_clust)

cluster <- factor(mixed_data_clust$cluster, order =  TRUE, levels = c(1:4))
data <- df_reordered.frame(df_reordered, cluster)
result_df <- mixed_data_clust$centers
Member <- mixed_data_clust$size
result <- data.frame(Member, result_df)
result
xtable::xtable(result)



# Assuming 'df' is your dataframe
x <- df_reordered$age
y <- df_reordered$prevention

library(ltm)
# Sostituisci 'x' e 'y' con i nomi delle tue variabili
biserial.cor(x, y)

# Supponendo che 'df' sia il tuo dataframe
numeric_columns <- df_reordered %>% select_if(is.numeric)
binary_column <- 'GP'  # Sostituisci con il nome reale della tua variabile binaria

# Calcola le correlazioni
correlations <- sapply(numeric_columns, function(x) biserial.cor(x, df_reordered[[binary_column]], level = 1))

# Stampa le correlazioni
print(correlations)




#------------------------------------------------

# - numerical variables + selected categorical
# - selected categorical: 
#genre
#citizenship
#religion
#legal_status
#italian_language_knowledge
#partner_citizenship

# column names to include
included_columns <- c("genre", "citizenship", "religion", "legal_status", 
                      "italian_language_knowledge", "partner_citizenship",
                      "yob", "age", "age_arrival_ita", "years_in_ita", "years_of_education", 
                      "avg_monthly_salary", "avg_working_hours_pw", 
                      "avg_monthly_income_cohabiting_persons", "n_of_children", 
                      "n_of_cohabiting_persons", "personal_money_back_home", 
                      "cohabiting_persons_money_back_home", "prevention")

data <- df %>%
  select_if(names(.) %in% included_columns)

#data <- tibble::rowid_to_column(data, "index")

# --------- HC mixed -----------------------------

# Gower distance for mixed data
# dissimilarity matrix
data_hc = subset(data, select= -c(prevention))
gower_dist<-daisy(data, metric = "gower")

# hierarchical clustering
hc_visual<-hclust(gower_dist, method = "ward.D2")

hc <- hclust(gower_dist, method = "ward.D2")
hc

# dendrogram 
plot(hc_visual, cex = 0.6, hang=-1, labels=FALSE)
rect.hclust(hc_visual, k=4, border="red")

# choose k, number of clusters 5 
cluster<-cutree(hc_visual, k=5)
# add cluster to original data 
data<-cbind(data,cluster = as.factor(cluster))
table(data$cluster)

summary(data)

# bar plot showing the distribution of "prevention" within each cluster
ggplot(data, aes(x = cluster, fill = prevention)) +
  geom_bar(position = "stack") +
  labs(x = "Cluster", y = "Count", fill = "Prevention") +
  ggtitle("Distribution of Prevention Variable within Clusters")

# percentage of "prevention"  to "Yes" within each cluster
prevention_percentage <- tapply(data$prevention == "Yes", data$cluster, mean) * 100

# cardinality of each cluster
cluster_cardinality <- table(data$cluster)

prevention_percentage
cluster_cardinality

# -------------------------------------

# ------- HC numeric ---------------------
data_num = subset(data, select= -c(genre, citizenship,
                                   religion, legal_status,
                                   italian_language_knowledge,
                                   partner_citizenship, cluster))

data_hc_num = subset(data_num, select= -c(prevention))
gower_dist<-daisy(data_hc_num, metric = "gower")

# hierarchical clustering
hc_visual_num<-hclust(gower_dist, method = "ward.D2")

hc_n <- hclust(gower_dist, method = "ward.D2")
hc_n

# dendrogram 
plot(hc_visual_num, cex = 0.6, hang=-1, labels=FALSE)
rect.hclust(hc_visual_num, k=4, border="red")

# choose k, number of clusters 4 
cluster_n<-cutree(hc_visual_num, k=4)
# add cluster to original data 
data_num<-cbind(data_num,cluster = as.factor(cluster_n))
table(data_num$cluster)

summary(data_num)

# Create a bar plot showing the distribution of "prevention" within each cluster
ggplot(data_num, aes(x = cluster, fill = prevention)) +
  geom_bar(position = "stack") +
  labs(x = "Cluster", y = "Count", fill = "Prevention") +
  ggtitle("Distribution of Prevention Variable within Clusters")

# Calculate the percentage of "prevention" equal to "Yes" within each cluster
prevention_percentage_n <- tapply(data_num$prevention == "Yes", data_num$cluster, mean) * 100

# Calculate the cardinality of each cluster
cluster_cardinality_n <- table(data_num$cluster)

prevention_percentage_n
cluster_cardinality_n

# ----------


# ------- HC categorical ---------------------
data_cat <- data %>% select_if(is.factor)
data_cat = subset(data_cat, select= -c(cluster))

data_hc_cat = subset(data_cat, select= -c(prevention))
gower_dist<-daisy(data_hc_cat, metric = "gower")

# hierarchical clustering
hc_visual_cat<-hclust(gower_dist, method = "ward.D2")

hc_c <- hclust(gower_dist, method = "ward.D2")
hc_c

# dendrogram 
plot(hc_visual_cat, cex = 0.6, hang=-1, labels=FALSE)
rect.hclust(hc_visual_cat, k=4, border="red")

# choose k, number of clusters 4 
cluster_c<-cutree(hc_visual_cat, k=4)
# add cluster to original data 
data_cat<-cbind(data_cat,cluster = as.factor(cluster_c))
table(data_cat$cluster)


# Create a bar plot showing the distribution of "prevention" within each cluster
ggplot(data_cat, aes(x = cluster, fill = prevention)) +
  geom_bar(position = "stack") +
  labs(x = "Cluster", y = "Count", fill = "Prevention") +
  ggtitle("Distribution of Prevention Variable within Clusters")

# Calculate the percentage of "prevention" equal to "Yes" within each cluster
prevention_percentage_n <- tapply(data_num$prevention == "Yes", data_num$cluster, mean) * 100

# Calculate the cardinality of each cluster
cluster_cardinality_n <- table(data_num$cluster)

prevention_percentage_n
cluster_cardinality_n






