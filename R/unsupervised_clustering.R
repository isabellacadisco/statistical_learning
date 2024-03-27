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


