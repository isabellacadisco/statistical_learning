source("R/packages.R")

# import data

df <- read.csv("data/survey_2016_filtered.csv", header = TRUE)

# ----------------------

# info about data

str(df)


# convert categorical variables INT -> FACTOR

# Convert D1 to factor with labels
df$D1 <- factor(df$D1, levels = c(1, 2), labels = c("Male", "Female"))

# Convert D3 to factor with labels
df$D3 <- factor(df$D3, levels = c(1, 2), labels = c("Abroad", "Italy"))

# new variable for continent BASED ON D7, citizenship
df$Continent <- ifelse(substr(df$D7, 1, 1) %in% c("2"), "Europe",
                       ifelse(substr(df$D7, 1, 1) %in% c("3"), "Asia",
                              ifelse(substr(df$D7, 1, 1) %in% c("4"), "Africa",
                                     ifelse(substr(df$D7, 1, 1) %in% c("5", "6"), "America",
                                            ifelse(df$D7 == "999", "Apolide", NA)))))

df$Continent <- factor(df$Continent)

# Convert D7 to factor
df$D7 <- as.factor(df$D7)

# Convert D8 to factor with labels
df$D8 <- factor(df$D8, levels = c(1, 2, 3, 4), labels = c("Single", "Married", "Widowed", "Divorced/Separated"))

# Convert D9 to factor with labels
df$D9 <- factor(df$D9, levels = c(1, 2, 3, 4, 5, 6), labels = c("None", "Primary school", "Lower secondary school", "Vocational qualification", "Upper secondary school", "University degree or postgraduate diploma"))

# D9b -> factor with labels + nuova colonna D9c titolo riconosciuto si/no
# Convert D9b column into a factor with English labels
df$D9b <- factor(df$D9b, levels = c(1, 2, 3, 4),
                labels = c("Yes", "No", "Don't know, not informed",
                           "Don't know, wouldn't need it given the job opportunities in Italy"))

# Create a new column with binary values (Yes/No)
df$D9c <- ifelse(df$D9b == "Yes", "Yes", "No")
df$D9c <- factor(df$D9c, levels = c("Yes", "No"))


# D10
df$D10 <- factor(df$D10)


# Convert D11 to factor with labels
df$D11 <- factor(df$D11, levels = c(1:11), labels = c("Muslim", "Catholic Christian", "Orthodox Christian", "Coptic Christian", "Evangelical Christian", "Other Christian", "Buddhist", "Hindu", "Sikh", "Other religion", "No religion"))

# Convert D12 to factor with labels
df$D12 <- factor(df$D12, levels = c(1:9), labels = c("Dual citizenship", "EU citizens (or dual citizenship of another EU country)", "CE long-term residence permit", "Valid residence permit/visa", "Expired residence permit/visa being renewed", "Asylum seeker (pending decision or appeal outcome)", "Expired residence permit/visa not being renewed (overstayer)", "Never had any legal status (undocumented)", "Asylum seeker with final rejection"))

# Convert D23 to factor with labels
df$D23 <- factor(df$D23, levels = c(1:3, 98), labels = c("Same as respondent", "Italian", "Other citizenship", "I do not have a spouse/partner"))

# Convert D35a to factor with labels
df$D35a <- factor(df$D35a, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D35b to factor with labels
df$D35b <- factor(df$D35b, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D35c to factor with labels
df$D35c <- factor(df$D35c, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D35d to factor with labels
df$D35d <- factor(df$D35d, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D35e to factor with labels
df$D35e <- factor(df$D35e, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D35f to factor with labels
df$D35f <- factor(df$D35f, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D35g to factor with labels
df$D35g <- factor(df$D35g, levels = c(1, 2), labels = c("Yes", "No"))

# Convert D36 to factor with labels
df$D36 <- factor(df$D36, levels = c(1, 2), labels = c("Yes", "No"))

# check conversion
str(df)
print(df)

# rename columns

# general info
names(df)[names(df) == "D1"] <- "genre"
names(df)[names(df) == "D2"] <- "yob"
names(df)[names(df) == "D3"] <- "pob" #place of birth
names(df)[names(df) == "Eta_arrivo_Italia"] <- "yo_arrival_ita" # anno arrivo in italia
names(df)[names(df) == "D7"] <- "citizenship_code"
names(df)[names(df) == "D8"] <- "marital_status"

# education info
names(df)[names(df) == "D9"] <- "educational_title"
names(df)[names(df) == "D9b"] <- "recognized"
names(df)[names(df) == "D9c"] <- "recognized_binary"
names(df)[names(df) == "D10"] <- "language_knowledge" # for italian language

# religion
names(df)[names(df) == "D11"] <- "religion"

# other info
names(df)[names(df) == "D12"] <- "legal_status" # Condizione giuridico-amministrativa
names(df)[names(df) == "D20a"] <- "n_of_children"
names(df)[names(df) == "D23"] <- "partner_citizenship"
names(df)[names(df) == "D25"] <- "n_of_cohabiting_persons"

# economic info
names(df)[names(df) == "D26"] <- "avg_monthly_income_cohabiting_persons"
names(df)[names(df) == "D27a"] <- "personal_money_back_home"
names(df)[names(df) == "D27b"] <- "family_money_back_home"
names(df)[names(df) == "D29"] <- "avg_working_hours_pw" #per week
names(df)[names(df) == "D32"] <- "avg_monthly_salary" # lavoro regolare, irregolare, pensioni

# health related info
names(df)[names(df) == "D35a"] <- "GP" # medico di famiglia, general practitioner
names(df)[names(df) == "D35b"] <- "first_aid" #pronto socc
names(df)[names(df) == "D35c"] <- "specialist_visits_SSN" 
names(df)[names(df) == "D35d"] <- "specialist_visits_private" # nel privato, libera professione
names(df)[names(df) == "D35e"] <- "hospitalization_SSN"
names(df)[names(df) == "D35f"] <- "hospitalization_private"
names(df)[names(df) == "D35g"] <- "volunteer_center"

names(df)[names(df) == "D36"] <- "prevention" # visite di prevenzione? Y/N


small_df = head(df)


# summary final dataset
summary(df)


# Analisi
chisq_test <- chisq.test(df$educational_title, df$avg_monthly_salary)
chisq_test

chisq_test <- chisq.test(df$educational_title, df$prevention)
chisq_test


install.packages("partykit", repos = "http://R-Forge.R-project.org")
library("partykit")
ct <- ctree(prevention ~ ., data = subset(df, select = -c(citizenship_code)))
plot(ct, tp_args = list(text = TRUE))

# Predict using the ctree model
predictions <- predict(ct, newdata = subset(df, select=-c(citizenship_code)))

# Calculate accuracy
accuracy <- confusionMatrix(predictions, data_3$prevention)$overall['Accuracy']
accuracy

library(ggplot2)

# Istogramma dei salari mensili medi
ggplot(df, aes(x = avg_monthly_salary)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  facet_wrap(~ educational_title, scales = "free") +
  labs(x = "Salario Mensile Medio", y = "Frequenza", title = "Distribuzione dei Salari Mensili Medi per Livello di Istruzione")

# Box plot dei salari mensili medi
ggplot(df, aes(x = educational_title, y = avg_monthly_salary)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "Livello di Istruzione", y = "Salario Mensile Medio", title = "Distribuzione dei Salari Mensili Medi per Livello di Istruzione")

# media dei salari mensili medi per ogni livello di istruzione
mean_salaries <- aggregate(avg_monthly_salary ~ educational_title, data = df, FUN = mean)

# Bar plot della media dei salari mensili medi per ogni livello di istruzione
ggplot(mean_salaries, aes(x = educational_title, y = avg_monthly_salary)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Livello di Istruzione", y = "Media Salario Mensile Medio", title = "Media dei Salari Mensili Medi per Livello di Istruzione")


# denaro mandato a casa per continente
ggplot(df, aes(x = Continent, y = personal_money_back_home, fill = Continent)) +
  geom_boxplot() +
  labs(x = "Continente", y = "Denaro Personale Riportato a Casa", title = "Distribuzione del Denaro Personale Riportato a Casa per Continente") +
  theme_minimal()

ggplot(df, aes(x = Continent, y = family_money_back_home, fill = Continent)) +
  geom_boxplot() +
  labs(x = "Continente", y = "Denaro Personale Riportato a Casa", title = "Distribuzione del Denaro Personale Riportato a Casa per Continente") +
  theme_minimal()

data = df

#gender visualisation
male_perc <- nrow(data %>% dplyr::select(genre) %>% filter(genre == "Male"))/length(data$genre)*100
female_perc <- nrow(data %>% dplyr::select(genre) %>% 
                      filter(genre == "Female"))/length(data$genre)*100

pgen <- data %>%  
  count(genre) %>% 
  mutate("perc" = round(c(female_perc,male_perc),2))

pgen <- pgen %>% 
  arrange(desc(genre)) %>%
  mutate(prop = n / sum(pgen$perc) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pgen, aes(x="", y=n, fill=genre)) +
  geom_bar(stat="identity", width=1,color="white")+
  coord_polar("y",0,1)+
  labs(x = "", y = "",title="Gender")+
  theme_minimal()+ # remove background, grid, numeric labels
  theme(plot.title=element_text(face="bold",  hjust=0.5))+
  geom_text(aes(y = ypos, label = perc), color = "white", size=6) +
  scale_fill_brewer(palette="Set2")

rm(pgen)
#--------------------------------------------------------------------------

#age for different genders
data$age <- 2016 - data$yob

data$age_group <- cut(data$age, c(0,20,24,35,70), labels = c("<21","21-24","25-35",">35") )

ggplot(data,aes(x=age_group, fill=genre))+
  geom_bar(col="black")+
  facet_wrap(.~genre)+
  stat_count(aes(y=..count.., label=..count..), vjust=-0.5,geom="text", col="black", size=3.5)+
  labs(x="Age Group", y = "Count", title="Age distribution", fill= "Sex")+
  theme_minimal()+
  theme(plot.title=element_text(face="bold",  hjust=0.5))+
  scale_fill_brewer(palette="Set2")


#-----------------------------------------------------------------------

#check for outliers

#prepare a boxplot for every numeric variable
#select numeric features
data_num <- select_if(data,is.numeric) 

#create a tibble with two columns: the feature name and the values
data_num_box <-data_num %>% gather(variable,values,1:10)

ggplot(data_num_box)+
  geom_boxplot(aes(x=variable,y=values), fill = "salmon") + 
  facet_wrap(~variable,ncol=3,scales="free") + 
  theme(strip.text.x = element_blank(),
        text = element_text(size=12))


library(corrplot)
correlation <- cor(data_num)
corrplot(correlation, type = "upper", method = "number", tl.col = "black", tl.srt = 45, tl.cex = 0.5)



# apply hclust function from cluster R package in order to cluster our dataset.

# The object returned by hclust function contains information about solutions with 
# different numbers of clusters, we pass the cutree function the cluster object and
# the number of clusters we’re interested in.
#We identify the appropriate number of clusters based on Dendrogram.

library(cluster)
# calculate distance (factors needed). This is my dissimilarity matrix
gower_dist<-daisy(data, metric = "gower")
# hierarchical clustering
hc_visual<-hclust(gower_dist, method = "ward.D2")
plot(hc_visual, cex = 0.6, hang=-1, labels=FALSE)

summary(data)


library(rpart)
library(rpart.plot) 
tree_model <- rpart(prevention ~ ., data = df, method = "class")
plot(tree_model)

tree <- rpart(prevention ~ ., data=subset(df, select = -c(citizenship_code)), control=rpart.control(cp=.0001))

#view results
printcp(tree)

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree, best)
prp(pruned_tree,
    faclen = 0,   # use full names for factor labels
    extra = 1,    # display number of obs. for each terminal node
    roundint = FALSE,  # don't round to integers in output
    digits = 5,   # display 5 decimal places in output
    cex = 0.5
    )    # adjust text size here (default is 1)


glm(prevention ~ ., data = df, family = binomial(link = 'logit'))



Logit<-glm(prevention~avg_working_hours_pw+age_group+genre+Continent+educational_title+yo_arrival_ita+religion+yob+marital_status+recognized_binary+recognized+age_group+language_knowledge+legal_status+n_of_children+partner_citizenship,data=data,family=binomial) 
summary(Logit) 


# Fit logistic regression model
logit_model <- glm(prevention ~ ., data = data, family = binomial)

names(data)
sapply(data, class)
sapply(data[, sapply(data, is.factor)], levels)


# Summary of the model
summary(logit_model)



summary(data)

# After selecting features and calculating the distance matrix, it is time to apply hclust function from cluster R package in order to cluster our dataset.
# 
# The object returned by hclust function contains information about solutions with different numbers of clusters, we pass the cutree function the cluster object and the number of clusters we’re interested in. We identify the appropriate number of clusters based on Dendrogram.
data <- data[,-16]
library(cluster)
library(gower)
# calculate distance (factors needed)
gower_dist<-daisy(data, metric = "gower")
# hierarchical clustering
hc<-hclust(gower_dist, method = "ward.D2")
# dendrogram 
plot(hc, labels=FALSE)
rect.hclust(hc, k=4, border="red")
coph.c=cophenetic(hc)
cor(gower_dist,coph.c) 


# choose k, number of clusters 
cluster<-cutree(hc, k=4)
# add cluster to original data 
data<-cbind(data, cluster = as.factor(cluster))

mymode<-function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


#table to summarise
cluster_data <- data %>% group_by(cluster) %>% 
  summarise(genre = mymode(genre),
            age = mymode(age),
            marital_status = mymode(marital_status),
            educational_title = mymode(educational_title)
  )

