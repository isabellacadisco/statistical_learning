source("R/packages.R")

# import df

df <- read.csv("data/survey_2016_filtered.csv", header = TRUE)

#---------------------------------------------------------------

# info about df

str(df)

#---------------------------------------------------------------

# PREPROCESSING

# I have lot of categorical variables encoded as int
# first step is to convert them into factors
# also some variables have lot of different levels, I am building some simplified versions of them 
#(binary or w/reduced number of classes)

# D1: genre 
df$D1 <- factor(df$D1, levels = c(1, 2), labels = c("Male", "Female"))

# D3: place of birth
df$D3 <- factor(df$D3, levels = c(1, 2), labels = c("Abroad", "Italy"))

# new variable for continent BASED ON D7, citizenship
df$citizenship <- ifelse(substr(df$D7, 1, 1) %in% c("2"), "Europe",
                       ifelse(substr(df$D7, 1, 1) %in% c("3"), "Asia",
                              ifelse(substr(df$D7, 1, 1) %in% c("4"), "Africa",
                                     ifelse(substr(df$D7, 1, 1) %in% c("5", "6"), "America",
                                            ifelse(df$D7 == "999", "Apolide", NA)))))
df$citizenship <- factor(df$citizenship)

# D7: citizenship code
df$D7 <- as.factor(df$D7)

# D8: marital status
df$D8 <- factor(df$D8, levels = c(1, 2, 3, 4), labels = c("Single", "Married", "Widowed", "Divorced/Separated"))

# D9: educational title
df$D9 <- factor(df$D9, levels = c(1, 2, 3, 4, 5, 6), labels = c("None", "Primary school", "Lower secondary school", "Vocational qualification", "Upper secondary school", "University degree or postgraduate diploma"))

# new variable "years_of_education"

# mapping of educational titles to years of education
education_mapping <- c("None" = 0, 
                       "Primary school" = 5, 
                       "Lower secondary school" = 8, 
                       "Vocational qualification" = 11, 
                       "Upper secondary school" = 13, 
                       "University degree or postgraduate diploma" = 16)
df$years_of_education <- education_mapping[df$D9]


# D9b: for those with foreign educational qualifications, is the title recognized?
df$D9b <- factor(df$D9b, levels = c(1, 2, 3, 4),
                 labels = c("Yes", "No", "Don't know, not informed",
                            "Don't know, wouldn't need it given the job opportunities in Italy"))
# new variable with binary values (Yes/No)
df$D9c <- ifelse(df$D9b == "Yes", "Yes", "No")
df$D9c <- as.factor(df$D9c)

# D10: Italian language knowledge
df$D10 <- factor(df$D10, levels = c(1, 2, 3, 4, 5),
                                   labels = c("Not at all", "Little", "Fairly", "Very", "Very well"))

# D11: religion
df$D11 <- factor(df$D11, levels = c(1:11), labels = c("Muslim", "Catholic Christian", "Orthodox Christian", "Coptic Christian", "Evangelical Christian", "Other Christian", "Buddhist", "Hindu", "Sikh", "Other religion", "No religion"))

# D12: legal status
df$D12 <- factor(df$D12, levels = c(1:9), labels = c("Dual citizenship", "EU citizens (or dual citizenship of another EU country)", "CE long-term residence permit", "Valid residence permit/visa", "Expired residence permit/visa being renewed", "Asylum seeker (pending decision or appeal outcome)", "Expired residence permit/visa not being renewed (overstayer)", "Never had any legal status (undocumented)", "Asylum seeker with final rejection"))

# D23: Citizenship of spouse/partner (if Italian, citizenship of origin, if any)
df$D23 <- factor(df$D23, levels = c(1:3, 98), labels = c("Same as respondent", "Italian", "Other citizenship", "I do not have a spouse/partner"))

# D35a: In the past 12 months, family doctor or general practitioner
df$D35a <- factor(df$D35a, levels = c(1, 2), labels = c("Yes", "No"))

# D35b: In the past 12 months, first aid
df$D35b <- factor(df$D35b, levels = c(1, 2), labels = c("Yes", "No"))

# D35c: In the past 12 months, specialist visits with SSN 
df$D35c <- factor(df$D35c, levels = c(1, 2), labels = c("Yes", "No"))

# D35d: In the past 12 months, private specialist visits 
df$D35d <- factor(df$D35d, levels = c(1, 2), labels = c("Yes", "No"))

# remove D35e, D35f
df <- select(df, -D35e, -D35f)

# D35g: In the past 12 months, volunteer health center.
df$D35g <- factor(df$D35g, levels = c(1, 2), labels = c("Yes", "No"))

# D36: Do you do checkups and prevention?
df$D36 <- factor(df$D36, levels = c(1, 2), labels = c("Yes", "No"))


# rename columns

# general info
names(df)[names(df) == "D1"] <- "genre"
names(df)[names(df) == "D2"] <- "yob"
names(df)[names(df) == "D3"] <- "pob" #place of birth
names(df)[names(df) == "Eta_arrivo_Italia"] <- "yo_arrival_ita" # anno arrivo in italia
names(df)[names(df) == "D7"] <- "citizenship_code"
names(df)[names(df) == "D8"] <- "marital_status"
names(df)[names(df) == "D12"] <- "legal_status" # Condizione giuridico-amministrativa
names(df)[names(df) == "D25"] <- "n_of_cohabiting_persons"
names(df)[names(df) == "D20a"] <- "n_of_children"
names(df)[names(df) == "D23"] <- "partner_citizenship"
# added column: age
df$age <- 2016 - df$yob

# education info
names(df)[names(df) == "D9"] <- "educational_title"
names(df)[names(df) == "D9b"] <- "recognized"
names(df)[names(df) == "D9c"] <- "recognized_binary"
names(df)[names(df) == "D10"] <- "italian_language_knowledge" # for Italian language

# religion
names(df)[names(df) == "D11"] <- "religion"

# economic info
names(df)[names(df) == "D26"] <- "avg_monthly_income_cohabiting_persons"
names(df)[names(df) == "D27a"] <- "personal_money_back_home"
names(df)[names(df) == "D27b"] <- "cohabiting_persons_money_back_home"
names(df)[names(df) == "D29"] <- "avg_working_hours_pw" #per week
names(df)[names(df) == "D32"] <- "avg_monthly_salary" # lavoro regolare, irregolare, pensioni

# health related info
names(df)[names(df) == "D35a"] <- "GP" # medico di famiglia, general practitioner
names(df)[names(df) == "D35b"] <- "first_aid" #pronto socc
names(df)[names(df) == "D35c"] <- "specialist_visits_SSN" 
names(df)[names(df) == "D35d"] <- "specialist_visits_private" # nel privato, libera professione
names(df)[names(df) == "D35g"] <- "volunteer_center"
names(df)[names(df) == "D36"] <- "prevention" # visite di prevenzione? Y/N


#check for OUTLIERS

#boxplot for every numeric variable
#select numeric features
df_num <- select_if(df,is.numeric) 
#create a tibble with two columns: the feature name and the values
data_num_box <-df_num %>% gather(variable,values)

ggplot(data_num_box)+
  geom_boxplot(aes(x=variable,y=values), fill = "aliceblue") + 
  facet_wrap(~variable,ncol=3,scales="free") + 
  theme(strip.text.x = element_blank(),
        text = element_text(size=12))

# Don't observe outliers, some rare values but not wrong.

# check for NA
missing_per_column_num <- colSums(is.na(df))
print(missing_per_column_num)

sum(df$yo_arrival_ita == 0, na.rm = TRUE)
sum(df$pob == 'Italy')

# if you are born in Italy, you will have yo_arrival_ita  = 0
# Replace NA values in a specific column with zero
df$yo_arrival_ita[is.na(df$yo_arrival_ita)] <- 0


# EDA -------------------------------------------------------

#genre visualization

#genre visualisation
male_perc <- nrow(df %>% dplyr::select(genre) %>% filter(genre == "Male"))/length(df$genre)*100
female_perc <- nrow(df %>% dplyr::select(genre) %>% 
                      filter(genre == "Female"))/length(df$genre)*100

pgen <- df %>%  
  count(genre) %>% 
  mutate("perc" = round(c(male_perc, female_perc),2))

pgen <- pgen %>% 
  arrange(desc(genre)) %>%
  mutate(prop = n / sum(pgen$perc) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(pgen, aes(x="", y=n, fill=genre)) +
  geom_bar(stat="identity", width=1,color="white")+
  coord_polar("y",0,1)+
  labs(x = "", y = "",title="genre")+
  theme_minimal()+ # remove background, grid, numeric labels
  theme(plot.title=element_text(face="bold",  hjust=0.5))+
  geom_text(aes(y = ypos, label = perc), color = "white", size=6) +
  scale_fill_brewer(palette="Set9")

#------------------------

#age for different genders

df$age_group <- cut(df$age, c(0, 18, 25, 50, 150), labels = c("<18", "18-24", "25-50", ">50"))

ggplot(df,aes(x=age_group, fill=genre))+
  geom_bar(col="black")+
  facet_wrap(.~genre)+
  stat_count(aes(y=..count.., label=..count..), vjust=-0.5,geom="text", col="black", size=3.5)+
  labs(x="Age Group", y = "Count", title="Age distribution", fill= "Sex")+
  theme_minimal()+
  theme(plot.title=element_text(face="bold",  hjust=0.5))+
  scale_fill_brewer(palette="Set9")

#--------------------------------
# place of birth

# Calculate percentages
italy_perc <- nrow(df %>% dplyr::select(pob) %>% filter(pob == "Italy")) / nrow(df) * 100
abroad_perc <- nrow(df %>% dplyr::select(pob) %>% filter(pob == "Abroad")) / nrow(df) * 100

# Create a dataframe for plotting
ppob <- df %>%  
  count(pob) %>% 
  mutate(perc = round(c(abroad_perc, italy_perc), 2))

# Arrange the dataframe and calculate y position for labels
ppob <- ppob %>% 
  arrange(desc(pob)) %>%
  mutate(prop = n / sum(ppob$perc) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

# Plot
ggplot(ppob, aes(x = "", y = n, fill = pob)) +
  geom_bar(stat = "identity", width = 1, color = "white")+
  coord_polar("y", 0, 1) +
  labs(x = "", y = "", title = "Place of Birth") +
  theme_minimal() + # Remove background, grid, numeric labels
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_text(aes(y = ypos, label = perc), color = "white", size = 6) +
  scale_fill_brewer(palette = "Set9")

#---------------------------------------

# continent citizenship

# Create a data frame with the counts
continent_counts <- data.frame(
  continent = c("Africa", "America", "Asia", "Europe"),
  count = c(416, 157, 235, 276)
)

# Create the bar plot
ggplot(continent_counts, aes(x = continent, y = count, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Citizenships by Continent", 
       x = "Continent", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") 


#-----------------------------------------------------

# prevention

# Calculate percentages
yes_perc <- nrow(df %>% dplyr::select(prevention) %>% filter(prevention == "Yes")) / nrow(df) * 100
no_perc <- nrow(df %>% dplyr::select(prevention) %>% filter(prevention == "No")) / nrow(df) * 100

# Create a dataframe for plotting
pprevention <- df %>%  
  count(prevention) %>% 
  mutate(perc = round(c(yes_perc, no_perc), 2))

# Arrange the dataframe and calculate y position for labels
pprevention <- pprevention %>% 
  arrange(desc(prevention)) %>%
  mutate(prop = n / sum(pprevention$perc) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

# Plot
ggplot(pprevention, aes(x = "", y = n, fill = prevention)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", 0, 1) +
  labs(x = "", y = "", title = "Prevention") +
  theme_minimal() + # Remove background, grid, numeric labels
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_text(aes(y = ypos, label = perc), color = "white", size = 6) +
  scale_fill_brewer(palette = "Set9") # Using Set3 palette for better contrast



# prevention by continent

ggplot(df, aes(x=citizenship, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set3")+
  labs(x=" ", fill="prevention", title="citizenship")+
  theme_minimal()+
  theme(plot.title= element_text(face = "bold", hjust=0.5))


# prevention by ed title

ggplot(df, aes(x=educational_title, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set3")+
  labs(x=" ", fill="prevention", title="ed title")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))  

# prevention by ed partner citizenship

ggplot(df, aes(x=partner_citizenship, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set3")+
  labs(x=" ", fill="prevention", title="ed title")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# prevention by religion

ggplot(df, aes(x=religion, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set3")+
  labs(x=" ", fill="prevention", title="ed title")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# -----------------------------------------------------------

# before analysis check correlation

# NUMERICAL VARIABLES
correlation <- cor(df_num, use = "pairwise.complete.obs")
#par(mar = c(1, 1, 1, 1))  # Adjust margin as needed
corrplot(correlation,type = "upper", method = "number", tl.col = "black",tl.srt = 45,
         tl.cex = 0.6)  # Adjust tl.cex for text size)


# CATEGORICAL VARIABLES ?????


library(DataExplorer)
DataExplorer::create_report(df)

#--------------------------------------------------------------
# correlation
library(psych)
corrs = psych::polychoric(df)

#correlation viz
GGally::ggcorr(data = NULL, 
               cor_matrix = corrs[["rho"]], 
               size = 2,
               hjust = .75,
               nbreaks = 7,
               palette = "RdYlBu",
               label = TRUE, 
               label_color = "black",
               digits = 2,
               #label_alpha = .3, 
               label_round = 2, 
               label_size = 1.85, 
               layout.exp = 0.2) + 
  theme(legend.position = "none")

corPlot(df,numbers=TRUE,upper=FALSE,diag=FALSE,
        ,xlas=2)


#--------------------------------------------------------------

# SUPERVISED

# LOGISTIC REGRESSION

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train <- df[sample, ]
test <- df[!sample, ] 




#fit logistic regression model
model <- glm(prevention~., family="binomial", data=subset(train, select=-c(citizenship_code)))
summary(model)

numerical_variables <- select_if(df, is.numeric)
data_with_prevention <- select(df, numerical_variables, prevention)

# Identify categorical variables
categorical_vars <- sapply(df, is.factor)

# Exclude 'prevention' from the list of categorical variables
categorical_vars["prevention"] <- FALSE

# Select only non-categorical variables or 'prevention'
data_2 <- df[, !categorical_vars | names(categorical_vars) == "prevention"]

model_2 <- glm(prevention~., family="binomial", data=data_2)
summary(model_2)


# Assuming your DataFrame is named df
# Assuming "genre" represents gender and "salary" is the variable of interest

# Create a boxplot
boxplot(avg_monthly_salary ~ genre, data = df, 
        xlab = "Gender", ylab = "Salary",
        main = "Boxplot of Salary by Gender")

anova_result <- anova(lm(avg_monthly_salary ~ genre, data = df))
anova_result

# tra uomini e donne c'è differenza nel fare prevenzione?
model_4 <- glm(prevention~genre, family="binomial", data=df)
summary(model_4)

data_3 <- df[, !categorical_vars | names(categorical_vars) == "genre"| names(categorical_vars) == "prevention" | names(categorical_vars) == "GP"]

na_indices <- is.na(data_3$yo_arrival_ita)


if (any(na_indices)) {
  data_3$yo_arrival_ita[na_indices] <- 0
}

data_3$ita_since <- data_3$age - data_3$yo_arrival_ita

model_5 <- glm(prevention~., family="binomial", data=subset(data_3, select=-c(yob, yo_arrival_ita)))
summary(model_5)

model_6 <- glm(prevention~., family="linear", data=subset(data_3, select=-c(yob, yo_arrival_ita)))
summary(model_6)

ct <- ctree(prevention ~ ., data=subset(data_3, select=-c(yob, yo_arrival_ita)))
plot(ct, tp_args = list(text = TRUE))


library(caret)

# Predict using the ctree model
predictions <- predict(ct, newdata = subset(data_3, select=-c(yob, yo_arrival_ita)))

# Calculate accuracy
accuracy <- confusionMatrix(predictions, data_3$prevention)$overall['Accuracy']

# Print accuracy
print(paste("Accuracy:", accuracy))

# Calculate confusion matrix
conf_matrix <- confusionMatrix(predictions, data_3$prevention)

# Print confusion matrix
print(conf_matrix)

summary(df)


your_data = df

# Load necessary libraries
library(MASS)
library(car)
library(pROC)

# Build the initial model
initial_model <- glm(prevention ~ ., family = binomial, data = your_data)

# Perform stepwise selection
final_model <- stepAIC(initial_model, direction = "both")

# Check the summary of the final model
summary(final_model)

# Make predictions
predictions <- predict(final_model, newdata = your_data, type = 'response')
classified_predictions <- ifelse(predictions > 0.5, "YES", "NO")

# Evaluate the model
conf_matrix <- table(your_data$prevention, classified_predictions)
roc_curve <- roc(response = your_data$prevention, predictor = as.numeric(classified_predictions))

# Output the results
print(conf_matrix)
plot(roc_curve)

# Ensure 'prevention' is a factor with two levels
your_data$prevention <- factor(your_data$prevention, levels = c("No", "Yes"))

# Convert 'classified_predictions' to numeric
classified_predictions <- as.numeric(classified_predictions)

# Check for NAs and remove them if necessary
na_indices <- which(is.na(classified_predictions))
classified_predictions <- classified_predictions[-na_indices]
your_data <- your_data[-na_indices, ]

# Now try generating the ROC curve again
roc_curve <- roc(response = your_data$prevention, predictor = classified_predictions)
plot(roc_curve)

install.packages('remotes')
remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.3/catboost-R-windows-x86_64-1.2.3.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))

# CATBOOST??
# mi sa proprio di no
library(catboost)

# Split data into predictors and target variable
X <- df[, !(names(df) %in% c("prevention"))]
y <- df$prevention

# Split data into training and testing sets
set.seed(123)
train_idx <- sample(1:nrow(df), 0.8 * nrow(df))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

# Train the CatBoost model
catboost_model <- catboost.train(X_train,
                                 y_train)

# Make predictions
predictions <- predict(model, catboost.load_pool(X_test))

# Evaluate model
evaluation <- catboost.eval_metrics(data = catboost.load_pool(X_test, y_test), prediction = predictions, metrics = "AUC")
print(evaluation)


# Load the CatBoost library
library(catboost)

pool_path <- "C:\\Users\\IsabellaCadisco\\Documents\\uni\\statistical learning"

catboost.save_pool(data = as.matrix(df), filename = pool_path)

pool <- catboost.load_pool(data = as.matrix(df), filename = pool_path)

target <- 1

fit_params <- list(iterations = 100,
                   loss_function = 'Logloss')

model <- catboost.train(pool, label = df[, target], params = fit_params)


pool_test <- catboost.load_pool(as.matrix(df_test))

# Make predictions
predictions <- catboost.predict(model, pool_test)

# View predictions
print(predictions)


# UNSUPERVISED, CLUSTERING

