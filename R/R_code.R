source("R/packages.R")

#-------------------------------PREPROCESSING------------------------------------------------------

# import df

df <- read.csv("data/survey_2016_filtered.csv", header = TRUE)

#---------------------------------------------------------------

# info about df

str(df)
glimpse(df)
# generate latex for report
subset_df <- df[1:5,]
latex_table <- xtable(subset_df)
print(latex_table, include.rownames = FALSE) 
rm(latex_table)
rm(subset_df)

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

rm(education_mapping)

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
df <- subset(df, select= -c(D35e, D35f))

# D35g: In the past 12 months, volunteer health center.
df$D35g <- factor(df$D35g, levels = c(1, 2), labels = c("Yes", "No"))

# D36: Do you do checkups and prevention?
df$D36 <- factor(df$D36, levels = c(1, 2), labels = c("Yes", "No"))


# rename columns

# general info
names(df)[names(df) == "D1"] <- "genre"
names(df)[names(df) == "D2"] <- "yob"
names(df)[names(df) == "D3"] <- "pob" #place of birth
names(df)[names(df) == "Eta_arrivo_Italia"] <- "age_arrival_ita" # anno arrivo in italia
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

colnames(df)

# check for NA

missing_per_column_num <- colSums(is.na(df))
print(missing_per_column_num)

sum(df$age_arrival_ita == 0, na.rm = TRUE)
sum(df$pob == 'Italy')

# if you are born in Italy, you will have age_arrival_ita  = 0
# Replace NA values in a specific column with zero
df$age_arrival_ita[is.na(df$age_arrival_ita)] <- 0
# questa è un'imprecisione, arrivare a 0 anni o essere nato qui non è uguale

missing_per_column_num <- colSums(is.na(df))
print(missing_per_column_num)
rm(missing_per_column_num)

# how many years in italy?
df$years_in_ita <- df$age - df$age_arrival_ita

# legal status
unique(df$legal_status)

df <- df %>%
  mutate(legal_status_binary = ifelse(legal_status %in% factor(c("Dual citizenship",
                                                                 "Valid residence permit/visa",
                                                                 "CE long-term residence permit",
                                                                 "EU citizens (or dual citizenship of another EU country)",
                                                                 "Expired residence permit/visa being renewed")), 
                                      "Legal", "Illegal"))


df$legal_status_binary <- as.factor(df$legal_status_binary)

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
rm(data_num_box)
# Don't observe outliers, some rare values but not wrong.

# re order variables

colnames(df)

df_reordered <- df[,c(
  # general info
  "genre",
  "yob",
  "age",
  "pob", 
  "citizenship",
  "citizenship_code",
  "religion",
  
  # migration and legal status
  "age_arrival_ita", # RINOMINARLA IN AGE!
  "years_in_ita",
  "legal_status",
  "legal_status_binary",
  
  # education
  "educational_title",
  "years_of_education",
  "recognized", 
  "italian_language_knowledge",
  "recognized_binary",
  
  # economic well being
  "avg_monthly_salary",
  "avg_working_hours_pw",
  "avg_monthly_income_cohabiting_persons",
  
  # family and relationships
  "marital_status",
  "partner_citizenship",
  "n_of_children",
  "n_of_cohabiting_persons",
  "personal_money_back_home",
  "cohabiting_persons_money_back_home",
  
  # health
  "GP",
  "first_aid",
  "specialist_visits_SSN",
  "specialist_visits_private",
  "volunteer_center",
  "prevention"
  
)]

summary(df_reordered)

write.csv(df_reordered, file="C:\\Users\\IsabellaCadisco\\Documents\\uni\\statistical learning\\statistical_learning\\data\\reordered_data.csv")

rm(df_num)
df <- df_reordered
rm(df_reordered)

# latex table

str(df)
glimpse(df)
# generate latex for report
subset_df <- df[1:5,]
latex_table <- xtable(subset_df)
print(latex_table, include.rownames = FALSE) 
rm(latex_table)
rm(subset_df)

# element example
# first row of the dataframe
first_row <- df[1, ]
latex_list <- "\\begin{itemize}[label=\\textbullet]\n"

for (variable in names(first_row)) {
  value <- first_row[[variable]]
  latex_list <- paste0(latex_list, "  \\item ", variable, ": ", value, "\n")
}
latex_list <- paste0(latex_list, "\\end{itemize}")
cat(latex_list)
rm(first_row)
rm(latex_list)
rm(value)
rm(variable)
#--------------------------------- end preprocessing -------------------------------------------

# ------------------- Visual EDA ----------------------------------------------------------------

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
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  geom_text(aes(y = ypos, label = perc), color = "white", size = 6) +
  scale_fill_brewer(palette = "Set4") 

# ------ prev by ... -----------------------

# prevention by continent

ggplot(df, aes(x=citizenship, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set4")+
  labs(x=" ", fill="prevention", title="citizenship")+
  theme_minimal()+
  theme(plot.title= element_text(face = "bold", hjust=0.5))


# prevention by ed title

ggplot(df, aes(x=educational_title, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set4")+
  labs(x=" ", fill="prevention", title="ed title")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))  

# prevention by partner citizenship

ggplot(df, aes(x=partner_citizenship, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set4")+
  labs(x=" ", fill="prevention", title="ed title")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# prevention by religion

ggplot(df, aes(x=religion, fill= prevention))+
  geom_bar(col="black", alpha=0.9)+
  scale_fill_brewer(label=c("No","Yes"),palette="Set4")+
  labs(x=" ", fill="prevention", title="ed title")+
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
#-------------------------------------------------

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
  scale_fill_brewer(palette="Set4")

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
  scale_fill_brewer(palette = "Set4")

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
  scale_fill_brewer(palette = "Set4") 

rm(continent_counts, pgen, ppob, pprevention, abroad_perc, female_perc, italy_perc, male_perc, no_perc, yes_perc)
# ---------------------------------------------------------

# variable of interest: prevention
summary(df$prevention)

##### rm yob, age_arrival_italy
df = subset(df, select=-c(yob, age_arrival_ita))

#------------------------------ CORRELATION STUDY ---------------------------------------------------

# between num variables

# NUMERICAL VARIABLES
df_num <- df %>% select_if(is.numeric)

correlation <- cor(df_num) #, use = "pairwise.complete.obs"
corrplot(correlation,type = "upper", method = "number", tl.col = "black",tl.srt = 45,
         tl.cex = 0.65)  # text size

# correlation not so strong, can avoid risk of multicollinearity

# correlation with numerical variables: Point-Biserial Correlation
# between a dichotomous and a continuous variable.
# https://rdocumentation.org/packages/ltm/versions/1.2-0/topics/biserial.cor

binary_column <- 'prevention'
correlations <- sapply(df_num, function(x) biserial.cor(x, df[[binary_column]], level = 1))
# level = 1 -> YES, R conta da 1!
correlations

# latex table
correlation_df <- data.frame(
  Variable = c("age", "years_in_ita", "years_of_education", 
               "avg_monthly_salary", "avg_working_hours_pw", "avg_monthly_income_cohabiting_persons", 
               "n_of_children", "n_of_cohabiting_persons", "personal_money_back_home", 
               "cohabiting_persons_money_back_home"),
  Correlation = c(0.16465706, 0.16004371, 0.09683370, 
                  0.06027333, -0.03337300, 0.17890122, 0.11117804, 0.09300404, 
                  -0.03718916, -0.04303892)
)
latex_table <- xtable(correlation_df, digits = 6)
print(latex_table)

# logistica anche su numerica

# these are the correlations with prevention = Yes.
# the ones with prevention = No are just the same with opposite sign.
# no strong correlations 
# le variabili individualmente sono poco correlate con prevention
# QUESTO è SBAGLIATO: variables likely to be useless to predict or explain the choice for prevention
# questo discorso ha senso se ho variabili tanto correlate che però non sono correlate con output (target, prevention)

# se io ho variabili che tra di loro non sono tanto corre
# mi danno tutte info diverse
# poco correlate con target
# ognuna mi dà un po' di info rispetto a output
# possono essere predittive se effetti sommati insieme

# non è detto che non posso predirla però
# le sto guardano singolarmente
# fare logistic regression

colnames(df_num)

# correlation with categorical variables: Chi Square test ------------------------------------

# Chi-Square test: if we assume that two variables are independent, 
#then the values of the contingency table for these variables should be distributed uniformly. 
#And then we check how far away from uniform the actual values are

# Crammer's V: a measure of association between two nominal variables, giving a value between 0 and +1 
# https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V
# Cramér's V is computed by taking the square root of the chi-squared statistic 
# divided by the sample size and the minimum dimension minus 1
# since our minimum dimension is 1, we simplyy divide by the sample size

categorical_columns <- df %>%
  select_if(is.factor)

categorical_columns <- subset(categorical_columns, select= -c(prevention))

chi_square_results <- list()

variable_names <- c()
p_values <- c()
crammers_v <- c()

for (col in names(categorical_columns)) {
  # contingency table
  contingency_table <- table(df[[col]], df[[binary_column]])
  
  # chi-square test
  chi_square_result <- chisq.test(contingency_table)
  variable_names <- c(variable_names, col)
  p_values <- c(p_values, round(chi_square_result$p.value, 6))
  crammers_v <- c(crammers_v, sqrt(chi_square_result$statistic / sum(contingency_table)))
}

results_df <- data.frame(
  Variable = variable_names,
  crammers = crammers_v,
  P_Value = p_values
)
print(results_df)

# latex
latex_table <- xtable(results_df, digits = 6)
print(latex_table)

rm(chi_square_result, correlation_df, latex_table, results_df, col, contingency_table, correlation, crammers_v, p_values, correlations)
rm(chi_square_results)

# almost not correlation between numerical variables and the target
# p values between categorical variables and target almost all significant 
#-------------------------------------------------------------------------------------------


# ----------------------- Supervised Learning ----------------------------------------------


# LOGISTIC REGRESSION
# https://r.qcbs.ca/workshop06/book-en/binomial-glm.html
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm

# THE FIRST LEVEL DENOTES FAILURE AND ALL OTHERS SUCCES
# invert levels
levels(df$prevention)
df$prevention <- factor(df$prevention, levels = rev(levels(df$prevention)))
# check inverted levels
levels(df$prevention)


# looking for the category with largest representation
# to choose the ref class for each variable

summary(df$genre)
# Male Female -> take Male as reference category
# 643    441 
df$genre = relevel(df$genre, ref = "Male")

summary(df$citizenship)
# Africa America    Asia  Europe -> take Africa as ref
# 416     157     235     276
df$citizenship = relevel(df$citizenship, ref = "Africa")

summary(df$religion)
# in this case the majority is Muslim, but i think i'll decide 
# to use No religion as reference category
df$religion = relevel(df$religion, ref = "No religion")

summary(df$legal_status_binary)
# Illegal   Legal -> the great majority legal, maybe don't include
# if you don't have documents maybe first aid
# but prevention, i don't think so
# 29    1055 
df$legal_status_binary = relevel(df$legal_status_binary, ref = "Legal")

summary(df$italian_language_knowledge)
# big majority 'very well'
# i expect more prevention but who knows
df$italian_language_knowledge = relevel(df$italian_language_knowledge, ref = "Very well")

summary(df$partner_citizenship)
# majority same as respondent
# i expect if italian easier to go for prevention
# also interesting i have the no partner option
# possible to say something also about the impact of being alone :(
df$partner_citizenship = relevel(df$partner_citizenship, ref = "Same as respondent")



# train test split
set.seed(123)  # reproducibility
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]



# NUMERICAL VARIABLES ---------------------------------------------------


numerical_vars_string <- paste(colnames(df_num), collapse = " + ")

model_0 <- glm(prevention ~ age + years_in_ita + years_of_education + 
                 avg_monthly_salary + avg_working_hours_pw + 
                 avg_monthly_income_cohabiting_persons + n_of_children + 
                 n_of_cohabiting_persons + personal_money_back_home + 
                 cohabiting_persons_money_back_home, data = train_data, family = "binomial")

summary(model_0)

# predictions
train_predictions <- predict(model_0, newdata = train_data, type = "response")
test_predictions <- predict(model_0, newdata = test_data, type = "response")
# predicted probabilities to binary predictions
train_pred_binary <- ifelse(train_predictions > 0.5, 1, 0)
test_pred_binary <- ifelse(test_predictions > 0.5, 1, 0)

# train and test accuracy
train_accuracy <- mean(train_pred_binary == ifelse(train_data$prevention == "No", 0, 1))
test_accuracy <- mean(test_pred_binary == ifelse(test_data$prevention == "No", 0, 1))

# train and test accuracy
print(paste("Train Accuracy:", train_accuracy))
print(paste("Test Accuracy:", test_accuracy))

# McFadden R^2
pscl::pR2(model_0)["McFadden"]


# CATEGORICAL VARIABLES ---------------------------------------------------

categorical_train <- train_data %>%
  select_if(is.factor)
categorical_test <- test_data %>%
  select_if(is.factor)


model_1 = glm(prevention ~ ., family = 'binomial', data = subset(categorical_train, select= -c(citizenship_code, recognized_binary,
                                                                                               recognized,legal_status, legal_status_binary,
                                                                                               specialist_visits_SSN,
                                                                                               specialist_visits_private)))
summary(model_1)

model_1$coefficients

# coefficients and p-values
coefficients <- coef(summary(model_1))[, 1]
p_values <- coef(summary(model_1))[, 4]

# coefficients with p-value < 0.05
significant_coefficients <- coefficients[p_values < 0.05]

significant_variables <- names(significant_coefficients)

# histogram of significant coefficients
barplot(significant_coefficients, main = "Histogram of Significant Coefficients", xlab = "Variables", ylab = "Coefficients", 
     names.arg = significant_variables, col = "skyblue", las=2, cex.names = 0.8)


# predictions
train_predictions <- predict(model_1, newdata = categorical_train, type = "response")
test_predictions <- predict(model_1, newdata = categorical_test, type = "response")
# predicted probabilities to binary predictions
train_pred_binary <- ifelse(train_predictions > 0.5, 1, 0)
test_pred_binary <- ifelse(test_predictions > 0.5, 1, 0)

# train and test accuracy
train_accuracy <- mean(train_pred_binary == ifelse(categorical_train$prevention == "No", 0, 1))
test_accuracy <- mean(test_pred_binary == ifelse(categorical_test$prevention == "No", 0, 1))

# train and test accuracy
print(paste("Train Accuracy:", train_accuracy))
print(paste("Test Accuracy:", test_accuracy))

# McFadden R^2
pscl::pR2(model_1)["McFadden"]

# ------------------ COMPLETE MODEL -------------------------------

model_2 = glm(prevention ~ ., family = 'binomial', data = subset(train_data, select= -c(citizenship_code, recognized_binary,
                                                                                               recognized,legal_status, legal_status_binary,
                                                                                               specialist_visits_SSN,
                                                                                               specialist_visits_private)))
summary(model_2)


# coefficients and p-values
coefficients <- coef(summary(model_2))[, 1]
p_values <- coef(summary(model_2))[, 4]

# coefficients with p-value < 0.05
significant_coefficients <- coefficients[p_values < 0.05]

significant_variables <- names(significant_coefficients)

# histogram of significant coefficients
barplot(significant_coefficients, main = "Histogram of Significant Coefficients", xlab = "Variables", ylab = "Coefficients", 
        names.arg = significant_variables, col = "skyblue", las=2, cex.names = 0.8)


# predictions
train_predictions <- predict(model_2, newdata = train_data, type = "response")
test_predictions <- predict(model_2, newdata = test_data, type = "response")
# predicted probabilities to binary predictions
train_pred_binary <- ifelse(train_predictions > 0.5, 1, 0)
test_pred_binary <- ifelse(test_predictions > 0.5, 1, 0)

# train and test accuracy
train_accuracy <- mean(train_pred_binary == ifelse(train_data$prevention == "No", 0, 1))
test_accuracy <- mean(test_pred_binary == ifelse(test_data$prevention == "No", 0, 1))

# train and test accuracy
print(paste("Train Accuracy:", train_accuracy))
print(paste("Test Accuracy:", test_accuracy))

# McFadden R^2
pscl::pR2(model_2)["McFadden"]

#-------------------------------------------------------------------------------------------------

# -------------- use genre -----------------------------------

model_1 = glm(prevention ~ genre, family = 'binomial', data = df)
summary(model_1)

# check how to go back to prob exactly
model_1$coefficients

# prob doing prev if female P(prev=1|femal=1)
# approsismata perchè appresa con MLE
1/(1+exp(-(1.1838950 + -0.4392785)))
# calcolare prob
mean(df[df$genre=='Female',]$prevention=='Yes')
# P(prev=1|female=0)


# space for more interpretations,
# but for now being female increases the odds of doing prevention

# ----------- use genre and religion -----------------------------------------------------
model_1 = glm(prevention ~ genre+religion, family = 'binomial', data = df)
summary(model_1)
# religionHindu  estimate 1.3805 w/ p-value < 0.05
# gli hindu fanno più prevenzione che nessuna religione
# ------------------------------------------------------

# ------------ use genre and legal status binary --------------
model_2 = glm(prevention ~ genre+legal_status_binary, family = 'binomial', data = df)
summary(model_2)
# legal status binary not significant, maybe too few obs
#---------------------------------------------------

# ------------ use genre and legal status --------------
model_3 = glm(prevention ~ genre+legal_status, family = 'binomial', data = df)
summary(model_3)
# interpretations to do 
#---------------------------------------

# ------------ use genre and citizenship --------------
model_4 = glm(prevention ~ genre+citizenship, family = 'binomial', data = df)
summary(model_4)
# interpretations to do
#---------------------------------------

# ------------ use citizenship --------------
model_5 = glm(prevention ~ citizenship, family = 'binomial', data = df)
summary(model_5)
# intepretations to do
#------------------------

# ------------ use partner citizenship --------------
model_6 = glm(prevention ~ partner_citizenship, family = 'binomial', data = df)
summary(model_6)
# to do intepretations
# ----------------------------------------------------

# ------------ COMPLETE MODEL -------------------------------
# just to try
# don't consider numerical variables
#categorical_columns <- df %>% QUESTA COSA LA FACCIO PRIMA PER TEST CORRELAZIONE
#  select_if(is.factor)


model_7 = glm(prevention ~ ., family = 'binomial', data = categorical_columns)
summary(model_7)


model_8 = glm(prevention ~ ., family = 'binomial', data = subset(df, select= -c(citizenship_code)))
summary(model_8)

# il coeff di genre femare duplica
# potrebbe essere perchè l'effetto negativo di un'altra classe di un'altra variabile 
# prima si mischiava alle donne??

# ci sono dei risultati folli sui citizenship_code, fare un modello con loro?? 

# --------------- use citizenship_code ------------------------------------------
model_8 = glm(prevention ~ citizenship_code, family = 'binomial', data = df)
summary(model_8)
# ho coeff minori per tutti
# perchè il livello di significatività decresce se tolgo le altre variabili?
# ----------------------------------------------------------------------------------------------------------------------------------

# ----------------- LASSO / RIDGE ---------
# may be interesting for the future
# ------------------------------------------



# ------------ DECISION TREE ------------------



df <- subset(df, select= -c(citizenship_code, recognized_binary,
                            recognized,legal_status, legal_status_binary,
                            specialist_visits_SSN,
                            specialist_visits_private))

set.seed(123)  # reproducibility
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# formula
formula <- prevention~ .

# train 
tree_model <- rpart(formula, data = train_data, method = "class", control = rpart.control(maxdepth = 7))

# predictions on the training data
train_predictions <- predict(tree_model, train_data, type = "class")

# predictions on the test data
test_predictions <- predict(tree_model, test_data, type = "class")

# training accuracy
train_accuracy <- sum(train_predictions == train_data$prevention) / nrow(train_data)
train_accuracy
# test accuracy
test_accuracy <- sum(test_predictions == test_data$prevention) / nrow(test_data)
test_accuracy
# print
cat("Training Accuracy:", train_accuracy, "\n")
cat("Test Accuracy:", test_accuracy, "\n")

# plot 
rpart.plot(tree_model, yesno = 2, type = 0, extra = 101, fallen.leaves = FALSE, cex = 0.6)




# ------------ UNSUPERVISED --------

# hierarchical considering: 

data_hc = subset(df, select= -c(prevention))
gower_dist<-daisy(df, metric = "gower")

# hierarchical clustering
hc_visual<-hclust(gower_dist, method = "ward.D2")

hc <- hclust(gower_dist, method = "ward.D2")
hc

# dendrogram 
plot(hc_visual, cex = 0.6, hang=-1, labels=FALSE)
rect.hclust(hc_visual, k=6, border="red")

# choose k, number of clusters 5 
cluster<-cutree(hc_visual, k=6)
# add cluster to original data 
data<-cbind(df,cluster = as.factor(cluster))
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
#------------------------------------------------


