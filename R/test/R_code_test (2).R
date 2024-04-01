source("R/packages.R")

# import data

df <- read.csv("data/survey_2016.csv", header = TRUE)

df_test <- read.csv("data/survey_2016.csv", header = TRUE)
df_test$D35b
df_test$D35b <- factor(df_test$D35b, levels = c(1, 2), labels = c("Yes", "No"))

df_test$D7 <- as.factor(df_test$D7)

str(df_test)

# ----------------------

# info about data

str(df)

'''
unique_values <- lapply(df, unique)
for (col in names(unique_values)) {
  cat("Column:", col, "\n")
  print(unique_values[[col]])
}'''


# convert categorical variables INT -> FACTOR

# Convert D1 to factor with labels
df$D1 <- factor(df$D1, levels = c(1, 2), labels = c("Male", "Female"))

# Convert D3 to factor with labels
df$D3 <- factor(df$D3, levels = c(1, 2), labels = c("Abroad", "Italy"))

# Convert D8 to factor with labels
df$D8 <- factor(df$D8, levels = c(1, 2, 3, 4), labels = c("Single", "Married", "Widowed", "Divorced/Separated"))

# Convert D9 to factor with labels
df$D9 <- factor(df$D9, levels = c(1, 2, 3, 4, 5, 6), labels = c("None", "Primary school", "Lower secondary school", "Vocational qualification", "Upper secondary school", "University degree or postgraduate diploma"))

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


# new variable for continent BASED ON D7, citizenship
df$Continent <- ifelse(substr(df$D7, 1, 1) %in% c("2"), "Europe",
                       ifelse(substr(df$D7, 1, 1) %in% c("3"), "Asia",
                              ifelse(substr(df$D7, 1, 1) %in% c("4"), "Africa",
                                     ifelse(substr(df$D7, 1, 1) %in% c("5", "6"), "America",
                                            ifelse(df$D7 == "999", "Apolide", NA)))))

df$Continent <- factor(df$Continent)

# check
str(df)
print(df)
head(df)


# rename columns

names(df)[names(df) == "D1"] <- "genre"
names(df)[names(df) == "D2"] <- "yob"
names(df)[names(df) == "D3"] <- "pob" #place of birth
names(df)[names(df) == "Eta_arrivo_Italia"] <- "yo_arrival_ita" # anno arrivo in italia
names(df)[names(df) == "D7"] <- "citizenship"
names(df)[names(df) == "D8"] <- "marital_status"
names(df)[names(df) == "D9"] <- "educational_title"
names(df)[names(df) == "D9b"] <- "recognized_ed_title"
names(df)[names(df) == "D10"] <- "language_knowledge" # for italian language
names(df)[names(df) == "D11"] <- "religion"
names(df)[names(df) == "D12"] <- "legal_status" # Condizione giuridico-amministrativa
names(df)[names(df) == "D20a"] <- "n_of_children"
names(df)[names(df) == "D23"] <- "partner_citizenship"
names(df)[names(df) == "D25"] <- "n_of_cohabiting_persons"
names(df)[names(df) == "D26"] <- "avg_monthly_income_cohabiting_persons"
names(df)[names(df) == "D27a"] <- "personal_money_back_home"
names(df)[names(df) == "D27b"] <- "family_money_back_home"
names(df)[names(df) == "D29"] <- "avg_working_hours_pw" #per week
names(df)[names(df) == "D32"] <- "avg_monthly_salary" # lavoro regolare, irregolare, pensioni

names(df)[names(df) == "D35a"] <- "GP" # medico di famiglia, general practitioner
names(df)[names(df) == "D35b"] <- "first_aid" #pronto socc
names(df)[names(df) == "D35c"] <- "specialist_visits_SSN" 
names(df)[names(df) == "D35d"] <- "specialist_visits_private" # nel privato, libera professione
names(df)[names(df) == "D35e"] <- "hospitalization_SSN"
names(df)[names(df) == "D35f"] <- "hospitalization_private"
names(df)[names(df) == "D35g"] <- "volunteer_center"

names(df)[names(df) == "D36"] <- "prevention" # visite di prevenzione? Y/N


# convert into factors

df$genre <- factor(df$genre, levels = c(1, 2), labels = c("male", "female"))
df$genre
