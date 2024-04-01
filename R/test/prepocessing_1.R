
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
# i want 1: Yes, 0: No
df$D35a
df$D35a[df$D35a == 2] <- 0
#df$D35a
df$D35a <- factor(df$D35a, levels = c(1, 0), labels = c("Yes", "No"))
df$D35a


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
df$D36[df$D36 == 2] <- 0
df$D36 <- factor(df$D36, levels = c(1, 0), labels = c("Yes", "No"))


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
rm(df)

