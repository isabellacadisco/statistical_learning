source("R/packages.R")

df <- df_reordered
summary(df)

rm(df_reordered)


df_num <- df %>% select(where(is.numeric))

# before analysis check CORRELATION

# NUMERICAL VARIABLES

# consider (yob, age_arrival_ita) OR (age, years_in_ita)
# age, years_in_ita
df_num_sel = subset(df_num, select=-c(yob, age_arrival_ita))

correlation <- cor(df_num_sel) #, use = "pairwise.complete.obs"
#par(mar = c(1, 1, 1, 1))  # Adjust margin as needed
corrplot(correlation,type = "upper", method = "number", tl.col = "black",tl.srt = 45,
         tl.cex = 0.65)  # Adjust tl.cex for text size)

# correlation not so strong, can avoid risk of multicollinearity


# ----------- # MODEL_0  --------------------------------

# data for MODEL_0
categorical_vars <- sapply(df, is.factor)
df_model_0 <- df[, !categorical_vars | names(categorical_vars) == "prevention"]
df_model_0 = subset(df_model_0, select=-c(yob, age_arrival_ita))
rm(categorical_vars)
summary(df_model_0)

# penso che faccia in automatico la conversione
#df_model_0$prevention <- df_model_0$prevention == 'Yes'
#summary(df_model_0)

#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df_model_0), replace=TRUE, prob=c(0.7,0.3))
train <- df_model_0[sample, ]
test <- df_model_0[!sample, ]  

summary(train)

#fit logistic regression model
model <- glm(prevention~., family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)

coef(model)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(model))

# model with just GP
simple_model_gp = glm(prevention~GP, family="binomial", data=df)
summary(simple_model_gp)
logit2prob(coef(simple_model_gp))
pscl::pR2(simple_model_gp)["McFadden"]

# model with just citizenship
simple_model_citizenship = glm(prevention~citizenship, family="binomial", data=df)
summary(simple_model_citizenship)
logit2prob(coef(simple_model_citizenship))
pscl::pR2(simple_model_citizenship)["McFadden"]

# model with just citizenship
model_citizenship_GP = glm(prevention~citizenship+GP, family="binomial", data=df)
summary(model_citizenship_GP)
logit2prob(coef(model_citizenship_GP))
pscl::pR2(model_citizenship_GP)["McFadden"]


# model with just citizenship
model_years_in_ita = glm(prevention~years_in_ita, family="binomial", data=df)
summary(model_years_in_ita)
logit2prob(coef(model_years_in_ita))
pscl::pR2(model_years_in_ita)["McFadden"]
