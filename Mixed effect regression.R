


#load packaged
library(sjPlot)
library(glmmTMB)
library(lme4)
library(psych)
library(ggplot2)
library(dplyr)
library(car)
library(lmerTest)
library(modelsummary)

#load data 
load("dataLab2-1.Rdata") #has two datasets 
head(dtA)
head(dtB)

# Combine the two datasets into one dataset
my_data<- rbind(dtA, dtB)


#exploring the data
View(my_data)
my_data <- subset(my_data, select = -c(ID)) #Removing ID variable(irrelevant)
names(my_data)
summary(my_data)
str(my_data)
which(is.na(my_data)==T) #check for missing data


#Descriptiv stat for the controll variable


#SEX

#summary stat showed that there is female, male and #woman!
#display the rows where sex is labeled as woman, to see if it is a typo
which(my_data$sex == "woman")
 
#It was only in one row, we will consider it as female
# Replace woman with female using if else
my_data$sex <- ifelse(my_data$sex == "woman", "female", my_data$sex)

# convert sex to factor and choose male as reference category
my_data$sex <- as.factor(my_data$sex)
my_data$sex <- relevel(my_data$sex, ref = "male")

#checking
unique(my_data$sex)
summary(my_data$sex)

#visualisation of Pain Levels by Gender

ggplot(my_data, aes(x = pain, fill = sex)) +
  geom_density(alpha = 0.9) +
  labs(title = "Pain Levels by Gender") +
  theme_minimal()




#AGE
#descriptiv stat for age
str(my_data$age)
describe(my_data$age)

# histogram for age variable
ggplot(my_data, aes(x = age)) +
  geom_histogram()

#visualisation of Pain Levels by age
ggplot(my_data, aes(x = age, y = pain)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm) + geom_jitter()+
  labs(title = "Pain by Age", x = "Age", y = "Pain Level") +
  theme_minimal()


#weight
str(my_data$weight)
describe(my_data$weight)

# histogram for weight variable
ggplot(my_data, aes(x = weight)) +
  geom_histogram()  

#visualisation of Pain Levels by weight
ggplot(my_data, aes(x = weight, y = pain)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm) + geom_jitter()+
  labs(title = "Pain by weight", x = "weight", y = "Pain Level") +
  theme_minimal()


#IQ
str(my_data$IQ)
describe(my_data$IQ)


# Create a histogram for 'IQ' variable
ggplot(my_data, aes(x = IQ)) +
  geom_histogram()

#visualisation of Pain Levels by IQ
ggplot(my_data, aes(x = IQ, y = pain)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm) + geom_jitter()+
  labs(title = "Pain by IQ", x = "IQ", y = "Pain Level") +
  theme_minimal()

#household_income

str(my_data$household_income)
summary(my_data$household_income)
#There is typo in household income, where one value is negative.
# Replace the negative household income value with its positive equivalents using function "abs"
my_data$household_income <- ifelse(my_data$household_income < 0, abs(my_data$household_income), my_data$household_income)
#checking 
describe(my_data$household_income)

#histogram for household_income variable
ggplot(my_data, aes(x = household_income)) +
  geom_histogram()

#visualisation of Pain Levels by household income
ggplot(my_data, aes(x = household_income, y = pain)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm) + geom_jitter()+
  labs(title = "Pain by household income", x = "household income", y = "Pain Level") +
  theme_minimal()





##exploring clustering in hospital variable

#changing the hospital variable to factor
my_data$hospital <- as.factor(my_data$hospital)
summary(my_data$hospital)

# Calculate variance of pain scores within each hospital to determine if there is clustering 
variance <- my_data %>%
  group_by(hospital) %>%
  summarize(variance_pain = var(pain)) 

print(variance)

# visualisation plot of pain levels by hospital 
ggplot(my_data, aes(x = hospital, y = pain, fill = hospital)) +
  geom_boxplot() +
  labs(title = "Distribution of Pain levels by Hospital",
       x = "Hospital", y = "Pain level") +
  theme_minimal()



#correlation between all numerical variables 
cor(my_data[,unlist(lapply(my_data, is.numeric))])
names(my_data)
#there is strong correlation between cortisol serum and cortisol saliva, with a coefficient of 0.9
#Multicollinarity:
mod_1 <- lm (pain ~ mindfulness + STAI_trait + cortisol_saliva + cortisol_serum + pain_cat + sex + age + weight + IQ + household_income, data = my_data)
vif(mod_1) 
#(cortisol_saliva: VIF = 7.52, cortisol_serum: VIF = 6.97. These high values indicating significant multicollinearity
#Only cortisol serum will be included in the analysis. 



#regression model in which all established findings are tested

mixed_model <- lmer(pain ~ STAI_trait + mindfulness + cortisol_serum + pain_cat + sex + age + weight + IQ + household_income + (1|hospital), data = my_data)
summary(mixed_model)



#create summary table
msummary(mixed_model,
         stars = TRUE,
         title = "Table 3. Mixed model",
         gof_omit = "AIC|BIC|ICC",
         out = "html")



#Normality of residuals
# Histogram of residuals
hist(residuals(mixed_model))
# QQ plot for residuals
qqnorm(residuals(mixed_model))
qqline(residuals(mixed_model))

#shapiro.test: Higher P value -> normally distributed 
shapiro.test(residuals(mixed_model))



#Normality of random effect

# Extract the random effects of the model
random_effects <- ranef(mixed_model)
print(random_effects)
#This output is in list form: (data frame with each row corresponding to a level of the hospital and the estimated random effect) 
# This can not be visualized. 
#Unlist function will solve this through displaying only the random effect values.
random_effects_values <- unlist(random_effects)  
print(random_effects_values)

#Now we can continue working with the plots
# Q-Q plot for random effect
qqnorm(random_effects_values)
qqline(random_effects_values)

# Histogram for random effect
hist(random_effects_values, )

#Stat summary for random effect
summary(random_effects_values)

#shapiro.test: Higher P value -> normally distributed 
shapiro.test(random_effects_values)




#calcuting the mean of each variable for the prediction task
averages <- data.frame(
  mindfulness = mean(my_data$mindfulness),
  STAI_trait = mean(my_data$STAI_trait),      
  cortisol_serum = mean(my_data$cortisol_serum),
  pain_cat = mean(my_data$pain_cat),
  household_income = mean(my_data$household_income),
  IQ = mean(my_data$IQ),
  weight = mean(my_data$weight))
print(averages)



