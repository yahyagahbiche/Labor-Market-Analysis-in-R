#uploading dataset
mydata<-read.csv("C:/Users/Yahya/Desktop/MLA project/Florida Dataset .csv", header = T)
  ##Running packages
#Read  library(tidyverse)
library(tidyverse)
  #Read library(stargazer)
library(stargazer)

      ##converting MAR into dummy variables (Marital status)
mydata$married_dummy<- ifelse(mydata$MAR == 1,1,0)
mydata$never_married_dummy<- ifelse(mydata$MAR == 5,1,0)


    ##Converting RAC1P into dummy variable (Race)
mydata$White_dummy<- ifelse (mydata$RAC1P == 1,1,0)
mydata$Black_dummy<- ifelse (mydata$RAC1P == 2,1,0)
mydata$Asian_dummy<- ifelse (mydata$RAC1P == 6,1,0)


    ##Converting SCHL into dummy variables (Educational Attainment)
mydata$HS_dummy<- ifelse (mydata$SCHL == 16,1,0)
mydata$GED_dummy<- ifelse(mydata$SCHL == 17,1,0)
mydata$SOME_college_dummy<- ifelse(mydata$SCHL == 18,1,0)
mydata$ONE_or_more_years_of_college_credit_dummy<- ifelse(mydata$SCHL == 19,1,0)
mydata$AA_dummy<- ifelse(mydata$SCHL== 20,1,0)
mydata$BA_dummy<- ifelse(mydata$SCHL == 21,1,0)
mydata$MS_dummy<- ifelse(mydata$SCHL== 22,1,0)
mydata$Pro_beyond_BA_dummy<- ifelse(mydata$SCHL== 23,1,0)
mydata$phd_dummy<- ifelse(mydata$SCHL == 24,1,0)
  
    ##Converting SEX into dummy variables (Gender)
mydata$male_dummy<- ifelse(mydata$SEX == 1,1,0)
mydata$female_dummy<- ifelse(mydata$SEX == 2,1,0)

   ##Converting WKHP into dummy variables (Hours worked per week)
mydata$full_time<- ifelse (mydata$WKHP > 20,1,0)
mydata$part_time<- ifelse (mydata$WKHP <= 20,1,0)

     ##converting ESR into dummy variables (Employment type)
mydata$civilian_employed_at_work<- ifelse(mydata$ESR == 1,1,0)
mydata$civilian_employed_not_at_work<- ifelse(mydata$ESR == 2,1,0)
mydata$unemployed<- ifelse(mydata$ESR == 3,1,0)
mydata$armed_forces_at_work<- ifelse(mydata$ESR == 4,1,0)
mydata$armed_forces_not_at_work<- ifelse(mydata$ESR == 5,1,0)
mydata$not_in_labor_force<- ifelse(mydata$ESR == 6,1,0)

    ##Narrowing down the dataset (Subsample)
subsample <- na.omit(mydata[,c("White_dummy", "male_dummy" , "Black_dummy", "Asian_dummy", "civilian_employed_at_work",
                               "married_dummy","never_married_dummy", "female_dummy","full_time","PERNP", 
                               "HS_dummy","GED_dummy","SOME_college_dummy","ONE_or_more_years_of_college_credit_dummy",
                               "AA_dummy","BA_dummy","MS_dummy","Pro_beyond_BA_dummy","phd_dummy")])

    ##Subset 
employedwithpay<- subset(subsample, PERNP > 0 & civilian_employed_at_work == 1 & full_time == 1)

  ##Female subset
employedwithpay_female<- subset(subsample, PERNP > 0 & female_dummy== 1  & civilian_employed_at_work == 1 & full_time == 1)

    ##Male subset
employedwithpay_male<- subset(subsample, PERNP > 0 &  female_dummy== 0 & civilian_employed_at_work == 1 & full_time == 1)

    ##Descriptive Statistics summary:
summary(employedwithpay_female)
summary(employedwithpay_male)

    ##Female descriptive Statistics with stargazer
stargazer(employedwithpay_female, type = "text", title="Descriptive statistics", digits=2)
  
    ##Male descriptive Statistics with stargazer
stargazer(employedwithpay_male, type = "text", title="Descriptive statistics", digits=2)

    ##Descriptive Statistics with stargazer for all the sample
stargazer(employedwithpay, type = "text", title="Descriptive statistics", digits=2)


    ##more descriptive statisitcs for employeewithpay Female
round( mean(employedwithpay_female[,"PERNP"]), 0)
median(employedwithpay_female[,"PERNP"])
round( sqrt(var(employedwithpay_female[,"PERNP"])),0)
min(employedwithpay_female[,"PERNP"])
max(employedwithpay_female[,"PERNP"])
sd(employedwithpay_female [,"PERNP"])

    ##more descriptive statisitcs for employeewithpay Male

round( mean(employedwithpay_male[,"PERNP"]), 0)
median(employedwithpay_male[,"PERNP"])
round( sqrt(var(employedwithpay_male[,"PERNP"])),0)
min(employedwithpay_male[,"PERNP"])
max(employedwithpay_male[,"PERNP"])
sd(employedwithpay_male [,"PERNP"])

    ## Estimate a MR Model
Earnings.Equation= lm(PERNP ~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + White_dummy + Black_dummy + Asian_dummy, data=employedwithpay)
summary(Earnings.Equation)


     ##Estimate a MR Model for female 
Earnings.Equation.female= lm(PERNP ~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + White_dummy + Black_dummy + Asian_dummy, data=employedwithpay_female)
summary(Earnings.Equation.female)

    ## Estimate a MR Model for male
Earnings.Equation.male= lm(PERNP ~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + White_dummy + Black_dummy + Asian_dummy, data=employedwithpay_male)
summary(Earnings.Equation.male)

    ##Estimate a MR model with interation terms

Earnings.Equation.interaction= lm (PERNP~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + female_dummy + I(female_dummy*married_dummy) + I(female_dummy*HS_dummy) + I(female_dummy*GED_dummy) + I(female_dummy*SOME_college_dummy) + I(female_dummy*ONE_or_more_years_of_college_credit_dummy)+ I(female_dummy*AA_dummy)+ I (female_dummy*BA_dummy) + I(female_dummy*MS_dummy) + I(female_dummy*Pro_beyond_BA_dummy)+ I(female_dummy*phd_dummy), data=employedwithpay)
summary(Earnings.Equation.interaction)   

    ##Log model with interaction terms
logEarnings.Equation.interaction= lm(log(PERNP, base = exp(1)) ~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + female_dummy + I(female_dummy*married_dummy) + I(female_dummy*HS_dummy) + I(female_dummy*GED_dummy) + I(female_dummy*SOME_college_dummy) + I(female_dummy*ONE_or_more_years_of_college_credit_dummy)+ I(female_dummy*AA_dummy)+ I (female_dummy*BA_dummy) + I(female_dummy*MS_dummy) + I(female_dummy*Pro_beyond_BA_dummy)+ I(female_dummy*phd_dummy), data=employedwithpay)
summary(logEarnings.Equation.interaction)

  ##logarithmic model for female
logEarning.Equation.female= lm(log(PERNP, base = exp(1)) ~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + White_dummy + Black_dummy + Asian_dummy, data=employedwithpay_female)
summary(logEarning.Equation.female)

  ##Logarithmic model for both male and female
logEarning.Equation= lm(log(PERNP, base = exp(1)) ~ married_dummy + female_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + White_dummy + Black_dummy + Asian_dummy, data=employedwithpay)
summary(logEarning.Equation)


   ##Install the Sandwich Package (only need to do once)
install.packages("sandwich")
library(sandwich)

    ###Conduct BPG Test ALL MALE AND FEMALE
library(lmtest)
bptest(Earnings.Equation.female)
bptestequation.female = lm(residuals(Earnings.Equation.female)*residuals(Earnings.Equation.female) ~ married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + White_dummy + Black_dummy + Asian_dummy, data=employedwithpay_female)
summary(bptestequation.female) #note: BP = n*RSquared of model with squared residuals as dependent variable 


    ##Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(Earnings.Equation, type = "HC") #the diagonal elements are the variances of the parameter estimates

    ##Generate the Robust standard errors and print them on screen 
sandwich_se <- diag(vcovHC(Earnings.Equation, type = "HC"))^0.5
sandwich_se


    ##BP test log interaction
bptest(logEarnings.Equation.interaction)
logbptestequation.interaction = lm(residuals(logEarnings.Equation.interaction)*residuals(logEarnings.Equation.interaction) ~  married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + female_dummy + I(female_dummy*married_dummy) + I(female_dummy*HS_dummy) + I(female_dummy*GED_dummy) + I(female_dummy*SOME_college_dummy) + I(female_dummy*ONE_or_more_years_of_college_credit_dummy)+ I(female_dummy*AA_dummy)+ I (female_dummy*BA_dummy) + I(female_dummy*MS_dummy) + I(female_dummy*Pro_beyond_BA_dummy)+ I(female_dummy*phd_dummy), data=employedwithpay)
summary(logbptestequation.interaction)
    
##Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(logEarnings.Equation.interaction, type = "HC") #the diagonal elements are the variances of the parameter estimates

    ##Generate the Robust standard errors and print them on screen 
log_sandwich_se_interaction <- diag(vcovHC(logEarnings.Equation.interaction, type = "HC"))^0.5
log_sandwich_se_interaction

##BP test interaction
bptest(Earnings.Equation.interaction)
bptestequation.interaction = lm(residuals(Earnings.Equation.interaction)*residuals(Earnings.Equation.interaction) ~  married_dummy + HS_dummy + GED_dummy + SOME_college_dummy + ONE_or_more_years_of_college_credit_dummy + AA_dummy + BA_dummy + MS_dummy + Pro_beyond_BA_dummy + phd_dummy + female_dummy + I(female_dummy*married_dummy) + I(female_dummy*HS_dummy) + I(female_dummy*GED_dummy) + I(female_dummy*SOME_college_dummy) + I(female_dummy*ONE_or_more_years_of_college_credit_dummy)+ I(female_dummy*AA_dummy)+ I (female_dummy*BA_dummy) + I(female_dummy*MS_dummy) + I(female_dummy*Pro_beyond_BA_dummy)+ I(female_dummy*phd_dummy), data=employedwithpay)
summary(bptestequation.interaction)

##Generate the Variance Covariance Matrix of the Parameter Estimates
vcovHC(Earnings.Equation.interaction, type = "HC") #the diagonal elements are the variances of the parameter estimates

##Generate the Robust standard errors and print them on screen 
sandwich_se_interaction <- diag(vcovHC(Earnings.Equation.interaction, type = "HC"))^0.5
sandwich_se_interaction
