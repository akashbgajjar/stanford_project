library(RSQLite)
library(ggplot2)
library(caret)
library (stats)

#PROBLEM DESCRIPTION
#Predict the graduation rate based on multiple variables. The target variable is C150_4
#C150_4: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion)	
#This is a regression problem.

#We use student data from 2011 to train the model and student data from 2013 to test the model.  

#We will analyze how the following variables can affect the graduation rate:

#1. ADM_RATE_ALL: rate of admission rate for all campuses   
#2. COSTT4_A: Average cost of attendance per year  
#3. PPTUG_EF: fraction of part-time students  
#4. INC_PCT_LO: Percentage of students from low-income families, defined as annual family income < $30,000   
#5. UG25ABV: Percentage of students above 25 years of age   
#6. PAR_ED_PCT_1STGEN: Percentage of first-generation college students in the family  
#7. PCTFLOAN: Percent of students receiving a federal loan
#8 SAT_AVG_ALL:Average SAT equivalent score of students admitted for all campuses


# set this to the directory where you downloaded the dataset
# setwd("~/Adriana/personal/HV/Clases/IntroDataScience/FinalProject/Dataset/college-scorecard")

# You can read in the SQLite datbase like this
db <- dbConnect(dbDriver("SQLite"), "college-scorecard/database.sqlite")

MyData <- dbGetQuery(db, "SELECT year,INSTNM College,ADM_RATE_ALL AdmissionRate,SAT_AVG_ALL,COSTT4_A Average_Cost,UGDS_BLACK,UGDS_WHITE,PPTUG_EF,INC_PCT_LO,UG25abv,PAR_ED_PCT_1STGEN,PCTFLOAN,C150_4 GradRate FROM Scorecard WHERE Year=2011 AND ADM_RATE_ALL != 'PrivacySuppressed' AND ADM_RATE_ALL IS NOT NULL AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND UGDS_WHITE IS NOT NULL AND UGDS_BLACK != 'PrivacySuppressed' AND UGDS_BLACK IS NOT NULL AND PPTUG_EF != 'PrivacySuppressed' AND PPTUG_EF IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND UG25abv != 'PrivacySuppressed' AND UG25abv IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND PCTFLOAN != 'PrivacySuppressed' AND PCTFLOAN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND C150_4 IS NOT NULL AND PREDDEG = 'Predominantly bachelor''s-degree granting' ORDER BY INSTNM ASC")


# any correlation between graduation rate and admission rate?
ggplot(MyData,aes(x=AdmissionRate,y=GradRate)) +
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$AdmissionRate) 
# Low negative correlation= -0.22

# any correlation between graduation rate and cost?
ggplot(MyData,aes(x=Average_Cost,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$Average_Cost) 
# High postive correlation= 0.60

# any correlation between graduation rate and part time students?
ggplot(MyData,aes(x=PPTUG_EF,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$PPTUG_EF) 
# Negative correlation= -0.40


# any correlation between graduation rate and low income families?
ggplot(MyData,aes(x=INC_PCT_LO,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$INC_PCT_LO) 
# High negative correlation= - 0.68

# any correlation between graduation rate and students above 25 years of age?
ggplot(MyData,aes(x=UG25abv,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$UG25abv) 
# correlation= 0.68

# any correlation between graduation rate and first-generation college students?
ggplot(MyData,aes(x=PAR_ED_PCT_1STGEN,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$PAR_ED_PCT_1STGEN) 
# High negative correlation= - 0.70

# any correlation between graduation rate and students receiving a federal loan?
ggplot(MyData,aes(x=PCTFLOAN,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$PCTFLOAN) 
# Low negative correlation= -0.20


# any correlation between graduation rate and SAT?
ggplot(MyData,aes(x=SAT_AVG_ALL,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$SAT_AVG_ALL) 
# High postive correlation= 0.77

# Use a plot to show the distribution of the SAT
ggplot(MyData,aes(SAT_AVG_ALL))+geom_histogram(binwidth=150,color="red",fill="blue")+ylim(0,500)


#  Variable  | Correlation with Graduation Rate(C150_4)
#- --------  | -----------------------------------
#ADM_RATE_ALL| -0.22
#COSTT4_A    |  0.60
#PPTUG_EF    | -0.40
#INC_PCT_LO  | -0.68
#UG25abv     | -0.48
#PAR_ED_PCT_1STGEN | -0.70
#PCTFLOAN    | -0.20  
#SAT_AVG_ALL |  0.77


#From these features we select the following to train the model

#1. SAT_AVG_ALL
#2. COSTT4_A: total cost of attendance per year
#3. INC_PCT_LO: students from low-income families
#4. PAR_ED_PCT_1STGEN: first-generation college goers in the family  


trainData <- dbGetQuery(db, "SELECT year,INSTNM College,COSTT4_A Average_Cost,INC_PCT_LO LowIncome,PAR_ED_PCT_1STGEN FirstGen,SAT_AVG_ALL AvgSat,C150_4 GradRate FROM Scorecard WHERE Year = 2011 AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND C150_4 IS NOT NULL AND PREDDEG = 'Predominantly bachelor''s-degree granting' ORDER BY INSTNM ASC")


testData <- dbGetQuery(db, "SELECT year,INSTNM College,COSTT4_A Average_Cost,INC_PCT_LO LowIncome,PAR_ED_PCT_1STGEN FirstGen,SAT_AVG_ALL AvgSat,C150_4 GradRate FROM Scorecard WHERE Year = 2013 AND INSTNM IN (SELECT INSTNM FROM Scorecard WHERE Year = 2011 AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND C150_4 IS NOT NULL AND PREDDEG = 'Predominantly bachelor''s-degree granting') AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND C150_4 IS NOT NULL AND PREDDEG = 'Predominantly bachelor''s-degree granting' ORDER BY INSTNM ASC")


#Linear model
modelLn = train(GradRate ~., data = trainData, method = "lm")

#predict the target variable GradRate
predictGradRateLn = postResample(predictGradRateLn,testData$GradRate)

predictGradRateLn = as.data.frame(predictGradRateLn)

#Random Forest

modelRF=train(GradRate ~.,data=trainData, method="rf")
predictGradRateRF=predict(modelRF,testData)
#calculate the accuracy of the model
sum(predictGradRateRF==testData$GradRate)/nrow(testData)

#Other variables
#ADM_RATE: Admission rate
#UGDS: Enrollment of undergraduate certificate/degree-seeking students	student	size	
#UGDS_WHITE: Total share of  students who are white		
#UGDS_BLACK: Total share of students who are black
#UGDS_HISP: Total share of who are Hispanic	
#UGDS_ASIAN: Total share of students who are Asian	
#UGDS_AIAN: Total share of students who are American Indian/Alaska Native			
#UGDS_NHPI: Total share of students who are Native Hawaiian/Pacific Islander	
#SAT_AVG: Average SAT equivalent score of students admitted	admissions	sat_scores.average.overall
#SAT_AVG_ALL: Average SAT equivalent score of students admitted for all campuses rolled up to the 6-digit OPE ID	
#SATVR75: 75% percentile score on SAT reading 
#SATWR75: 75% percentile score on SAT writing   
#SATMT75: 75% percentile score on SAT math   
#TUITIONFEE_IN: In-state tuition and fees		
#TUITIONFEE_OUT: Out-of-state tuition and fees	
#C150_4_WHITE: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for white students		
#C150_4_BLACK: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for black students		
#C150_4_HISP: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for Hispanic students	
#C150_4_ASIAN:Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for Asian students	
#C150_4_AIAN: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for American Indian/Alaska Native students		
#C150_4_NHPI: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for Native Hawaiian/Pacific Islander students			
#CONTROL 

