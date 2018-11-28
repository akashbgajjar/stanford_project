
#How do the different fields of study rank in popularity?

# Author: https://www.kaggle.com/caropie67/field-popularity/code

#The goal is to transform the dataset from having one row per institution to having one row 
#per institution/field combination, so aggregation by field can be done over one column instead of 38.

# Read the latest (2013) data file

setwd("~/Adriana/personal/HV/Clases/IntroDataScience/FinalProject/Dataset/college-scorecard")

data2013<-read.csv(file = "MERGED2013_PP.csv" , na.strings = "NULL")

## the name of the first column is not reading in correctly
## compensate by renaming the col (not optimal but I need a work around).
names(data2013)[1]<-"UNITID"

data2013_nrow<-nrow(data2013)
data2013_ncol<-ncol(data2013)

#Create a smaller "schools" dataset with just the columns of interest and rename the PCIP columns to reflect the field name:

library(dplyr)
schools<-select(data2013, 
                UNITID, 
                INSTNM, 
                UGDS, 
                Agriculture=PCIP01,
                Conservation=PCIP03,
                Architecture=PCIP04,
                GroupStudies=PCIP05,
                CommAndJournalism=PCIP09,
                CommTechnologies=PCIP10,
                Computers=PCIP11,
                Culinary=PCIP12,
                Education=PCIP13,
                Engineering=PCIP14,
                EngTechnologies=PCIP15,
                Languages=PCIP16,
                Family=PCIP19,
                Legal=PCIP22,
                English=PCIP23,
                LiberalArts=PCIP24,
                Library=PCIP25,
                Biology=PCIP26,
                MathAndStat=PCIP27,
                Military=PCIP29,
                MultiDiscipline=PCIP30,
                ParksAndRec=PCIP31,
                Philosophy=PCIP38,
                Theology=PCIP39,
                PhysicalScience=PCIP40,
                ScienceTech=PCIP41,
                Psychology=PCIP42,
                ProtectServices=PCIP43,
                PublicAdmin=PCIP44,
                SocialServices=PCIP45,
                Construction=PCIP46,
                Mechanical=PCIP47,
                PrecisionProd=PCIP48,
                Transportation=PCIP49,
                PerformingArts=PCIP50,
                Health=PCIP51,
                Business=PCIP52,
                History=PCIP54)

schools_nrow<-nrow(schools)
schools_ncol<-ncol(schools)

#Transform the "schools" dataset, which is short and wide, into a "programs" dataset that is long and narrow with one row for every institution/field combination and just 5 columns (UNITID, INSTNM, UGDS, a column for the program name, and a column for the percent degrees awarded):

## create the start of a "programs" dataset 
## it should only contain the percent degrees awarded for 
## the first program (in col 4)

programs<-select(schools, UNITID, INSTNM, UGDS, prcntDgrs=4)

## add another column called "prog" to programs
## fill this column with the program name (i.e. col name for col 4)
programs$prog<-names(schools)[4]

## repeat for each remaining program, binding the rows to the programs dataset
for (i in 5:41) {
     temp<-select(schools, UNITID, INSTNM, UGDS, prcntDgrs=i)  
     temp$prog<-names(schools)[i]
     programs<-rbind(programs, temp)
}

programs_nrow<-nrow(programs)
programs_ncol<-ncol(programs)

#Remove any missing values for enrollment or percent degrees awarded from the "programs" data set:

programs<-filter(programs, is.na(UGDS)==FALSE & is.na(prcntDgrs)==FALSE)

programs_nrow<-nrow(programs)
programs_ncol<-ncol(programs)

#The dataset is ready for analysis.

## Results

#What is the total undergraduate enrollment for all schools?

grandTotal<-sum(schools$UGDS, na.rm=TRUE)

#What is the total enrollment for each field ranked highest to lowest? 

## for each program, sum the school's enrollment * percent degrees awarded
progTotals<-aggregate((programs$UGDS*programs$prcntDgrs),
                       by=list(Program=programs$prog), FUN=sum)

## progTotals has two vars: Program and x (total)
## rename x for clarity
names(progTotals)[2]<-"Enrollment"

## add a third column indicating percent of total enrollment
progTotals$Percent<-round(progTotals$Enrollment/grandTotal*100, 2)

## round ProgramEnrollment to make it an even number for display
progTotals$Enrollment<-round(progTotals$Enrollment, 0)

## rank by highest enrollment first
progTotals<-arrange(progTotals, desc(Enrollment), Program)

#Here is another view of the results in bar chart form: 

library(ggplot2)
## Make sure Program is a factor with levels assigned based on the sorted Program values.
progTotals$Program <- factor(progTotals$Program, 
                             levels = progTotals$Program)

## This is simply to prevent the values from displaying as scientific notation in the labels.
options(scipen=10)

g <- ggplot(progTotals, aes(x=Program, y=Enrollment)) 
g <- g + geom_bar(stat='identity')   
g <- g + labs(x="", y="")
g <- g + ggtitle("Total Enrollment by Field of Study in 2013")
## Add percent labels to end of bars.
## In stmt below, y is the label position and adding 150000
## pushed it beyond end of bar so it can be seen.
g <- g + geom_text(aes(label = paste(as.character(Percent), "%"), 
                       y = Enrollment + 150000), size = 3) 
g <- g + coord_flip() ## makes bars horizontal
g

## Reset scipen option
options(scipen=0)