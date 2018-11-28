# This scripts reads the data from the  SQLite database file

library(RSQLite)
library(ggplot2)
library(caret)
library (stats)

# set this to the directory where you downloaded the dataset
setwd("~/Adriana/personal/HV/Clases/IntroDataScience/FinalProject/Dataset/college-scorecard")

# You can read in the SQLite datbase like this
db <- dbConnect(dbDriver("SQLite"), "database.sqlite")


# Self-join because the CCBASIC data is only attached to 2013 data and 2013 data has no 10 year out earnings data 
#Filter out medical schools and the like s13.CCBASIC NOT LIKE '%Special%'


earnings2011 <- dbGetQuery(db, "
             SELECT s11.INSTNM College,
                     s11.Region Region,
                     s11.STABBR State,
                     s11.Control CollegeType,
                     s11.COSTT4_A Average_Cost,
                     s11.md_earn_wne_p10 Median_earning,
                     s11.mn_earn_wne_p10 Mean_earning,
                     s11.SD_EARN_WNE_P10 Standard_Deviation,
                     s11.pct10_earn_wne_p10 t10PercentEarnings,
                     s11.pct25_earn_wne_p10 t25PercentEarnings,
                     s11.pct75_earn_wne_p10 t75PercentEarnings,
                     s11.pct90_earn_wne_p10 t90PercentEarnings
              FROM Scorecard s11
              INNER JOIN Scorecard s13 ON s11.UNITID=s13.UNITID
                      WHERE s11.Year=2011
                     AND s13.Year=2013
                     AND s11.md_earn_wne_p10 IS NOT NULL
                     AND s11.md_earn_wne_p10 != 'PrivacySuppressed'
                     AND s11.pct75_earn_wne_p10 IS NOT NULL
                     AND s11.pct75_earn_wne_p10 != 'PrivacySuppressed'
                     AND s11.COSTT4_A IS NOT NULL
                     AND s11.PREDDEG = 'Predominantly bachelor''s-degree granting'
                     AND s13.CCBASIC NOT LIKE '%Special%'
                     ORDER BY s11.md_earn_wne_p10 DESC")


earnings2011 <- cbind(Rank=1:nrow(earnings2011), earnings2011)


# any correlation between cost and earnings?

earnings_cost = earnings2011 [ , c("Average_Cost", "Median_earning")]
ggplot(earnings_cost,aes(Average_Cost,Median_earning))+
  geom_point()+geom_smooth(se=FALSE,method="lm")


#I created a new variable to show standard deviation across percentiles: It shows how much salaries can change within a certain college.
#The dataset has already a variable for that (SD_EARN_WNE_P10) so I just wanted to compare.

earnings2011$Percentiles_sd <-  apply(earnings2011[10:13] , 1 , sd)


#clustering with k-means to categorizing the Colleges into distinct groups.
# I'm not sure if these groups tell me something? Maybe it categorized the schools based on control: Public, Private nonprofit, private forprofit?


#Clustering with 2 variables: Median Salary and Percentiles Standard Deviation

#Best number of clusters
#Based on the graph it seems the best number is 5?

earnings=earnings2011[,c("Median_earning","Percentiles_sd")]

wss <- (nrow(earnings)-1)*sum(apply(earnings,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(earnings,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups
     sum of squares")

#using 5 clusters


myCluster=kmeans(earnings2011[,c("Median_earning","Percentiles_sd")],5)

earnings2011$groups=as.factor(myCluster$cluster)

ggplot(earnings2011,aes(Median_earning,Percentiles_sd,
                        color = groups))+geom_point(size=4)

#Group 1

g1 <- earnings2011[earnings2011$groups ==1,c("Rank","College","Region","State","CollegeType","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

#Group 2

g2 <- earnings2011[earnings2011$groups ==2,c("Rank","College","Region","State","CollegeType","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

#Group 3

g3 <- earnings2011[earnings2011$groups ==3,c("Rank","College","Region","State","CollegeType","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

#Group 4

g4 <- earnings2011[earnings2011$groups ==4,c("Rank","College","Region","State","CollegeType","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

#Group 5

g5 <- earnings2011[earnings2011$groups ==5,c("Rank","College","Region","State","CollegeType","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]



#Clustering with three variables

earnings3Var=earnings2011[,c("Median_earning","Percentiles_sd","Average_Cost")]

#Best number of clusters
#Based on the graph it seems the best number is 4?

wss <- (nrow(earnings3Var)-1)*sum(apply(earnings3Var,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(earnings3Var,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups
     sum of squares")

#using 4 clusters

myCluster2=kmeans(earnings2011[,c("Median_earning","Percentiles_sd","Average_Cost")],3)
                              
earnings2011$groups=as.factor(myCluster2$cluster)
                              
ggplot(earnings2011,aes(Median_earning,Percentiles_sd,
                        color = groups))+geom_point(size=4)
                              
#Group 1
g1 <- earnings2011[earnings2011$groups ==1,c("Rank","College","Region","State","CollegeType","Average_Cost","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

ggplot(g1, aes(x=Median_earning, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings") + ylab("")+
  ggtitle("Earnings 10 Years after Entry")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15))
                              
#Group 2

g2 <- earnings2011[earnings2011$groups ==2,c("Rank","College","Region","State","CollegeType","Average_Cost","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

ggplot(g2, aes(x=Median_earning, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings") + ylab("")+
  ggtitle("Earnings 10 Years after Entry")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15))

#Group 3
g3 <- earnings2011[earnings2011$groups ==3,c("Rank","College","Region","State","CollegeType","Average_Cost","Median_earning","t10PercentEarnings","t25PercentEarnings","t75PercentEarnings","t90PercentEarnings","Percentiles_sd")]

ggplot(g3, aes(x=Median_earning, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings") + ylab("")+
  ggtitle("Earnings 10 Years after Entry")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15))



#Exploring the data: Some scripts to review the information in the dataset

# Author https://www.kaggle.com/benhamner/exploring-the-us-college-scorecard-data/code

#Distribution of earnings 10 years after entry for school type: private non-profit, private for-profit, and public. 
#The median earnings for public and private non-profit schools look similar. 
#At the very low end, there are more private non-profit schools than public schools. 
#Curiously, the median earnings distribution for private for-profit schools is bimodal.


ggplot(earnings2011, aes(x=Median_earning, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("Median Earnings") + ylab("")+
  ggtitle("Earnings 10 Years after Entry")+
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=15))


#what colleges have students that earn the most?
#The graph below ranks colleges by their top quartile of earnings ten years after the students matriculated, and plots the distribution of student earnings. 
#Some of the usual suspects appear on top: Harvard, MIT, Stanford, Geogetown, Princeton, and my alma mater Duke.

ggplot(earnings2011[1:20,], aes(x=College, ymin=Top10PercentEarnings, lower=Top25PercentEarnings, middle=Median_earning, upper=Top75PercentEarnings, ymax=Top90PercentEarnings)) +
  geom_boxplot(stat="identity", fill="#53cfff") + 
  geom_text(aes(x=College, y=Top75PercentEarnings-2000, ymax=Top75PercentEarnings, hjust=0.95, label=paste0("$", Top75PercentEarnings)), size=3) + 
  theme_light(base_size=10) +
  theme(axis.text.y = element_text(hjust=0, color="black")) +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("Colleges with Top Earnings ($)")
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=12))


#The cost here reflects the average annual total cost of attendance, including tuition and fees, books and supplies, and living expenses for all full-time, 
#first-time, degree/certificate-seeking undergraduates who receive Title IV aid.

cost <- dbGetQuery(db, "
                   SELECT INSTNM College,
                   COSTT4_A Cost,
                   CONTROL CollegeType
                   FROM Scorecard
                   WHERE Year=2013
                   AND PREDDEG='Predominantly bachelor''s-degree granting'
                   AND CCBASIC NOT LIKE '%Special Focus%'
                   AND COSTT4_A IS NOT NULL
                   ORDER BY COSTT4_A DESC")
cost <- cbind(Rank=1:nrow(cost), cost)

#Cost distribution by college type

ggplot(cost, aes(x=Cost, color=CollegeType, fill=CollegeType, group=CollegeType)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  xlab("") + ylab("Cost of Attendance")


#the total cost of attendance for the most expensive colleges 

ggplot(cost[1:20,], aes(x=College, y=Cost)) +
  geom_bar(stat="identity", fill="#53cfff") +
  geom_text(aes(x=College, y=Cost-500, ymax=Cost, hjust=0.95, label=paste0("$", Cost)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black"), axis.text.x=element_blank()) +
  xlab("") + ylab("") +
  coord_flip() +
  ggtitle("Most Expensive Colleges")




