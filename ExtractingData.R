# This scripts reads the data from the  SQLite database file

library(RSQLite)

setwd("~/Adriana/personal/HV/Clases/IntroDataScience/FinalProject/Dataset/college-scorecard")

# You can read in the SQLite datbase like this
db <- dbConnect(dbDriver("SQLite"), "database.sqlite")

# This query extracts the Average cost, the mean, top 10%, and bottom 10% earnings 10 years after
MyData <- dbGetQuery(db, "
SELECT INSTNM College_Name,
       Region,
       STABBR State,
       Control CollegeType,
       COSTT4_A Average_Cost,
       md_earn_wne_p10 median_earning,
       mn_earn_wne_p10 mean_earning,
       pct90_earn_wne_p10 Top10PercentEarnings,
       pct10_earn_wne_p10 Bottom10PercentEarnings,
       PCTFLOAN FSFLoans,
       C150_4 CompletionRate,
       CDR2 Default2,
       GRAD_DEBT_MDN MedianDebtGrad,
       ADM_RATE AdmRate,
       WDRAW_DEBT_MDN MedianDebtNGrad,
       GRAD_DEBT_MDN/md_earn_wne_p6 DebtToEarn,
       PPTUG_EF PartTime,
       SATMTMID Math,
       SATVRMID Verbal,
       SATWRMID Writing,
       UGDS UndergradEnrollment,
       PREDDEG,
       Year
FROM Scorecard
WHERE INSTNM='Stanford University'
AND PREDDEG='Predominantly bachelor''s-degree granting'
ORDER BY INSTNM ASC")



# VARIABLE DESCRIPTION
#____________________________________________________________________________________
# INSTNM Institution name
# CONTROL Control of institution
# UGDS Enrollment of undergraduate degree-seeking students
# SATMTMID Math Midpoint of SAT scores at the institution (math)
# SATVRMID Midpoint of SAT scores at the institution (verbal)
# SATWRMID Midpoint of SAT scores at the institution (written)
# ADM_RATE Admission rate
# C150_4 Completion rate for first-time, full-time students at four-year institutions 
# CCSIZSET Whether the school is a 2-year or 4-year institution
# PCTFLOAN Fraction of students on federal loan
# CDR2 Two-year cohort default rate
# CDR3 Three-year cohort default rate,
# GRAD_DEBT_MDN The median debt for students who have completed 
# WDRAW_DEBT_MDN The median debt for students who have not completed
# COSTT4_A Average cost of attendance (academic year institutions)
# mn_earn_wne_p10 Earnings10 Mean earnings of students working and not enrolled 10 years after entry

print(MyData)


