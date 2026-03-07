Spring26GroupProject1_[Group 1]
Group Members: Angel Beltran, Andrew Carias, Anna Le, Johanna Vergara

Project Description: 

# Import data into R 
HINTSData_2020_clean <- read.csv("HINTSData_2020_clean.csv")
# Preview of first 6 rows 
head(HINTSData_2020_clean)
HHID    PersonID QualityCare
1 11000103 11000103-01           1
2 11000420 11000420-01           4
3 11000804 11000804-01           2
4 11001045 11001045-01           2
5 11001070 11001070-03           3
6 11001281 11001281-02           3
HealthInsurance Age BirthGender
1               1  59           2
2               1  61           1
3               1  31           2
4               1  48           2
5               1  70           1
6               1  55           2
FullTimeOcc_Cat MaritalStatus
1               5             3
2               3             6
3               1             1
4               1             1
5               5             3
6               1             1
SexualOrientation AgeGrpB EducA RaceEthn5
1                 3       3     4         5
2                 1       3     3         5
3                 1       1     4         5
4                 1       2     4         5
5                 1       4     3         5
6                 1       3     4         5
HHInc  BMI smokeStat
1     2 20.7         3
2     1 35.4         2
3     5 23.0         3
4     5 35.1         3
5     2 29.2         2
6     5 18.4         3
WeeklyMinutesModerateExercise AvgDrinksPerWeek
1                          1200               14
2                             0                0
3                           180                2
4                            60                1
5                             0                0
6                           540                2
Select only the 8 variables 
HINTS_subset <- HINTSData_2020_clean[, c("Age", 
                                          "AvgDrinksPerWeek", 
                                          "WeeklyMinutesModerateExercise", 
                                          "BMI", "QualityCare", 
                                          "BirthGender", 
                                          "smokeStat", 
                                          "RaceEthn5")]
#Check subset data and summary 
str(HINTS_subset)
'data.frame':	2402 obs. of  8 variables:
  $ Age                          : int  59 61 31 48 70 55 49 64 47 20 ...
$ AvgDrinksPerWeek             : int  14 0 2 1 0 2 2 0 6 0 ...
$ WeeklyMinutesModerateExercise: int  1200 0 180 60 0 540 90 0 120 0 ...
$ BMI                          : num  20.7 35.4 23 35.1 29.2 18.4 25.8 39.8 36.9 34.9 ...
$ QualityCare                  : int  1 4 2 2 3 3 1 4 1 2 ...
$ BirthGender                  : int  2 1 2 2 1 2 1 1 1 1 ...
$ smokeStat                    : int  3 2 3 3 2 3 3 1 2 3 ...
$ RaceEthn5                    : int  5 5 5 5 5 5 5 5 5 5 ...
summary(HINTS_subset) 
Age         AvgDrinksPerWeek
Min.   : 18.00   Min.   : 0.000  
1st Qu.: 43.00   1st Qu.: 0.000  
Median : 58.00   Median : 0.000  
Mean   : 55.59   Mean   : 3.358  
3rd Qu.: 68.00   3rd Qu.: 4.000  
Max.   :100.00   Max.   :70.000  
WeeklyMinutesModerateExercise      BMI       
Min.   :   0.0                Min.   :10.90  
1st Qu.:   0.0                1st Qu.:24.00  
Median :  90.0                Median :27.50  
Mean   : 161.8                Mean   :28.61  
3rd Qu.: 210.0                3rd Qu.:31.90  
Max.   :4620.0                Max.   :73.80  
QualityCare     BirthGender      smokeStat    
Min.   :1.000   Min.   :1.000   Min.   :1.000  
1st Qu.:1.000   1st Qu.:1.000   1st Qu.:2.000  
Median :2.000   Median :2.000   Median :3.000  
Mean   :2.003   Mean   :1.585   Mean   :2.535  
3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:3.000  
Max.   :5.000   Max.   :2.000   Max.   :3.000  
RaceEthn5    
Min.   :1.000  
1st Qu.:1.000  
Median :1.000  
Mean   :1.695  
3rd Qu.:2.000  
Max.   :5.000  

# Continuous numeric variables only, making a new vector
continuous_variables <- c("Age", "AvgDrinksPerWeek", "WeeklyMinutesModerateExercise", "BMI")
sd_results <- sapply(HINTS_subset[continuous_variables], sd, na.rm = TRUE)  
# Print SD results
print(sd_results)
Age 
16.579024 
AvgDrinksPerWeek 
6.588142 
WeeklyMinutesModerateExercise 
271.189253 
BMI 
6.538369 

  # Install Dyplr first
install.packages("dplyr")
library(dplyr) # Run Dyplr

grouped_summary <- HINTS_subset |>
group_by(smokeStat) |>
summarise(
  across(
      all_of(continuous_variables),
    list(
    Min = ~min(.x, na.rm = TRUE),
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
    Median = ~median(.x, na.rm = TRUE),
    Mean = ~mean(.x, na.rm = TRUE), 
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE),
    Var = ~var(.x, na.rm = TRUE)
  ),
  .names = "{.col}_{.fn}"
    ))
  # view summary
  print(grouped_summary)
