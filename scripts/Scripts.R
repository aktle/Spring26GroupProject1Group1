#B-1 
## Data retrieval 

# read data 
HINTSData_2020_clean <- read.csv("data/HINTSData_2020_clean.csv")

head(HINTSData_2020_clean)
HINTS_subset <- HINTSData_2020_clean[, c("Age", 
                                 "AvgDrinksPerWeek", 
                                 "WeeklyMinutesModerateExercise", 
                                 "BMI", "QualityCare", 
                                 "BirthGender", 
                                 "smokeStat", 
                                 "RaceEthn5")]
#Check subset data and summary 
str(HINTS_subset)
summary(HINTS_subset) 

# Continuous numeric variables only, making a new vector
continuous_variables <- c("Age", "AvgDrinksPerWeek", "WeeklyMinutesModerateExercise", "BMI")
sd_results <- sapply(HINTS_subset[continuous_variables], sd, na.rm = TRUE)  

# Print SD results
print(sd_results)

#B-2
# Install Dyplr first
install.packages("dplyr")
library(dplyr) # Run Dyplr

grouped_summary <- HINTS_subset |>
  group_by(smokeStat) |>
  summarise(
    across(
      all_of(continuous_variables),
    list(
    Min = ~min(.x, na.rm = TRUE), # .x is the variable being used, in this case within the continuous_variables ("Age", "AvgDrinksPerWeek", "WeeklyMinutesModerateExercise", "BMI") for example. R runs the command once for each of these
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE), # Ignore missing values (NA) is what na.rn means. Its used to ignore when a tableset might have null/no data spots
    Median = ~median(.x, na.rm = TRUE),
    Mean = ~mean(.x, na.rm = TRUE), 
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE),
    Var = ~var(.x, na.rm = TRUE)
  ),
  .names = "{.col}_{.fn}" # {.col} = original column name and {.fn} = function name (Min, Mean, SD,), It gives the Original Name to the columns to its function
    ))

  # view statistical summary. The five-number summary, mean, standard deviation, and variance of continuous variables grouped or stratified by smoking status significantly impacts the individual's health. 
  print(grouped_summary)
# skimr section
  install.packages("skimr")
  library(skimr)
    skim(HINTS_subset)
    skim(HINTS_subset[continuous_variables]) 
  # view statistical summary for continuous variables. 
    
# For question b.3. Writing Functions   
my_summary_func <- function(data) {
# Five-number summary and mean
print(summary(data[continuous_variables]))
# Standard deviation
sd_results <- sapply(data[continuous_variables], sd, na.rm = TRUE)
print(sd_results)
# Variance
var_results <- sapply(data[continuous_variables], var, na.rm = TRUE)
print(var_results) }

# Print
my_summary_func(HINTS_subset)

# Question 4. I
HINTS_subset$QualityCare <- factor (HINTS_subset$QualityCare)
barplot(table(HINTS_subset$QualityCare),
        main = "Distribution of QualityCare",
        xlab = "Quality of Care",
        ylab = "Frequency",
        col = "lightblue")
  # Interpretation: The bar chart shows the distribution of self-reported healthcare quality. Most respondents fall into the middle categories. Fewer rate 4-5. 

HINTS_subset$RaceEthn5 <- factor (HINTS_subset$RaceEthn5)
barplot(table(HINTS_subset$RaceEthn5),
            main = "Distribution of Race and Ethnicity",
            xlab = "Race and Ethnicity",
            ylab = "Frequency",
            col = "lightgreen")
# Interpretation: The bar chart shows the distribution of respondents across racial and ethnic groups. Some groups have higher frequencies than others.


# Question 4. II
  class(HINTS_subset$Age)
  "numeric"
  HINTS_subset$Age <- as.numeric(as.character(HINTS_subset$Age))
    hist(HINTS_subset$Age,
         main = "Distribution of Age",
         xlab = "Age",
         col = "darkgreen",
         border = "black")
# Interpretation: Some age groups appear more common than others, indicating variation in the age distribution
  
class(HINTS_subset$BMI)
  "numeric"
  HINTS_subset$BMI <- as.numeric(as.character(HINTS_subset$BMI))
    hist(HINTS_subset$BMI,
         main = "Distribution of BMI",
         xlab = "BMI",
         col = "darkblue",
         border = "black")
# Interpretation: Most BMI values fall within a central range, with fewer individuals at very low or very high values
    
  # Question 4. III
  install.packages("ggplot2")
  library(ggplot2)
  
# Showing barchar
  ggplot(HINTS_subset, aes (x = QualityCare)) + # ggplot() is telling R to use which data, aes -> variables mapped to axes
         geom_bar(fill = "lightblue") + # type of graph (in this case bar)
    labs(title = "Distribution of QualityCare", # labs is the titles and labels of axis
         x = "Quality of Care",
         y = "Frequency") +
    theme_bw() # overall appearance
  
  # RaceEthn5
  ggplot(HINTS_subset, aes(x = RaceEthn5)) +
    geom_bar(fill = "lightgreen") +
    labs(title = "Distribution of Race and Ethnicity",
         x = "Race/Ethnicity",
         y = "Frequency") +
    theme_bw()
  
  # Age
  ggplot(HINTS_subset, aes(x = Age)) +
    geom_histogram(binwidth = 5, fill = "darkgreen", color = "black") +
    labs(title = "Distribution of Age",
         x = "Age",
         y = "Count") +
    theme_bw()
  
  #BMI ]
  ggplot(HINTS_subset, aes(x = BMI)) +
    geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
    labs(title = "Distribution of BMI",
         x = "BMI",
         y = "Count") +
    theme_bw()
  
# Interpretation: The ggplot2 graphs show the same distributions as the base R graphs, but they look cleaner. Bar charts are used for categorical variables, and histograms are used for continuous variables.
  
  # Question 4. IV
  my_visual_func_I <- function() {
    HINTS_subset$QualityCare <- factor (HINTS_subset$QualityCare)
    barplot(table(HINTS_subset$QualityCare),
                main = "Distribution of QualityCare",
                xlab = "Quality of Care",
                ylab = "Frequency",
                col = "lightblue")
      HINTS_subset$RaceEthn5 <- factor (HINTS_subset$RaceEthn5)
      barplot(table(HINTS_subset$RaceEthn5),
                    main = "Distribution of Race and Ethnicity",
                    xlab = "Race and Ethnicity",
                    ylab = "Frequency",
                    col = "lightgreen")  
      }
# print
  my_visual_func_I () 
  
  my_visual_func_II <- function() {
    hist(HINTS_subset$Age,
               main = "Distribution of Age",
               xlab = "Age",
               col = "green",
               border = "black") 
      class(HINTS_subset$BMI)
    HINTS_subset$BMI <- as.numeric(as.character(HINTS_subset$BMI))
    hist(HINTS_subset$BMI,
               main = "Distribution of BMI",
               xlab = "BMI",
               col = "lightblue",
               border = "black")}
  # print 
my_visual_func_II()
    
    
    
    
    
  
                                                                                                                                                            
                                                                                                                                                              