# Question 4. I
# Creates barplot for Quality Care 
HINTS_subset$QualityCare <- factor (HINTS_subset$QualityCare)
barplot(table(HINTS_subset$QualityCare),
        main = "Distribution of QualityCare",
        xlab = "Quality of Care",
        ylab = "Frequency",
        col = "lightblue")
# Interpretation: The bar chart shows the distribution of self-reported healthcare quality. Most respondents fall into the middle categories. Fewer rate 4-5. 

# Creates barplot for RaceEthn5 
HINTS_subset$RaceEthn5 <- factor (HINTS_subset$RaceEthn5)
barplot(table(HINTS_subset$RaceEthn5),
        main = "Distribution of Race and Ethnicity",
        xlab = "Race and Ethnicity",
        ylab = "Frequency",
        col = "lightgreen")
# Interpretation: The bar chart shows the distribution of respondents across racial and ethnic groups. Some groups have higher frequencies than others.


# Question 4. II
# Allows for histogram to be made 
class(HINTS_subset$Age)
"numeric"
# Creates the histogram for Age 
HINTS_subset$Age <- as.numeric(as.character(HINTS_subset$Age))
hist(HINTS_subset$Age,
     main = "Distribution of Age",
     xlab = "Age",
     col = "darkgreen",
     border = "black")
# Interpretation: Some age groups appear more common than others, indicating variation in the age distribution

# Allows for histogram to be made 
class(HINTS_subset$BMI)
"numeric"
# Creates histogram for BMI
HINTS_subset$BMI <- as.numeric(as.character(HINTS_subset$BMI))
hist(HINTS_subset$BMI,
     main = "Distribution of BMI",
     xlab = "BMI",
     col = "darkblue",
     border = "black")
# Interpretation: Most BMI values fall within a central range, with fewer individuals at very low or very high values

# Question 4. III
# Dowload ggplot2 
install.packages("ggplot2")
# Connects ggplot2 to R 
library(ggplot2)

# Creates barchart Quality Care 
ggplot(HINTS_subset, aes (x = QualityCare)) + # ggplot() is telling R to use which data, aes -> variables mapped to axes
  geom_bar(fill = "lightblue") + # type of graph (in this case bar)
  labs(title = "Distribution of QualityCare", # labs is the titles and labels of axis
       x = "Quality of Care",
       y = "Frequency") +
  theme_bw() # overall appearance

# Creates barchart for RaceEthn5
ggplot(HINTS_subset, aes(x = RaceEthn5)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Race and Ethnicity",
       x = "Race/Ethnicity",
       y = "Frequency") +
  theme_bw()

# Creates histogram for Age
ggplot(HINTS_subset, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count") +
  theme_bw()

# Creates histogram for BMI
ggplot(HINTS_subset, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "Distribution of BMI",
       x = "BMI",
       y = "Count") +
  theme_bw()

# Interpretation: The ggplot2 graphs show the same distributions as the base R graphs, but they look cleaner. Bar charts are used for categorical variables, and histograms are used for continuous variables.

# Question 4. IV
# Allows for Quality Care and RaceEthn5 to be under one function 
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

# Allows for Age and BMI to be under one function
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

