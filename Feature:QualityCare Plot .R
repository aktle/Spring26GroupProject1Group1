Plot 1
# Creates barplot for Quality Care 
HINTS_subset$QualityCare <- factor (HINTS_subset$QualityCare)
barplot(table(HINTS_subset$QualityCare),
        main = "Distribution of QualityCare",
        xlab = "Quality of Care",
        ylab = "Frequency",
        col = "lightblue")
# Interpretation: The bar chart shows the distribution of self-reported healthcare quality. Most respondents fall into the middle categories. Fewer rate 4-5. 

