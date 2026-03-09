# Allows for barplot to be made for RaceEthn5
HINTS_subset$RaceEthn5 <- factor (HINTS_subset$RaceEthn5)
barplot(table(HINTS_subset$RaceEthn5),
        main = "Distribution of Race and Ethnicity",
        xlab = "Race and Ethnicity",
        ylab = "Frequency",
        col = "lightgreen")
# Interpretation: The bar chart shows the distribution of respondents across racial and ethnic groups. Some groups have higher frequencies than others.

