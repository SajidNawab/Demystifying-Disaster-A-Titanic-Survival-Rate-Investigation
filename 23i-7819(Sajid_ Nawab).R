library(readr)
library(ggplot2)  ##import library for horizontal Bar chart
#install.packages("ggplot2")
titanic <- read_csv("C:/Users/Sajid/Downloads/titanic.csv")
#View(titanic)
print(colnames(titanic))

                          ##Part -01

# Create a contingency table  for survived and pclass
contingency_table <- table(titanic$survived, titanic$pclass)
# Print the contingency table
print(contingency_table)

# Create a contingency table  for survived and pclass

contingency_table_with_margins <- addmargins(contingency_table, FUN = 'sum')

# Print the updated contingency table with row and column margins
print(contingency_table_with_margins)

probability_survival_pclass=contingency_table_with_margins[2,1:3]/contingency_table_with_margins[3,1:3]

print(probability_survival_pclass)

if(probability_survival_pclass[1]>probability_survival_pclass[2] & probability_survival_pclass[1]>probability_survival_pclass[3] )
{
  print(("Yes! the survival rate of rich folk is higher from other"))
  print("You can verify from the visualization!")
}

##Visualization through  pie Chart 

# # Passenger class levels
pclass <- c("Rich folks", "Second Class", "Third Class")
# 
# # Sample survival rates for each passenger class
# #probability_survival_pclass <- c(0.65, 0.45, 0.25)
# 
# Calculate percentages
percentages <- probability_survival_pclass * 100
# 
# # Create labels with percentages
labels <- paste(pclass, "\n", round(percentages, 1), "%", sep = "")
# 
# # Define colors for each class
colors <- c("red", "blue", "green")


# 
# # Create a pie chart with different colors for each class
pie(probability_survival_pclass, labels = labels, main = "Survival Rate by Passenger Class", col = colors)
# 
# # Add a legend
#legend("topright", legend = pclass, fill = colors)
legend("topright", legend = pclass, fill=colors )


                          ##Part-02



# Create a contingency table  for survived and embarked
contingency_table_02 <- table(titanic$survived, titanic$embarked)
# Print the contingency table
print(contingency_table_02)
##Sum of Contigency_Table 
contingency_table_with_margins_02 <- addmargins(contingency_table_02, FUN = 'sum')

# Print the updated contingency table with row and column margins
print(contingency_table_with_margins_02)

probability_survival_embarked=contingency_table_with_margins_02[2,1:3]/contingency_table_with_margins_02[3,1:3]

print(probability_survival_embarked)
if(probability_survival_embarked[1]>probability_survival_embarked[2] & probability_survival_embarked[1]>probability_survival_embarked[3] )
{
   print(("Yes! the survive Ability depends where you got onboard"))
  print("You can verify from the visualization!")
}

## Visualization through Horizontal bar Chart

# Passenger embarked levels
em <- c("C", "Q", "S")
# Calculate percentages
percentages <- probability_survival_embarked * 100
# Create data frame
data <- data.frame(Embarked = em, Survival_Rate = percentages)
# Create a horizontal bar plot
ggplot(data, aes(x = reorder(Embarked, -Survival_Rate), y = Survival_Rate, fill = Embarked)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Port of Embarkation", y = "Survival Rate", title = "Survival Rate by Embarked") +
  coord_flip()

                      ##Part-03

# Create a contingency table  for survived and Sex'
contingency_table_03 <- table(titanic$survived, titanic$sex)
# Print the contingency table
print(contingency_table_03)


# Create a contingency table  for survived and 'Sex'

contingency_table_with_margins <- addmargins(contingency_table_03, FUN = 'sum')

# Print the updated contingency table with row and column margins
print(contingency_table_with_margins)

probability_survival_gender=contingency_table_with_margins[2,1:2]/contingency_table_with_margins[3,1:2]

print(probability_survival_gender)

if(probability_survival_gender[1]>probability_survival_gender[2] )
{
  print(("Yes! the male Sacrifies themselves to save female"))
  print("You can verify from the visualization!")
}



## Visualization through Horizontal bar Chart

# Passenger Gender wise
em <- c("Female", "Male")
# Calculate percentages
percentages <- probability_survival_gender * 100
# Create data frame
data <- data.frame(Sex = em, Survival_Rate = percentages)
# Create a horizontal bar plot
custom_colors <- c("#FF6F61", "#6A0572") 
ggplot(data, aes(x = reorder(Sex, -Survival_Rate), y = Survival_Rate, fill = Sex)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal() +
  labs(x = "Gender", y = "Survival Rate", title = "Survival Rate by Gender") +
  coord_flip()
#View(titanic)


