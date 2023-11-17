# Commands from scripts that I deleted, but may be useful in the future:

#The dataframe name will often be "tube_test_[something]"

#To see the version of R studio
R.Version()

#How to delete ROWS -c()
tube_test_long <- tube_test_long[-c(10,11),] #row 10 and 11 are deleted

#How to delete COLUMNS -c()
merged_data <- merged_data[,-c(10,11)] #column 10 and 11 are deleted
tube_test_long$Number_rows <- NULL #how also to remove a column e.g. "Number_rows"

#How to delete COLUMNS within()
tube_test_long <- within(tube_test_long, rm(Replicates))

#How to renaming a column:
names(tube_test_long)[10] <- "Replicates" #in this example it is column number 10 in the dataframe that needs to be renamed into "Replicates"

# Remove values in a column. Examples here: Remove "M" and "F" from the column Cage_ID
df_split$Cage_ID <- gsub("M", "", df_split$Cage_ID)
df_split$Cage_ID <- gsub("F", "", df_split$Cage_ID)

#Merging of dataframes by columns or rows
merged_data <- cbind(merged_data, df_split) #column joining
merged_data <- rbind(tt_loosers, tt_winners) #row joining

#Sorting the values of a column
sorted_cage <- merged_data[order(merged_data$Cage_ID), ] #sorting the df column "Cage_ID" in ascending order

#Add NA to empty cells
library(zoo)
filled_df <- na.locf(merged_df) # Fill forward missing values in the data frame

#Loop for repeating the same number/value for 20 times
# Create an example data frame
df <- data.frame(ID = 1:6384, Value = rnorm(6384))

# Initialize an empty vector to store the values
new_column <- character(6384)

# Define the number of rows for each pattern repetition
pattern_repetition <- 20

# Iterate through the rows and assign values based on the pattern
for (i in 1:6384) {
  # Calculate the pattern number
  pattern_number <- 1 + (i - 1) %/% pattern_repetition
  # Create the pattern with leading zeros
  pattern <- sprintf("T%02d", pattern_number)
  # Insert the pattern value
  new_column[i] <- pattern
}

# Add the new column to the data frame
df$NewColumn <- new_column

#To save a dataframe (or the alike) as a csv file:
#Remember to write the name of the file at the end of the directory path
write.csv(sorted_test_merged,"~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis/R_input_files/binomial_tube_test_tesults.csv", row.names = FALSE)