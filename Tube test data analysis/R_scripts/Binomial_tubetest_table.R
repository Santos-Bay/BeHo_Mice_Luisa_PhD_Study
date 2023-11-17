################################ BINOMIAL TUBE TEST TABLE #############################

#This script was developed after talking to Inaki Oct 17th 2023
#We need to make the tube test results binomial (1 = win and 0 = loss). 
#Thus, the csv file from the Airtable "Tube test results" for win and loss was downloaded

#Load libraries
library(dplyr)

#Setting environment/sessions and reading in files
setwd("~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis")
tt_loosers = read.csv("R_input_files/Tube test results - loosers only-Grid view.csv", header = TRUE)
tt_winners = read.csv("R_input_files/Tube test results - winners only-Grid view.csv", header = TRUE)
tt_win_loose = read.csv("R_input_files/Tube test results - without notes.csv", header = TRUE)

#Renaming relevant columns in order to later merge the dfs
names(tt_loosers)[4] <- "Mouse"
names(tt_winners)[4] <- "Mouse"
names(tt_loosers)[10] <- "Binomial_data"
names(tt_winners)[10] <- "Binomial_data"
#Check that renaming worked
names(tt_loosers)
names(tt_winners)

#Merge dataframes and modify it by e.g. removing irrelevant data
merged_data <- rbind(tt_loosers, tt_winners) #row joining
merged_data <- merged_data[,-c(6,8,9)] #remove irrelevant columns
#Split the values by "_" into new columns
df_split <- data.frame(do.call('rbind', strsplit(merged_data$Test.code, "_")))
colnames(df_split) <- c("Cage_ID", "Treatment", "Tube_Replicate")
# Remove "M" and "F" from the values in Cage_ID column
df_split$Cage_ID <- gsub("M", "", df_split$Cage_ID)
df_split$Cage_ID <- gsub("F", "", df_split$Cage_ID)

#Join dataframes and remove old column
merged_data <- cbind(merged_data, df_split) #column joining
merged_data <- merged_data[,-c(2)]
head(merged_data)

#Sort the merged dataframe and add new relevant columns
sorted_cage <- merged_data[order(merged_data$Cage_ID), ] #sorting the df column "Cage_ID" in ascending order
sex <- c(rep("male",3080), rep("female",3304))
sorted_cage$Sex <- sex #sex is added to the table
sorted_rep <- sorted_cage[order(sorted_cage$Tube_Replicate), ]
sorted_treat <- sorted_rep[order(sorted_rep$Treatment), ]
sorted_date <- sorted_treat[order(sorted_treat$Date,decreasing = TRUE), ]
sorted_date$RowNumber <- 1:nrow(sorted_date) #adding numbers corresponding to the new sorted number of rows
sorted_mis_date <- sorted_date[-c(1:1736),]
sorted_mis_date$RowNumber <- 1:nrow(sorted_mis_date) 
dates <- c(rep("30/11/2020",184),rep("1/12/2020",280),rep("2/12/2020",280),rep("4/11/2020",320),
           rep("5/11/2020",320),rep("6/11/2020",320),rep("5/12/2020",280),rep("8/12/2020",280),
           rep("11/12/2020",280),rep("12/12/2020",280),rep("13/12/2020",280),rep("19/10/2020",280),
           rep("20/10/2020",320),rep("21/10/2020",320),rep("4/10/2020",304),rep("5/10/2020",320))
##### DOUBLE CHECK THAT THE DATES ARE CORRECT ######
sorted_mis_date$Date <- dates #add a new column to the df
sorted_mis_date$RowNumber <- NULL
sorted_date$RowNumber <- NULL
sorted_date <- sorted_date[-c(1737:6384),] #remove old rows/data with no dates
merged_dates <- rbind(sorted_date, sorted_mis_date) #merge data with new dates to the remaining data

#For now, NA will be written for "TIE" tube test results in column 3 (Mouse) and 6 (binomial_data)
merged_dates$RowNumber <- 1:nrow(merged_dates)
merged_dates[c(34,44,69,75,264,274,1987,1987,
               1990,1997,2000,3229,3239,3787,
               3797,4367,4370,4377,4380,4574, 
               4584,5145,5155,6094,6104), c(3,6)] = NA

#Adding a TubeTest_ID column
sorted_test_merged <- merged_dates[order(merged_dates$Name), ]

# Initialize an empty vector to store the values
new_column <- character(3332)

# Define the number of rows for each pattern repetition
pattern_repetition <- 20

# Iterate through the rows and assign values based on the pattern
for (i in 1:3332) {
  # Calculate the pattern number
  pattern_number <- 1 + (i - 1) %/% pattern_repetition
  # Create the pattern with leading zeros
  pattern <- sprintf("T%02d", pattern_number)
  # Insert the pattern value
  new_column[i] <- pattern
}

#From 3332 it is mixed numbers of trails. FIX THIS BEFORE ANY ANALYSES ARE DONE!
# Add the new column to the data frame
sorted_test_merged$TubeTest_ID <- new_column

#Cleaning up the dataframe and saving it as a csv file
names(sorted_test_merged)[1] <- "Trail_ID"
names(sorted_test_merged)[2] <- "Trial_Number"
names(sorted_test_merged)[4] <- "Tube_Win_Position"
names(sorted_test_merged)[6] <- "Win_Loss_Binomial"
sorted_test_merged$RowNumber <- NULL
sorted_test_merged$Row_Number <- 1:nrow(sorted_test_merged)
head(sorted_test_merged)
write.csv(sorted_test_merged,"~/Documents/PhD 2020/BeHo:ZiBA study/Data analysis/Tube test data analysis/R_input_files/binomial_tube_test_tesults.csv", row.names = FALSE)

######### Codes NOT working but could be useful for later ############

#df_split_tag <- data.frame(do.call('rbind', strsplit(merged_data$Mouse, "_")))
#df_split_tag$X1 <- NULL #remove column X1
#colnames(df_split_tag) <- c("Mouse_Tag")

#sorted_df <- merged_data[order(merged_data$Mouse), ]
#sorted_df <- merged_data[order(merged_data$Test.code), ] 

#sorted_test_merged$TubeTest_ID <- c(rep("T01",20),rep("T02",20),rep("T03",20),rep("T04",20),
#                                    rep("T05",20),rep("T06",20),rep("T07",20),rep("T08",20),
#                                    rep("T10",20),rep("T11",20),rep("T12",20),rep("T13",20),
#                                    rep("T14",20),rep("T15",20),rep("T16",20),rep("T17",20),
#                                    rep("T18",20),rep("T19",20),rep("T20",20),rep("T21",20),
#                                    rep("T22",20),rep("T23",20),rep("T24",20),rep("T25",20),
#                                    rep("T26",20),rep("T27",20),rep("T28",20),rep("T29",20),
#                                    rep("T30",20),rep("T31",20),rep("T32",20),rep("T33",20),
#                                    rep("T34",20),rep("T35",20),rep("T36",20),rep("T37",20),
#                                    rep("T38",20),rep("T39",20),rep("T40",20),rep("T41",20),
#                                    rep("T42",20),rep("T43",20),rep("T44",20),rep("T45",20),
#                                    rep("T46",20),rep("T47",20),rep("T48",20),rep("T49",20),
#                                    rep("T50",20),rep("T51",20),rep("T52",20),rep("T53",20),
#                                    rep("T54",20),rep("T55",20),rep("T56",20),rep("T57",20),
#                                    rep("T58",20),rep("T59",20),rep("T60",20),rep("T61",20),
#                                    rep("T62",20),rep("T63",20),rep("T64",20),rep("T65",20),
#                                    rep("T66",20),rep("T67",20),rep("T68",20),rep("T69",20),
#                                    rep("T70",20),rep("T71",20),rep("T72",20),rep("T73",20),
#                                    rep("T74",20),rep("T75",20),rep("T76",20),rep("T77",20),
#                                    rep("T78",20),rep("T79",20),rep("T80",20),rep("T81",20),
#                                    rep("T82",20),rep("T83",20),rep("T84",20),rep("T85",20),
#                                    rep("T86",20),rep("T87",20),rep("T88",20),rep("T89",20),
#                                    rep("T90",20),rep("T91",20),rep("T92",20),rep("T93",20),
#                                    rep("T94",20),rep("T95",20),rep("T96",20),rep("T97",20),
#                                    rep("T98",20),rep("T99",20),rep("T100",20),rep("T101",20),
#                                    rep("T102",20),rep("T103",20),rep("T104",20),rep("T105",20),
#                                    rep("T106",20),rep("T107",20),rep("T108",20),rep("T109",20),
#                                    rep("T110",20),rep("T111",20),rep("T112",20),rep("T113",20),
#                                    rep("T114",20),rep("T115",20),rep("T116",20),rep("T117",20),
#                                    rep("T118",20),rep("T119",20),rep("T120",20),rep("T121",20),
#                                    rep("T122",20),rep("T123",20),rep("T124",20),rep("T124",20),
#                                    rep("T125",20),rep("T126",20),rep("T127",20),rep("T128",20),
#                                    rep("T129",20),rep("T130",20),rep("T131",20),rep("T132",20),
#                                    rep("T133",20),rep("T134",20),rep("T135",20),rep("T136",20),
#                                    rep("T137",20),rep("T138",20),rep("T139",20),rep("T140",20),
#                                    rep("T141",20),rep("T142",20),rep("T143",20),rep("T144",20),
#                                    rep("T145",20),rep("T146",20),rep("T147",20),rep("T148",20),
#                                    rep("T149",20),rep("T150",20),rep("T151",20),rep("T152",20),
#                                    rep("T153",20),rep("T154",20),rep("T155",20),rep("T156",20),
#                                    rep("T157",20),rep("T158",20),rep("T159",20),rep("T160",20),
#                                    rep("T161",20),rep("T162",20),rep("T163",20),rep("T164",20),
#                                    rep("T165",20),rep("T166",20),rep("T167",20),rep("T168",20),
#                                    rep("T169",20),rep("T170",20),rep("T171",20),rep("T172",20),
#                                    rep("T173",20),rep("T174",20),rep("T175",20),rep("T176",20),
#                                    rep("T177",20),rep("T178",20),rep("T179",20),rep("T180",20),
#                                    rep("T181",20),rep("T182",20),rep("T183",20),rep("T184",20),
#                                    rep("T185",20),rep("T186",20),rep("T187",20),rep("T188",20),
#                                    rep("T189",20),rep("T190",20),rep("T191",20),rep("T192",20),
#                                    rep("T193",20),rep("T194",20),rep("T195",20),rep("T195",20),
#                                    rep("T196",20),rep("T197",20),rep("T198",20),rep("T199",20),
#                                    rep("T200",20),rep("T201",20),rep("T202",20),rep("T203",20),
#                                    rep("T204",20),rep("T205",20),rep("T206",20),rep("T207",20),
#                                    rep("T208",20),rep("T209",20),rep("T210",20),rep("T211",20),
#                                    rep("T212",20),rep("T213",20),rep("T214",20),rep("T215",20),
#                                    rep("T216",20),rep("T217",20),rep("T218",20),rep("T219",20),
#                                    rep("T220",20),rep("T221",20),rep("T222",20),rep("T223",20),
#                                    rep("T224",20),rep("T225",20),rep("T226",20),rep("T227",20),
#                                    rep("T228",20),rep("T229",20),rep("T230",20),rep("T231",20),
#                                    rep("T232",20),rep("T233",20),rep("T234",20),rep("T235",20),
#                                    rep("T236",20),rep("T237",20),rep("T238",20),rep("T239",20),
#                                    rep("T240",20),rep("T241",20),rep("T242",20),rep("T243",20),
#                                    rep("T244",20),rep("T245",20),rep("T246",20),rep("T247",20),
#                                    rep("T248",20),rep("T249",20),rep("T250",20),rep("T251",20),
#                                    rep("T252",20),rep("T253",20),rep("T254",20),rep("T255",20),
#                                    rep("T256",20),rep("T257",20),rep("T258",20),rep("T259",20),
#                                    rep("T260",20),rep("T261",20),rep("T262",20),rep("T263",20),
#                                    rep("T264",20),rep("T265",20),rep("T266",20),rep("T267",20),
#                                    rep("T268",20),rep("T269",20),rep("T270",20),rep("T271",20),
#                                    rep("T272",20),rep("T273",20),rep("T274",20),rep("T275",20),
#                                    rep("T276",20),rep("T277",20),rep("T278",20),rep("T279",20),
#                                    rep("T280",20),rep("T281",20),rep("T282",20),rep("T283",20),
#                                    rep("T284",20),rep("T285",20),rep("T286",20),rep("T287",20),
#                                    rep("T288",20),rep("T289",20),rep("T290",20),rep("T291",20),
#                                    rep("T292",20),rep("T293",20),rep("T294",20),rep("T295",20),
#                                    rep("T296",20),rep("T297",20),rep("T298",20),rep("T299",20),
#                                    rep("T300",20),rep("T301",20),rep("T302",20),rep("T303",20),
#                                    rep("T304",20),rep("T305",20),rep("T306",20),rep("T307",20),
#                                    rep("T308",20),rep("T309",20),rep("T310",20),rep("T311",20),
#                                    rep("T312",20),rep("T313",20),rep("T314",20),rep("T315",20),
#                                    rep("T316",20),rep("T317",20),rep("T318",20),rep("T319",4))