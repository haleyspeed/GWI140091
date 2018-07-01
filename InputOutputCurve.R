# Input/Output Curve Descriptive Statistics
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Example Spreadsheet Setup                                                                             #
#-------------------------------------------------------------------------------------------------------#
#                                                                                                       #
#        |     A   |   B   |     C     |     D    |     E   |     F    |                                #
#       _______________________________________________________________                                 #
#       1|  group | Mouse |  slices  | intensity |   slope  |    fv   |                                 #
#       2|  WT    |  A1   |     2    |     0     | -0.00323 | -0.0043 |                                 #
#       3|  WT    |  A1   |     2    |     50    | -0.03421 | -0.0211 |                                 #
#       4|  WT    |  A1   |     3    |     100   | -0.52671 | -0.4521 |                                 #
#       2|  KO    |  A2   |     2    |     0     | -0.00256 | -0.0023 |                                 #
#       3|  KO    |  A2   |     1    |     50    | -0.07532 | -0.0215 |                                 #
#       4|  KO    |  A2   |     2    |     100   | -0.64367 | -0.5212 |                                 #
#                                                                                                       #
#-------------------------------------------------------------------------------------------------------#
                        

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
#filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//IO Curve//IO Curve Per Slice No Cohort F.csv"
filename <- "C://Users//haley//Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//IO Curve//IO Curve Per Slice No Cohort F.csv"


#-------------------------------------------------------------------------------------------------------#
# Do NOT MODIFY                                                                                         #
#-------------------------------------------------------------------------------------------------------#
library(dplyr)
library(rcompanion)
library(openxlsx)

# Import data from csv spreadsheet
data <- read.csv(filename, header = TRUE)
#data <- read.xlsx(filename, sheet = 1, colNames = TRUE)

# Calculate descriptive stats using Aggregate()
#ag.slices <- aggregate(data, by = list(data$intensity, data$group), FUN = sum)
ag.n <- aggregate(data, by = list(data$intensity, data$group), FUN = length)
ag.mean <- aggregate(data, by = list(data$intensity, data$group), FUN = mean)
ag.sd <- aggregate(data, by = list(data$intensity, data$group), FUN = sd)
ag.se <- aggregate(data, by = list(data$intensity, data$group), FUN = function(x) sd(x)/sqrt(length(x)))

# Create output table
output <- data.frame(ag.n[2], ag.n[3], ag.n[1], ag.mean[7], ag.sd[7], ag.se[7], ag.mean[8], ag.sd[8], ag.se[8])
colnames(output) <- c("group", "n", "intensity", "slope.mean", "slope.sd", "slope.se", "fv.mean", "fv.sd", "fv.se")

# Create output folder
fileDir <- dirname(filename)
outDir <- paste(fileDir, "analyzed", sep = "//")

# Create output file name
fileBase <- basename(filename)
fileBase <- strsplit(fileBase, ".csv")
#filebase <- strsplit(filebase, ".xlsx")
fileBase <- paste(fileBase, "descriptive.csv", sep = "_")
outFile <- paste(outDir, fileBase, sep = "//")

# Create Analyzed directory
if (!dir.exists(outDir)) { dir.create(outDir)}

# Write analyzed data to file
write.csv (output, outFile,  row.names = FALSE) 

