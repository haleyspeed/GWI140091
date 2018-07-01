# Biochemistry Descriptive Statistics
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//Biochem//delayed.csv"

#-------------------------------------------------------------------------------------------------------#
# Do NOT MODIFY                                                                                         #
#-------------------------------------------------------------------------------------------------------#
library(dplyr)
library(rcompanion)
library(openxlsx)

# Import data from csv spreadsheet
data <- read.csv(filename, header = TRUE)
#data <- read.xlsx(filename, sheet = 1, colNames = TRUE)

# Calculate descriptive stats 
ag.n <- aggregate(data, by = list(data$group, data$region), FUN = length)
ag.mean <- aggregate(data, by = list(data$group, data$region), FUN = mean)
ag.sd <- aggregate(data, by = list(data$group, data$region), FUN = sd)
ag.se <- aggregate(data, by = list(data$group, data$region), FUN = function(x) sd(x)/sqrt(length(x)))

# Create output table
output <- data.frame(ag.n[1], ag.n[2], ag.n[3], ag.mean[6], ag.sd[6], ag.se[6], ag.mean[7], ag.sd[7], ag.se[7])
colnames(output) <- c("group", "region", "n", "raw.mean", "raw.sd", "raw.se", "norm.mean", "norm.sd", "norm.se")
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
