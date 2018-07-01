# Morphology Descriptive Statistics
# Author: Haley Speed, PhD


#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
file.name  <- "C://Users//haley//Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//Morphology//Apical//Aggregated//summary_per_slice.csv"
#file.name <- "C://Users//haley//Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//Morphology//Apical//Aggregated//summary_per_mouse.csv"


#-------------------------------------------------------------------------------------------------------#
# Do NOT MODIFY                                                                                         #
#-------------------------------------------------------------------------------------------------------#
library(dplyr)
library(rcompanion)
library(openxlsx)

# Import data from csv spreadsheet
data <- read.csv(file.name, header = TRUE)
#data <- read.xlsx(filename, sheet = 1, colNames = TRUE)

# Calculate descriptive stats using Aggregate()
#ag.slices <- aggregate(data, by = list(data$intensity, data$group), FUN = sum)
ag.n <- aggregate(data, by = list(data$group), FUN = length)
ag.mean <- aggregate(data, by = list(data$group), FUN = mean)
ag.sd <- aggregate(data, by = list(data$group), FUN = sd)
ag.se <- aggregate(data, by = list(data$group), FUN = function(x) sd(x)/sqrt(length(x)))

# Create output folder
file.dir <- dirname(file.name)
out.dir <- paste(file.dir, "analyzed", sep = "//")

# Create Analyzed directory
if (!dir.exists(out.dir)) { dir.create(out.dir)}

# Create output table
i = 4
j = 3
header <- colnames(data)
while (j <= length(header)) {
        output <- data.frame(ag.n[1], ag.n[2], ag.mean[i], ag.sd[i], ag.se[i])
        colnames(output) <- c("group", "n", "mean","sd", "se") 
        
        # Create output file name
        file.base <- basename(file.name)
        file.base <- strsplit(file.base, ".csv")
        #filebase <- strsplit(filebase, ".xlsx")
        
        # Construct file name
        file.base <- paste(file.base, header[j], sep = "_") 
        file.base <- paste(file.base, "descriptive.csv", sep = "_")
        out.file <- paste(out.dir, file.base, sep = "//")
        
        # Write analyzed data to file
        write.csv (output, out.file, row.names = FALSE) 
        j = j + 1
        i = i + 1
}




