# Input/Output Curve Descriptive Statistics
# Author: Haley Speed, PhD



library(knitr)
library(dplyr)
library(openxlsx)

#-------------------------------------------------------------------------------------------------------#
# Function definitions                                                                                  #
#-------------------------------------------------------------------------------------------------------#

# Function to get working directory from the user
getDir <- function (){
        file.inDir <- readline("Enter the path to the working directory: ")
        return (file.inDir)
}

# Function to get data from file
getDataTables <- function (file.name, file.dir) {
        setwd(file.dir)
        file.data <- read.xlsx(file.name, sheet =  1, colNames=TRUE)
        return(file.data)
}
# Function to save data to file
save_file <- function (output, file.name, file.dir) {
        if (!dir.exists("analyzed")) { dir.create("analyzed") }
        file.dir <- paste(file.dir, "analyzed", sep = "\\")
        file.out <- paste(file.dir, file.name, sep = "\\")
        write.csv(output, file.out, row.names = FALSE)
}
        
# Function to read in each file
read_files <- function (file.list, file.dir, output) {
       file <- 1
       i <- 1
       while (file <= length(file.list)) {
        
               # Read in data from each file 
               data <- getDataTables(file.list[file], file.dir)
               maxCol <- length(colnames(data))
        
               # Warning "NAs introduced by coersion" may be ignored
               events <- length(data$amplitude)
               freq <- events/300
        
               mean.amp <- mean(data$amp[!is.na(as.numeric(data$amp))])
               mean.interval <- mean(as.numeric(data$interval[!is.na(as.numeric(data$interval))]))
               mean.half_width <- mean(as.numeric(data$half_width[!is.na(as.numeric(data$half_width))]))
               mean.rise <- mean(as.numeric(data$rise[!is.na(as.numeric(data$rise))]))
               mean.decay <- mean(as.numeric(data$decay[!is.na(as.numeric(data$decay))]))
               mean.area <- mean(as.numeric(data$area[!is.na(as.numeric(data$area))]))
               singleData <- cbind(filename = data$file[1], mouse = data$mouse[1], cell = data$cell[1], 
                            genotype = data$genotype[1], treatment = data$treatment[1],
                            events = events, frequency = freq, amplitude = mean.amp, 
                            interval = mean.interval, half_width = mean.half_width, rise = mean.rise, 
                            decay = mean.decay, area = mean.area)
               output <- rbind(output, singleData)
               file <- file + 1  
       }                
       save_file(output, "all_cells.csv", file.dir) 
       
       return(output)
}
       
# Get summary data per cell
cell_summary <- function (output, file.dir) {
        output[,6:13] <- lapply(output[,6:13], function(x) type.convert(as.character((x))))
        ag.n <- aggregate(output, by = list(output$genotype, output$treatment), FUN = length)
        ag.mean <- aggregate(output, by = list(output$genotype, output$treatment), FUN = mean)
        ag.sd <- aggregate(output, by = list(output$genotype, output$treatment), FUN = sd)
        ag.se <- aggregate(output, by = list(output$genotype, output$treatment), FUN = function(x) sd(x)/sqrt(length(x)))

        summary.cell <- cbind(ag.mean[1], ag.mean[2], ag.n[3], ag.mean[9], ag.sd[9], ag.se[9], ag.mean[10], ag.sd[10], ag.se[10],
                 ag.mean[11], ag.sd[11], ag.se[11], ag.mean[12], ag.sd[12], ag.se[12],
                 ag.mean[13], ag.sd[13], ag.se[13], ag.mean[14], ag.sd[14], ag.se[14],
                 ag.mean[15], ag.sd[15], ag.se[15])
        colnames(summary.cell) <- c("genotype", "treatment", "n", "freq.mean", "freq.sd", "freq.se",
                       "amp.mean","amp.sd", "amp.se", "interval.mean", "interval.sd",
                       "interval.se", "half_width.mean","half_width.sd","half_width.se",
                       "rise.mean","rise.sd","rise.se","decay.mean","decay.sd","decay.se",
                       "area.mean", "area.sd", "area.se")

         save_file(summary.cell, "summary_per_cell.csv", file.dir) 
         
}

# Function to get summary data per mouse
mouse_summary <- function(output, file.dir) {
        output[,6:13] <- lapply(output[,6:13], function(x) type.convert(as.character((x))))
        output <- aggregate(output, by = list(output$mouse, output$genotype, output$treatment), mean)
        output <- cbind(output[2:3],output[9:16])
        colnames(output) <- c("genotype", "treatment", "events","freq","amp","interval", "half_width",
                      "rise","decay","area")
        ag.n <- aggregate(output, by = list(output$genotype, output$treatment), FUN = length)
        ag.mean <- aggregate(output, by = list(output$genotype, output$treatment), FUN = mean)
        ag.sd <- aggregate(output, by = list(output$genotype, output$treatment), FUN = sd)
        ag.se <- aggregate(output, by = list(output$genotype, output$treatment), FUN = function(x) sd(x)/sqrt(length(x)))

        summary.mouse <- cbind(ag.mean[1], ag.mean[2], ag.n[3], ag.mean[6], ag.sd[6], ag.se[6], ag.mean[7], ag.sd[7], ag.se[7],
                      ag.mean[8], ag.sd[8], ag.se[8], ag.mean[9], ag.sd[9], ag.se[9],
                      ag.mean[10], ag.sd[10], ag.se[10], ag.mean[11], ag.sd[11], ag.se[11],
                      ag.mean[12], ag.sd[12], ag.se[12])
        colnames(summary.mouse) <- c("genotype", "treatment", "n", "freq.mean", "freq.sd", "freq.se",
                            "amp.mean","amp.sd", "amp.se", "interval.mean", "interval.sd",
                            "interval.se", "half_width.mean","half_width.sd","half_width.se",
                            "rise.mean","rise.sd","rise.se","decay.mean","decay.sd","decay.se",
                            "area.mean", "area.sd", "area.se")

        save_file(summary.mouse, "summary_per_mouse.csv", file.dir) 
}

#-------------------------------------------------------------------------------------------------------#
# Run the script                                                                                        #
#-------------------------------------------------------------------------------------------------------#

# Get user input
file.dir <- getDir() 

# Get the file list within the analyzed directory
file.list <- list.files(file.dir)
file.list <- file.list[grepl("xlsx", file.list)]

# Set up the output file
output <- data.frame(filename = character(), mouse = character(), cell = character(), 
                     genotype = character(), treatment = character(),
                     events = numeric(), frequency = numeric(), amplitude = numeric(), 
                     interval = numeric(), half_width = numeric(), rise = numeric(), 
                     decay = numeric(), area = numeric())

output <- read_files(file.list, file.dir, output)
cell_summary(output, file.dir)
mouse_summary(output, file.dir)
        