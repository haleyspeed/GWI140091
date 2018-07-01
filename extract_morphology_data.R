# print user details
author <- "Haley E. Speed, Ph.D."
analysisDate <- date()
project <- "Gulf War"
info <- data.frame(Author = author, Date = analysisDate, Project = project)

library(xlsx)
library(dplyr)
# User inputs the directory to the files to be analyzed
dir <- readline("Enter the path to the working directory: ")
fileList <- as.data.frame(list.files(dir))

# Import data into a dataframe

getFiles <-function (){
        
        ## Code for opening each file programmatically, but too memory intensive
        # 8 consecutive files was as good as I could get with these datasets
        # filenames <- list.files(dir, pattern = "xlsx", full.names = TRUE)
        # sapply(filenames, analyze) 
        
        ## Code for analyzing each file individually
        
        ## Code if you want the user to input the meta data manually
        # Gets the user input for treatment/exposure group
        #getGroup <- function () { 
        #        print("Choose the treatment group:")
        #        print("1. Oil + Saline")
        #        print("2. Oil + IGF-1")
        #        print("3. CPF + Saline")
        #        print("4. CPF + IGF-1")
        #        groupSelect <- readline("Your selection: ")
        #        return(checkGroup(groupSelect))
        #}
        
        #checkGroup <- function (groupSelect){
        #        switch (groupSelect,
        #                "1" = {group <- "oil_saline"; return(group)},
        #                "2" = {group <- "oil_igf"; return(group)},
        #                "3" = {group <- "cpf_saline"; return(group)},
        #                "4" = {group <- "cpf_igf"; return(group)},
        #                {getGroup()})
        #}
        
        #getRegion <- function (){
        #        print("Choose the dendritic region:")
        #        print("1. Apical")
        #        print("2. Basal")
        #        regionSelect <- readline("Your selection: ")
        #        return(checkRegion(regionSelect))
        #}
        
        #checkRegion <- function (regionSelect){
        #        switch (regionSelect,
        #                "1" = {region <- "Apical"; return(region)},
        #                "2" = {region <- "Basal"; return(region)},
        #                {getRegion()})
        #}
        #name <- tolower(readline("Enter the neuron name (i.e. G3.1): "))
        #group <- tolower(getGroup())
        #region <- tolower(getRegion())
        
        file <- readline("Enter the .xlsx file to be analyzed: ")
        return(file)
     
}

readFile <- function (filename) {
        # Open the file and read the relevant sheets
        sheetBranching <- read.xlsx(filename, sheetIndex = "Tree Totals-Dendrite") 
        sheetDistance <- read.xlsx(filename, sheetIndex = "Tortuous Distance-Dendrite")
        sheetNeuron <- read.xlsx(filename, sheetIndex = "Neuron Summary")
        sheets <- c(sheetBranching,sheetDistance,sheetNeuron)
        return(sheets)
}   

analyzeBranching <- function (sheetBranching){ return("branching")}
analyzeDistance <- function (sheetDistance){ return("branching")}


writeNeuronFile <- function(file) {
        #C:\\Users\hspeed\\Dropbox\\Sync Data Analysis Computers\\Gulf War Project\\
        #Confocal Images\\Neurolucida Data\\DAT files 170519\\Excel Files 170519\\
        #morphology_neuron_summary.csv
        
        neuronOutfile <- read.xlsx("C:\\Users\hspeed\\Dropbox\\Sync Data Analysis Computers\\
                                   Gulf War Project\\Confocal Images\\Neurolucida Data\\
                                   DAT files 170519\\Excel Files 170519\\
                                   morphology_neuron_summary.csv")
        
        #Where row in File == filename, write all the data (SQL Would be best here)
        
}

## Run the program
file <- getFiles ()             # Open the file and the user inputs meta data(optional)
sheets <- readFiles ()          # Reads xlsx data into 3 separate data frames

branching <- as.data.frame(analyzeBranching(sheetBranching))
distance <- as.data.frame(analyzeDistance(sheetDistance))

writeNeuronFile(file, sheets$sheetNeuon)      # Writes the neuron data to file