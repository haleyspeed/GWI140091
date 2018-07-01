author <- "Haley E. Speed, Ph.D."
analysisDate <- date()
project <- "Gulf War"
info <- data.frame(Author = author, Date = analysisDate, Project = project)
print(info)

library(nlme)
library (dplyr)
library(rcompanion)

## FUNCTION DEFINITIONS

# Get directory and file information 
getUserInput <- function (){
  file.dir <- readline("Enter the path to the directory containing *_distance.csv files: ")
  return (file.dir)
}

getDataTables <- function (name.group,file.dir) {
  
  file.name <- paste(name.group,"_branching.csv",sep = "")
  file.name <- paste(file.dir,file.name , sep = "\\")
  data.group <- read.csv(file.name, header = TRUE, colClasses = "character")
  
  data.group <- mutate(data.group, group = as.character(name.group))
  #data.group<- filter(data.group, dendriteLength != 0)
  return(data.group)
}

# Function to calculate descriptive statistics for each bin
getStats <- function (data.group){
        n      <- length(unique(data.group$cell))
        
        mean.length <- tapply(as.numeric(data.group$dendriteLength), 
                       data.group$order, mean)
        sd.length   <- tapply(as.numeric(data.group$dendriteLength), 
                       data.group$order, sd)
        se.length   <- as.numeric(sd.length)/sqrt(n)
        
        mean.area   <- tapply(as.numeric(data.group$dendriteSurfaceArea), 
                       data.group$order, mean)
        sd.area     <- tapply(as.numeric(data.group$dendriteSurfaceArea), 
                       data.group$order, sd)
        se.area     <- as.numeric(sd.area)/sqrt(n)
        
        mean.volume <- tapply(as.numeric(data.group$dendriteVolume), 
                              data.group$order, mean)
        sd.volume   <- tapply(as.numeric(data.group$dendriteVolume), 
                              data.group$order, sd)
        se.volume     <- as.numeric(sd.volume)/sqrt(n)
        
        mean.spines <- tapply(as.numeric(data.group$spines), 
                            data.group$order, mean)
        sd.spines   <- tapply(as.numeric(data.group$spines), 
                            data.group$order, sd)
        se.spines     <- as.numeric(sd.spines)/sqrt(n)
        
        mean.thin <- tapply(as.numeric(data.group$thin), 
                              data.group$order, mean)
        sd.thin   <- tapply(as.numeric(data.group$thin), 
                              data.group$order, sd)
        se.thin     <- as.numeric(sd.thin)/sqrt(n)
        
        mean.stubby <- tapply(as.numeric(data.group$stubby), 
                            data.group$order, mean)
        sd.stubby   <- tapply(as.numeric(data.group$stubby), 
                            data.group$order, sd)
        se.stubby     <- as.numeric(sd.stubby)/sqrt(n)
        
        mean.mushroom <- tapply(as.numeric(data.group$mushroom), 
                            data.group$order, mean)
        sd.mushroom   <- tapply(as.numeric(data.group$mushroom), 
                            data.group$order, sd)
        se.mushroom     <- as.numeric(sd.mushroom)/sqrt(n)
        
        mean.filopodia <- tapply(as.numeric(data.group$filopodia), 
                            data.group$order, mean)
        sd.filopodia   <- tapply(as.numeric(data.group$filopodia), 
                            data.group$order, sd)
        se.filopodia     <- as.numeric(sd.filopodia)/sqrt(n)
        
        rows = unique(data.group$order)
        data.stats  <- as.data.frame(cbind(order = as.character(rows), mean.length,sd.length,se.length,
                                           mean.area,sd.area,se.area,
                                           mean.volume,sd.volume,se.volume,
                                           mean.spines,sd.spines,se.spines,
                                           mean.thin,sd.thin,se.thin,
                                           mean.stubby,sd.stubby,se.stubby,
                                           mean.mushroom,sd.mushroom,se.mushroom,
                                           mean.filopodia,sd.filopodia,se.filopodia,n))
        
        return (as.data.frame(data.stats))
}

# Function to get binned means for branch orders (bin = 5 orders)
## Add sd, se, n 
## return dataframe
getBinned <- function (data.group){
        
        #function to do descriptive stats on binned data
        getRowStats <- function (data, outRow, outCol, outTable){
                n <- length(data)
                mean <- mean(data)
                sd <- sd(data)
                se <- sd/sqrt(n)

                outTable[outRow,outCol] <- mean
                outTable[outRow,outCol+1] <- sd 
                outTable[outRow,outCol+2] <- se
                outTable[outRow,outCol+3] <- n
               
                return(outTable)
        }
        
        maxOrder <- tail(length(unique(data.group1$order)), n=1)
        row <- 1
        column <- 3
        i <- 1
        
        histo.out <- matrix(,nrow = 7,ncol = 33)
        colnames(histo.out) <- c("order", "meanDendriteLength", 
                                "sdDendriteLength", "seDendriteLength",
                                "nDendriteLength","meanDendriteSurfaceArea",
                                "sdDendriteSurfaceArea", "seDendriteSurfaceArea", 
                                "nDendriteSurfaceArea","meanDendriteVolume",
                                "sdDendriteVolume","seDendriteVolume",
                                "nDendriteVolume", "meanAllSpines","sdAllSpines",
                                "seAllSpines","nAllSpines", "meanThinSpines",
                                "sdThinSpines","seThinSpines","nThinSpines",
                                "meanStubbySpines","sdStubbySpines",
                                "seStubbySpines","nStubbySpines",
                                "meanMushroomSpines","sdMushroomSpines",
                                "seMushroomSpines", "nMushroomSpines", 
                                "meanFilopodiaSpines","sdFilopodiaSpines",
                                "seFilopodiaSpines","nFilopodiaSpines")
        
        outCol <- 2
        
        # Cycles through columns
        while (column < 11){
                bin5 <- c() 
                bin10 <- c()
                bin15 <- c()
                bin20 <- c()
                bin25 <- c()
                bin30 <- c()
                bin35 <- c()
                row <- 1
                
                # Cycles through rows of each column binning the data by order        
                while (row <= length(data.group$cell)){
                        if (data.group[row,2] <= 5){
                                bin5[length(bin5)+1] <- as.numeric(as.character
                                              (data.group[row,column]))
                        } else if (data.group[row,2] > 5 & data.group[row,2] <= 10){                              
                                bin10[length(bin10)+1]  <- as.numeric(as.character
                                                 (data.group[row,column]))
                        } else if (data.group[row,2] > 10 & data.group[row,2] <= 15){
                                bin15[length(bin15)+1]  <- as.numeric(as.character
                                                  (data.group[row,column]))                                
                        } else if (data.group[row,2] > 15 & data.group[row,2] <= 20){
                                bin20[length(bin20)+1]  <- as.numeric(as.character
                                                  (data.group[row,column]))
                        } else if (data.group[row,2] > 20 & data.group[row,2] <= 25){
                                bin25[length(bin25)+1]  <- as.numeric(as.character
                                                  (data.group[row,column]))
                        } else if (data.group[row,2] > 25 & data.group[row,2] <= 30){
                                bin30[length(bin30)+1]  <- as.numeric(as.character
                                                  (data.group[row,column]))
                        } else if (data.group[row,2] > 30 & data.group[row,2] <= 35){
                                bin35[length(bin35)+1]  <- as.numeric(as.character
                                                  (data.group[row,column]))
                        }
                        row <- row + 1
                }
                
                # Calculates descriptive stats for each bin
                histo.out <- getRowStats(bin5, 1, outCol, histo.out)
                histo.out <- getRowStats(bin10, 2, outCol, histo.out)
                histo.out <- getRowStats(bin15, 3, outCol, histo.out)
                histo.out <- getRowStats(bin20, 4, outCol, histo.out)
                histo.out <- getRowStats(bin25, 5, outCol, histo.out)
                histo.out <- getRowStats(bin30, 6, outCol, histo.out)
                histo.out <- getRowStats(bin35, 7, outCol, histo.out)
                
                outCol <- outCol + 4
                column <- column + 1
        }
        # Format output data as data frame and add row lables
        histo.out <- as.data.frame(histo.out)
        histo.out$order <-  c("1-5","6-10","11-15","16-20","21-25",
                                     "26-30","31-35")
        return(histo.out)
}

# Function to write descriptive statistics results to file
writeDescriptive <- function (file.data, file.name, file.dir){
        file.name <- paste(file.name, "_descriptive.csv", sep = "")
        file.dir <- paste(file.dir, "by_branching", sep = "\\")
        if (!dir.exists(file.dir)) {dir.create(file.dir)}
        file.dir <- paste(file.dir, file.name, sep = "\\")
        write.csv(file.data, file.dir, row.names = TRUE) 
}

# Function to write descriptive statistics results to file
writeBinned <- function (file.data, file.name, file.dir){
        file.name <- paste(file.name, "_binned.csv", sep = "")
        file.dir <- paste(file.dir, "by_branching", sep = "\\")
        if (!dir.exists(file.dir)) {dir.create(file.dir)}
        file.dir <- paste(file.dir, file.name, sep = "\\")
        write.csv(file.data, file.dir, row.names = TRUE) 
}

## Run the script
## Run the script
# Get user input from the console
file.dir <- getUserInput()

# Assign groups
name.group1 <- "oil_saline"
name.group2 <- "oil_igf"
name.group3 <- "cpf_saline"
name.group4 <- "cpf_igf"

# Read in data from each file and format it for analysis
data.group1  <- getDataTables(name.group1,file.dir)
data.group2  <- getDataTables(name.group2,file.dir)
data.group3  <- getDataTables(name.group3,file.dir)
data.group4  <- getDataTables(name.group4,file.dir)

# Get descriptive statistics for each group
stats.group1 <- getStats(data.group1)
stats.group2 <- getStats(data.group2)
stats.group3 <- getStats(data.group3)
stats.group4 <- getStats(data.group4)

# Write decriptive statistics to file
write.desc1 <- writeDescriptive(stats.group1, name.group1, file.dir) 
write.desc2 <- writeDescriptive(stats.group2, name.group2, file.dir) 
write.desc3 <- writeDescriptive(stats.group3, name.group3, file.dir) 
write.desc4 <- writeDescriptive(stats.group4, name.group4, file.dir)

# Get binned descriptive stats for each group
histo.group1 <- getBinned(data.group1)
histo.group2 <- getBinned(data.group2)
histo.group3 <- getBinned(data.group3)
histo.group4 <- getBinned(data.group4)

# Write binned statistics to file
write.histo1 <- writeBinned(histo.group1, name.group1, file.dir) 
write.histo2 <- writeBinned(histo.group2, name.group2, file.dir) 
write.histo3 <- writeBinned(histo.group3, name.group3, file.dir) 
write.histo4 <- writeBinned(histo.group4, name.group4, file.dir)

 

