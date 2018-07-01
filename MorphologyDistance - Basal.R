author <- "Haley E. Speed, Ph.D."
analysisDate <- date()
project <- "Gulf War"
info <- data.frame(Author = author, Date = analysisDate, Project = project)
print(info)

library(nlme)
library (dplyr)
library(rcompanion)
require(multcomp)

## FUNCTION DEFINITIONS

# Get directory and file information 
getUserInput <- function (){
        file.dir <- readline("Enter the path to the directory containing *_distance.csv files: ")
        return (file.dir)
}

getDataTables <- function (name.group,file.dir) {
        
        file.name <- paste(name.group,"_distance.csv",sep = "")
        file.name <- paste(file.dir,file.name , sep = "\\")
        data.group <- read.csv(file.name, header = TRUE, colClasses = "character")
        
        data.group <- mutate(data.group, group = as.character(name.group))
        data.group<- filter(data.group, spineDensity != 0)
        return(data.group)
}



# Function to calculate descriptive statistics
getStats <- function (data.group){
  data.mean   <- tapply(as.numeric(data.group$spineDensity), 
                        data.group$distance, mean)
  data.sd     <- tapply(as.numeric(data.group$spineDensity), 
                        data.group$distance, sd)
  data.n      <- length(unique(data.group$cell))
  data.se     <- as.numeric(data.sd)/sqrt(data.n)
  
  data.stats  <- as.data.frame(cbind(mean = data.mean, stdDev = data.sd, 
                                     stdErr = as.character(data.se), n = data.n))
  
  return (as.data.frame(data.stats))
}

# Calculate one-way ANOVA results
getANOVA <- function (data.compare){
  lme.spineDensity = lme(as.numeric(as.character(spineDensity)) ~ group*distance, 
                         random = ~1|cell, data = data.compare)
  anova.spineDensity <- as.data.frame(anova(lme.spineDensity))
  return(anova.spineDensity)
}

# Function to generate pairwise comparisons (Tukey posthoc analysis with
# Bonferroni corrections)
getTukey <- function (data.compare){
  data.compare$interaction <- interaction(data.compare$group, 
                                          data.compare$distance)
  model = lme(as.numeric(as.character(spineDensity)) ~ interaction, data = data.compare, random = ~1|cell)
  anova.tukey <- summary(glht(model, linfct = mcp(interaction = "Tukey")), 
                         test = adjusted(type = "bonferroni"))
  print(anova.tukey)
  #anova.tukey <- summary(anova.tukey)
  return(anova.tukey)
}



# Function to write descriptive statistics results to file
writeDescriptive <- function (file.data, file.name, file.dir){
  file.name <- paste(file.name, "_descriptive.csv", sep = "")
  file.dir <- paste(file.dir, "by_distance", sep = "\\")
  if (!dir.exists(file.dir)) {dir.create(file.dir)}
  file.dir <- paste(file.dir, file.name, sep = "\\")
  write.csv(file.data, file.dir, row.names = TRUE) 
}

# Function to write anova results to file
writeANOVA <- function (file.data, file.name, file.dir){
  file.name <- paste(file.name, "_anova.csv", sep = "")
  file.dir <- paste(file.dir, "by_distance", sep = "\\")
  if (!dir.exists(file.dir)) {dir.create(file.dir)}
  file.dir <- paste(file.dir, file.name, sep = "\\")
  write.csv(file.data, file.dir, row.names = TRUE) 
}

writeTukey <- function (tukey.compare, file.dir, file.name){
  file.data <- as.data.frame(cbind(coefficients = 
                                     tukey.compare$test$coefficients,
                                   sigma = tukey.compare$test$sigma, 
                                   t = tukey.compare$test$tstat,
                                   p = tukey.compare$test$pvalues))
  file.name <- paste(file.name, "_tukey.csv", sep = "")
  file.dir <- paste(file.dir, "by_distance", sep = "\\")
  if (!dir.exists(file.dir)) {dir.create(file.dir)}
  file.dir <- paste(file.dir, file.name, sep = "\\")
  write.csv(file.data, file.dir, row.names = TRUE)
  #capture.output(tukey.compare, file = file.dir) 
}

getConfidence <- function (data.compare){
  conf.spineDensity <-groupwiseMean(as.numeric(as.character(spineDensity)) ~ group*distance, 
                                    data = data.compare, conf=0.95, digits=4)
  return(as.data.frame(conf.spineDensity))
}

# Function to write 95% confidence intervals to file
writeConfidence <- function (file.data, file.name, file.dir){
  file.name <- paste(file.name, "_confidence.csv", sep = "")
  file.dir <- paste(file.dir, "by_distance", sep = "\\")
  if (!dir.exists(file.dir)) {dir.create(file.dir)}
  file.dir <- paste(file.dir, file.name, sep = "\\")
  write.csv(file.data, file.dir, row.names = TRUE) 
}

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

# Combine data for comparison between exposure/treatment groups
data.compare1 <- rbind(data.group1, data.group2)
data.compare2 <- rbind(data.group1, data.group3)
data.compare3 <- rbind(data.group1, data.group4)
data.compare4 <- rbind(data.group2, data.group3)
data.compare5 <- rbind(data.group2, data.group4)
data.compare6 <- rbind(data.group3, data.group4)

# Remove NAs from data.compare
data.compare1 <- filter(data.compare1, dendriteLength != 0)
data.compare2 <- filter(data.compare2, dendriteLength != 0)
data.compare3 <- filter(data.compare3, dendriteLength != 0)
data.compare4 <- filter(data.compare4, dendriteLength != 0)
data.compare5 <- filter(data.compare5, dendriteLength != 0)
data.compare6 <- filter(data.compare6, dendriteLength != 0)

# Assemble ANOVA output file
file.compare1 <- paste(name.group1,name.group2, sep = "_vs_")
file.compare2 <- paste(name.group1,name.group3, sep = "_vs_")
file.compare3 <- paste(name.group1,name.group4, sep = "_vs_")
file.compare4 <- paste(name.group2,name.group3, sep = "_vs_")
file.compare5 <- paste(name.group2,name.group4, sep = "_vs_")
file.compare6 <- paste(name.group3,name.group4, sep = "_vs_")

# Get two-way anova reports for each possible comparison
# If you get the MEEM error, make sure you have the same number of levels for your interactions
# For now, hacking it with data.compare1b, etc
data.compare1b <- filter(data.compare1, distance == "0-149" | distance == "150-299")
data.compare2b <- filter(data.compare2, distance == "0-149" | distance == "150-299")
data.compare3b <- filter(data.compare3, distance == "0-149" | distance == "150-299")

anova.compare1 <- getANOVA(data.compare1b)
anova.compare2 <- getANOVA(data.compare2b)
anova.compare3 <- getANOVA(data.compare3b)
anova.compare4 <- getANOVA(data.compare4)
anova.compare5 <- getANOVA(data.compare5)
anova.compare6 <- getANOVA(data.compare6)

# Write anova data to file
writeANOVA (anova.compare1, file.compare1, file.dir)
writeANOVA (anova.compare2, file.compare2, file.dir)
writeANOVA (anova.compare3, file.compare3, file.dir)
writeANOVA (anova.compare4, file.compare4, file.dir)
writeANOVA (anova.compare5, file.compare5, file.dir)
writeANOVA (anova.compare6, file.compare6, file.dir)

# Get Tukey posthoc analysis for each possible comparison
tukey.compare1 <- getTukey(data.compare1b)
tukey.compare2 <- getTukey(data.compare2b)
tukey.compare3 <- getTukey(data.compare3b)
tukey.compare4 <- getTukey(data.compare4)
tukey.compare5 <- getTukey(data.compare5)
tukey.compare6 <- getTukey(data.compare6)

# Write Tukey posthoc analysis to file
writeTukey(tukey.compare1, file.dir, file.compare1)
writeTukey(tukey.compare2, file.dir, file.compare2)
writeTukey(tukey.compare3, file.dir, file.compare3)
writeTukey(tukey.compare4, file.dir, file.compare4)
writeTukey(tukey.compare5, file.dir, file.compare5)
writeTukey(tukey.compare6, file.dir, file.compare6)


