# Biochemistry Stats Tests
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
#filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//Biochem//acute.csv"
filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//Biochem//delayed.csv"

#-------------------------------------------------------------------------------------------------------#
# Do NOT MODIFY                                                                                         #
#-------------------------------------------------------------------------------------------------------#

library(rcompanion)
library(nlme)
library(dplyr)
library(multcomp)

data <- read.csv(filename, header = TRUE)

# Set up output filenames
file.dir <- dirname(filename)
file.outdir <- paste(file.dir, "analyzed", sep = "//")
file.base <- basename(filename)
file.base <- strsplit(file.base, ".csv")
file.anova <- paste(file.base, "_anova.csv", sep = "_")
file.anova <- paste(file.outdir, file.anova, sep = "\\")
file.tukey <- paste(file.base, "tukey.txt", sep = "_")
file.tukey <- paste(file.outdir, file.tukey, sep = "\\")
file.ci <- paste(file.base, "ci.csv", sep = "_")
file.ci <- paste(file.outdir, file.ci, sep = "\\")

# Get summary SS, F, and p
lme_normalized = lme(as.numeric(as.character(normalized)) ~ as.factor(region) * group, data=data, random = ~1|mouse)
write.csv(anova(lme_normalized), file.anova, row.names = TRUE)

# Get detailed Tukey pairwise comparisons
data$interact <- interaction(data$group, as.factor(data$region))
model = lme(normalized ~ interact, data=data, random = ~1|mouse)

tukey <- summary(glht(model, linfct=mcp(interact="Tukey")), test = adjusted(type = "none"))
tukey.output <- cbind(tukey$test$coefficients, tukey$test$pvalues)
write.csv (tukey.output, file.tukey)

# Get Confidence regions for means
ci <- groupwiseMean(normalized ~ group + region, data = data, conf = 0.95, digits = 3)
write.csv(ci, file.ci, row.names = FALSE)


