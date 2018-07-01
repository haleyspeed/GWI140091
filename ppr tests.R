# PPR Stats Tests
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//PPR//ppr per slice 3 groups.csv"

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
file.tukey_group <- paste(file.base, "group_tukey.csv", sep = "_")
file.tukey_group <- paste(file.outdir, file.tukey_group, sep = "\\")
file.ci <- paste(file.base, "ci.csv", sep = "_")
file.ci <- paste(file.outdir, file.ci, sep = "\\")

# Get summary SS, F, and p
lme_ppr = lme(as.numeric(as.character(ppr)) ~ as.factor(interval) * group, data=data, random = ~1|mouse)
write.csv(anova(lme_ppr), file.anova, row.names = TRUE)

# Get group Tukey pairwise comparisons
aov_group <- aov(ppr ~ group, data)
tukey_group <- TukeyHSD(aov_group)
write.csv (tukey_group$group, file.tukey_group)

# Get detailed Tukey pairwise comparisons
data$interact <- interaction(data$group, as.factor(data$interval))
model = lme(ppr ~ interact, data=data, random = ~1|mouse)

tukey <- summary(glht(model, linfct=mcp(interact="Tukey")), test = adjusted(type = "none"))
tukey.output <- cbind(tukey$test$coefficients, tukey$test$pvalues)
write.csv (tukey.output, file.tukey)

# Get Confidence intervals for means
ci <- groupwiseMean(ppr ~ group + interval, data = data, conf = 0.95, digits = 3)
write.csv(ci, file.ci, row.names = FALSE)


