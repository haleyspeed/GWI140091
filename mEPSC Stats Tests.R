# mEPSC Stats Tests
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//mEPSC//mEPSC_per_cell_3_groups.csv"

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

file.anova_freq <- paste(file.base, "freq_anova.csv", sep = "_")
file.anova_freq <- paste(file.outdir, file.anova_freq, sep = "\\")
file.tukey_freq <- paste(file.base, "freq_tukey.txt", sep = "_")
file.tukey_freq <- paste(file.outdir, file.tukey_freq, sep = "\\")
file.ci_freq <- paste(file.base, "freq_ci.csv", sep = "_")
file.ci_freq <- paste(file.outdir, file.ci_freq, sep = "\\")

file.anova_amp <- paste(file.base, "amp__anova.csv", sep = "_")
file.anova_amp <- paste(file.outdir, file.anova_amp, sep = "\\")
file.tukey_amp <- paste(file.base, "amp_tukey.txt", sep = "_")
file.tukey_amp <- paste(file.outdir, file.tukey_amp, sep = "\\")
file.ci_amp <- paste(file.base, "amp_ci.csv", sep = "_")
file.ci_amp <- paste(file.outdir, file.ci_amp, sep = "\\")

# Get summary SS, F, and p for amp
lme_freq = lme(as.numeric(as.character(freq)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_freq), file.anova_freq, row.names = TRUE)

lme_amp = lme(as.numeric(as.character(amp)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_amp), file.anova_amp, row.names = TRUE)


# Get detailed Tukey pairwise comparisons
tukey_freq <- summary(glht(lme_freq, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_freq <- cbind(tukey_freq$test$coefficients, tukey_freq$test$pvalues)
write.csv (tukey.output_freq, file.tukey_freq)

tukey_amp <- summary(glht(lme_amp, linfct=mcp(group ="Tukey")), test = adjusted(type = "none"))
tukey.output_amp <- cbind(tukey_amp$test$coefficients, tukey_amp$test$pvalues)
write.csv (tukey.output_amp, file.tukey_amp)

# Get Confidence intensitys for means
ci_freq <- groupwiseMean(freq ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_freq, file.ci_freq, row.names = FALSE)

ci_amp <- groupwiseMean(amp ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_amp, file.ci_amp, row.names = FALSE)