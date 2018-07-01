# IO Curve Stats Tests
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//IO Curve//IO Curve Per Mouse No Cohort F 3 Groups.csv"

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

file.anova_slope <- paste(file.base, "slope_anova.csv", sep = "_")
file.anova_slope <- paste(file.outdir, file.anova_slope, sep = "\\")
file.tukey_slope <- paste(file.base, "slope_tukey.txt", sep = "_")
file.tukey_slope <- paste(file.outdir, file.tukey_slope, sep = "\\")
file.tukey_slope_group <- paste(file.base, "group_slope_tukey.csv", sep = "_")
file.tukey_slope_group <- paste(file.outdir, file.tukey_slope_group, sep = "\\")
file.ci_slope <- paste(file.base, "slope_ci.csv", sep = "_")
file.ci_slope <- paste(file.outdir, file.ci_slope, sep = "\\")

file.anova_fv <- paste(file.base, "fv__anova.csv", sep = "_")
file.anova_fv <- paste(file.outdir, file.anova_fv, sep = "\\")
file.tukey_fv <- paste(file.base, "fv_tukey.txt", sep = "_")
file.tukey_fv <- paste(file.outdir, file.tukey_fv, sep = "\\")
file.tukey_fv_group <- paste(file.base, "group_fv_tukey.csv", sep = "_")
file.tukey_fv_group <- paste(file.outdir, file.tukey_fv_group, sep = "\\")
file.ci_fv <- paste(file.base, "fv_ci.csv", sep = "_")
file.ci_fv <- paste(file.outdir, file.ci_fv, sep = "\\")

# Get summary SS, F, and p for fv
lme_slope = lme(as.numeric(as.character(slope)) ~ as.factor(intensity) * group, data=data, random = ~1|mouse)
write.csv(anova(lme_slope), file.anova_slope, row.names = TRUE)

lme_fv = lme(as.numeric(as.character(fv)) ~ as.factor(intensity) * group, data=data, random = ~1|mouse)
write.csv(anova(lme_fv), file.anova_fv, row.names = TRUE)

# Get group Tukey pairwise comparisons
aov_slope_group <- aov(slope ~ group, data)
tukey_slope_group <- TukeyHSD(aov_slope_group)
write.csv (tukey_slope_group$group, file.tukey_slope_group)

aov_fv_group <- aov(fv ~ group, data)
tukey_fv_group <- TukeyHSD(aov_fv_group)
write.csv (tukey_fv_group$group, file.tukey_fv_group)

# Get detailed Tukey pairwise comparisons
data$interact <- interaction(data$group, as.factor(data$intensity))
model_slope = lme(slope ~ interact, data=data, random = ~1|mouse)
tukey_slope <- summary(glht(model_slope, linfct=mcp(interact="Tukey")), test = adjusted(type = "none"))
tukey.output_slope <- cbind(tukey_slope$test$coefficients, tukey_slope$test$pvalues)
write.csv (tukey.output_slope, file.tukey_slope)

data$interact <- interaction(data$group, as.factor(data$intensity))
model_fv = lme(fv ~ interact, data=data, random = ~1|mouse)
tukey_fv <- summary(glht(model_fv, linfct=mcp(interact="Tukey")), test = adjusted(type = "none"))
tukey.output_fv <- cbind(tukey_fv$test$coefficients, tukey_fv$test$pvalues)
write.csv (tukey.output_fv, file.tukey_fv)

# Get Confidence intensitys for means
ci_slope <- groupwiseMean(slope ~ group + intensity, data = data, conf = 0.95, digits = 3)
write.csv(ci_slope, file.ci_slope, row.names = FALSE)

ci_fv <- groupwiseMean(fv ~ group + intensity, data = data, conf = 0.95, digits = 3)
write.csv(ci_fv, file.ci_fv, row.names = FALSE)

