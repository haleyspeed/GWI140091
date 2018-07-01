# Morphology Stats Tests
# Author: Haley Speed, PhD

#-------------------------------------------------------------------------------------------------------#
# Things the user should change                                                                         #
#-------------------------------------------------------------------------------------------------------#

# Assign filename and path (replace all backslashes with double forward slashes in path)
filename  <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//Morphology//Apical//aggregated//summary_per_cell.csv"

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

file.anova_dendrite_length <- paste(file.base, "dendrite_length_anova.csv", sep = "_")
file.anova_dendrite_length <- paste(file.outdir, file.anova_dendrite_length, sep = "\\")
file.tukey_dendrite_length <- paste(file.base, "dendrite_length_tukey.txt", sep = "_")
file.tukey_dendrite_length <- paste(file.outdir, file.tukey_dendrite_length, sep = "\\")
file.tukey_dendrite_length_group <- paste(file.base, "dendrite_length_group_tukey.csv", sep = "_")
file.tukey_dendrite_length_group <- paste(file.outdir, file.tukey_dendrite_length_group, sep = "\\")
file.ci_dendrite_length <- paste(file.base, "dendrite_length_ci.csv", sep = "_")
file.ci_dendrite_length <- paste(file.outdir, file.ci_dendrite_length, sep = "\\")

file.anova_dendrite_branching <- paste(file.base, "dendrite_branching_anova.csv", sep = "_")
file.anova_dendrite_branching <- paste(file.outdir, file.anova_dendrite_branching, sep = "\\")
file.tukey_dendrite_branching <- paste(file.base, "dendrite_branching_tukey.txt", sep = "_")
file.tukey_dendrite_branching <- paste(file.outdir, file.tukey_dendrite_branching, sep = "\\")
file.tukey_dendrite_branching_group <- paste(file.base, "dendrite_branching_group_tukey.csv", sep = "_")
file.tukey_dendrite_branching_group <- paste(file.outdir, file.tukey_dendrite_branching_group, sep = "\\")
file.ci_dendrite_branching <- paste(file.base, "dendrite_branching_ci.csv", sep = "_")
file.ci_dendrite_branching <- paste(file.outdir, file.ci_dendrite_branching, sep = "\\")

file.anova_dendrite_orders <- paste(file.base, "dendrite_orders_anova.csv", sep = "_")
file.anova_dendrite_orders <- paste(file.outdir, file.anova_dendrite_orders, sep = "\\")
file.tukey_dendrite_orders <- paste(file.base, "dendrite_orders_tukey.txt", sep = "_")
file.tukey_dendrite_orders <- paste(file.outdir, file.tukey_dendrite_orders, sep = "\\")
file.tukey_dendrite_orders_group <- paste(file.base, "dendrite_orders_group_tukey.csv", sep = "_")
file.tukey_dendrite_orders_group <- paste(file.outdir, file.tukey_dendrite_orders_group, sep = "\\")
file.ci_dendrite_orders <- paste(file.base, "dendrite_orders_ci.csv", sep = "_")
file.ci_dendrite_orders <- paste(file.outdir, file.ci_dendrite_orders, sep = "\\")

file.anova_dendrite_area <- paste(file.base, "dendrite_area_anova.csv", sep = "_")
file.anova_dendrite_area <- paste(file.outdir, file.anova_dendrite_area, sep = "\\")
file.tukey_dendrite_area <- paste(file.base, "dendrite_area_tukey.txt", sep = "_")
file.tukey_dendrite_area <- paste(file.outdir, file.tukey_dendrite_area, sep = "\\")
file.tukey_dendrite_area_group <- paste(file.base, "dendrite_area_group_tukey.csv", sep = "_")
file.tukey_dendrite_area_group <- paste(file.outdir, file.tukey_dendrite_area_group, sep = "\\")
file.ci_dendrite_area <- paste(file.base, "dendrite_area_ci.csv", sep = "_")
file.ci_dendrite_area <- paste(file.outdir, file.ci_dendrite_area, sep = "\\")

file.anova_dendrite_volume <- paste(file.base, "dendrite_volume_anova.csv", sep = "_")
file.anova_dendrite_volume <- paste(file.outdir, file.anova_dendrite_volume, sep = "\\")
file.tukey_dendrite_volume <- paste(file.base, "dendrite_volume_tukey.txt", sep = "_")
file.tukey_dendrite_volume <- paste(file.outdir, file.tukey_dendrite_volume, sep = "\\")
file.tukey_dendrite_volume_group <- paste(file.base, "dendrite_volume_group_tukey.csv", sep = "_")
file.tukey_dendrite_volume_group <- paste(file.outdir, file.tukey_dendrite_volume_group, sep = "\\")
file.ci_dendrite_volume <- paste(file.base, "dendrite_volume_ci.csv", sep = "_")
file.ci_dendrite_volume <- paste(file.outdir, file.ci_dendrite_volume, sep = "\\")

file.anova_filopodia <- paste(file.base, "filopodia_anova.csv", sep = "_")
file.anova_filopodia <- paste(file.outdir, file.anova_filopodia, sep = "\\")
file.tukey_filopodia <- paste(file.base, "filopodia_tukey.txt", sep = "_")
file.tukey_filopodia <- paste(file.outdir, file.tukey_filopodia, sep = "\\")
file.tukey_filopodia_group <- paste(file.base, "filopodia_group_tukey.csv", sep = "_")
file.tukey_filopodia_group <- paste(file.outdir, file.tukey_filopodia_group, sep = "\\")
file.ci_filopodia <- paste(file.base, "filopodia_ci.csv", sep = "_")
file.ci_filopodia <- paste(file.outdir, file.ci_filopodia, sep = "\\")

file.anova_mushroom <- paste(file.base, "mushroom_anova.csv", sep = "_")
file.anova_mushroom <- paste(file.outdir, file.anova_mushroom, sep = "\\")
file.tukey_mushroom <- paste(file.base, "mushroom_tukey.txt", sep = "_")
file.tukey_mushroom <- paste(file.outdir, file.tukey_mushroom, sep = "\\")
file.tukey_mushroom_group <- paste(file.base, "mushroom_group_tukey.csv", sep = "_")
file.tukey_mushroom_group <- paste(file.outdir, file.tukey_mushroom_group, sep = "\\")
file.ci_mushroom <- paste(file.base, "mushroom_ci.csv", sep = "_")
file.ci_mushroom <- paste(file.outdir, file.ci_mushroom, sep = "\\")

file.anova_spine_area <- paste(file.base, "spine_area_anova.csv", sep = "_")
file.anova_spine_area <- paste(file.outdir, file.anova_spine_area, sep = "\\")
file.tukey_spine_area <- paste(file.base, "spine_area_tukey.txt", sep = "_")
file.tukey_spine_area <- paste(file.outdir, file.tukey_spine_area, sep = "\\")
file.tukey_spine_area_group <- paste(file.base, "spine_area_group_tukey.csv", sep = "_")
file.tukey_spine_area_group <- paste(file.outdir, file.tukey_spine_area_group, sep = "\\")
file.ci_spine_area <- paste(file.base, "spine_area_ci.csv", sep = "_")
file.ci_spine_area <- paste(file.outdir, file.ci_spine_area, sep = "\\")

file.anova_spine_volume <- paste(file.base, "spine_volume_anova.csv", sep = "_")
file.anova_spine_volume <- paste(file.outdir, file.anova_spine_volume, sep = "\\")
file.tukey_spine_volume <- paste(file.base, "spine_volume_tukey.txt", sep = "_")
file.tukey_spine_volume <- paste(file.outdir, file.tukey_spine_volume, sep = "\\")
file.tukey_spine_volume_group <- paste(file.base, "spine_volume_group_tukey.csv", sep = "_")
file.tukey_spine_volume_group <- paste(file.outdir, file.tukey_spine_volume_group, sep = "\\")
file.ci_spine_volume <- paste(file.base, "spine_volume_ci.csv", sep = "_")
file.ci_spine_volume <- paste(file.outdir, file.ci_spine_volume, sep = "\\")

file.anova_stubby <- paste(file.base, "stubby_anova.csv", sep = "_")
file.anova_stubby <- paste(file.outdir, file.anova_stubby, sep = "\\")
file.tukey_stubby <- paste(file.base, "stubby_tukey.txt", sep = "_")
file.tukey_stubby <- paste(file.outdir, file.tukey_stubby, sep = "\\")
file.tukey_stubby_group <- paste(file.base, "stubby_group_tukey.csv", sep = "_")
file.tukey_stubby_group <- paste(file.outdir, file.tukey_stubby_group, sep = "\\")
file.ci_stubby <- paste(file.base, "stubby_ci.csv", sep = "_")
file.ci_stubby <- paste(file.outdir, file.ci_stubby, sep = "\\")

file.anova_thin <- paste(file.base, "thin_anova.csv", sep = "_")
file.anova_thin <- paste(file.outdir, file.anova_thin, sep = "\\")
file.tukey_thin <- paste(file.base, "thin_tukey.txt", sep = "_")
file.tukey_thin <- paste(file.outdir, file.tukey_thin, sep = "\\")
file.tukey_thin_group <- paste(file.base, "thin_group_tukey.csv", sep = "_")
file.tukey_thin_group <- paste(file.outdir, file.tukey_thin_group, sep = "\\")
file.ci_thin <- paste(file.base, "thin_ci.csv", sep = "_")
file.ci_thin <- paste(file.outdir, file.ci_thin, sep = "\\")

file.anova_total_spines <- paste(file.base, "total_spines_anova.csv", sep = "_")
file.anova_total_spines <- paste(file.outdir, file.anova_total_spines, sep = "\\")
file.tukey_total_spines <- paste(file.base, "total_spines_tukey.txt", sep = "_")
file.tukey_total_spines <- paste(file.outdir, file.tukey_total_spines, sep = "\\")
file.tukey_total_spines_group <- paste(file.base, "total_spines_group_tukey.csv", sep = "_")
file.tukey_total_spines_group <- paste(file.outdir, file.tukey_total_spines_group, sep = "\\")
file.ci_total_spines <- paste(file.base, "total_spines_ci.csv", sep = "_")
file.ci_total_spines <- paste(file.outdir, file.ci_total_spines, sep = "\\")

# Get summary SS, F, and p
lme_branch = lme(as.numeric(as.character(dendriteBranches)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_branch), file.anova_dendrite_branching, row.names = TRUE)

lme_length = lme(as.numeric(as.character(dendriteLength)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_length), file.anova_dendrite_length, row.names = TRUE)

lme_orders = lme(as.numeric(as.character(dendriteOrders)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_orders), file.anova_dendrite_orders, row.names = TRUE)

lme_darea = lme(as.numeric(as.character(dendriteSurfaceArea)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_darea), file.anova_dendrite_area, row.names = TRUE)

lme_dvolume = lme(as.numeric(as.character(dendriteVolume)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_dvolume), file.anova_dendrite_volume, row.names = TRUE)

lme_filopodia = lme(as.numeric(as.character(filopodia)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_filopodia), file.anova_filopodia, row.names = TRUE)

lme_mushroom = lme(as.numeric(as.character(mushroomSpines)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_mushroom), file.anova_mushroom, row.names = TRUE)

lme_sarea = lme(as.numeric(as.character(spineArea)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_sarea), file.anova_spine_area, row.names = TRUE)

lme_svolume = lme(as.numeric(as.character(spineVolume)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_svolume), file.anova_spine_volume, row.names = TRUE)

lme_stubby = lme(as.numeric(as.character(stubbySpines)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_stubby), file.anova_stubby, row.names = TRUE)

lme_thin = lme(as.numeric(as.character(thinSpines)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_thin), file.anova_thin, row.names = TRUE)

lme_total = lme(as.numeric(as.character(totalSpines)) ~ group, data=data, random = ~1|mouse)
write.csv(anova(lme_total), file.anova_total_spines, row.names = TRUE)

# Get group Tukey pairwise comparisons
aov_dendrite_length_group <- aov(dendriteLength ~ group, data)
tukey_dendrite_length_group <- TukeyHSD(aov_dendrite_length_group)
write.csv (tukey_dendrite_length_group$group, file.tukey_dendrite_length_group)

aov_dendrite_branching_group <- aov(dendriteBranches ~ group, data)
tukey_dendrite_branching_group <- TukeyHSD(aov_dendrite_branching_group)
write.csv (tukey_dendrite_branching_group$group, file.tukey_dendrite_branching_group)

aov_dendrite_orders_group <- aov(dendriteOrders ~ group, data)
tukey_dendrite_orders_group <- TukeyHSD(aov_dendrite_orders_group)
write.csv (tukey_dendrite_orders_group$group, file.tukey_dendrite_orders_group)

aov_dendrite_volume_group <- aov(dendriteVolume ~ group, data)
tukey_dendrite_volume_group <- TukeyHSD(aov_dendrite_volume_group)
write.csv (tukey_dendrite_volume_group$group, file.tukey_dendrite_volume_group)

aov_filopodia_group <- aov(filopodia ~ group, data)
tukey_filopodia_group <- TukeyHSD(aov_filopodia_group)
write.csv (tukey_filopodia_group$group, file.tukey_filopodia_group)

aov_mushroom_group <- aov(mushroomSpines ~ group, data)
tukey_mushroom_group <- TukeyHSD(aov_mushroom_group)
write.csv (tukey_mushroom_group$group, file.tukey_mushroom_group)

aov_spine_area_group <- aov(spineArea ~ group, data)
tukey_spine_area_group <- TukeyHSD(aov_spine_area_group)
write.csv (tukey_spine_area_group$group, file.tukey_spine_area_group)

aov_spine_volume_group <- aov(spineVolume ~ group, data)
tukey_spine_volume_group <- TukeyHSD(aov_spine_volume_group)
write.csv (tukey_spine_volume_group$group, file.tukey_spine_volume_group)

aov_stubby_group <- aov(stubbySpines ~ group, data)
tukey_stubby_group <- TukeyHSD(aov_stubby_group)
write.csv (tukey_stubby_group$group, file.tukey_stubby_group)

aov_thin_group <- aov(thinSpines ~ group, data)
tukey_thin_group <- TukeyHSD(aov_thin_group)
write.csv (tukey_thin_group$group, file.tukey_thin_group)

aov_total_spines_group <- aov(totalSpines ~ group, data)
tukey_total_spines_group <- TukeyHSD(aov_total_spines_group)
write.csv (tukey_total_spines_group$group, file.tukey_total_spines_group)

# Get detailed Tukey pairwise comparisons
tukey_branch <- summary(glht(lme_branch, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_branch <- cbind(tukey_branch$test$coefficients, tukey_branch$test$pvalues)
write.csv (tukey.output_branch, file.tukey_dendrite_branching)

tukey_length <- summary(glht(lme_length, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_length <- cbind(tukey_length$test$coefficients, tukey_length$test$pvalues)
write.csv (tukey.output_length, file.tukey_dendrite_length)

tukey_order <- summary(glht(lme_orders, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_order <- cbind(tukey_order$test$coefficients, tukey_order$test$pvalues)
write.csv (tukey.output_order, file.tukey_dendrite_orders)

tukey_darea <- summary(glht(lme_darea, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_darea <- cbind(tukey_darea$test$coefficients, tukey_darea$test$pvalues)
write.csv (tukey.output_darea, file.tukey_dendrite_area)

tukey_dvolume <- summary(glht(lme_dvolume, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_dvolume <- cbind(tukey_dvolume$test$coefficients, tukey_dvolume$test$pvalues)
write.csv (tukey.output_dvolume, file.tukey_dendrite_volume)

tukey_filopodia <- summary(glht(lme_filopodia, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_filopodia <- cbind(tukey_filopodia$test$coefficients, tukey_filopodia$test$pvalues)
write.csv (tukey.output_filopodia, file.tukey_filopodia)

tukey_mushroom <- summary(glht(lme_mushroom, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_mushroom <- cbind(tukey_mushroom$test$coefficients, tukey_mushroom$test$pvalues)
write.csv (tukey.output_mushroom, file.tukey_mushroom)

tukey_sarea <- summary(glht(lme_sarea, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_sarea <- cbind(tukey_sarea$test$coefficients, tukey_sarea$test$pvalues)
write.csv (tukey.output_sarea, file.tukey_spine_area)

tukey_svolume <- summary(glht(lme_svolume, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_svolume <- cbind(tukey_svolume$test$coefficients, tukey_svolume$test$pvalues)
write.csv (tukey.output_svolume, file.tukey_spine_volume)

tukey_stubby <- summary(glht(lme_stubby, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_stubby <- cbind(tukey_stubby$test$coefficients, tukey_stubby$test$pvalues)
write.csv (tukey.output_stubby, file.tukey_stubby)

tukey_thin <- summary(glht(lme_thin, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_thin <- cbind(tukey_thin$test$coefficients, tukey_thin$test$pvalues)
write.csv (tukey.output_thin, file.tukey_thin)

tukey_total <- summary(glht(lme_total, linfct=mcp(group="Tukey")), test = adjusted(type = "none"))
tukey.output_total <- cbind(tukey_total$test$coefficients, tukey_total$test$pvalues)
write.csv (tukey.output_total, file.tukey_total_spines)

# Get Confidence intervals for means
ci_branch <- groupwiseMean(dendriteBranches ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_branch, file.ci_dendrite_branching, row.names = FALSE)

ci_length <- groupwiseMean(dendriteLength ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_length, file.ci_dendrite_length, row.names = FALSE)

ci_order <- groupwiseMean(dendriteOrders ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_order, file.ci_dendrite_orders, row.names = FALSE)

ci_darea <- groupwiseMean(dendriteSurfaceArea ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_darea, file.ci_dendrite_area, row.names = FALSE)

ci_dvolume <- groupwiseMean(dendriteVolume ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_dvolume, file.ci_dendrite_volume, row.names = FALSE)

ci_filopodia <- groupwiseMean(filopodia ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_filopodia, file.ci_filopodia, row.names = FALSE)

ci_mushroom <- groupwiseMean(mushroomSpines ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_mushroom, file.ci_mushroom, row.names = FALSE)

ci_sarea <- groupwiseMean(spineArea ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_sarea, file.ci_spine_area, row.names = FALSE)

ci_stubby <- groupwiseMean(stubbySpines ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_stubby, file.ci_stubby, row.names = FALSE)

ci_svolume <- groupwiseMean(spineVolume ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_svolume, file.ci_spine_volume, row.names = FALSE)

ci_thin <- groupwiseMean(thinSpines ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_thin, file.ci_thin, row.names = FALSE)

ci_total <- groupwiseMean(totalSpines ~ group, data = data, conf = 0.95, digits = 3)
write.csv(ci_total, file.ci_total_spines, row.names = FALSE)
