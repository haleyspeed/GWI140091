
library(dplyr)

file.name <- "C:\\Users\\haley\\Dropbox\\Sync Data Analysis Computers\\Gulf War Project\\Statistics\\R Scripts\\mEPSC\\mEPSC with Cumulative.csv"
#file.name <- readline("Enter the complete file path:")
file.data <- read.csv(file.name, header=TRUE)

row <- 1
mice <- c()
while (row <= length(file.data$cell)){
        cell <- as.character(file.data$cell[row])
        mouse <- substring(cell, 1, 2)
        mice <- c(mice, mouse) 
        row <- row + 1
}

temp.data <- cbind(mouse = mice, file.data)

summary <- function (temp.data){
       
        mouse.list <- unique(temp.data$mouse)
        out.data <- temp.data[1,]
        out.data <- out.data[,-2]
        rep <- 1
        
        for (n in mouse.list){
                filtered <- filter(temp.data, mouse == n)
                group <- filtered[1,3]
                groupName <- filtered[1,4]
                summary <- aggregate(filtered, by = list(filtered$mouse), FUN = mean, na.rm = TRUE)
                summary <- summary[,-(1:5)]
                row <- cbind(n, group, groupName, summary)
                out.data[rep,] <- row
                rep <- rep + 1
        }
        return(out.data)
}
out.data <- summary (temp.data)

file.outName <- basename(file.name)
file.outName <- paste(strsplit(file.outName, ".csv"), "_per_mouse", sep = "")
file.outName <- paste(file.outName, ".csv", sep = "")
file.outName <- paste(dirname(file.name), file.outName, sep ="\\")

write.csv(out.data, file.outName, col.names = TRUE, row.names = FALSE)