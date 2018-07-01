
library(dplyr)

file.name <- "D://Dropbox//Sync Data Analysis Computers//Gulf War Project//Statistics//R Scripts//PPR//PPR Per Slice.csv"

#file.name <- readline("Enter the complete file path:")
file.data <- read.csv(file.name, header=TRUE)

row <- 1
mice <- c()
while (row <= length(file.data$slice)){
        slice <- as.character(file.data$slice[row])
        mouse <- substring(slice, 1, 2)
        mice <- c(mice, mouse) 
        row <- row + 1
}

temp.data <- cbind(mouse = mice, file.data)

summary <- function (temp.data){
       
        mouse.list <- unique(temp.data$mouse)
        out.data <- data.frame(mouse = character(), group = character(), interval = numeric(), ppr = numeric())
       
        rep <- 1
        
        for (n in mouse.list){
                filtered <- filter(temp.data, mouse == n)
                group <- filtered[1,3]
                groupName <- filtered[1,4]
                summary <- aggregate(filtered, by = list(filtered$mouse, filtered$interval), FUN = mean, na.rm = TRUE)

                row <- cbind(n, group, summary[2], summary[7])
                colnames(row) <- c("mouse", "group", "interval","ppr")
                out.data <- rbind(out.data, row)
                rep <- rep + 1
        }
        
        return(out.data)
}
out.data <- summary (temp.data)

#file.outName <- basename(file.name)
#file.outName <- paste(strsplit(file.outName, ".csv"), "_per_mouse", sep = "")
#file.outName <- paste(file.outName, ".csv", sep = "")
file.outName <- paste(dirname(file.name), "ppr per mouse.csv", sep ="/")

write.csv(out.data, file.outName, col.names = TRUE, row.names = FALSE)