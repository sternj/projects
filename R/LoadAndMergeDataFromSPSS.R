
library(foreign)
set1 <- read.spss("path/to/file1.sav", to.data.frame=TRUE)
set2 <- read.spss("path/to/file2.sav", to.data.frame = TRUE)
#Why not read.csv? Read.csv doesn't have the question labels. Granted, the rbind function
#doesn't either, but we'll ignore that and deal with it later

#From here, repeat as needed

#These next two lines take whitespace in the data
#and replace it with an empty string (""). This makes laterally combining
#columns easier.
set1 %>% mutate_if(is.factor, as.character) -> set1_1
set2 %>% mutate_if(is.factor, as.character) -> set2_1
set1_1 <- data.frame(lapply(set1_1, function(x) replace(x, grep("^[ \t\n\r]*$",x, value=FALSE), "")))
set2_1 <- data.frame(lapply(set2_1, function(x) replace(x, grep("^[ \t\n\r]*$",x, value=FALSE), "")))
#The reason why this creates new data.frames is that it is designed to preserve variable
#labels, one of the prime advantages of SAV files. These labels are not present in the 
#sanitized version, as the lapply creates a matrix, so the labels aren't there


attributes(set1_1)$variable.labels <- attributes(set1)$variable.labels
attributes(set2_1)$variable.labels <- attributes(set2)$variable.labels
library(plyr)
#rbind.fill 
mergedSet <- rbind.fill(set1, set2)
mergedSet2 <- mergedSet
#Might want to replace NA with "" here
library(dplyr)
#This is a weird line, so here's what it does:
#I want to delete NAs easily, so I am simply changing all factors
#in this data.frame into character vectors, then subsetting them.
mergedSet %>% mutate_if(is.factor, as.character) -> mergedSet
mergedSet[is.na(mergedSet)] <- ""
#This recoerces all of the character vectors to Factors
mergedSet <- data.frame(mergedSet)
# These data sets have variable labels. These may match even when the variable names
# do not. This merges the columns when variable names do not match but variable labels
# do.
set1Cols <- colnames(set1)
set2Cols <- colnames(set2)
for(i in set1Cols) {
  for(j in set2Cols){
    if(!is.na(attributes(set1)$variable.labels[i]) &&  !is.na(attributes(set2)$variable.labels[j])) {
      if(attributes(set1)$variable.labels[i] == attributes(set2)$variable.labels[j]) {
        if(i != j) {
          print(j %in% colnames(mergedSet))
          mergedSet[[i]] <- paste(mergedSet[[i]],mergedSet[[j]])
          mergedSet[[j]] <- NULL
          set1Cols <- set1Cols[set1Cols != i]
          set1Cols <- set1Cols[set1Cols != j]
          set2Cols <- set2Cols[set2Cols != j]
          set2Cols <- set2Cols[set2Cols != i]
          print(j %in% colnames(mergedSet))
          print(j)
        }
      }
    } }}

#Really simple thing here, it just puts column names from the original sets
#on to the merged set
for(i in colnames(set1)) {
  if(i %in% colnames(mergedSet)) {
    attributes(mergedSet)$variable.labels[i] <- attributes(set1)$variable.labels[i]
  }
}
for(i in colnames(set2)) {
  if(i %in% colnames(mergedSet) && is.null(attributes(mergedSet)$variable.labels[i])) {
    attributes(mergedSet)$variable.labels[i] <- attributes(set1)$variable.labels[i]
  }
}
print("done")
