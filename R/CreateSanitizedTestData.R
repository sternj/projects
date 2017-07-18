testSet <- data.frame(c(1,3,5),c("a","b","c"))
testSet <- data.frame(testSet,c("s","","h"))
attributes(testSet)$variable.labels["gas"] <- "three"
attributes(testSet)$variable.labels["xhs"] <- "three"
attributes(testSet)$variable.labels["hhh"] <- "notSame"
names(testSet) <- c("gas","hhh","xhs")
setL <- data.frame(c(2),c(4))
names(setL) <- c("gas","factpr")
attributes(setL)$variable.labels["gas"] <- "three"
attributes(setL)$variable.labels["factpr"] <- "nah"
setR <- data.frame(c("asd"),c("pqr"))
names(setR) <- c("non","hhh")
attributes(setR)$variable.labels["non"] <- "something"
attributes(setR)$variable.labels["hhh"] <- "three"
toRm <- colnames(setR)
for(i in colnames(setL)) { for(j in toRm) {if(!is.na(attributes(setL)$variable.labels[i]) &&  !is.na(attributes(setR)$variable.labels[j])) {
  if(attributes(setL)$variable.labels[i] == attributes(setR)$variable.labels[j]) {
    if(i != j) {
      print(testSet)
      
      l <- paste(testSet[[i]],testSet[[j]])
      print(l)
      testSet[[i]] <- paste(testSet[[i]],testSet[[j]],sep="")
     
      
      print(j %in% colnames(testSet))
      testSet[[j]] <- NULL
      View(testSet)
      print(colnames(testSet))
      }
  }
} }}