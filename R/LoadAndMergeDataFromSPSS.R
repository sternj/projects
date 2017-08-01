#NOTE: writeForeignSPSS is a slight modification of the function found in the cran/foreign repo located here: https://github.com/cran/foreign
#This package is licensed under the GNU gpl-2, which can be found here: https://github.com/cran/foreign/blob/master/GPL-2
#All other code is under the unlicense
mergeData <- function(source1, source2, destination) {
  options(error=NULL)
  adQuote <- function(x) paste("\"", x, "\"", sep = "")
if(grepl("\\w:\\\\",source1) ||grepl("\\w:\\\\",source2)||grepl("\\w:\\\\",destination) )
{
  stop("You should use / rather than \\ for file paths here")
}
if(!grepl("\\w:/", source1))
{
  stop("Your first source must start with a drive name!")
}
if(!grepl("\\w:/", source2))
{
  stop("Your second source must start with a drive name!")
}
if(!grepl("\\.sav", source1))
{
  stop("Your first source must be a .sav file!")
}
if(!grepl("\\.sav", source1))
{
    stop("Your second source must be a .sav file!")
}
if(!grepl("\\w:/", destination))
{
  stop("Your destination must start with a drive letter! (i.e. C:/, H:/")
}
  if(grepl("\\.\\w", destination))
  {
    stop("Please do not end this with a file extension. I will do that for you.")
  }
  

install.packages(c("foreign","plyr","dplyr"))
print("Enabling libraries")
suppressMessages(library(foreign))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
print("Loading data files")
set1 <- read.spss(source1, to.data.frame=TRUE)
set2 <- read.spss(source2, to.data.frame = TRUE)
#Why not read.csv? Read.csv doesn't have the question labels. Granted, the rbind function
#doesn't either, but we'll ignore that and deal with it later
print("Done loading data files")
#This is a slight variation on the "writeForeignSPSS" function
writeForeignSPSS <- function(df, datafile, codefile, varnames = NULL, maxchars = 32L)
{
  ## FIXME: re-write this to hold a connection open
  dfn <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
  write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE,
              sep = ",", quote = FALSE, na = "", eol = ",\n")
  
  varlabels <- names(df)
  if (is.null(varnames)) {
    varnames <- abbreviate(names(df), maxchars)
    if (any(sapply(varnames, nchar) > maxchars))
      stop("I cannot abbreviate the variable names to 'maxchars' or fewer chars")
    if (any(varnames != varlabels))
      warning("some variable names were abbreviated")
  }
  
  varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
  
  dl.varnames <- varnames
  chv <- sapply(df, is.character)
  fav <- sapply(df, is.factor)
  if (any(chv)) {
    lengths <- sapply(df[chv],function(v) max(c(nchar(v),8), na.rm=TRUE))
    lengths <- paste0("(A", lengths, ")")
    dl.varnames[chv] <- paste(dl.varnames[chv], lengths)
  }
  if (any(fav)) {
    dl.varnames[fav] <- paste(dl.varnames[fav], "(F8.0)")  # Factor-Format
  }
  if (any(chv) || any(fav)) {
    ## actually the rule is: prepend a star if a variable with type/size declaration
    ## follows on a variable without declaration; no star for first variable or variables
    ## following other variables with declarations
    star <- ifelse(c(FALSE, diff(chv | fav) == 1)[chv | fav], " *", " ")
    dl.varnames[chv | fav] <- paste(star,  dl.varnames[chv | fav])
  }
  
  cat("SET DECIMAL=DOT.\n\n", file = codefile) # required if SPSS runs in a locale with DECIMAL=comma
  cat("DATA LIST FILE=", adQuote(datafile), " free (\",\")\n",
      file = codefile, append = TRUE)
  cat('ENCODING="Locale"\n', file = codefile, append = TRUE)
  
  ## No line longer than 251 chars:
  cat("/", paste(strwrap(paste(dl.varnames, collapse=" "), width=70), "\n"), " .\n\n",
      file = codefile, append = TRUE)
  cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
  if(!is.null(attributes(df)$variable.labels))
  {
    cat(paste(varnames, adQuote(attributes(df)$variable.labels),"\n"), ".\n",
        file = codefile, append = TRUE)
  }
  else {
    cat(paste(varnames, adQuote(varlabels),"\n"), ".\n",
        file = codefile, append = TRUE)
  }
  if (any(fav)) {
    cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
    for(v in which(fav)){
      cat("/\n", file = codefile, append = TRUE)
      cat(varnames[v]," \n", file = codefile, append = TRUE, sep = "")
      levs <- levels(df[[v]])
      cat(paste(seq_along(levs), adQuote(levs), "\n", sep = " "),
          file = codefile, append = TRUE)
    }
    cat(".\n", file = codefile, append = TRUE)
  }
  
  ord <- sapply(df, is.ordered)
  if(any(ord))
    cat("VARIABLE LEVEL",
        paste(strwrap(paste(varnames[ord], collapse = ", "), width=70), "\n"),
        "(ordinal).\n", file = codefile, append = TRUE)
  
  num <- sapply(df, is.numeric)
  if(any(num))
    cat("VARIABLE LEVEL",
        paste(strwrap(paste(varnames[num], collapse = ", "), width=70), "\n"),
        "(scale).\n", file = codefile, append = TRUE)
  
  cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}

#From here, repeat as needed
print("Sanitizing whitespace")
#These next two lines take whitespace in the data
#and replace it with an empty string (""). This makes laterally combining
#columns easier.
set1 %>% mutate_if(is.factor, as.character) -> set1_1
set2 %>% mutate_if(is.factor, as.character) -> set2_1
set1_1 <- data.frame(lapply(set1_1, function (x) if (class(x) == "character") {gsub("^\\s+|\\s+$", "", x)} else {x}),stringsAsFactors = FALSE)
set2_1 <- data.frame(lapply(set2_1, function (x) if (class(x) == "character") {gsub("^\\s+|\\s+$", "", x)} else {x}),stringsAsFactors = FALSE)
#The reason why this creates new data.frames is that it is designed to preserve variable
#labels, one of the prime advantages of SAV files. These labels are not present in the 
#sanitized version, as the lapply creates a matrix, so the labels aren't there


attributes(set1_1)$variable.labels <- attributes(set1)$variable.labels
attributes(set2_1)$variable.labels <- attributes(set2)$variable.labels
print("Merging data")
#rbind.fill 
mergedSet <- rbind.fill(set1, set2)
mergedSet2 <- mergedSet
#Might want to replace NA with "" here

#This is a weird line, so here's what it does:
#I want to delete NAs easily, so I am simply changing all factors
#in this data.frame into character vectors, then subsetting them.
mergedSet %>% mutate_if(is.factor, as.character) -> mergedSet

mergedSet[is.na(mergedSet) && is.character(mergedSet)] <- ""


#This recoerces all of the character vectors to Factors
mergedSet <- data.frame(mergedSet,stringsAsFactors = FALSE)

# These data sets have variable labels. These may match even when the variable names
# do not. This merges the columns when variable names do not match but variable labels
# do.
set1Cols <- colnames(set1)
set2Cols <- colnames(set2)
print("Merging identical columns (this will take a while)")
for(i in set1Cols) {
  for(j in set2Cols){
    if((!is.na(attributes(set1)$variable.labels[i]) &&  !is.na(attributes(set2)$variable.labels[j])) || (tolower(i) == tolower(j))) {
      if((attributes(set1)$variable.labels[i] == attributes(set2)$variable.labels[j]) || (tolower(i) == tolower(j))) {
        if(i != j) {
         # print(j %in% colnames(mergedSet))
          mergedSet[[i]] <- paste(mergedSet[[i]],mergedSet[[j]])
          mergedSet[[i]] <- gsub("NA","",mergedSet[[i]])
          mergedSet[[j]] <- NULL
          set1Cols <- set1Cols[set1Cols != i]
          set1Cols <- set1Cols[set1Cols != j]
          set2Cols <- set2Cols[set2Cols != j]
          set2Cols <- set2Cols[set2Cols != i]
         
        }
      }
    } 
    }}

#Really simple thing here, it just puts column names from the original sets
#on to the merged set

#mergedSet <- data.frame(lapply(mergedSet, function(x) replace(x, grep("[ \t\n\r]*$",x, value=FALSE), "")))
#mergedSet <- data.frame(lapply(mergedSet, function(x) replace(x, grep("^[ \t\n\r]*",x, value=FALSE), "")))
mergedSet <- data.frame(lapply(mergedSet, function (x) if (class(x) == "character") {gsub("^\\s+|\\s+$", "", x)} else {x}),stringsAsFactors = FALSE)
print("Preparing data for export")
for(i in colnames(mergedSet))
{
  if(i %in% colnames(set1))
  {
    if(i %in% colnames(set2)){
      if(is.character(mergedSet[[i]])) {
       if(!is.null(levels(set1[[i]])) && !is.null(levels(set2[[i]])) && (levels(set1[[i]]) == levels(set2[[i]])) )
       {
         mergedSet[[i]] <- factor(mergedSet[[i]], levels = gsub("^\\s+|\\s+$", "", levels(set1[[i]])))
       }
       else
       {
         mergedSet[[i]] <- factor(mergedSet[[i]], levels = gsub("^\\s+|\\s+$", "", union(levels(set1[[i]]),levels(set2[[i]]))))
       }
      }
    }
    
  }
  else if(i %in% colnames(set2))
  {
    if(is.character(mergedSet[[i]])) {
      mergedSet[[i]] <- factor(mergedSet[[i]], levels = gsub("^\\s+|\\s+$", "", levels(set2[[i]])) )
    }
  }
}

mergedSet <- mergedSet%>% mutate_if(is.character,as.factor)
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
print(attributes(mergedSet)$variable.labels["S1"])
print("Exporting data")
writeForeignSPSS(mergedSet,paste(destination,".txt",sep=""), paste(destination,".sps",sep=""),maxchars = 64L)
print(paste("You may find your syntax file at ",paste(destination,".sps",sep=""),sep=""))
print("done") }

cat("function mergeData(source1, source2, destination)
    use: to take two .sav files and return a .sps syntax file.
    NOTE: surround ALL arguments with double quotes (\"\")
    arguments:
      source1 and source2: files ending with .sav
      NOTE: you MUST include the ENTIRE path to the file, including the .sav extension. 
            This includes the drive letter (C:/, H:/, etc). You also must use the 
            forward slash (/) rather than the backslash (\\) to address the files.

      destination: a path to the file you want to create
      NOTE: you must NOT use the file extension here. This creates two files, the 
            extensions are appended while the program is running.    
    
    Example:
    mergeData(\"C:/documents/data1.sav\",\"C:/documents/data2.sav\",\"C:/documents/outputFile\")")
