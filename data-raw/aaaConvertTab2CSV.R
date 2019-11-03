################################################################################
## Use this to convert tab-delimited text files (*.txt) to comma separated
##  values files (*.csv) in the data-raw irectory of the FSA package.
################################################################################

library(tools)

## Get all .txt files in the data-raw directory
setwd("C:/aaaWork/Programs/GitHub/FSA/data-raw")
raw <- list.files(pattern="*.txt")

## Cycle through each file to make it a CSV file
for (f in raw) {
  print(f)
  ## Read file
  tmp <- read.table(f,header=TRUE)
  ## Write out as a CSV file
  write.csv(tmp,paste0(file_path_sans_ext(f),".csv"),quote=FALSE,row.names=FALSE)
}
