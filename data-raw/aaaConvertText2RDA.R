library(tools)
## Get all .txt files in the data-raw directory
setwd("C:/aaaWork/Programs/GitHub/FSA/data-raw")
raw <- list.files(pattern="*.txt")
## Cycle through each file to make it an RDA file
for (f in raw) {
  print(f)
  ## Read file
  tmp <- readLines(f)
  ## Remove comments at top
  cmnts <- which(grepl("#",tmp))
  if (length(cmnts)>0) tmp <- tmp[-cmnts]
  ## Write to a temporary file
  write.table(tmp,"tmp.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  ## Read that file in as a data.frame
  nm <- file_path_sans_ext(f)
  assign(nm,read.table("tmp.txt",header=TRUE))
  ## Save as an .rdata file
  save(list=nm,file=paste0("../data/",nm,".rdata"))
}

## Remove the tmp.txt temporary file
file.remove("tmp.txt")