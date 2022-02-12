################################################################################
## Use this to convert comma separated values files (*.csv) in the data-raw
##  directory of the FSA package to R data files (.rdata) in the data directory
##  of the FSA package. Note, there should be no .txt files in data-raw ... run
##  aaaConvertTab2CSV.R if there is.
################################################################################

## Get all .csv files in the data-raw directory (assumes wd is project directory)
raw <- list.files(path="./data-raw",pattern="*.csv")
## If you only want to use a few files then do this (e.g.)
#raw <- raw[c(10,16)]
raw

#### Convert all in raw to .rdata files
for (f in raw) {
  oldf <- paste0("data-raw/",f)
  nm <- tools::file_path_sans_ext(f)
  newf <- paste0("data/",nm,".rdata")
  cat(oldf,"---->",newf,"\n")
  assign(nm,read.csv(oldf))    ## Read file
  save(list=nm,file=newf)      ## Save as an .rdata file
}
