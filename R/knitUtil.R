#' @title Specific utilities for use in a knitr document.
#'
#' @description Specific utilities for pretty printing various items in a knitr document.
#'
#' @details
#'  \itemize{
#'    \item \code{kCounts} is used to convert numeric numbers to \sQuote{word} numbers in a sentence.
#'    \item \code{kPvalue} is used to print \sQuote{pretty} p-values. 
#'    \item \code{purl2} is used to create a modified (see below) Stangled or purled script.
#'    \item \code{reproInfo} is used to print \sQuote{reproducibility information} for the document.
#'  }
#'
#' @note In \code{reproInfo}, \code{elapsed} can be used to print the time it took to process the document by sending the elasped time for processing to this argument.  The simplest way to get an approximate elapsed time is to put  \code{st <- proc.time()} very early (first line?) in your knitr code, put \code{et <- proc.time()-st} very late in your knitr code (i.e., just prior to \code{reproInfo}), and then used \code{elapsed=et["user.self"]+et["sys.self"]} in \code{reproInfo}.
#' 
#' @param value A single numeric count or p-value.
#' @param capitalize A logical that indicates if the returned words should be capitalized or not (the default).
#' @param digits Number of decimal places to round the values to.
#' @param include.p A logical that indicates whether the result should be a character string with \dQuote{p=} appended to the numerical result.
#' @param latex A logical that indicates whether the resultant p-value string should be contained within dollar signs to form a latex formula.
#' @param file A string that contains the root name of the .RNW file.  This will also be the name of the resultant Stangled file with .R appended, the resultant PDF file with .PDF appended, the resultant asciidoc file with .txt appended, and the defaul HTML file with .html appended.  If missing then will search for a .Rnw file in the current working directory.
#' @param out.dir A string that indicates the directory structure in which the purled file should be located.  This should not have a forward slash at the end.
#' @param moreItems A string that contains additional words that when found in the Stangled file will result in the entire line with those words to be deleted.
#' @param blanks A string that indicates if blank lines should be removed.  If \code{blanks="all"} then all blank lines will be removed.  If \code{blanks="extra"} then only \dQuote{extra} blanks lines will be removed (i.e., one blank line will be left where there was originally more than one blank line).
#' @param topnotes A character vector of lines to be added to the top of output file.  Each value in the vector will be placed on a single line at the top of the output file.
#' @param timestamp A logical that indicates whether a timestampe comment should be appended to the bottom of the script created by \code{purl2}.
#' @param rqrdPkgs A string vector that contains packages that are required for the vignette and for which all dependencies should be found.
#' @param closeGraphics A logical that indicates whether the graphics device should be closed or not.
#' @param addTOC A logical that indicates whether or not a table of contents entry for the reproducibity section should be added to the LaTeX output.
#' @param newPage A logical that indicates whether a new page call should be added to the LaTeX output so that the reproducibility sections starts on a new page.
#' @param elapsed A numeric, usually from \code{proc.time}, that is the time required to run the vignette.  If \code{NULL} then this output will not be used.  See the note below.
#' @param listFiles A logical that indicates whether the Sweave/knitr markup and R code files should be listed in the reproducibility information list.
#' @param \dots Additional arguments for the original \code{purl}.
#'
#' @return
#'  \itemize{
#'    \item \code{kCounts} returns a numeric value if the count is less than zero or greater than ten and returns a character string of the number \sQuote{name}.  See the examples.
#'    \item \code{kPvalue} returns a character string of the supplied p-value rounded to the requested number of digits or a character string that indicates what the p-value is less than the value with a \sQuote{5} in the \code{digits}+1 place.  See the examples. 
#'    \item \code{purl2} is a modification of \code{\link[knitr]{purl}} from \pkg{knitr} that creates a file with the same name as \code{file} but with lines removed that contain certain words (those found in \code{ItemsToRemove} and \code{moreItems}). 
#'    \item \code{reproInfo} returns LaTeX code that prints \dQuote{reproducibility information} at the bottom of the Sweaved/knitted document.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link{formatC}} for functionality similar to \code{kPvalue}.  See \code{\link[knitr]{purl}} and \code{\link[knitr]{knit}} in \pkg{knitr} for functionality similar to \code{purl2}.
#'
#' @keywords manip
#'
#' @aliases kCounts kPvalue purl2 reproInfo
#'
#' @examples
#' kCounts(7)
#' kCounts(17)
#' kCounts(0)
#' kCounts(-6)
#' kCounts(3,capitalize=TRUE)
#'
#' kPvalue(0.123456789)
#' kPvalue(0.000123456)
#' kPvalue(0.000012345)
#' kPvalue(0.000012345,include.p=FALSE)
#' kPvalue(0.000012345,include.p=FALSE,latex=FALSE)
#'
#' @rdname knitUtil
#' @export
kCounts <- function(value,capitalize=FALSE) {    
  numwords <- c("one","two","three","four","five","six","seven","eight","nine","ten")
  if (value == 0) "zero"
    else if (value <= 10 & value >= 1) {
      value <- numwords[value]
      if (capitalize) capFirst(value)
        else value
    } else value
}

#' @rdname knitUtil
#' @export
kPvalue <- function(value,digits=4,include.p=TRUE,latex=TRUE) {
  if(round(value,digits) == 0) {
    res <- paste("<",formatC(value,format="f",digits=digits),"5",sep="")
    if(include.p) res <- paste("p",res,sep="")
  } else if (value>1) {
    res <- ">1"
    if(include.p) res <- paste("p",res,sep="")
  } else {
    res <- formatC(value,format="f",digits=digits)
    if (include.p) res <- paste("p=",res,sep="")
  }
  if (latex) paste("$",res,"$",sep="")
    else res
}

#' @rdname knitUtil
#' @export
purl2 <- function(file,out.dir=NULL,topnotes=NULL,
                  moreItems=NULL,blanks=c("extra","all","none"),
                  timestamp=TRUE,...) {
  ## Some checks
  blanks <- match.arg(blanks)
  ## if no file is sent then stop
  if (missing(file)) stop("Must given filename with extenstion.",call.=FALSE)
  ## Get input directory (from filename) and potentially change the output directory
  in.dir <- file.path(dirname(file))
  if (is.null(out.dir)) out.dir <- in.dir
  ## Get just the filename prefix (i.e., remove directory and .Rnw or .Rmd if it exists)
  fn.pre <- unlist(strsplit(basename(file),"\\.Rnw|\\.rnw|\\.RNW|\\.rmd|\\.Rmd|\\.RMD"))
  ## Make intermediate file for the tangled result (in in.dir)
  fn.Ri <- iMakeFilename(fn.pre,".R",in.dir)
  ## Make output file (in out.dir)
  fn.Ro <- iMakeFilename(fn.pre,".R",out.dir)
  ## Remove already existing .R files with the same name
  # Delete the original tangled file
  unlink(fn.Ri)
  unlink(fn.Ro)
  ## Purl the results and then read those results back into flines
  # handle documentation (i.e., pure code)
  knitr::purl(file,fn.Ri,documentation=1)
  flines <- readLines(fn.Ri)
  ## Expand itemsToRemove if something in moreItems
  itemsToRemove <- iMakeItemsToRemove(moreItems)
  ## Find the rows with the items to remove and then remove those items if they exist
  for (i in 1:length(itemsToRemove)) {                            
    RowsToRemove <- grep(itemsToRemove[i],flines)
    if (length(RowsToRemove)>0) flines <- flines[-RowsToRemove]
  }
  ## Remove the blank lines as directed in blanks argument
  if (blanks=="all") {
    flines <- flines[-which(flines=="")]
  } else {
    if (blanks=="extra") {
      blankLines <- which(flines=="")
      extras <- which(c(2,diff(blankLines))==1)
      if (length(extras)>0) flines <- flines[-blankLines[extras]]
    }
  }
  ## Add topnotes if any given
  if (!is.null(topnotes)) flines <- c(paste("#",topnotes),flines)
  ## Delete first line if it is blank (often the case after removeItems)
  if (flines[1]=="") flines <- flines[-1]
  ## Add timestampe if asked for
  if (timestamp) flines <- c(flines,"",paste("# Script created at",Sys.time()))
  ## Delete the original purled/stangled file
  unlink(fn.Ri)
  ## Write out a new purled/stangled file
  write(flines,fn.Ro)
}

#' @rdname knitUtil
#' @export
reproInfo <- function(file,rqrdPkgs=NULL,closeGraphics=TRUE,
                      addTOC=TRUE,newPage=FALSE,elapsed=NULL,listFiles=FALSE) {
  ## if no file is sent then find the .Rnw file in the current working directory
  if (missing(file)) file <- list.files(pattern=".Rnw",ignore.case=TRUE)
  ## Get just the filename prefix (i.e., remove directory and .Rnw if it exists)
  file <- unlist(strsplit(basename(file),c("\\.Rnw","\\.rnw","\\.RNW")))
  knitFile <- iMakeFilename(file,".rnw")
  rFile <- iMakeFilename(file,".r")
  ses <- iProcessSessionInfo()
  if (is.null(rqrdPkgs)) rqrdPkgs <- "None given.\n"
    else {
      deps <- iGetAllDependencies(rqrdPkgs)
      deps <- deps[!(deps %in% rqrdPkgs)]
      rqrdPkgs <- paste(paste(rqrdPkgs,collapse=", ")," and ",ifelse(length(rqrdPkgs)>1,"their","its")," dependencies (",paste(deps,collapse=", "),")\n",sep="")
    }
  compDate <- format(Sys.time(),'%a %b %d %Y') 
  compTime <- format(Sys.time(),'%X')
  pdfFile <- iMakeFilename(file,".pdf")
  outp <- character()
  if (newPage) {
    if (addTOC) outp <- "\\cleardoublepage\n\\phantomsection\n"
    else outp <- "\\newpage\n"
  }
  outp <- paste(outp,"\\section*{Reproducibility Information}\n",sep="")
  if (addTOC) outp <- paste(outp,"\\addcontentsline{toc}{section}{Reproducibility Information}\n")
  cat(outp)
  outp <- "  \\subsection*{Version Information}\n"
  outp <- paste(outp,"    \\begin{Itemize}\n",sep="")
  outp <- paste(outp,"      \\item \\textbf{Compiled Date:} ",compDate,"\n",sep="")
  outp <- paste(outp,"      \\item \\textbf{Compiled Time:} ",compTime,"\n",sep="")
  if (!is.null(elapsed)) outp <- paste(outp,"      \\item \\textbf{Code Execution Time:} ",round(elapsed,2)," s\n",sep="")
  outp <- paste(outp,"    \\end{Itemize}\n\n",sep="")  
  cat(outp)   
  if (listFiles) {
    outp <- "  \\subsection*{Files}\n"    
    outp <- paste(outp,"    \\begin{Itemize}\n",sep="")
    outp <- paste(outp,"      \\item \\href{",knitFile,"}{NoWeb Source (.Rnw) file}\n",sep="")
    outp <- paste(outp,"      \\item \\href{",rFile,"}{R Script (.R) file}\n",sep="")
    outp <- paste(outp,"    \\end{Itemize}\n\n",sep="")  
    cat(outp)
  }
  outp <- "  \\subsection*{R Information}\n"
  outp <- paste(outp,"    \\begin{Itemize}\n",sep="")
  outp <- paste(outp,"      \\item \\textbf{R Version:} ",ses$vers,sep="")
  outp <- paste(outp,"      \\item \\textbf{System:} ",gsub("[_]","\\\\_",ses$sys),sep="")
  outp <- paste(outp,"      \\item \\textbf{Base Packages:} ",gsub("[_]","\\\\_",ses$bpkgsP),sep="")
  outp <- paste(outp,"      \\item \\textbf{Other Packages:} ",gsub("[_]","\\\\_",ses$opkgsP),sep="")
  outp <- paste(outp,"      \\item \\textbf{Loaded-Only Packages:} ",gsub("[_]","\\\\_",ses$lpkgsP),sep="")
  outp <- paste(outp,"      \\item \\textbf{Required Packages:} ",rqrdPkgs,sep="")
  outp <- paste(outp,"    \\end{Itemize}\n\n",sep="")
  cat(outp)
  if (closeGraphics) graphics.off()
}


##################################################################
## Internal files used in knitUtils functions                                      
##################################################################
iGetAllDependencies <- function(pkgs) {
  # get a list of available packages ... this is needed for package.dependencies below
  inst <- installed.packages()
  #   isolate to the supplied packages
  inst <- inst[inst[,"Package"] %in% pkgs,]
  deps <- NULL
  # a catch if there is only one package
  if (is.vector(inst)) n <- 1
  else n <- nrow(inst)
  # must cycle through provided packages
  for (i in 1:n) {
    # get dependent packages for ith provided package
    d <- tools::package.dependencies(inst,,"Depends")[[i]]
    # handles those with no dependent packages
    if (is.matrix(d)) deps <- c(deps,d[,1])
    # ditto for imported paxckages
    d <- tools::package.dependencies(inst,,"Imports")[[i]]
    if (is.matrix(d)) deps <- c(deps,d[,1])
  }
  # remove "R" if it is listed as a dependent
  deps <- deps[deps!="R"]
  # remove names attribute
  attr(deps,"names") <- NULL
  # alphabetize
  deps <- sort(deps)
  # return original list and dependents
  unique(c(pkgs,deps))
}

iMakeFilename <- function(file,extension,directory=NULL) {
  res <- paste(file,extension,sep="")
  if (!is.null(directory)) res <- paste(directory,res,sep="/")
  res
}

iMakeItemsToRemove <- function(moreItems) {
  mainItems <- c("Stangle","SweaveHooks","purl2","reproInfo","#line","## ","graphics.off()","purl")
  c(mainItems,moreItems)
}

iProcessSessionInfo <- function() {
  mkLabel <- function(L, n) { 
    # this is from print.sessionInfo in utils package
    vers <- sapply(L[[n]], function(x) x[["Version"]])
    pkg <- sapply(L[[n]], function(x) x[["Package"]])
    paste(pkg, vers, sep = "_")
  } # end internal mkLabel
  ses <- sessionInfo()
  sys <- paste(Sys.info()["sysname"],", ",ses$platform,"\n",sep="") 
  vers <- paste(ses$R.version$version.string,"\n",sep="")
  bpkgs <- sort(ses$basePkgs) 
  bpkgsP <- paste(paste(sort(ses$basePkgs),collapse=", "),"\n",sep="")
  opkgs <- names(ses$otherPkgs)
  opkgsP <- paste(paste(sort(mkLabel(ses,"otherPkgs")),collapse=", "),"\n",sep="")
  lpkgs <- names(ses$loadedOnly)
  lpkgsP <- paste(paste(sort(mkLabel(ses,"loadedOnly")),collapse=", "),"\n",sep="")
  list(sys=sys,vers=vers,bpkgs=bpkgs,bpkgsP=bpkgsP,opkgs=opkgs,opkgsP=opkgsP,lpkgs=lpkgs,lpkgsP=lpkgsP)
}

