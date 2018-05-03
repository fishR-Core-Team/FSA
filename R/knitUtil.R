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
#' @note In \code{reproInfo}, \code{elapsed} can be used to print the time it took to process the document by sending the elapsed time for processing to this argument. The simplest way to get an approximate elapsed time is to put  \code{st <- proc.time()} very early (first line?) in your knitr code, put \code{et <- proc.time()-st} very late in your knitr code (i.e., just prior to \code{reproInfo}), and then used \code{elapsed=et["user.self"]+et["sys.self"]} in \code{reproInfo}.
#' 
#' @param value A single numeric count or p-value.
#' @param capitalize A logical that indicates if the returned words should be capitalized or not (the default).
#' @param digits Number of decimal places to round the values to.
#' @param include.p A logical that indicates whether the result should be a character string with \dQuote{p=} appended to the numerical result.
#' @param latex A logical that indicates whether the resultant p-value string should be contained within dollar signs to form a latex formula.
#' @param file A string that contains the root name of the .RNW file. This will also be the name of the resultant purled file with .R appended.
#' @param out.dir A string that indicates the directory structure in which the purled file should be located. This should not have a forward slash at the end.
#' @param newname A string for the output filename (without the extension) from \code{purl2}.
#' @param moreItems A string that contains additional words that when found in the purled file will result in the entire line with those words to be deleted.
#' @param blanks A string that indicates if blank lines should be removed. If \code{blanks="all"} then all blank lines will be removed. If \code{blanks="extra"} then only \dQuote{extra} blanks lines will be removed (i.e., one blank line will be left where there was originally more than one blank line).
#' @param topnotes A character vector of lines to be added to the top of the output file. Each value in the vector will be placed on a single line at the top of the output file.
#' @param delHeader A single character that denotes the top and bottom of a block of lines that should be deleted from the script created by \code{purl2}.
#' @param timestamp A logical that indicates whether a timestamp comment should be appended to the bottom of the script created by \code{purl2}.
#' @param out A string that indicates the type of output from \code{reproInfo} -- Markdown, LaTeX, or simple R code.
#' @param rqrdPkgs A string vector that contains packages that are required for the vignette and for which all dependencies should be found.
#' @param elapsed A numeric, usually from \code{proc.time}, that is the time required to run the vignette. If \code{NULL} then this output will not be used. See the note below.
#' @param width A numeric that indicates the width to use for wrapping the reproducibility information when \code{out="r"}.
#' @param addTOC A logical that indicates whether or not a table of contents entry for the reproducibility section should be added to the LaTeX output. Used only if \R{out="latex"}
#' @param newpage A logical that indicates whether or not the reproducibility information should begin on a new page. Used only if \R{out="latex"}
#' @param links A named character vector that will add a links bullet to the reproducibility information. The names will be shown and the values are the links. Used only if \R{out="markdown}.
#' @param closeGraphics A logical that indicates whether the graphics device should be closed or not.
#' @param ind An integer that indicates the CRAN mirror to use. Defaults to 1.
#' @param \dots Additional arguments for the original \code{purl}.
#'
#' @return
#'  \itemize{
#'    \item \code{kCounts} returns a numeric value if the count is less than zero or greater than ten and returns a character string of the number \sQuote{name}. See the examples.
#'    \item \code{kPvalue} returns a character string of the supplied p-value rounded to the requested number of digits or a character string that indicates what the p-value is less than the value with a \sQuote{5} in the \code{digits}+1 place. See the examples. 
#'    \item \code{purl2} is a modification of \code{purl} from \pkg{knitr} that creates a file with the same name as \code{file} but with lines removed that contain certain words (those found in \code{ItemsToRemove} and \code{moreItems}). 
#'    \item \code{reproInfo} returns Markdown, LaTeX, or R code that prints \dQuote{reproducibility information} at the bottom of the knitted document.
#'  }
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link{formatC}} for functionality similar to \code{kPvalue}. See \code{purl} and \code{\link[knitr]{knit}} in \pkg{knitr} for functionality similar to \code{purl2}.
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
  if (value <= 10 & value >= 0) {
    numwords <- c("zero","one","two","three","four","five",
                  "six","seven","eight","nine","ten")
    value <- numwords[value+1]
    if (capitalize) value <- capFirst(value)
  }
  value
}

#' @rdname knitUtil
#' @export
kPvalue <- function(value,digits=4,include.p=TRUE,latex=TRUE) {
  if(round(value,digits) == 0) {
    res <- paste0("<",formatC(value,format="f",digits=digits),"5")
    if(include.p) res <- paste0("p",res)
  } else if (value>1) {
    res <- ">1"
    if(include.p) res <- paste0("p",res)
  } else {
    res <- formatC(value,format="f",digits=digits)
    if (include.p) res <- paste0("p=",res)
  }
  if (latex) paste0("$",res,"$")
    else res
}

#' @rdname knitUtil
#' @export
purl2 <- function(file,out.dir=NULL,newname=NULL,topnotes=NULL,
                  moreItems=NULL,blanks=c("extra","all","none"),
                  delHeader=NULL,timestamp=TRUE,...) { # nocov start
  if (!requireNamespace("knitr")) STOP("'purl2' requires the 'knitr' package to be installed.")
  else {
    ## Some checks
    blanks <- match.arg(blanks)
    ## if no file is sent then stop
    if (missing(file)) STOP("Must given filename with extenstion.")
    ## Get input directory (from filename) and potentially change the output directory
    in.dir <- file.path(dirname(file))
    if (is.null(out.dir)) out.dir <- in.dir
    ## Get just the filename prefix (i.e., remove directory and .Rnw or .Rmd if it exists)
    fn.pre <- unlist(strsplit(basename(file),"\\.Rnw|\\.rnw|\\.RNW|\\.rmd|\\.Rmd|\\.RMD"))
    ## Make intermediate file for the tangled result (in in.dir)
    fn.Ri <- iMakeFilename(fn.pre,".R",in.dir)
    ## Make output file (in out.dir)
    if (is.null(newname)) fn.Ro <- iMakeFilename(fn.pre,".R",out.dir)
    else fn.Ro <- iMakeFilename(newname,".R",out.dir)
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
    for (i in seq_along(itemsToRemove)) {                            
      RowsToRemove <- grep(itemsToRemove[i],flines)
      if (length(RowsToRemove)>0) flines <- flines[-RowsToRemove]
    }
    ## Delete blocks if directed
    if (!is.null(delHeader)) {
      delLines <- grep(delHeader,flines)
      ndel <- length(delLines)
      if (ndel>0) {
        if (is.odd(ndel))WARN("Odd number of lines contain 'delHeader`, no blocks deleted")
        else {
          delLines1 <- delLines[is.odd(seq_len(ndel))]
          delLines2 <- delLines[is.even(seq_len(ndel))]
          delLines <- NULL
          for (i in seq_len(ndel/2)) delLines <- c(delLines,seq(delLines1[i],delLines2[i],1))
          flines <- flines[-delLines]
        }
      }
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
}  # nocov end

#' @rdname knitUtil
#' @export
reproInfo <- function(out=c("r","markdown","latex"),rqrdPkgs=NULL,elapsed=NULL,
                      width=0.95*getOption("width"),
                      addTOC=TRUE,newpage=FALSE,links=NULL,
                      closeGraphics=TRUE,ind=1) { # nocov start
  ## Set a standard mirror (helps avoid a common error when knitting)
  utils::chooseCRANmirror(ind=ind)
  ## Process the session info
  ses <- iProcessSessionInfo()
  ## Handle the rqrdPkgs
  if (is.null(rqrdPkgs)) rqrdPkgs <- "None given.\n"
  else {
    deps <- iGetAllDependencies(rqrdPkgs)
    deps <- deps[!(deps %in% rqrdPkgs)]
    rqrdPkgs <- paste0(paste(rqrdPkgs,collapse=", ")," and ",
                       ifelse(length(rqrdPkgs)>1,"their","its"),
                       " dependencies (",paste(deps,collapse=", "),")\n")
  }
  ## Get date and time
  compDate <- format(Sys.time(),'%a %b %d %Y') 
  compTime <- format(Sys.time(),'%X')
  if (!is.null(elapsed)) elapsed <- round(elapsed,2)
  ## Prepare output depending on type of out
  out <- tolower(match.arg(out))
  if (out=="r") iReproInfoR(rqrdPkgs,ses,elapsed,compDate,compTime,width)
  else if (out=="latex") iReproInfoLaTeX(rqrdPkgs,ses,elapsed,compDate,
                                         compTime,addTOC,newpage)
  else iReproInfoMarkdown(rqrdPkgs,ses,elapsed,compDate,compTime,links)
  if (closeGraphics) grDevices::graphics.off()
} # nocov end


##################################################################
## Internal files used in knitUtils functions
##################################################################
iMakeItemsToRemove <- function(moreItems) {
  mainItems <- c("Stangle","SweaveHooks","purl2","reproInfo","#line",
                 "## ","graphics.off()","purl")
  c(mainItems,moreItems)
}

iMakeFilename <- function(file,extension,directory=NULL) {
  res <- paste0(file,extension)
  if (!is.null(directory)) res <- paste(directory,res,sep="/")
  res
}

iGetAllDependencies <- function(pkgs) { # nocov start
  ## Check if repository has been set
  if ("@CRAN@" %in% options("repos")$repos && interactive()) {
    cat(gettext("--- Please select a CRAN mirror for this session ---"),"\n",sep="")
    utils::flush.console()
    utils::chooseCRANmirror()
  }
  ## Get a list of packages that pkgs depends on
  deps <- tools::package_dependencies(pkgs,utils::available.packages(),
                                      which=c("Depends","Imports"))
  ## remove list, sort, and remove names attribute
  deps <- sort(unlist(deps))
  attr(deps,"names") <- NULL
  # return original list and dependents
  unique(c(pkgs,deps))
} # nocov end

iProcessSessionInfo <- function() { # nocov start
  mkLabel <- function(L, n) { 
    # this is from print.sessionInfo in utils package
    vers <- sapply(L[[n]], function(x) x[["Version"]])
    pkg <- sapply(L[[n]], function(x) x[["Package"]])
    paste(pkg, vers, sep = "_")
  } # end internal mkLabel
  ses <- utils::sessionInfo()
  sys <- paste0(Sys.info()["sysname"],", ",ses$platform,"\n") 
  vers <- paste0(ses$R.version$version.string,"\n")
  bpkgs <- sort(ses$basePkgs) 
  bpkgsP <- paste0(paste(sort(ses$basePkgs),collapse=", "),"\n")
  opkgs <- names(ses$otherPkgs)
  opkgsP <- paste0(paste(sort(mkLabel(ses,"otherPkgs")),collapse=", "),"\n")
  lpkgs <- names(ses$loadedOnly)
  lpkgsP <- paste0(paste(sort(mkLabel(ses,"loadedOnly")),collapse=", "),"\n")
  list(sys=sys,vers=vers,bpkgs=bpkgs,bpkgsP=bpkgsP,opkgs=opkgs,opkgsP=opkgsP,lpkgs=lpkgs,lpkgsP=lpkgsP)
} # nocov end


iReproInfoLaTeX <- function(rqrdPkgs,ses,elapsed,compDate,compTime,
                            addTOC,newPage) { # nocov start
  outp <- character()
  if (newPage) {
    if (addTOC) outp <- "\\cleardoublepage\n\\phantomsection\n"
    else outp <- "\\newpage\n"
  }
  outp <- paste0(outp,"\\section*{Reproducibility Information}\n")
  if (addTOC) outp <- paste(outp,"\\addcontentsline{toc}{section}{Reproducibility Information}\n")
  cat(outp)
  outp <- paste0("\\begin{Itemize}\n")
  outp <- paste0(outp,"  \\item \\textbf{Compiled Date:} ",compDate,"\n")
  outp <- paste0(outp,"  \\item \\textbf{Compiled Time:} ",compTime,"\n")
  if (!is.null(elapsed)) outp <- paste0(outp,"  \\item \\textbf{Code Execution Time:} ",elapsed," s\n")
  outp <- paste0(outp,"  \\item \\textbf{R Version:} ",ses$vers)
  outp <- paste0(outp,"  \\item \\textbf{System:} ",gsub("[_]","\\\\_",ses$sys))
  outp <- paste0(outp,"  \\item \\textbf{Base Packages:} ",gsub("[_]","\\\\_",ses$bpkgsP))
  outp <- paste0(outp,"  \\item \\textbf{Required Packages:} ",rqrdPkgs)
  outp <- paste0(outp,"  \\item \\textbf{Other Packages:} ",gsub("[_]","\\\\_",ses$opkgsP))
  outp <- paste0(outp,"  \\item \\textbf{Loaded-Only Packages:} ",gsub("[_]","\\\\_",ses$lpkgsP))
  outp <- paste0(outp,"\\end{Itemize}\n\n")
  cat(outp)
} # nocov end

iReproInfoR <- function(rqrdPkgs,ses,elapsed,compDate,compTime,width) { # nocov start
  outp <- cat("Reproducibility Information\n")
  outp <- paste0(outp,"  Compiled Date: ",compDate,"\n")
  outp <- paste0(outp,"  Compiled Time: ",compTime,"\n")
  if (!is.null(elapsed)) outp <- paste0(outp,"  Code Execution Time: ",elapsed," s\n")
  outp <- paste0(outp,"\n")  
  cat(outp)
  outp <- paste0("  R Version: ",ses$vers)
  outp <- paste0(outp,"  System: ",ses$sys)
  cat(outp)
  outp <- paste0("Base Packages: ",ses$bpkgsP)
  cat(unlist(strwrap(outp,indent=2,exdent=4,simplify=FALSE,width=width)),sep="\n")
  outp <- paste0("Required Packages: ",rqrdPkgs)
  cat(unlist(strwrap(outp,indent=2,exdent=4,simplify=FALSE,width=width)),sep="\n")
  outp <- paste0("Other Packages: ",ses$opkgsP)
  cat(unlist(strwrap(outp,indent=2,exdent=4,simplify=FALSE,width=width)),sep="\n")
  outp <- paste0("Loaded-Only Packages: ",ses$lpkgsP)
  cat(unlist(strwrap(outp,indent=2,exdent=4,simplify=FALSE,width=width)),sep="\n")
} # nocov end

iReproInfoMarkdown <- function(rqrdPkgs,ses,elapsed,compDate,
                               compTime,links) { # nocov start
  cat("## Reproducibility Information\n\n")
  outp <- paste0("* **Compiled Date:** ",compDate,"\n")
  outp <- paste0(outp,"* **Compiled Time:** ",compTime,"\n")
  if (!is.null(elapsed)) outp <- paste0(outp,"* **Code Execution Time:** ",elapsed," s\n")
  oupt <- paste0(outp,"\n")
  outp <- paste0(outp,"* **R Version:** ",ses$vers)
  outp <- paste0(outp,"* **System:** ",gsub("[_]","\\\\_",ses$sys))
  outp <- paste0(outp,"* **Base Packages:** ",gsub("[_]","\\\\_",ses$bpkgsP))
  outp <- paste0(outp,"* **Required Packages:** ",rqrdPkgs)
  outp <- paste0(outp,"* **Other Packages:** ",gsub("[_]","\\\\_",ses$opkgsP))
  outp <- paste0(outp,"* **Loaded-Only Packages:** ",gsub("[_]","\\\\_",ses$lpkgsP))
  if (!is.null(links)) {
    outp <- paste0(outp,"* **Links:** ")
    outp <- paste0(outp,paste(paste0("[",names(links),"](",links,")"),collapse=" / "),"\n")
  }
  cat(outp)
} # nocov end
