#' @title Specific utilities for use in a Sweave/knitr document.
#'
#' @description Specific utilities for pretty printing various items in a Sweave/knitr document.
#'
#' @details
#'  \itemize{
#'    \item \code{swvCounts} is used to convert numeric numbers to \sQuote{word} numbers in a sentence.
#'    \item \code{swvPvalue} is used to print \sQuote{pretty} p-values. 
#'    \item \code{swvANOVA} is used to print \sQuote{pretty} ANOVA tables (i.e., some output removed (see below)).
#'    \item \code{swvGLHT} is used to print \sQuote{pretty} multiple comparison tables (i.e., some output removed (see below)).
#'    \item \code{swvREG} is used to print \sQuote{pretty} summary regression results (i.e., some output removed (see below)).
#'    \item \code{swvHtest} is used to print \sQuote{pretty} hypothesis test (e.g., from \code{t.test} or \code{chisq.test}) tables (i.e., some output removed (see below)).
#'    \item \code{swvCode} is used to create a modified (see below) Stangled or purled script.
#'    \item \code{swvFinish} is used to print \sQuote{reproducibility information} for the document.
#'  }
#'
#' @note In \code{swvFinish}, \code{elapsed} can be used to print the time it took to process the document by sending the elasped time for processing to this argument.  The simplest way to get an approximate elapsed time is to put  \code{st <- proc.time()} very early (first line?) in your Sweave/knitr code, put \code{et <- proc.time()-st} very late in your Sweave/knitr code (i.e., just prior to \code{swvFinish}), and then used \code{elapsed=et["user.self"]+et["sys.self"]} in \code{swvFinish}.
#'
#' @section warning:
#'I have not checked these commands with Sweave for quite sometime, but do often with knitr.  Please let me know if you have any problems when using Sweave.
#'
#' @aliases swvCounts swvPvalue swvANOVA swvGLHT swvREG swvHtest swvCode swvFinish
#' 
#' @param value A single numeric count or p-value.
#' @param capitalize A logical that indicates if the returned words should be capitalized or not (the default).
#' @param digits Number of decimal places to round the values to.
#' @param include.p A logical that indicates whether the result should be a character string with \dQuote{p=} appended to the numerical result.
#' @param latex A logical that indicates whether the resultant p-value string should be contained within dollar signs to form a latex formula.
#' @param x An object saved from \code{lm} or \code{glht} or an object of class \code{htest} (e.g., saved from \code{t.test}).
#' @param type A string that indicates the type of glht result to extract.
#' @param file A string that contains the root name of the .RNW file.  This will also be the name of the resultant Stangled file with .R appended, the resultant PDF file with .PDF appended, the resultant asciidoc file with .txt appended, and the defaul HTML file with .html appended.  If missing then will search for a .Rnw file in the current working directory.
#' @param out.dir A string that indicates the directory structure in which the purled file should be located.  This should not have a forward slash at the end.
#' @param moreItems A string that contains additional words that when found in the Stangled file will result in the entire line with those words to be deleted.
#' @param blanks A string that indicates if blank lines should be removed.  If \code{blanks="all"} then all blank lines will be removed.  If \code{blanks="extra"} then only \dQuote{extra} blanks lines will be removed (i.e., one blank line will be left where there was originally more than one blank line).
#' @param topnotes A character vector of lines to be added to the top of output file.  Each value in the vector will be placed on a single line at the top of the output file.
#' @param annotate A logical that indicates whether decorating comments around code chunks should be used (\code{TRUE}) or not (\code{FALSE}; default).
#' @param show.alt A logical that indicates whether the line stating what the alternative hypothesis is should be printed (\code{TRUE}) or not (\code{FALSE}; default).
#' @param rqrdPkgs A string vector that contains packages that are required for the vignette and for which all dependencies should be found.
#' @param closeGraphics A logical that indicates whether the graphics device should be closed or not.
#' @param addTOC A logical that indicates whether or not a table of contents entry for the reproducibity section should be added to the LaTeX output.
#' @param newPage A logical that indicates whether a new page call should be added to the LaTeX output so that the reproducibility sections starts on a new page.
#' @param elapsed A numeric, usually from \code{proc.time}, that is the time required to run the vignette.  If \code{NULL} then this output will not be used.  See the note below.
#' @param listFiles A logical that indicates whether the Sweave/knitr markup and R code files should be listed in the reproducibility information list.
#' @param \dots Additional arguments for the original \code{Stangle} or \code{purl}.
#'
#' @return
#'  \itemize{
#'    \item \code{swvCounts} returns a numeric value if the count is less than zero or greater than ten and returns a character string of the number \sQuote{name}.  See the examples.
#'    \item \code{swvPvalue} returns a character string of the supplied p-value rounded to the requested number of digits or a character string that indicates what the p-value is less than the value with a \sQuote{5} in the \code{digits}+1 place.  See the examples. 
#'    \item \code{swvANOVA} returns the results of \code{anova} but without the heading attribute. 
#'    \item \code{swvGLHT} returns a matrix of just the hypothesis test or confidence interval results from a \code{glht} object (i.e., all messages that are usually printed are stripped away). 
#'    \item \code{swvREG} returns the results of \code{summary} but without the call and the information about the residuals. 
#'    \item \code{swvHtest} returns the same as \code{stats:::print.htest} except that the name of the test, the data used, and, optionally, the descriptor of the alternative hypothesis are not printed. 
#'    \item \code{swvCode} is a modification of \code{Stangle} (if using Sweave) or \code{purl} (if using knitr) that creates a file with the same name as \code{file} but with lines removed that contain certain words (those found in \code{ItemsToRemove} and \code{moreItems}). 
#'    \item \code{swvFinish} returns LaTeX code that prints \dQuote{reproducibility information} at the bottom of the Sweaved/knitted document.
#'  }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{formatC} in base R; \code{knit} in \pkg{knitr}; and \code{glht} in \pkg{multcomp}.
#'
#' @keywords hplot models manip
#'
#' @examples
#' swvCounts(7)
#' swvCounts(17)
#' swvCounts(0)
#' swvCounts(-6)
#' swvCounts(3,capitalize=TRUE)
#'
#' swvPvalue(0.123456789)
#' swvPvalue(0.000123456)
#' swvPvalue(0.000012345)
#' swvPvalue(0.000012345,include.p=FALSE)
#' swvPvalue(0.000012345,include.p=FALSE,latex=FALSE)
#'
#' @rdname swvUtil
#' @export
swvCounts <- function(value,capitalize=FALSE) {    
  numwords <- c("one","two","three","four","five","six","seven","eight","nine","ten")
  if (value == 0) "zero"
    else if (value <= 10 & value >= 1) {
      value <- numwords[value]
      if (capitalize) capFirst(value)
        else value
    } else value
}

#' @rdname swvUtil
#' @export
swvPvalue <- function(value,digits=4,include.p=TRUE,latex=TRUE) {
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

#' @rdname swvUtil
#' @export
swvANOVA <- function(x) {
  x <- anova(x)
  attr(x,"heading") <- NULL
  x
}

#' @rdname swvUtil
#' @export
swvGLHT <- function(x,type=c("hypothesis","confidence")) {
  type <- match.arg(type)
  if (type=="hypothesis") {
    int <- summary(x)$test
    res <- cbind(int$coefficients,int$sigma,int$tstat,int$pvalues)
    colnames(res) <- c("Estimate","Std. Error","t value","p value")
    rownames(res) <- paste (rownames(res),"= 0")
  } else res <- confint(x)$confint[,1:3]
  res
}

#' @rdname swvUtil
#' @export
swvREG <- function(x,digits=max(3,getOption("digits")-3)) {
  x <- summary(x)
  rdf <- x$df[2L]
  coefs <- x$coefficients
  if (!is.null(aliased <- x$aliased) && any(aliased)) {
    cn <- names(aliased)
    coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
    coefs[!aliased, ] <- x$coefficients
  }
  printCoefmat(coefs, digits = digits, na.print = "NA")
  cat("---")
  cat("\nResidual standard error:", format(signif(x$sigma,digits)), "on", rdf, "degrees of freedom\n")
  if (nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared:", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared:", formatC(x$adj.r.squared,digits = digits), "\nF-statistic:",
        formatC(x$fstatistic[1],digits=digits), "on", x$fstatistic[2], "and", x$fstatistic[3], 
        "DF,  p-value:",base::format.pval(pf(x$fstatistic[1L],x$fstatistic[2L],x$fstatistic[3L],lower.tail=FALSE),digits=digits), "\n")
    }
  cat("\n")
}
                
#' @rdname swvUtil
#' @export
swvHtest <- function(x,digits=4,show.alt=FALSE,...) {
  # the same as stats:::print.htest without the test name and listing the data
  outp <- character()
  if (show.alt & !is.null(x$alternative)) {
    cat("alternative hypothesis: ")
    if (!is.null(x$null.value)) {
      if (length(x$null.value) == 1L) {
        alt.char <- switch(x$alternative,two.sided="not equal to",less="less than",greater="greater than")
        cat("true",names(x$null.value),"is",alt.char,x$null.value, "\n")
      }
      else {
        cat(x$alternative, "\nnull values:\n")
        print(x$null.value, ...)
      }
    }
    else cat(x$alternative, "\n")
  }
  if (!is.null(x$statistic)) outp <- c(outp,paste(names(x$statistic),"=",format(round(x$statistic,4))))
  if (!is.null(x$parameter)) outp <- c(outp,paste(names(x$parameter),"=",format(round(x$parameter,3))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value,digits=digits)
    outp <- c(outp,paste("p-value",if(substr(fp,1L,1L) == "<") fp else paste("=",fp)))
  }
  cat(strwrap(paste(outp,collapse=", ")),sep="\n")
  if (!is.null(x$conf.int)) {cat(format(100*attr(x$conf.int,"conf.level")),"percent confidence interval:\n",format(c(x$conf.int[1L],x$conf.int[2L])),"\n") }
  if (!is.null(x$estimate)) {
    cat("sample estimates:\n")
    print(x$estimate, ...)
  }
  cat("\n")
  invisible(x)
}

#' @rdname swvUtil
#' @export
swvCode <- function(file,out.dir=NULL,topnotes=NULL,
                    moreItems=NULL,blanks=c("extra","all","none"),
                    annotate=FALSE,...) {
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
  ## tangle the results and then read those results back into flines
  # handle documentation (i.e., pure code)
#  on.exit(knitr::opts_knit$set(documentation=knitr::opts_knit$get("documentation")))
#  knitr::opts_knit$set(documentation=0)
  knitr::purl(file,fn.Ri,documentation=0)
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
      flines <- flines[-blankLines[extras]]
    }
  }
  ## Add topnotes if any given
  if (!is.null(topnotes)) flines <- c(paste("#",topnotes),flines)
  ## Delete first line if it is blank (often the case after removeItems)
  if (flines[1]=="") flines <- flines[-1]
  ## Delete the original purled/stangled file
  unlink(fn.Ri)
  ## Write out a new purled/stangled file
  write(flines,fn.Ro)
}

#' @rdname swvUtil
#' @export
swvFinish <- function(file,rqrdPkgs=NULL,closeGraphics=TRUE,
                      addTOC=TRUE,newPage=FALSE,elapsed=NULL,listFiles=FALSE) {
  ## if no file is sent then find the .Rnw file in the current working directory
  if (missing(file)) file <- list.files(pattern=".Rnw",ignore.case=TRUE)
  ## Get just the filename prefix (i.e., remove directory and .Rnw if it exists)
  file <- unlist(strsplit(basename(file),c("\\.Rnw","\\.rnw","\\.RNW")))
  swvFile <- iMakeFilename(file,".rnw")
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
    outp <- paste(outp,"      \\item \\href{",swvFile,"}{NoWeb Source (.Rnw) file}\n",sep="")
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
## Internal files used in swvUtils functions                                      
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
  mainItems <- c("Stangle","SweaveHooks","swvCode","swvFinish","#line","## ","graphics.off()","purl")
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

