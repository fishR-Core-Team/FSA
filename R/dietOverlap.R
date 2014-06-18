#' @title Computes indices of diet overlap between two species.
#'
#' @description Computes various diet overlap indices between two species from summarized (i.e., not individual) diet data.
#'
#' @details NEED MORE DETAILS HERE
#'
#' @note Only the Pianka measurements have been tested against results from other softwares.
#'
#' @aliases dietOverlap print.dietOverlap summary.dietOverlap
#'
#' @param diet1 A numerical vector of \sQuote{amount} (count or biomass) of prey items for the first predator.  Items should be in the same order as amounts in \code{diet2} and categories in \code{prey}.
#' @param diet2 A numerical vector of \sQuote{amount} (count or biomass) of prey items for the second predator.  Items should be in the same order as amounts in \code{diet2} and categories in \code{prey}.
#' @param type A single string that indicates the type of diet overlap index to compute.  See details.
#' @param prey An optional string vector that contains the prey/diet category names.  If supplied the \code{prop} matrix in the returned list will use these prey categories as row names.  Items should be in the same order as items in \code{diet1} and \code{diet2}.
#' @param num1 A numerical vector of the number of individuals of the first predator that consumed each prey item.  Items should be in same order as amounts in  \code{diet1} and categories in \code{prey}.
#' @param num2 A numerical vector of the number of individuals of the second predator that consumed each prey item.  Items should be in same order as amounts in  \code{diet1} and categories in \code{prey}.
#' @param N1 A single numeric value that is the total number of the first predator sampled.
#' @param N2 A single numeric value that is the total number of the second predator sampled.
#' @param object A \code{dietOverlap} object.
#' @param x A \code{dietOverlap} object.
#' @param verbose A single logical that indicates whether more verbose summary information should be printed.
#' @param digits A single numeric that indicates the number of digits to which the results should be printed.
#' @param \dots Additional arguments for the S3 methods.  Not implemented for \code{summary}.
#'
#' @return The main function returns a list with the following three items:
#'\itemize{
#'\item type A single string that indicates the type of diet overlap indice used.
#'\item doi A single numeric with the diet overlap index value is returned for all \code{type}s except for \sQuote{Levins} where a numeric vector of length two is returned with the overlap of the first predator on the second as the first value and the overlap of the second predator on the first as the second value.
#'\item propdiet A matrix that contains the proportion of the total diet in each of the diet items for each predator.
#'}
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{piankabio} in \pkg{pgirmess} for similar functionality.
#'
#' @references
#' Horn, H.S. 1966. Measurement of overlap in comparative ecological studies. American Naturalist 100:419-424.
#'
#' Krebs, C.J.  1989.  Ecological Methodology.  Harper Collins, New York.
#'
#' Levins, R. 1968. Evolution in changing environments: Some theoretical explorations.  Princeton University Press, Princeton.
#'
#' Morisita, M. 1959. Measuring of interspecific association and similarity between communities.  Memoirs of the Faculty of Science of Kyusha University.  Series in Evolutionary Biology 3:65-80.
#'
#' Pianka R.D. 1973.  The structure of lizard communities. Annual Review of Ecology and Systematics 4:53-74.

#' Schoener, T.W. 1970. Nonsynchronous spatial overlap of lizards in patchy habitats.  Ecology 51:408-418.
#'
#' Smith, E.P. and T.M. Zaret.  1982.  Bias in estimating niche overlap.  Ecology 63:1248-1253.
#'
#' @keywords manip
#'
#' @examples
#'# Hypothetical data -- prey categories and biomasses in diets
#'names <- c("bluegill","perch","minnows","bullheads","insects","zooplankton")
#'lmb <- c(55,35,23,7,3,1)
#'wae <- c(23,45,2,17,7,2)
#'
#'# demonstrate different indices (see below for Morisita)
#'dietOverlap(lmb,wae,prey=names,type="Horn")
#'dietOverlap(lmb,wae,prey=names,type="Levin")
#'dietOverlap(lmb,wae,prey=names,type="Pianka")
#'dietOverlap(lmb,wae,prey=names,type="Schoener")
#'
#'# demonstrate summary()
#'do1 <- dietOverlap(lmb,wae,names,type="Horn")
#'summary(do1)
#'summary(do1,digits=3)
#'
#'# demonstrate using a single matrix rather than two separate vectors
#'diet <- cbind(lmb,wae)
#'rownames(diet) <- names
#'dietOverlap(diet,prey=rownames(diet),type="Horn")
#'
#'# Continuation of first example with 'extra' info required for Morisita's index
#'# Hypothetical numbers of fish with each food item (occurrence)
#'num.lmb <- c(17,13,4,7,24,13)
#'num.wae <- c(37,12,23,8,9,13)
#'# Hypothetical total numbers of each predator
#'N.lmb <- 30
#'N.wae <- 40
#'
#'dietOverlap(lmb,wae,prey=names,type="Morisita",num1=num.lmb,num2=num.wae,N1=N.lmb,N2=N.wae)
#'
#'## An extended example which requires summarization of raw data
#'data(TroutDietSL)
#'# add percentage per fish, add zeroes for empty stomachs
#'TroutDietSL$pvol.mysis <- TroutDietSL$vol.mysis/TroutDietSL$vol.ttl
#'TroutDietSL$pvol.oi <- TroutDietSL$vol.oi/TroutDietSL$vol.ttl
#'TroutDietSL$pvol.fish <- TroutDietSL$vol.fish/TroutDietSL$vol.ttl
#'# add empty variable
#'TroutDietSL$empty <- ifelse(TroutDietSL$vol.ttl==0,"YES","NO")
#'# create TL from SL for Bull Trout using formula in paper -- TL=4.403+1.118SL
#'TroutDietSL$tl[TroutDietSL$species=="BLT"] <- 4.403+1.118*TroutDietSL$sl[TroutDietSL$species=="BLT"]
#'# add LCat length categories
#'TroutDietSL$LCat <- lencat(TroutDietSL$tl,breaks=c(0,350,500,650,950),right=TRUE)
#'
#'# isolate the non-empty fish (as the authors did)
#'TroutDietSL1 <- Subset(TroutDietSL,empty=="NO")
#'with(TroutDietSL1,table(species,LCat))
#'
#'# summarize by computing the mean percent of the three diet items
#'mn.mysis <- aggregate(pvol.mysis~species*LCat,data=TroutDietSL1,FUN=mean)
#'mn.oi <- aggregate(pvol.oi~species*LCat,data=TroutDietSL1,FUN=mean)
#'mn.fish <- aggregate(pvol.fish~species*LCat,data=TroutDietSL1,FUN=mean)
#'mndiet <- cbind(mn.mysis,mn.oi[,"pvol.oi"],mn.fish[,"pvol.fish"])
#'colnames(mndiet)[3:5] <- c("mysis","oi","fish")
#'# reorganize the result
#'mndiet <- reshape(mndiet,direction="long",varying=c("mysis","oi","fish"),v.names="mnprop",
#'                  idvar=c("species","LCat"),timevar="item",times=c("mysis","oi","fish"))
#'mndiet <- reshape(mndiet,direct="wide",v.names="mnprop",idvar=c("item","LCat"),timevar="species")
#'                  
#'# Now compute diet overlap between same sized fishes of the two species
#'dietOverlap(mndiet[mndiet$LCat=="0",3:4],prey=levels(mndiet$item),type="Schoener")
#'dietOverlap(mndiet[mndiet$LCat=="350",3:4],prey=levels(mndiet$item),type="Schoener")
#'dietOverlap(mndiet[mndiet$LCat=="500",3:4],prey=levels(mndiet$item),type="Schoener")
#'dietOverlap(mndiet[mndiet$LCat=="650",3:4],prey=levels(mndiet$item),type="Schoener")
#'
#' @rdname dietOverlap
#' @export dietOverlap
dietOverlap <- function(diet1,diet2=NULL,type=c("Horn","Levins","Morisita","Pianka","Schoener"),prey=NULL,num1=NULL,num2=NULL,N1=NULL,N2=NULL) {
  ## Internal functions
    dietProp <- function(diet) { diet/sum(diet) }
  
    DOHorns <- function(diet1,diet2) {
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      if (any(c(prop1,prop2)==0)) stop("'Horn's' diet overlap index cannot be used when the proportion of any diet item is 0.",call.=FALSE)
      doi <- (sum((prop1+prop2)*log(prop1+prop2))-sum(prop1*log(prop1))-sum(prop2*log(prop2)))/(2*log(2))
      list(type="Horns",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOHorns internal function
  
    DOLevins <- function(diet1,diet2){
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      res12 <- sum(prop1*prop2)/sum(prop2^2)       # overlap of 1 on 2
      res21 <- sum(prop1*prop2)/sum(prop1^2)       # overlap of 2 on 1
      list(type="Levins",doi=c(res12,res21),propdiet=cbind(prop1,prop2))
    } # end DOLevins internal function
  
    DOMorisita <- function(diet1,diet2,num1,num2,N1,N2){
      if (is.null(num1)|is.null(num2)) stop("'num1' and 'num2' must not be NULL if type='Morisita'.",call.=FALSE)
      if (is.null(N1)|is.null(N2)) stop("'N1' and 'N2' must not be NULL if type='Morisita'.",call.=FALSE)
      if (length(num1)!=length(num2)) stop("Lengths of 'num1' and 'num2' must be equal.",call.=FALSE)
      if (length(num1)!=length(diet1)) stop("Lengths of 'diet1', 'diet2', 'num1', and 'num2' must be equal.",call.=FALSE)
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      doi <- (2*sum(prop1*prop2))/(sum(prop1*((num1-1)*(N1-1)))+sum(prop2*((num2-1)*(N2-1))))
      list(type="Morisita",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOMorisita internal function
  
    DOPianka <- function(diet1,diet2) {
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      doi <- sum(prop1*prop2)/sqrt(sum(prop1^2)*sum(prop2^2))
      list(type="Pianka",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOPianka internal function
  
    DOSchoener <- function(diet1,diet2) {
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      doi <- 1-0.5*sum(abs(prop1-prop2))  
      list(type="Schoener",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOSchoener internal function
  
  ## Start of main function
  type <- match.arg(type)
  if (!is.vector(diet1)) {
    if (!(is.matrix(diet1) | is.data.frame(diet1))) stop("'diet1' must be either a vector, matrix, or data.frame.",call.=FALSE)
    if (!is.null(diet2)) stop("'diet2' should be null if 'diet1' is a matrix or data.frame.",call.=FALSE)
    if (ncol(diet1)>2) stop("'diet1' must contain only two columns (corresponding to two predators)",call.=FALSE)
    diet2 <- as.vector(diet1[,2])
    diet1 <- as.vector(diet1[,1])
  }
  if (length(diet1)!=length(diet2)) stop("Length (number of diet items) of 'diet1' and 'diet2' must be the same.",call.=FALSE)
  switch(type,
         Horn={res <- DOHorns(diet1,diet2)},
         Levins={res <- DOLevins(diet1,diet2)},
         Morisita={res <- DOMorisita(diet1,diet2,num1,num2,N1,N2)},
         Pianka={res <- DOPianka(diet1,diet2)},
         Schoener={res <- DOSchoener(diet1,diet2)}
         ) # end switch
  if (!is.null(prey)) {
    if (length(diet1)!=length(prey)) stop("Number of prey categories in 'prey' must be same as number of items in 'diet1' and 'diet2'.",call.=FALSE)
    rownames(res[["propdiet"]]) <- prey
  }
  class(res) <- "dietOverlap"
  res
}

#' @rdname dietOverlap
#' @method print dietOverlap
#' @export
print.dietOverlap <- function(x,...) { print(x$doi,...) }

#' @rdname dietOverlap
#' @export
summary.dietOverlap <- function(object,verbose=TRUE,digits=getOption("digits"),...) {
  if (verbose) {
    if (object$type!="Levins") cat("The",object$type,"diet overlap index is",round(object$doi,digits),"\n")
    else {
      cat("The",object$type,"diet overlap is",round(object$doi[1],digits),"for predator 1 on predator 2\n")
      cat("and",round(object$doi[2],digits),"for predator 2 on predator 1\n")
    }
    cat("\nUsing the following observed proportional diets:\n")
    print(round(object$propdiet,digits))
  } else round(object$doi,digits)
}
