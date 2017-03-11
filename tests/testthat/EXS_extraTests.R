## Example data
df <- data.frame(x=c(1,2,3,4,5,6,7,8,9,10),
                 y=c(4,6,5,7,9,8,7,12,16,22),
                 z=as.factor(rep(c("A","B"),each=5)),
                 w=as.factor(rep(c("A","B"),times=5)))
df$x2 <- df$x^2

## Some fits
lm.0 <- lm(y~1,data=df)
lm.1 <- lm(y~x,data=df)
lm.2 <- lm(y~x+x2,data=df)
lm.2b <- lm(y~x*z,data=df)
lm.1a <- lm(y~w,data=df)
lm.2c <- lm(y~w*z,data=df)

# note that the rep() in the first model is needed to eliminate a warning
# that comes from nls().
nls.0 <- nls(y~rep(c,length(df$y)),data=df,start=list(c=10))
nls.1 <- nls(y~a*x+c,data=df,start=list(a=1,c=1))
nls.2 <- nls(y~b*x2+a*x+c,data=df,start=list(a=-1,b=0.3,c=10))

if (suppressMessages(require(nlme))) {
  gls.0 <- gls(y~1,data=df,method="ML")
  gls.1 <- gls(y~x,data=df,method="ML")
  gls.2 <- gls(y~x+x2,data=df,method="ML")  
}

suppressMessages(library(lmtest))

