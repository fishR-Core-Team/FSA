data(BrookTroutTH)
BrookTroutTH$fact <- as.factor(sample(c("A","B"),nrow(BrookTroutTH),replace=TRUE))
d <- BrookTroutTH

cc <- catchCurve(catch~age,data=d,ages2use=2:6)
cc2 <- catchCurve(catch~age,data=d,ages2use=2:6,weighted=TRUE)
cr <- chapmanRobson(catch~age,data=d,ages2use=2:6)
cr1 <- chapmanRobson(catch~age,data=d,ages2use=2:6,zmethod="Hoenigetal")
cr2 <- chapmanRobson(catch~age,data=d,ages2use=2:6,zmethod="original")
