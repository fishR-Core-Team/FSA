## pH in four ponds data from Zar (2010)
ponds <- data.frame(pond=rep(1:4,each=8),
                     pH=c(7.68,7.69,7.70,7.70,7.72,7.73,7.73,7.76,
                          7.71,7.73,7.74,7.74,7.78,7.78,7.80,7.81,
                          7.74,7.75,7.77,7.78,7.80,7.81,7.84,NA,
                          7.71,7.71,7.74,7.79,7.81,7.85,7.87,7.91))
ponds$cpond <- paste0("pond_",ponds$pond)
ponds$fpond <- as.factor(ponds$pond)
ponds <- ponds[complete.cases(ponds),]

## p-value results from UniStat (http://www.unistat.com/guide/nonparametric-tests-kruskal-wallis-one-way-anova/)
unistat <- data.frame(Comparison=c("2-1","3-1","3-2","4-1","4-2","4-3"),
                      P.adj=c(0.1956,0.0191,1,0.0166,1,1))
