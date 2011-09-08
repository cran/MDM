#############################################################
### Simulations to assess differences between pD and D    ###
### Estimate correlations etc for q = 0,1,2               ###
#############################################################

nsim <- 100
res0 <- matrix(NA,nrow=nsim,ncol=3)
dimnames(res0) <- list(1:nsim,c("alpha","beta","gamma"))
res1 <- res2 <- resp0 <- resp2 <- res0
mat <- matrix(NA,nrow=3,ncol=7)
dimnames(mat) <- list(c("alpha","beta","gamma"),c("D1/qD1","D0","qD0","R0","D2","qD2","R2"))

#####################
#### DUNE data ######
#####################

library(vegan)
data(dune)
summary(dune)
yy <- y2p(dune)
for (i in 1:nsim) {
	y <- yy[sample(1:nrow(yy),nrow(yy),rep=T),]
	res0[i,] <- ed(y,q=0)
	res1[i,] <- ed(y,q=1)
	res2[i,] <- ed(y,q=2)
	resp0[i,] <- eds(y,q=0)
	resp2[i,] <- eds(y,q=2)
}
mat[,4]<-diag(cor(res0,resp0))
mat[,7]<-diag(cor(res2,resp2))
mat[,1]<-apply(res1,2,mean)
mat[,2]<-apply(res0,2,mean)
mat[,3]<-apply(resp0,2,mean)
mat[,5]<-apply(res2,2,mean)
mat[,6]<-apply(resp2,2,mean)
round(mat,2)

#####################
#### SPIDER data ####
#####################

library(mvpart)
data(spider)
summary(spider)
yy <- y2p(spider[,1:12])
for (i in 1:nsim) {
	y <- yy[sample(1:nrow(yy),nrow(yy),rep=T),]
	res0[i,] <- ed(y,q=0)
	res1[i,] <- ed(y,q=1)
	res2[i,] <- ed(y,q=2)
	resp0[i,] <- eds(y,q=0)
	resp2[i,] <- eds(y,q=2)
}
mat[,4]<-diag(cor(res0,resp0))
mat[,7]<-diag(cor(res2,resp2))
mat[,1]<-apply(res1,2,mean)
mat[,2]<-apply(res0,2,mean)
mat[,3]<-apply(resp0,2,mean)
mat[,5]<-apply(res2,2,mean)
mat[,6]<-apply(resp2,2,mean)
round(mat,2)

#####################
#### CORALS data ####
#####################

data(corals)
yy <- y2p(corals[,-c(1:6)])
for (i in 1:nsim) {
	y <- yy[sample(1:nrow(yy),nrow(yy),rep=T),]
	res0[i,] <- ed(y,q=0)
	res1[i,] <- ed(y,q=1)
	res2[i,] <- ed(y,q=2)
	resp0[i,] <- eds(y,q=0)
	resp2[i,] <- eds(y,q=2)
}
mat[,4]<-diag(cor(res0,resp0))
mat[,7]<-diag(cor(res2,resp2))
mat[,1]<-apply(res1,2,mean)
mat[,2]<-apply(res0,2,mean)
mat[,3]<-apply(resp0,2,mean)
mat[,5]<-apply(res2,2,mean)
mat[,6]<-apply(resp2,2,mean)
round(mat,2)

#####################
## SIMULATED data ###
#####################

dat <- pcsim2(n=25,p=20,s=0.1,err=TRUE,plot=FALSE)
yy <- y2p(dat[,-1])
for (i in 1:nsim) {
	y <- yy[sample(1:nrow(yy),nrow(yy),rep=T),]
	res0[i,] <- ed(y,q=0)
	res1[i,] <- ed(y,q=1)
	res2[i,] <- ed(y,q=2)
	resp0[i,] <- eds(y,q=0)
	resp2[i,] <- eds(y,q=2)
}
mat[,4]<-diag(cor(res0,resp0))
mat[,7]<-diag(cor(res2,resp2))
mat[,1]<-apply(res1,2,mean)
mat[,2]<-apply(res0,2,mean)
mat[,3]<-apply(resp0,2,mean)
mat[,5]<-apply(res2,2,mean)
mat[,6]<-apply(resp2,2,mean)
round(mat,2)


