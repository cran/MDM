################################
### Simulation Example  ########
################################


n <- 25; pp <- 5; p <- 4*pp; lst <- TRUE; cex2=0.85; cext=0.9
ins <- seq(1,n,by=1)
dat <- simdata(n=n,p=p,s=sample(c(1,3,5),p,rep=TRUE)/10,amp=sample(c(2,4,8),p,rep=TRUE),err=TRUE,lst=lst,mu.rand=TRUE,d.rand=FALSE,plotit=FALSE)
if(lst) {
	xlst <- dat
	dat <- data.frame(xlst$x)
	dat2 <- data.frame(xlst$xs)
}
names(dat)[2:(p+1)] <- paste("S",1:p,sep="")
names(dat2)[2:(p+1)] <- paste("S",1:p,sep="")
y <- y2p(dat[,-1])
y2 <- y2p(dat2[,-1])
x1 <- dat[,1]
fit0 <- mdm(y~1,data=dat,trace=FALSE)
fit1 <- mdm(y~x1,data=dat,trace=FALSE)
fit2 <- mdm(y~poly(x1,2),data=dat,trace=FALSE)
fit3 <- mdm(y~poly(x1,3),data=dat,trace=FALSE)
fit4 <- mdm(y~factor(x1),data=dat,trace=FALSE)
fit02 <- mdm(y2~1,data=dat2,trace=FALSE)
fit12 <- mdm(y2~x1,data=dat2,trace=FALSE)
fit22 <- mdm(y2~poly(x1,2),data=dat2,trace=FALSE)
fit32 <- mdm(y2~poly(x1,3),data=dat2,trace=FALSE)
fit42 <- mdm(y2~factor(x1),data=dat2,trace=FALSE)
anova(fit0,fit1,fit2,fit3,fit4)
anova(fit02,fit12,fit22,fit32,fit42)
mmat <- xlst$xs[,-1]/apply(xlst$xs[,-1],1,sum)
mmatd <- xlst$x[,-1]/apply(xlst$x[,-1],1,sum)
mmatx <- max(mmatd)
x11()
par(mfrow=c(4,pp),mar=c(rep(0.1,4)),mgp=c(2,0.75,0),oma=c(4.5,4.75,0.1,0.1),las=1)
for (i in 1:p) {
	plot(xlst$x[,1],mmat[,i],type="l",lty=2,axes=FALSE,ylim=c(0,mmatx),xlim=c(-0.025,1.025))
	lines(xlst$x[,1],fitted(fit2)[,i],lty=1)
	points(xlst$x[ins,1],mmatd[ins,i],type="p")
	legend("topleft",legend=paste("S",i,sep=""),bty="n")
	box()
	if (any(i==seq(1,p,by=pp))) axis(2,cex.axis=cex2)
	if (any(i==seq(p-pp+1,p))) axis(1,cex.axis=cex2)
}
mtext("Gradient",side=1,outer=TRUE,line=2.5,cex=cext)
par(las=0)
mtext("Proportional abundance",side=2,outer=TRUE,line=3.25,cex=cext)
