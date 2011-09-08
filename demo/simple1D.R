#################################
### Introductory Example ########
#################################

nrow <- 4
y <- diag(1,nrow)
x <- factor(1:nrow)
xv <- 1:nrow
xv2 <- c(1,1.2,2,4)
xg1 <- factor(c(1,1,2,2))
xg2 <- factor(c(1,1,1,2))
df <- data.frame(y=y,x=x)
df

fit40 <- mdm(y~1,data=df)
fit41 <- mdm(y~x,data=df)
fit42 <- mdm(y~xv,maxit=10000,data=df)
fit43 <- mdm(y~xv2,maxit=10000,data=df)
fit44 <- mdm(y~xg1,data=df)
fit45 <- mdm(y~xg2,data=df)

sapply(list(fit40,fit41,fit42,fit43,fit44,fit45),dev2div)

xp <- seq(1,4,length=2000)
xp2 <- predict(fit42,newdata=data.frame(xv=xp),type="prob")
xp43 <- predict(fit43,newdata=data.frame(xv2=xp),type="prob")

x11(wid=8,hei=3.5)
par(mfrow=c(1,2),mar=c(4,4,0.1,0.1),mgp=c(2.5,0.7,0),las=1)
matplot(xp,xp2,type="l",col=1,xlab="Gradient X",ylab="Proportional Abundances")
matplot(xp,apply(xp2,1,ed1),type="l",col=1,xlab="Gradient X",ylab="Diversity")

x11(wid=8,hei=3.5)
par(mfrow=c(1,2),mar=c(4,4,0.1,0.1),mgp=c(2.5,0.7,0),las=1)
matplot(xp,xp43,type="l",col=1,xlab="Gradient X",ylab="Proportional Abundances")
matplot(xp,apply(xp43,1,ed1),type="l",col=1,xlab="Gradient X",ylab="Diversity")
