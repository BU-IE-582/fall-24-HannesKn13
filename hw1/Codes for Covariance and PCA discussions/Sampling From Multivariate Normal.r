require(MASS)
nofsample=200
nvar=2
mu=rep(0,nvar)
covmat=matrix(0,nvar,nvar)
diag(covmat)=1
dat=mvrnorm(nofsample, mu, covmat)
head(dat)


plotlim=1.1*max(abs(dat))
plot(dat[,1],dat[,2],lwd=2,
	xlim=c(-plotlim,plotlim),
	ylim=c(-plotlim,plotlim),
	xlab='Dim 1',ylab='Dim 2')
	
abline(h=0,lty=2,lwd=2,col='red')
abline(v=0,lty=2,lwd=2,col='red')

covmat=matrix(0,nvar,nvar)
diag(covmat)=1
covmat[1,2]=0.5
covmat[2,1]=0.5
dat=mvrnorm(nofsample, mu, covmat)

plotlim=1.1*max(abs(dat))
plot(dat[,1],dat[,2],
	xlim=c(-plotlim,plotlim),
	ylim=c(-plotlim,plotlim),
	xlab='Dim 1',ylab='Dim 2')
	
abline(h=0,lty=2,lwd=2,col='red')
abline(v=0,lty=2,lwd=2,col='red')

covmat=matrix(0,nvar,nvar)
diag(covmat)=1
covmat[1,2]=0.8
covmat[2,1]=0.8
dat=mvrnorm(nofsample, mu, covmat)

plotlim=1.1*max(abs(dat))
plot(dat[,1],dat[,2],
	xlim=c(-plotlim,plotlim),
	ylim=c(-plotlim,plotlim),
	xlab='Dim 1',ylab='Dim 2')
	
abline(h=0,lty=2,lwd=2,col='red')
abline(v=0,lty=2,lwd=2,col='red')



kde <- kde2d(dat[,1], dat[,2], n = 50)
contour(kde)
image(kde)
persp(kde, phi = 45, theta = 90)

# fancy contour with image
image(kde); contour(kde, add = T)

# fancy perspective
persp(kde, phi = 30, theta = 45, shade = .1, border = NA)

nofsample=50
nvar=2
mu=rep(0,nvar)
covmat=matrix(0,nvar,nvar)
diag(covmat)=1
covmat[1,2]=0.8
covmat[2,1]=0.8
dat=mvrnorm(nofsample, mu, covmat)

par(mfrow=c(1,2))
plotlim=1.1*max(abs(dat))
plot(dat[,1],xlab='Observations',ylab='',main='First dimension',ylim=c(-plotlim,plotlim))
plot(dat[,2],xlab='Observations',ylab='',main='Second dimension',ylim=c(-plotlim,plotlim))
plot(dat[,1],dat[,2],
	xlim=c(-plotlim,plotlim),
	ylim=c(-plotlim,plotlim),
	xlab='Dim 1',ylab='Dim 2',pch=21,bg='red',cex=1)



