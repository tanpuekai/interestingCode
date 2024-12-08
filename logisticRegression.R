########################################################################
#    This is a set of interesting code that I wrote from scratch that 
#    show how logistic regression are trained. Conceptually, i randomly
#    generated a vector of random values between 0 and 1.0, and use it 
#    as initial probabilities, i then converted them to odds:
#      odds = {p(i)/(1-p(i)), for all i=0 .. 400}
#
#    the cool thing is that odds ranges from -inf to inf.
#
#    step t, I performed linear regression to obtain beta_hat(t):
#      odds ~ beta0 + X * beta, where X is the model matrix
#    step t+1, i calculate the difference:
#      delta(t+1) = y - 1/(1 + exp(-x)), where x= beta0_hat(t) + X * beta_hat(t)
#    I then updated odds:
#      odds = y + lambda * delta(t+1)
#    where lambda is the learning rate.
#    From here, we returned to step t, and repeat the whole process
#    till convergence.
########################################################################
x1<-matrix(rnorm(8e2), ncol=2)
x2<-matrix(rnorm(8e2), ncol=2)+2
X<-rbind(x1, x2)

y<-rep(c(0, 1), rep(400, 2))

plot(X[, 1], X[, 2], col=y+1)


X<-x12

model2 <- glm(factor(y) ~ X[,1]+ X[,2], family = binomial())

for(i in 1:10){
	p0<-runif(nrow(X))
	lp0<-log(p0/(1-p0))

	Lbeta<-list()
	Ldelta<-list()
	Lval<-list()
	Lrss<-c(1e6)

	Lp<-list()
	lambda  <- 1e-1
	Lp[[1]]<-lp0
	for(iter in 2:1500){
		lp<-Lp[[iter-1]]
		beta.i<-as.matrix(coef(lm(lp~X[, 1] + X[, 2])))
		Lbeta[[iter]]<-beta.i
		X1<-cbind(1 ,X)
		t1<-(X1 %*% beta.i)[,1]
		vali<- 1/(1+exp(-t1))
		Lval[[iter]]<-vali
		deltai<- y - vali
		lpnew<- lp + lambda  *deltai
		Ldelta[[iter]]<-deltai
		Lrss[iter]<-sum(deltai^2)
		Lp[[iter]]<-lpnew
		#if(abs(Lrss[iter]-Lrss[iter-1])/Lrss[iter-1]< 1e-5)
			#break

		betaM<-as.numeric(coef(model2 ))


	}
	tM<-(X1 %*%betaM)[,1]
	valM<- 1/(1+exp(-tM))
	deltaM<- y - valM
	sum(deltaM^2)

	beta.i[, 1] / betaM


	plot(t1, tM)
	abline(0, -1, col=2, lwd=2)
	plot((X1 %*% beta.i)[,1], (X1 %*%betaM)[,1])
	abline(0, 1, col=2, lwd=2)
	plot(1/(1+exp(-t1)), 1/(1+exp(-tM)))
	abline(0, 1, col=2, lwd=2)
	boxplot((1/(1+exp(-tM)))~y)
	boxplot(vali~y)
	boxplot(valM~y)
	plot(vali, valM)

	hist(vali)
	hist(valM)

	pMat<-sapply(Lp, function(x)1/(1+exp(-x)))
	valMat<-sapply(Lval[-1], function(x)x)
	betaMat<-sapply(Lbeta[-1], function(x)x)
	deltaMat<-sapply(Ldelta[-1], function(x)x)

	cat(i, ":\t", beta.i, "\n")
	plot(Lrss)
	boxplot(vali~y)
	boxplot(pMat[, ncol(pMat)]~y)
	boxplot(valMat[, ncol(valMat)]~y)

	matplot(t(pMat[1:800, ]), type="l")
	matplot(t(valMat[, ]), type="l")
	matplot(t(betaMat[, ]), type="l")
	matplot(t(deltaMat[, ]), type="l")
	#boxplot(pMat[, 50]~y)
}
plot(Lrss[-1])
boxplot(vali~y)

