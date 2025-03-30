X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
Z=X
library(dglm)
mu <- fitted(fit.model)
resp <- fit.model$y
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
p = fi*(fi*trigamma(fi) - 1)
P <- diag(p)
R <- solve(t(Z)%*%P%*%Z)
R <- sqrt(P)%*%Z%*%R%*%t(Z)%*%sqrt(P)
r <- diag(R)
t <- log(resp/mu) - resp/mu 
t1 = t + 1 + log(fi) - digamma(fi)
tt = t1/sqrt((1-r)*(trigamma(fi) - 1/fi))
par(mfrow=c(1,2))
td <- resid(fit.model$dispersion,type="deviance")/sqrt(1-r)
#
a <- max(td)
b <- min(td)
di = (r/(1-r))*(tt^2)
#plot(fitted(fit.model$dispersion),r,xlab="Dispersão Ajustada", ylab="Medida r", pch=16)
#identify(fitted(fit.model$dispersion), r, n=2)
#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
identify(di, n=1)
#
plot(fitted(fit.model$dispersion),td,xlab="Valor Ajustado", 
     ylab="Resíduo Componente do Desvio",ylim=c(b-1,a+1),pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model)$dispersion,td, n=1)
#
#eta <- predict(fit.model$dispersion)
#z <- eta + t1/(fi*trigamma(fi)-1)  
#plot(eta,z,xlab="Preditor Linear", 
#ylab="Variável z", pch=16)