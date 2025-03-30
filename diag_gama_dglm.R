X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
library(dglm)
fi <- fitted(fit.model$dispersion)
fi <- 1/fi
w <- fit.model$weights
w <- w*fi
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")*sqrt(fi/(1-h))
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
di <- (h/(1-h))*(ts^2)
a <- max(td)
b <- min(td)
#plot(fitted(fit.model),h,xlab="Média Ajustada", 
#ylab="Medida h", pch=16)
#identify(fitted(fit.model), h, n=2)
#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
identify(di, n=1)
#
plot(fitted(fit.model),td,xlab="Valor Ajustado", 
     ylab="Resíduo Componente do Desvio",
     ylim=c(b-1,a+1),pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),td, n=1)
#
#w <- fit.model$weights
#eta <- predict(fit.model)
#z <- eta + resid(fit.model, type="pearson")/sqrt(w)
#plot(predict(fit.model),z,xlab="Preditor Linear", 
#ylab="Variável z", pch=16)