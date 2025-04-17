dados <- read.table("claims/claims.txt", header = TRUE)
head(dados)

#################### Modelando apenas mu ####################
# definindo as variáveis
valorv <- dados$valorv
expos <- dados$expos
cmsinistros <- dados$csinistros / dados$nsinistros
tipov <- factor(dados$tipov)
levels(tipov) <- c(
  "Trab", "Passeio", "Passeio", "Passeio", "Passeio",
  "Trab", "Trab", "Passeio", "Passeio", "Trab", "Trab"
)
idadev <- factor(dados$idadev)
levels(idadev) <- c(
  "1-2", "1-2", "3-4", "3-4"
)
sexoc <- factor(dados$sexoc)
areac <- factor(dados$areac)
levels(areac) <- c(
  "AB", "AB", "CDEF", "CDEF",
  "CDEF", "CDEF"
)
idadec <- factor(dados$idadec)
levels(idadec) <- c("1-2", "1-2", "3-4-5-6", "3-4-5-6", "3-4-5-6", "3-4-5-6")

library(MASS)

stepAIC(glm(
  cmsinistros ~ valorv + expos + tipov + idadev + sexoc + areac + idadec,
  family = Gamma
))

fit_model <- glm(
  cmsinistros ~ expos + tipov + areac,
  family = Gamma
)

summary(fit_model)

#################### Análise de Diagnóstico ####################
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)
w <- fit_model$weights
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)
library(MASS)
fi <- gamma.shape(fit_model)$alpha
ts <- resid(fit_model, type = "pearson") * sqrt(fi / (1 - h))
td <- resid(fit_model, type = "deviance") * sqrt(fi / (1 - h))
di <- (h / (1 - h)) * (ts^2)

a <- max(td)
b <- min(td)
plot(fitted(fit_model), h, xlab = "Valor Ajustado", ylab = "Medida h", pch = 16)
# identify(fitted(fit_model), h, n=2, tolerance=2)
#
plot(di, xlab = "?ndice", ylab = "Dist?ncia de Cook", pch = 16)
abline(2 * p / n, 0, lty = 2)
# identify(di, n=2, tolerance=2)
#
plot(fitted(fit_model), td,
  xlab = "Valor Ajustado",
  ylab = "Resíduo Componente do Desvio",
  ylim = c(b - 1, a + 1), pch = 16
)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(fitted(fit.model),td, n=1)
#
w <- fit_model$weights
eta <- predict(fit_model)
z <- eta + resid(fit_model, type = "pearson") / sqrt(w)
plot(predict(fit_model), z,
  xlab = "Preditor Linear",
  ylab = "Vari?vel z", pch = 16
)

########################### Envelope ###########################
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)
w <- fit_model$weights
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)
ro <- resid(fit_model, type = "response")
fi <- (n - p) / sum((ro / (fitted(fit_model)))^2)
td <- resid(fit_model, type = "deviance") * sqrt(fi / (1 - h))
#
e <- matrix(0, n, 100)
#
for (i in 1:100) {
  resp <- rgamma(n, fi)
  resp <- (fitted(fit_model) / fi) * resp
  fit <- glm(resp ~ X, family = Gamma(link = log))
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X) %*% W %*% X)
  H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
  h <- diag(H)
  ro <- resid(fit, type = "response")
  phi <- (n - p) / sum((ro / (fitted(fit)))^2)
  e[, i] <- sort(resid(fit, type = "deviance") * sqrt(phi / (1 - h)))
}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for (i in 1:n) {
  eo <- sort(e[i, ])
  e1[i] <- (eo[2] + eo[3]) / 2
  e2[i] <- (eo[97] + eo[98]) / 2
}
#
med <- apply(e, 1, mean)
faixa <- range(td, e1, e2)
par(pty = "s")
qqnorm(td,
  xlab = "Percentil da N(0,1)",
  ylab = "Componente do Desvio", ylim = faixa, pch = 16, main = ""
)
par(new = TRUE)
#
qqnorm(e1,
  axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 1, main = ""
)
par(new = TRUE)
qqnorm(e2,
  axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 1, main = ""
)
par(new = TRUE)
qqnorm(med,
  axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 2, main = ""
)

#################### Modelo duplo ####################
library(dglm)
valorv_2 <- valorv * valorv
head(valorv_2)
fit_model <- dglm(
  log(cmsinistros) ~ expos + tipov + areac, ~valorv,
  family = Gamma(link = log)
)

summary(fit_model)

#################### Análise de Diagnóstico Mu ####################
par(mfrow = c(2, 2))
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)
fi <- fitted(fit_model$dispersion)
fi <- 1 / fi
w <- fit_model$weights
w <- w * fi
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)
ts <- resid(fit_model, type = "pearson") * sqrt(fi / (1 - h))
td <- resid(fit_model, type = "deviance") * sqrt(fi / (1 - h))
di <- (h / (1 - h)) * (ts^2)
a <- max(td)
b <- min(td)
plot(fitted(fit_model), h,
  xlab = "Média Ajustada",
  ylab = "Medida h", pch = 16
)
# identify(fitted(fit_model), h, n=2)
#
plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
abline(2 * p / n, 0, lty = 2)
# identify(di, n = 1)
#
plot(fitted(fit_model), td,
  xlab = "Valor Ajustado",
  ylab = "Resíduo Componente do Desvio",
  ylim = c(b - 1, a + 1), pch = 16
)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(fitted(fit_model),td, n=1)
#
w <- fit_model$weights
eta <- predict(fit_model)
z <- eta + resid(fit_model, type = "pearson") / sqrt(w)
plot(predict(fit_model), z,
  xlab = "Preditor Linear",
  ylab = "Variável z", pch = 16
)
par(mfrow = c(1, 1))

#################### Envelope Mu ####################
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)
Z <- X
fi <- fitted(fit_model$dispersion)
fi <- 1 / fi
mu <- fitted(fit_model)
w <- fi / (mu^2)
W <- diag(w)
H <- solve(t(X) %*% W %*% X)
H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
h <- diag(H)
td <- resid(fit_model, type = "deviance") * sqrt(fi / (1 - h))
#
e <- matrix(0, n, 100)
#
for (i in 1:100) {
  resp <- rgamma(n, fi)
  resp <- (fitted(fit_model) / fi) * resp
  fit <- dglm(resp ~ X, ~Z, family = Gamma(link = log))
  fi <- fitted(fit$dispersion)
  fi <- 1 / fi
  mu <- fitted(fit)
  w <- fi / (mu^2)
  W <- diag(w)
  H <- solve(t(X) %*% W %*% X)
  H <- sqrt(W) %*% X %*% H %*% t(X) %*% sqrt(W)
  h <- diag(H)
  e[, i] <- sort(resid(fit, type = "deviance") * sqrt(fi / (1 - h)))
}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for (i in 1:n) {
  eo <- sort(e[i, ])
  e1[i] <- (eo[2] + eo[3]) / 2
  e2[i] <- (eo[97] + eo[98]) / 2
}
#
med <- apply(e, 1, mean)
faixa <- range(td, e1, e2)
par(pty = "s")
qqnorm(td,
  xlab = "Percentil da N(0,1)",
  ylab = "Resíduo Componente do Desvio", ylim = faixa, pch = 16,
  main = ""
)
par(new = T)
#
qqnorm(e1,
  axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 1, main = ""
)
par(new = T)
qqnorm(e2,
  axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 1, main = ""
)
par(new = T)
qqnorm(med,
  axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 2, main = ""
)

#################### Análise de Diagnóstico Phi ####################
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)
Z <- X
library(dglm)
mu <- fitted(fit_model)
resp <- fit_model$y
fi <- fitted(fit_model$dispersion)
fi <- 1 / fi
p <- fi * (fi * trigamma(fi) - 1)
P <- diag(p)
R <- solve(t(Z) %*% P %*% Z)
R <- sqrt(P) %*% Z %*% R %*% t(Z) %*% sqrt(P)
r <- diag(R)
t <- log(resp / mu) - resp / mu
t1 <- t + 1 + log(fi) - digamma(fi)
tt <- t1 / sqrt((1 - r) * (trigamma(fi) - 1 / fi))
par(mfrow = c(1, 2))
td <- resid(fit_model$dispersion, type = "deviance") / sqrt(1 - r)
#
a <- max(td)
b <- min(td)
di <- (r / (1 - r)) * (tt^2)
plot(fitted(fit_model$dispersion), r, xlab = "Dispersão Ajustada", ylab = "Medida r", pch = 16)
# identify(fitted(fit_model$dispersion), r, n=2)
#
plot(di, xlab = "Índice", ylab = "Distância de Cook", pch = 16)
identify(di, n = 1)
#
plot(fitted(fit_model$dispersion), td,
  xlab = "Valor Ajustado",
  ylab = "Resíduo Componente do Desvio", ylim = c(b - 1, a + 1), pch = 16
)
abline(2, 0, lty = 2)
abline(-2, 0, lty = 2)
# identify(fitted(fit_model)$dispersion,td, n=1)
#
eta <- predict(fit_model$dispersion)
z <- eta + t1 / (fi * trigamma(fi) - 1)
plot(eta, z,
  xlab = "Preditor Linear",
  ylab = "Variável z", pch = 16
)

#################### Envelope Phi ####################
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)
Z <- X
mu <- fitted(fit_model)
fi <- fitted(fit_model$dispersion)
fi <- 1 / fi
p <- fi * (fi * trigamma(fi) - 1)
P <- diag(p)
R <- solve(t(Z) %*% P %*% Z)
R <- sqrt(P) %*% Z %*% R %*% t(Z) %*% sqrt(P)
r <- diag(R)
td <- resid(fit_model$dispersion, type = "deviance") / sqrt(1 - r)
#
e <- matrix(0, n, 100)
#
for (i in 1:100) {
  resp1 <- rgamma(n, fi)
  resp1 <- (fitted(fit_model) / fi) * resp1
  fit <- dglm(resp1 ~ X, ~Z, family = Gamma(link = log))
  mu <- fitted(fit)
  fi1 <- fitted(fit$dispersion)
  fi1 <- 1 / fi1
  p <- fi1 * (fi1 * trigamma(fi1) - 1)
  P <- diag(p)
  R <- solve(t(Z) %*% P %*% Z)
  R <- sqrt(P) %*% Z %*% R %*% t(Z) %*% sqrt(P)
  r <- diag(R)
  e[, i] <- sort(resid(fit$dispersion, type = "deviance") / sqrt(1 - r))
}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for (i in 1:n) {
  eo <- sort(e[i, ])
  e1[i] <- (eo[2] + eo[3]) / 2
  e2[i] <- (eo[97] + eo[98]) / 2
}
#
med <- apply(e, 1, mean)
faixa <- range(td, e1, e2)
par(pty = "s")
qqnorm(td,
  xlab = "Percentil da N(0,1)",
  ylab = "Resíduo Componente do Desvio", ylim = faixa, pch = 16,
  main = ""
)
par(new = TRUE)
#
qqnorm(e1, axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 1, main = ""
)
par(new = TRUE)
qqnorm(e2, axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 1, main = ""
)
par(new = TRUE)
qqnorm(med, axes = F, xlab = "", ylab = "", type = "l",
  ylim = faixa, lty = 2, main = ""
)
