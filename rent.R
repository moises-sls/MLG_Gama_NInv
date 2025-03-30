library(MASS)
library(tidyverse)
library(polyserial)
library(gamlss)
library(dglm)

dados = rent %>% select(R, Fl, A, H, loc)
dados$loc = as.factor(dados$loc)
dados$H = as.factor(dados$H)

dados %>% ggplot(aes(R)) + geom_density() + xlim(c(-5,NA))

dados %>% ggplot(aes(log(Fl), log(R))) + geom_point()

dados %>% ggplot(aes(A, R)) + geom_point()

dados %>% ggplot(aes(y=R, group=H, fill=H)) + geom_boxplot()

dados %>% ggplot(aes(y=R, group=loc, fill=loc)) + geom_boxplot()

dados %>% ggplot(aes(A, R)) + geom_point()


# log(\mu_{ijklm}) = \alfa + \betaFl_j + \thetaA_k + \gammaH_l + 
#                           \deltaLoc_{m}
# em que \beta_{j} representa o efeito da j-ésima area util, \theta_{k}
# representa o efeito do k-ésimo ano, \gamma_{l} o efeito de possuir aquecimento
# ou nao, e \delta_{m} representa o efeito da localização do imóvel

fit.model = glm(R ~ Fl + A + H + loc, family=Gamma(link=log), data=dados)
summary(fit.model)

gamma.shape(fit.model)

# o desvio do modelo é dado por: 7.1306201*282.57 = 2014.899 ; 1963 g.l.






############################### Modelo Duplo ###################################

fit.model = dglm(R ~ Fl + A + H + loc, ~Fl + A + H + loc,
                 family=Gamma(link=log), data=dados)
summary(fit.model)





