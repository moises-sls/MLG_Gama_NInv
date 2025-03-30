library(MASS)
library(tidyverse)

dados = read.table("pesca.txt", header=T)

dados$frota = factor(dados$frota)
dados$ano = factor(dados$ano)
dados$trimestre = factor(dados$trimestre)


dados %>% ggplot(aes(cpue)) + geom_density(lwd=0.8) + ylab("Densidade") +
  geom_hline(yintercept=0) + scale_x_continuous(limits=c(-50, 700)) + theme_bw()

dados %>% ggplot(aes(frota, cpue)) + geom_boxplot(fill='grey') +
  xlab("Frota") + theme_bw() 

dados %>% ggplot(aes(ano, cpue)) + geom_boxplot(fill='grey') +
  xlab("Ano") + theme_bw() 

dados %>% ggplot(aes(trimestre, cpue)) + geom_boxplot(fill='grey') +
  xlab("Trimestre") + theme_bw()

dados %>% ggplot(aes(frota, lat)) + geom_boxplot(fill='grey') +
  xlab("Frota") + ylab("Latitude") + theme_bw() 

dados %>% ggplot(aes(frota, long)) + geom_boxplot(fill='grey') +
  xlab("Frota") + ylab("Longitude") + theme_bw() 

dados %>% ggplot(aes(lat,cpue)) + geom_point() + xlab("Latitude")+ theme_bw()

dados %>% ggplot(aes(long,cpue)) + geom_point() + xlab("Longitude")+ theme_bw()


# definindo então Y_{ijkl} \sim G(\mu_{ijkl}, \phi), com parte sistematica dada
# por: 
#
# log(\mu_{ijkl}) = \alpha + \beta_j + \gamma_k + \theta_l + 
#                   \delta_1Latitude_{ijkl} + \delta_2Longitude_{ijkl}
################################################################################
# cada embarcação i possui um valor de latitude e longitude correspondente,
# e essa latitude/longitude pode influenciar no valor da cpue média, por isso
# deve ser inserida no modelo, e a latitude/longitude corresponde a embarcação
# i, da frota j, no ano k e no trimestre l, ou seja, existe uma latitude/
# longitude correspondente a cada observação 
################################################################################
#
# em que \beta_j, \gamma_k, \theta_l denota, respectivamente, os efeitos da 
# j-ésima frota, k-ésimo ano e l-ésimo trimestre.
# 

attach(dados)
fit.model = glm(cpue ~ frota + ano + trimestre + lat + long, 
                family=Gamma(link=log))
summary(fit.model)

stepAIC(fit.model)
fit0.model = glm(cpue ~ frota + ano + lat + long, 
                family=Gamma(link=log))



fit.model = glm(formula = cpue ~ frota + ano + lat + long + frota*ano,
                family = Gamma(link = log))

summary(fit.model)



anova(fit0.model,fit1.model, test="LRT")

gamma.shape(fit1.model)

desvio_modelo_int =  3.6780793 * 44.322 #phi * desvio dos residuos

desvio_modelo_sem_int = 3.3829507 * 48.366 



dados %>% ggplot(aes(ano, fitted(fit1.model))) + geom_line(aes(linetype=frota))

fit1.model$fitted.values %>% filter(26<= lat & lat < 27 & long < 47 & long > 46)

dados_estimados = dados
dados_estimados$cpue_estimada = fit1.model$fitted.values

dados_estimados = dados_estimados %>% select(!(cpue))



dados_final_santos = dados_estimados %>% 
  filter(frota=="Santos") %>%
  select(frota, ano,cpue_estimada)

dados_final_ubatuba = dados_estimados %>% 
  filter(frota=="Ubatuba") %>%
  select(frota, ano,cpue_estimada)



tapply(dados_final_santos$cpue_estimada, dados_final_santos$ano, mean)

tapply(dados_final_ubatuba$cpue_estimada, dados_final_ubatuba$ano, mean)

cpue_est_santos = c(222.4516, 187.2742, 257.6996, 211.3143, 205.0932)
cpue_est_ubatuba = c(47.08333, 96.89728, 210.55556, 174.42857, 138.48750)
anos = c(1995, 1996, 1997, 1998, 1999)

dados_final = cbind(cpue_est_santos, cpue_est_ubatuba)

library(reshape2)
dados_final <- melt(dados_final, 
                    measure.vars=c(cpue_est_santos, cpue_est_ubatuba),
                    value.name = "cpue_estimada")

dados_final$Var1 <- rep(anos, 2)
dados_final$Var2 <- rep(c("Santos", "Ubatuba"), each=5)
colnames(dados_final) <- c('ano', 'frota', 'cpue_est')

dados_final %>% ggplot(aes(ano, cpue_est)) + geom_line(aes(linetype = frota))
