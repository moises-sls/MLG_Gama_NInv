dados = read.table("insurance.txt", header=T)

dados$legrep = factor(dados$legrep)

dados %>% ggplot(aes(optime, log(amount))) + geom_point() + facet_wrap(~legrep) +
  theme_bw()

dados %>% ggplot(aes(amount)) + geom_density() +  xlim(-4000, 120000) + 
  theme_bw() + ylab("Densidade") + xlab("Valor do Seguro") + facet_wrap(~legrep)


# pelo grafico de dispersao do valor pago pelo tempo operacional, podemos perceber
# um crescimento quadratico da quantidade paga em função do optime nos individuos
# que NÃO possuem representação legal, já no caso dos indivíduos que possuem rep-
# resentação legal, o crescimento é linear. Por esse motivo, dois modelos serão
# sujeridos, um para os indivíduso SEM representação legal, este possuindo um 
# termo quadrático, e um para os indivíduos com representação legal:
#
#
# Denote por Y_{ij} o valor pago do seguro para o j-ésimo indivíduo do i-ésimo
# grupo (i=0, sem representação legal e i=1 com representação legal) e 
# j = 1, ..., n_i, sendo n_0 = 227 e n_1 = 542. 
#
# Conforme sugerido pelos gráficos da densidade do valor pago, sera assumido
# inicialmente Y_{ij} \sim  G(\mu_{ij}, \phi_{i}), com parte sistemática dada
# por:
#
# log(\mu_{0j}) = \alpha_0 + \beta_{0j}Optime + \theta_{0j}Optime^2
#


dados_0 = dados %>% filter(legrep == 0)
dados_1 = dados %>% filter(legrep == 1)


fit0.model = glm(dados_0$amount ~ dados_0$optime + I(dados_0$optime^2),
                 family = Gamma(link=log))
summary(fit0.model)


fit1.model = glm(dados_1$amount ~dados_1$optime, family = Gamma(link=log))
summary(fit1.model)



# parametros de precisao:
gamma.shape(fit0.model)
gamma.shape(fit1.model)


# os desvios dos modelos são dados por:
# sem_rep = 347.15  * 0.77891509 = 270.4004, 224 g.l.
# testando a adequacidade do modelo:
# 1 - pchisq(270.4, 224)



# com_rep = 261.45 * 2.2254546 = 581.8451, 540 g.l.
# 1 - pchisq(581.8451 ,540)





