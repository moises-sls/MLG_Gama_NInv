dados = read.table("turbina.txt")

# library(reshape2)
# library(dplyr)
# library(tidyverse)


dados = melt(dados, value.name="tempo",  variable.name = "variável", 
             measure.vars = c("V2", "V4", "V6", "V8", "V10"))

# melt é tipo pivot.table(); digo quais variáveis eu quero empilhar com o 
# argumento measure.vars, nesse caso queremos empilhar os tempos de cada turbina
# todos os tempos do tipo 1, depois do tipo 2, e assim por diante; ai, com o 
# argumento value.name eu digo qual o nome da nova coluna que vai receber os 
# valores empilhados; variable.name vai ser uma coluna que possuira os valores
# do tipo da turbina, perceba que os dados estão da seguinte forma: 
# tipo1 - valor1, a primeira coluna indica o tipo da turbina, e a segunda o 
# o tempo correspondente aquele tipo de turbina, então, como estamos empilhando
# os valores do tempo, a coluna chamada "variável" vai indicar qual é o tipo da
# turbina. basicamente uma coluna: "variável" vai ser as colunas V1, V3, V5, 
# V7 e V9 empilhadas, e a coluna tempo será as colunas V2, V4, V6, V8 e V10 
# empilhadas

dados$tipo = rep(1:5, each=10)

# criando uma nova coluna com os tipos da turbina, existem 10 observaçoes
# para cada tipo

# especificando de qual pacote quero usar a funçao select, o pacote mass possui 
# a msm funçao
dados = dplyr::select(dados, tipo, tempo)

# aqui seleciono quais variáveis, em ordem, eu desejo manter 

dados$tipo = factor(dados$tipo, levels=c("1", "2",'3','4','5'),
                    labels=c("tipo1", 'tipo2', 'tipo3', 'tipo4', 'tipo5'))


################################################################################
# estatisticas descritivas:
tapply(dados$tempo, dados$tipo, mean)
tapply(dados$tempo, dados$tipo, sd)

cv = function(x) {
  (sd(x) / mean(x))*100
}

tapply(dados$tempo, dados$tipo, cv)

################################################################################

dados %>% ggplot(aes(tempo)) + geom_density(lwd=0.9) + 
  scale_x_continuous(limits=c(-3,33)) + ylab("Densidade") + 
  xlab("Tempo")+ theme_bw()


dados %>% ggplot(aes(tipo, tempo)) + geom_boxplot(fill="gray") +
  xlab("") + ylab("Tempo") + theme_bw()


fit.model = glm(dados$tempo ~ dados$tipo, family=Gamma(link=identity), data=dados)
summary(fit.model)

# pegando o parâmetro de precisão \phi
require(MASS)
gamma.shape(fit.model)


# o desvio de um modelo gama é dado por:
# D*(y; \hat{\mu}) = \phi \times D(y;\hat{\mu})
#
# em que D(y;\hat{\mu}), a função desvio, é a diferença entre o log da funçao de 
# verossimilhança do modelo saturado e do modelo sob investigaçao
#
# o R devolve a função desvio, D(y;\hat{\mu}), como o valor Residual deviance
# = 8.861
#
# o Null deviance denota o desvio do modelo apenas com o intercepto, no caso
# D(y;\bar{y}) = 12.965
#
# logo o desvio do modelo é dado por 5.803896 * 8.861 = 51.43

# podemos calcular a medida R² = 1 - D(y;\hat{\mu}) / D(y;\bar{y})
# 1 - (8.61 / 12.965) = 0.236 



# como a estimativa do paramêtro de precisão é dada por 5.804, um valor alto, 
# isso implica que as distribuições dos tempos até a perda da velocidade não
# devem ser muito assimétricas, já que, quanto maior o parâmetro de precisão
# mais a distribuição gama se torna simétrica, ela se aproxima de uma normal


# podemos testar a significancia 
# F = ({D(y;\hat{\mu}^0) - D(y;\hat{\mu})}/p) / {D(y;\hat{\mu})/(n-p)}

# para testar \beta_3 = \beta_4 = 0, agrupamos os níveis 3 e 4:
levels(dados$tipo) = c("tipo1", "tipo2", "tipo1", "tipo1", "tipo5")


# D(y;\hat{\mu}^0) = 9.091 ; D(y;\hat{\mu}) = 8.861
#
# F = {(9.091 - 8.861)/2} / {8.861/45}  = 0.584


attach(dados)


# retirando a obs #49, devido a distancia de cook
summary(glm(formula = dados[-49, ]$tempo ~ dados[-49, ]$tipo, family = Gamma(link = identity), 
    data = dados[-49, ]))

# nao é perceptivel nenhuma mundança inferencial com a retirada do ponto, logo
# ele pode ser mantido no modelo


