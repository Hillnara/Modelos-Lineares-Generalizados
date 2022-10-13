################################################################################
# UNIVERSIDADE ESTADUAL DA PARAÍBA - UEPB                                      #  
# CENTRO DE CIÊNCIAS E TECNOLOGIA - CCT                                        #
# DEPARTAMENTO DE ESTATÍSTICA                                                  #
# Disciplina: Modelo Linear Generalizado                                       #
# Professor: Pedro Almeida                                                     #                                                    #
# Alunos: João Vitor Andrade                                                   #
#         Hillnara de Paiva                                                    #
################################################################################

# MODELAGEM PARA DADOS DE CONTAGEM 

## DESCRIÇÃO DO PROBLEMA 

#  Os dados estão relacionados as postagem publicadas durante o ano de 2014 na 
# página do Facebook. Este conjunto de dados contém 500 observações e 7 
# variáveis, são ela:

# PPDS: Postagem por dia da semana

# ATP: Alcance total da postagem

# TVP: Total de visualizações da postagem

# US: Usuários engajados

# CP: Consumidores de postagem

# PEP: Pessoas ao longo da vida que gostaram do seu perfil e se envolveram com o
#sua postagem

# IT: Interações totais 


## RETIRADO DO SITE:
 
# UCI - Machine Learning Repository 
# https://archive.ics.uci.edu/ml/datasets/CSM+%28Conventional+and+Social+Media+Movies%29+Dataset+2014+and+2015

### PACOTES

library(GLMsData) # Dataset MLG
library(ISLR)     # Dataset livro: introduction a statistical learning
library(MASS)     # Modelos residuais
library(pscl)     # Calcular pseudo R2
library(qqplotr)  # Gráfico

### BANCO DE DADOS
dados<-Facebook
View(dados)

### OBJETIVO 

# Análisar o comportamento de uma variável dependente (Interações totais), em 
#função da variáveis preditoras.


## VARIÁVEL RESPOSTA

hist(dados$IT) 
# Os dados se concentram entorno de 0 e 1000
# Uma distribuição assimetria positiva

plot(dados$IT) # Grafico de disperção

mean(dados$IT) # Média

sd(dados$IT)   # Desvio padrão, os valores do conjunto de dados IT estão
# aglomerados com proximidade uns dos outros


boxplot(dados$IT)
# Os dados estão aglomerados,sera dado pelo intervalo interquartilíco
#(tamanho da caixa).
# Dados são assimétricos positivos, pois a linha da mediana está proxima ao 
#primeiro quartil. 
# Mediana menor que 500
# Presença de Outlier, indicado pelos valores discrepantes.

### METODOLOGIA UTILIZADA

## MODELO POISSON

# Ajuste do modelo pela função glm()

mod = glm( formula = IT ~ PPDS + ATP + TVP + UE + CP + PEP , data=dados,
           family=poisson(link="log"),
           control=list(trace=TRUE))
summary(mod)

#R2

pR2(mod)

### ANALISE DE RESIDUO
hist( mod$residuals )
qqnorm(mod$residuals)
qqline( mod$residuals, col = 'red' )
shapiro.test( mod$residuals )
plot(cooks.distance(mod))



cooksD = cooks.distance(mod)
n <- nrow(dados)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # Adicionar linha de corte

## REMOVENDO DADOS INFLUENTES 
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])
dados_new <- dados[-influential_obs, ]


### NOVO AJUSTE COM DADOS SEM PONTOS INFLUENTES

mod = glm(  formula = IT ~ PPDS + ATP + TVP + UE + CP + PEP 
, data = dados_new , family = poisson(link = 'log'),
control=list(trace=TRUE))
summary(mod)

#R2

pR2(mod)

### ANALISE DE RESIDUO
hist( mod$residuals )
qqnorm(mod$residuals)
qqline( mod$residuals, col = 'red' )
shapiro.test( mod$residuals )
plot(cooks.distance(mod))


cooksD = cooks.distance(mod)
n <- nrow(dados_new)
plot(cooksD, main = "Cooks Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue") # Adicionar linha de corte

## REMOVENDO DADOS INFLUENTE

# Identificar pontos influentes
influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])

# Definir novo quadro de dados com pontos influentes removidos
dados_new1 <- dados_new[-influential_obs, ]


mod = glm(  formula = IT ~ PPDS + ATP + TVP + UE + CP + PEP 
           , data = dados_new1 , family = poisson(link = 'log')
           ,control=list(trace=TRUE))
summary(mod)

#R2

pR2(mod)

### ANALISE DE RESIDUO
hist( mod$residuals )
qqnorm(mod$residuals)
qqline( mod$residuals, col = 'red' )
shapiro.test( mod$residuals )
acf(mod$residuals)
plot(cooks.distance(mod))


plot(dados_new1$IT, mod$fitted.values )


### RESULTADOS
 
### CONCLUSÃO
