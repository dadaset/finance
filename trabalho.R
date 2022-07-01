library(tidyquant)
library(ggplot2)
library(dplyr)

#puxando a SELIC
selic<-read.csv('taxa_selic_apurada.csv', sep = ";")
selic<-selic[-c(1), ]
selic$X<-as.numeric(gsub(",", ".", gsub("\\.", "", selic$X)))

setwd('C:/Users/dadaset/Desktop/Aulas/Finanças')
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)


# Fazendo o download de Petr3
getSymbols("PETR3.SA", from = '2017-01-01',
           to = "2022-06-23",warnings = FALSE,
           auto.assign = TRUE)
# lpuxando wege3
getSymbols("WEGE3.SA", from = '2017-01-01',
           to = "2022-06-23",warnings = FALSE,
           auto.assign = TRUE)

# Puxando bova11
getSymbols("BOVA11.SA", from = '2017-01-01',
           to = "2022-06-23",warnings = FALSE,
           auto.assign = TRUE)

# Puxando IBC-Br
ibcbr<-read.csv('STP-20220630165013366.csv')

#tranformar em data.frame
PETR3.SA<-as.data.frame(PETR3.SA)
WEGE3.SA<-as.data.frame(WEGE3.SA) 
selic<-as.data.frame(selic)
BOVA11.SA<-as.data.frame(BOVA11.SA)
ibcbr<-as.data.frame(ibcbr)

#trasnformando nome das linhas em coluna de data

PETR3.SA$data<-rownames(PETR3.SA)
WEGE3.SA$data<-rownames(WEGE3.SA) 
BOVA11.SA$data<-rownames(BOVA11.SA)
ibcbr$data<-rownames(ibcbr)

#fazendo limpeza das variáveis de data
ibcbr$data<-gsub('.{4}$', '', ibcbr$data)

#calculando o retorno dos ativos

ret_PETR4<-diff(PETR3.SA$PETR3.SA.Close)/PETR3.SA$PETR3.SA.Close[-length(PETR3.SA$PETR3.SA.Close)]
ret_WEGE3.SA<-diff(WEGE3.SA$WEGE3.SA.Close)/WEGE3.SA$WEGE3.SA.Close[-length(WEGE3.SA$WEGE3.SA.Close)]
ret_BOVA11.SA<-diff(BOVA11.SA$BOVA11.SA.Close)/BOVA11.SA$BOVA11.SA.Close[-length(BOVA11.SA$BOVA11.SA.Close)]
ret_ibcbr<-diff(PETR3.SA$PETR3.SA.Close)/PETR3.SA$PETR3.SA.Close[-length(PETR3.SA$PETR3.SA.Close)]
ret_selic <- selic$X

# X já é o retorno da selic


# calculado a media dos retornos
mu_petr<-mean(ret_PETR4)
mu_wege<-mean(ret_WEGE3.SA)
mu_bova11<-mean(ret_BOVA11.SA)
mu_ibcbr<-mean(ret_ibcbr)
mu_selic<-mean(ret_selic)-1


# plotando os graficos de bova11

p_bova <- ggplot(BOVA11.SA, aes(x=as.Date(data), y=BOVA11.SA.Close)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
plot(p_bova)

# plotando os graficos de prt3

p_pet <- ggplot(PETR3.SA, aes(x=as.Date(data), y=PETR3.SA.Close)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
plot(p_pet)

# plotando os graficos de wege

p_weg <- ggplot(WEGE3.SA, aes(x=as.Date(data), y=WEGE3.SA.Close)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
plot(p_weg)

# plotando os graficos de wege

p_weg <- ggplot(WEGE3.SA, aes(x=as.Date(data), y=WEGE3.SA.Close)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
plot(p_weg)

# plotando os retornos weg

p_ret_weg <- ggplot(as.data.frame(ret_WEGE3.SA),
                    aes(x=as.Date(WEGE3.SA$data[-length(WEGE3.SA$data)]), y=ret_WEGE3.SA)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
plot(p_ret_weg)


# plotanod os retornos da petro

p_ret_pet <- ggplot(as.data.frame(ret_PETR4),
                    aes(x=as.Date(WEGE3.SA$data[-length(WEGE3.SA$data)]), y=ret_PETR4)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
plot(p_ret_pet)



#limpando selic

ret_selic<-ret_selic[1:1360]

# calculando a correlação e variância entre os ativos

corr_12<-cor(ret_PETR4, ret_WEGE3.SA)
corr_13<- cor(ret_PETR4, ret_selic)
corr_23<-cor(ret_WEGE3.SA,ret_selic)
sigma_1<-sd(ret_PETR4)
sigma_2<-sd(ret_WEGE3.SA)
sigma_3<-sd(ret_selic)
sigma_11 <-  sigma_1^2 
sigma_22 <-  sigma_2^2 
sigma_33 <-  sigma_3^2 
sigma_12 <- corr_12*sigma_1*sigma_2
sigma_21 = sigma_12 
sigma_13 <- corr_13*sigma_1*sigma_3
sigma_31 = sigma_13 
sigma_23 <- corr_23*sigma_3*sigma_2
sigma_32 = sigma_23 


# vetores de retornos esperados
mu <-  matrix(c(mu_1, mu_2, mu_3), nrow = 3, ncol = 1) # vetor coluna
mu.t <- t(mu) # vetor linha


ones <- matrix(c(1, 1, 1), nrow = 3, ncol = 1) # vetor coluna
ones.t <- t(ones) # vetor linha


# matriz de var-cov
sigma <- matrix(c(sigma_11,sigma_12, sigma_13, sigma_21, sigma_22, sigma_23, sigma_31, sigma_32, sigma_33), nrow = 3, ncol = 3, byrow=TRUE)
sigma.inv<-solve(sigma)

# fun��o criada para gerar o vetor coluna de pesos �timos (ver notas de aula para entender as vari�veis abaixo)

omega.otimo <- function(rp) {
  denominador <- (mu.t%*%sigma.inv%*%mu)%*%(ones.t%*%sigma.inv%*%ones)-(ones.t%*%sigma.inv%*%mu)^2
  numerador.lambda.1 <-(ones.t%*%sigma.inv%*%ones)*rp-(ones.t%*%sigma.inv%*%mu)
  numerador.lambda.2 <- (mu.t%*%sigma.inv%*%mu)-(ones.t%*%sigma.inv%*%mu)*rp
  lambda_1 <- as.numeric(numerador.lambda.1/denominador)
  lambda_2 <- as.numeric(numerador.lambda.2/denominador)
  omega_otimo <- (lambda_1*(sigma.inv%*%mu))+(lambda_2*(sigma.inv%*%ones))
  omega_otimo
}



var.carteira <-function(rp) {
  t(omega.otimo(rp))%*%sigma%*%omega.otimo(rp)
}

# parametros para gerar o eixo vertical do grafico do conjunto de oportunidades

return.min <- 0
return.max <- 0.15
steps <- 0.001
return <- seq(from=return.min, to=return.max, by=steps)

# Loop para gerar a sequ�ncia de desvios-padr�o m�nimos (via Markowitz, fun��o var.carteira(rp)) para cada retorno esperado

sd.min <- c()

for (rp in return) {
  x <- var.carteira(rp)^(1/2)
  sd.min <- c(sd.min,x)
}

par(mar = rep(2, 4))

# gera o gr�fico desejado: conjunto de oportunidades
plot(sd.min, return, type = "l",                                 
     main = "Conjunto de Oportunidades",
     xlab = "Desvio Padrão",
     ylab = "Retorno Esperado",
     col = "black",
     lwd = 6)































