#BIBLIOTECAS

install.packages("ACDm")
install.packages("zipfR")
library(ACDm)
library(zipfR)
library(forecast)
library(ggplot2)
library(gridExtra)

#IMPORTANDO OS BANCOS DE DADOS

mydata = read.csv("C:/Users/User/Downloads/AAPL.USUSD_Ticks_15.08.2023-15.08.2023.csv", header=T)
apple<-mydata 
mydata = read.csv("C:/Users/User/Downloads/BAYN.DEEUR_Ticks_12.04.2024-12.04.2024.csv", header=T)
bayn<-mydata
mydata = read.csv("C:/Users/User/Downloads/BAS.DE_Ticks_19.04.2016-19.04.2016.csv", header=T)
base<-mydata


#ALTERANDO O BANCO 

apple$time = apple$Local.time
apple$price = (apple$Ask+apple$Bid)/2
apple$volume = max(apple$AskVolume, apple$BidVolume)

bayn$time = bayn$Local.time
bayn$price = (bayn$Ask+bayn$Bid)/2
bayn$volume = max(bayn$AskVolume, bayn$BidVolume)

base$time = base$Local.time
base$price = (base$Ask+base$Bid)/2
base$volume = max(base$AskVolume, base$BidVolume)

#REMOVENDO VARIAVEIS

apple$Local.time = NULL
apple$Ask = NULL
apple$Bid = NULL
apple$AskVolume = NULL
apple$BidVolume = NULL

bayn$Local.time = NULL
bayn$Ask = NULL
bayn$Bid = NULL
bayn$AskVolume = NULL
bayn$BidVolume = NULL

base$Local.time = NULL
base$Ask = NULL
base$Bid = NULL
base$AskVolume = NULL
base$BidVolume = NULL

#ALTERANDO O HORARIO

apple$time = as.factor(substr(strptime(apple$time, "%d.%m.%Y %H:%M:%S"),1,25))
bayn$time = as.factor(substr(strptime(bayn$time, "%d.%m.%Y %H:%M:%S"),1,25))
base$time = as.factor(substr(strptime(base$time, "%d.%m.%Y %H:%M:%S"),1,25))

#COMPUTANDO AS DURACOES

appleData <- computeDurations(apple, 
                                 open = "9:00:00", close = "14:30:00",
                                 rm0dur =TRUE, type = "price", priceDiff = 0.01, cumVol = 10000)
baynData <- computeDurations(bayn, 
                                 open = "4:30:00", close = "12:00:00",
                                 rm0dur =TRUE, type = "price", priceDiff = 0.01, cumVol = 10000)
baseData <- computeDurations(base, 
                                 open = "8:30:00", close = "16:00:00",
                                 rm0dur =TRUE, type = "price", priceDiff = 0.01, cumVol = 10000)

#OLHANDO OS DADOS

#APPLE

str(appleData)   
head(appleData)
summary(appleData)

#FIGURA 1
fig1 <- ggplot(appleData, aes(x = time, y = durations)) +
  geom_step() +
  labs(x = "Tempo", y = "Duração",
       title = "")
fig1 + scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

#BAYN

str(baynData)   
head(baynData)
summary(baynData)

#FIGURA 2
fig2 <- ggplot(baynData, aes(x = time, y = durations)) +
  geom_step() +
  labs(x = "Tempo", y = "Duração",
       title = "")
fig2 + scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

#BASE

str(baseData)   
head(baseData)
summary(baseData)

#FIGURA 3
fig3 <- ggplot(baseData, aes(x = time, y = durations)) +
  geom_step() +
  labs(x = "Tempo", y = "Duração",
       title = "")
fig3 + scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")


#RETIRANDO SAZONALIDADE DIARIA
#APPLE

appleAdj = diurnalAdj(appleData, aggregation = "all",  method = "supsmu")
durapple <- as.vector(appleAdj$adjDur)
orgapple<-as.vector(appleAdj$durations)
sazapple<-orgapple-durapple
appleAdj$time <- as.POSIXct(appleAdj$time)
applefinal<-orgapple[(length(orgapple)-49):length(orgapple)]
durapple<-durapple[1:(length(durapple)-50)]

#FIGURA 4
fig4 <- ggplot(appleAdj, aes(x = time, y = adjDur)) +
  geom_step() +
  labs(x = "Tempo", y = "Duração ajustada",
       title = "")
fig4 + scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

#HISTOGRAMA 1
ggplot(appleAdj, aes(x = adjDur)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  labs(x = "Duração Ajustada", y = "Frequência",
       title = "")


#BAYN

baynAdj = diurnalAdj(baynData, aggregation = "all",  method = "supsmu")
durbayn <- as.vector(baynAdj$adjDur) 
orgbayn<-as.vector(baynAdj$durations)
sazbayn<-orgbayn-durbayn
baynAdj$time <- as.POSIXct(baynAdj$time)
baynfinal<-orgbayn[(length(orgbayn)-49):length(orgbayn)]
durbayn<-durbayn[1:(length(durbayn)-50)]
#FIGURA 5
fig5 <- ggplot(baynAdj, aes(x = time, y = adjDur)) +
  geom_step() +
  labs(x = "Tempo", y = "Duração ajustada",
       title = "")
fig5 + scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

#HISTOGRAMA 2
ggplot(baynAdj, aes(x = adjDur)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  labs(x = "Duração Ajustada", y = "Frequência",
       title = "")

#BASE

baseAdj = diurnalAdj(baseData, aggregation = "all",  method = "supsmu")
durbase <- as.vector(baseAdj$adjDur) 
orgbase<-as.vector(baseAdj$durations)
sazbase<-orgbase-durbase
baseAdj$time <- as.POSIXct(baseAdj$time)
basefinal<-orgbase[(length(orgbase)-49):length(orgbase)]
durbase<-durbase[1:(length(durbase)-50)]

#FIGURA 6
fig6 <- ggplot(baseAdj, aes(x = time, y = adjDur)) +
  geom_step() +
  labs(x = "Tempo", y = "Duração ajustada",
       title = "")
fig6 + scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

#HISTOGRAMA 3
ggplot(baseAdj, aes(x = adjDur)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") +
  labs(x = "Duração Ajustada", y = "Frequência",
       title = "")

################################################################################################################################################
#MODELOS E ADEQUACOES
#########################################################################################################################################################

#APPLE

#EXPONENCIAL
appleexp <- acdFit(durations = durapple, model = "ACD",dist = "exponential", order = c(1,1))
acf_acd(appleexp, conf_level = 0.95, max = 50, min = 1)
plotHazard(appleexp)
qqplotAcd(appleexp, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(appleexp, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col="red")
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
appleexp$goodnessOfFit

#WEIBULL
applewei <- acdFit(durations = durapple, model = "ACD",dist = "weibull", order = c(1,1))
acf_acd(applewei)
plotHazard(applewei)
qqplotAcd(applewei, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(applewei, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col="red")
Box.test (applewei$residuals, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
applewei$goodnessOfFit

#BURR
applebur <- acdFit(durations = durapple, model = "ACD",dist = "burr", order = c(1,1))
acf_acd(applebur)
plotHazard(applebur)
qqplotAcd(applebur, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(applebur, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
applebur$goodnessOfFit

#GAMMA-GENERALIZADA
applegam <- acdFit(durations = durapple, model = "ACD",dist = "gengamma", order = c(1,1),startPara = c(0.0349,0.0246,0.9418,19,1.2426))
acf_acd(applegam)
plotHazard(applegam)
qqplotAcd(applegam, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(applegam, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
applegam$goodnessOfFit


#BAYN

#EXPONENCIAL
baynexp <- acdFit(durations = durbayn, model = "ACD",dist = "exponential", order = c(1,1))
acf_acd(baynexp, conf_level = 0.95, max = 50, min = 1)
plotHazard(baynexp)
qqplotAcd(baynexp, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(baynexp, transformation = "cox-snell")
qqplot(quantis_teoricos,amostra)
abline(0,1,col="red")
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
baynexp$goodnessOfFit

#WEIBULL
baynwei <- acdFit(durations = durbayn, model = "ACD",dist = "weibull", order = c(1,1),forceErrExpec = TRUE)
acf_acd(baynwei)
plotHazard(baynwei)
qqplotAcd(baynwei, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(baynwei, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col="red")
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
baynwei$goodnessOfFit

#BURR
baynbur <- acdFit(durations = durbayn, model = "ACD",dist = "burr", order = c(1,1))
acf_acd(baynbur)
plotHazard(baynbur)
qqplotAcd(baynbur, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(baynbur, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
baynbur$goodnessOfFit

#GAMMA-GENERALIZADA
bayngam <- acdFit(durations = durbayn, model = "ACD",dist = "gengamma", order = c(1,1))
acf_acd(bayngam)
plotHazard(bayngam)
qqplotAcd(bayngam, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(bayngam, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
bayngam$goodnessOfFit


#BASE

#EXPONENCIAL
baseexp <- acdFit(durations = durbase, model = "ACD",dist = "exponential", order = c(1,1))
acf_acd(baseexp)
plotHazard(baseexp)
qqplotAcd(baseexp, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(baseexp, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
baseexp$goodnessOfFit

#WEIBULL
basewei <- acdFit(durations = durbase, model = "ACD",dist = "weibull", order = c(1,1))
acf_acd(basewei)
plotHazard(basewei)
qqplotAcd(basewei, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(basewei, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
basewei$goodnessOfFit

#BURR
basebur <- acdFit(durations = durbase, model = "ACD",dist = "burr", order = c(1,1))
acf_acd(basebur)
plotHazard(basebur)
qqplotAcd(basebur, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(basebur, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
basebur$goodnessOfFit

#GAMMA-GENERALIZADA
basegam <- acdFit(durations = durbase, model = "ACD",dist = "gengamma", order = c(1,1))
acf_acd(basegam)
plotHazard(basegam)
qqplotAcd(basegam, xlim = NULL, ylim = NULL)
amostra<-standardizeResi(basegam, transformation = "cox-snell")
quantis_teoricos<-qexp(ppoints(length(amostra)))
qqplot(quantis_teoricos,amostra)
abline(0,1,col='red')
Box.test (amostra, lag = 4 , type="Ljung")
Box.test (amostra, lag = 16, type="Ljung")
basegam$goodnessOfFit

##############################################################
## MBB
##############################################################
mbb<-function(x,n,t){
     # Definir o tamanho do bloco
    block_size <- t
    # Definir o número de versões bootstrap
    num <- n
    # Criar uma lista vazia para armazenar as versões bootstrap
    boot_list <- list()
    # Incluir a série original na lista
    boot_list[[1]] <- x
    # Gerar as versões bootstrap usando um loop
    for (i in 2:num) {
      # Obter o número de blocos na série
      n_blocks <- length(x) %/% block_size
      # Obter os índices dos blocos
      block_index <- sample(1:n_blocks, n_blocks, replace = TRUE)
      # Obter os índices dos elementos da série
      element_index <- rep(block_index, each = block_size) * block_size - block_size + 1:block_size
      # Obter a versão bootstrap da série
      boot_series <- x[element_index]
      # Ajustar o início e o fim da série
      boot_series <- ts(boot_series, start = start(x), end = end(x), frequency = frequency(x))
      # Adicionar a versão bootstrap na lista
      boot_list[[i]] <- boot_series
    }
    return(boot_list)
}

set.seed(123)  # Para reprodutibilidade
apple1<-mbb(durapple,50,5)
apple2<-mbb(durapple,50,10)
apple3<-mbb(durapple,50,20)
apple4<-mbb(durapple,50,30)
apple5<-mbb(durapple,50,40)
apple6<-mbb(durapple,50,50)

bayn1<-mbb(durbayn,50,5)
bayn2<-mbb(durbayn,50,10)
bayn3<-mbb(durbayn,50,20)
bayn4<-mbb(durbayn,50,30)
bayn5<-mbb(durbayn,50,40)
bayn6<-mbb(durbayn,50,50)

base1<-mbb(durbase,50,5)
base2<-mbb(durbase,50,10)
base3<-mbb(durbase,50,20)
base4<-mbb(durbase,50,30)
base5<-mbb(durbase,50,40)
base6<-mbb(durbase,50,50)


#####################################################################################################
# MODELOS COM BAGGING
#####################################################################################################
# Criar uma lista vazia
mod<-function(boot_list,modelo){
    model <- list()
    list_data <- list()
    for (j in 1:length(boot_list)) {
      # Atribuir o vetor da lista à variável mydata
      ok <- boot_list[[j]]
      # Definir os dados de firstmydata usando a variável mydata
      list_data[[j]] <- as.vector(ok)
      model[[j]] <- acdFit(durations = list_data[[j]], model = "ACD",dist =paste(modelo), order = c(1,1))
    }
    # Aplicar a função is.null a cada elemento da lista model
    nulls <- sapply(model, is.null)
    # Obter os índices dos elementos que têm como tipo NULL
    indices <- which(nulls)
    # Retirar os elementos que estão nos índices 2 e 4
    list_data <- list_data[-indices]
    # Retirar os elementos que têm como tipo NULL
    model <- Filter(Negate(is.null), model)
    return(model)
}

#APLICANDO
#APPLE
#EXP
expapple1<-mod(apple1,"exponential")
expapple2<-mod(apple2,"exponential")
expapple3<-mod(apple3,"exponential")
expapple4<-mod(apple4,"exponential")
expapple5<-mod(apple5,"exponential")
expapple6<-mod(apple6,"exponential")

#WEI
weiapple1<-mod(apple1,"weibull")
weiapple2<-mod(apple2,"weibull")
weiapple3<-mod(apple3,"weibull")
weiapple4<-mod(apple4,"weibull")
weiapple5<-mod(apple5,"weibull")
weiapple6<-mod(apple6,"weibull")

#BURR
burrapple1<-mod(apple1,"burr")
burrapple2<-mod(apple2,"burr")
burrapple3<-mod(apple3,"burr")
burrapple4<-mod(apple4,"burr")
burrapple5<-mod(apple5,"burr")
burrapple6<-mod(apple6,"burr")

#GAMMA
gengapple1<-mod(apple1,"gengamma")
gengapple2<-mod(apple2,"gengamma")
gengapple3<-mod(apple3,"gengamma")
gengapple4<-mod(apple4,"gengamma")
gengapple5<-mod(apple5,"gengamma")
gengapple6<-mod(apple6,"gengamma")

#BAYN
#EXP
expbayn1<-mod(bayn1,"exponential")
expbayn2<-mod(bayn2,"exponential")
expbayn3<-mod(bayn3,"exponential")
expbayn4<-mod(bayn4,"exponential")
expbayn5<-mod(bayn5,"exponential")
expbayn6<-mod(bayn6,"exponential")

#WEI
weibayn1<-mod(bayn1,"weibull")
weibayn2<-mod(bayn2,"weibull")
weibayn3<-mod(bayn3,"weibull")
weibayn4<-mod(bayn4,"weibull")
weibayn5<-mod(bayn5,"weibull")
weibayn6<-mod(bayn6,"weibull")

#BURR
burrbayn1<-mod(bayn1,"burr")
burrbayn2<-mod(bayn2,"burr")
burrbayn3<-mod(bayn3,"burr")
burrbayn4<-mod(bayn4,"burr")
burrbayn5<-mod(bayn5,"burr")
burrbayn6<-mod(bayn6,"burr")

#GAMMA
gengbayn1<-mod(bayn1,"gengamma")
gengbayn2<-mod(bayn2,"gengamma")
gengbayn3<-mod(bayn3,"gengamma")
gengbayn4<-mod(bayn4,"gengamma")
gengbayn5<-mod(bayn5,"gengamma")
gengbayn6<-mod(bayn6,"gengamma")

#BASE
#EXP
expbase1<-mod(base1,"exponential")
expbase2<-mod(base2,"exponential")
expbase3<-mod(base3,"exponential")
expbase4<-mod(base4,"exponential")
expbase5<-mod(base5,"exponential")
expbase6<-mod(base6,"exponential")

#WEI
weibase1<-mod(base1,"weibull")
weibase2<-mod(base2,"weibull")
weibase3<-mod(base3,"weibull")
weibase4<-mod(base4,"weibull")
weibase5<-mod(base5,"weibull")
weibase6<-mod(base6,"weibull")

#BURR
burrbase1<-mod(base1,"burr")
burrbase2<-mod(base2,"burr")
burrbase3<-mod(base3,"burr")
burrbase4<-mod(base4,"burr")
burrbase5<-mod(base5,"burr")
burrbase6<-mod(base6,"burr")

#GAMMA
gengbase1<-mod(base1,"gengamma")
gengbase2<-mod(base2,"gengamma")
gengbase3<-mod(base3,"gengamma")
gengbase4<-mod(base4,"gengamma")
gengbase5<-mod(base5,"gengamma")
gengbase6<-mod(base6,"gengamma")

#####################################################################################################
# PREVISOES 
#####################################################################################################
# Fazer um loop para cada vetor da lista boot_list
prevfun<-function(dados,modelo){
  h<-length(modelo)
  phi <- as.data.frame(matrix(0, nrow = h, ncol = 50))
  n.ahead = 50
  last_value <- tail(dados, 1)
  prev<-c(1)
    for (j in 1:length(modelo)) {
      usando <- modelo[[j]]
      # Extrair os parâmetros estimados do modelo
      omega <- as.numeric(usando$mPar[1])
      beta1 <- as.numeric(usando$mPar[3])
      alpha1 <- as.numeric(usando$mPar[2])      
      # Inicializar um vetor para armazenar as previsões
      phi[j,1] <- omega + alpha1 * last_value + beta1 * mean(dados)
          # Fazer um loop para cada passo de previsão
          for (i in 2:n.ahead) {
            # Calcular o valor esperado condicional da duração
            phi[j,i] <- omega + alpha1 * phi[j,i-1] + beta1 * phi[j,i-1]
          }
        
    }
  print(phi)
}
  
#APPLE
#EXPONENCIAL

prevappleexp1<-prevfun(durapple,expapple1)
prevappleexp2<-prevfun(durapple,expapple2)
prevappleexp3<-prevfun(durapple,expapple3)
prevappleexp4<-prevfun(durapple,expapple4)
prevappleexp5<-prevfun(durapple,expapple5)
prevappleexp6<-prevfun(durapple,expapple6)

#WEIBULL

prevapplewei1<-prevfun(durapple,weiapple1)
prevapplewei2<-prevfun(durapple,weiapple2)
prevapplewei3<-prevfun(durapple,weiapple3)
prevapplewei4<-prevfun(durapple,weiapple4)
prevapplewei5<-prevfun(durapple,weiapple5)
prevapplewei6<-prevfun(durapple,weiapple6)

#BURR

prevapplebur1<-prevfun(durapple,burrapple1)
prevapplebur2<-prevfun(durapple,burrapple2)
prevapplebur3<-prevfun(durapple,burrapple3)
prevapplebur3<-prevapplebur3[-10,]
prevapplebur4<-prevfun(durapple,burrapple4)
prevapplebur4<-prevapplebur4[-30,]
prevapplebur5<-prevfun(durapple,burrapple5)
prevapplebur6<-prevfun(durapple,burrapple6)
prevapplebur6<-prevapplebur6[-7,]

#GAMMA-GENERALIZADA

prevapplegam1<-prevfun(durapple,gengapple1)
prevapplegam2<-prevfun(durapple,gengapple2)
prevapplegam3<-prevfun(durapple,gengapple3)
prevapplegam4<-prevfun(durapple,gengapple4)
prevapplegam5<-prevfun(durapple,gengapple5)
prevapplegam6<-prevfun(durapple,gengapple6)

#BAYN

#EXPONENCIAL

prevbaynexp1<-prevfun(durbayn,expbayn1)
prevbaynexp2<-prevfun(durbayn,expbayn2)
prevbaynexp3<-prevfun(durbayn,expbayn3)
prevbaynexp4<-prevfun(durbayn,expbayn4)
prevbaynexp5<-prevfun(durbayn,expbayn5)
prevbaynexp6<-prevfun(durbayn,expbayn6)

#WEIBULL

prevbaynwei1<-prevfun(durbayn,weibayn1)
prevbaynwei2<-prevfun(durbayn,weibayn2)
prevbaynwei3<-prevfun(durbayn,weibayn3)
prevbaynwei4<-prevfun(durbayn,weibayn4)
prevbaynwei5<-prevfun(durbayn,weibayn5)
prevbaynwei6<-prevfun(durbayn,weibayn6)

#BURR

prevbaynbur1<-prevfun(durbayn,burrbayn1)
prevbaynbur2<-prevfun(durbayn,burrbayn2)
prevbaynbur3<-prevfun(durbayn,burrbayn3)
prevbaynbur4<-prevfun(durbayn,burrbayn4)
prevbaynbur5<-prevfun(durbayn,burrbayn5)
prevbaynbur6<-prevfun(durbayn,burrbayn6)

#GAMMA-GENERALIZADA

prevbayngam1<-prevfun(durbayn,gengbayn1)
prevbayngam2<-prevfun(durbayn,gengbayn2)
prevbayngam3<-prevfun(durbayn,gengbayn3)
prevbayngam4<-prevfun(durbayn,gengbayn4)
prevbayngam5<-prevfun(durbayn,gengbayn5)
prevbayngam6<-prevfun(durbayn,gengbayn6)

#BASE

#EXPONENCIAL

prevbaseexp1<-prevfun(durbase,expbase1)
prevbaseexp2<-prevfun(durbase,expbase2)
prevbaseexp3<-prevfun(durbase,expbase3)
prevbaseexp4<-prevfun(durbase,expbase4)
prevbaseexp5<-prevfun(durbase,expbase5)
prevbaseexp6<-prevfun(durbase,expbase6)

#WEIBULL

prevbasewei1<-prevfun(durbase,weibase1)
prevbasewei2<-prevfun(durbase,weibase2)
prevbasewei3<-prevfun(durbase,weibase3)
prevbasewei4<-prevfun(durbase,weibase4)
prevbasewei5<-prevfun(durbase,weibase5)
prevbasewei6<-prevfun(durbase,weibase6)

#BURR

prevbasebur1<-prevfun(durbase,burrbase1)
prevbasebur2<-prevfun(durbase,burrbase2)
prevbasebur3<-prevfun(durbase,burrbase3)
prevbasebur4<-prevfun(durbase,burrbase4)
prevbasebur5<-prevfun(durbase,burrbase5)
prevbasebur6<-prevfun(durbase,burrbase6)

#GAMMA-GENERALIZADA

prevbasegam1<-prevfun(durbase,gengbase1)
prevbasegam2<-prevfun(durbase,gengbase2)
prevbasegam3<-prevfun(durbase,gengbase3)
prevbasegam4<-prevfun(durbase,gengbase4)
prevbasegam5<-prevfun(durbase,gengbase5)
prevbasegam6<-prevfun(durbase,gengbase6)

#####################################################################################################################
#PREVISOES SEM BAGGING
#####################################################################################################################
#APPLE
prevappleexp<-as.numeric(prevappleexp1[1,])
prevapplewei<-as.numeric(prevapplewei1[1,])
prevapplebur<-as.numeric(prevapplebur1[1,])
prevapplegam<-as.numeric(prevapplegam1[1,])

#BAYN
prevbaynexp<-as.numeric(prevbaynexp1[1,])
prevbaynwei<-as.numeric(prevbaynwei1[1,])
prevbaynbur<-as.numeric(prevbaynbur1[1,])
prevbayngam<-as.numeric(prevbayngam1[1,])

#BASE
prevbaseexp<-as.numeric(prevbaseexp1[1,])
prevbasewei<-as.numeric(prevbasewei1[1,])
prevbasebur<-as.numeric(prevbasebur1[1,])
prevbasegam<-as.numeric(prevbasegam1[1,])

#####################################################################################################################
#PREVISOES
#####################################################################################################################
#APPLE
#EXPONENCIAL

prevappleexp1<-colMeans(prevappleexp1)
prevappleexp2<-colMeans(prevappleexp2)
prevappleexp3<-colMeans(prevappleexp3)
prevappleexp4<-colMeans(prevappleexp4)
prevappleexp5<-colMeans(prevappleexp5)
prevappleexp6<-colMeans(prevappleexp6)

#WEIBULL

prevapplewei1<-colMeans(prevapplewei1)
prevapplewei2<-colMeans(prevapplewei2)
prevapplewei3<-colMeans(prevapplewei3)
prevapplewei4<-colMeans(prevapplewei4)
prevapplewei5<-colMeans(prevapplewei5)
prevapplewei6<-colMeans(prevapplewei6)

#BURR

prevapplebur1<-colMeans(prevapplebur1)
prevapplebur2<-colMeans(prevapplebur2)
prevapplebur3<-colMeans(prevapplebur3)
prevapplebur4<-colMeans(prevapplebur4)
prevapplebur5<-colMeans(prevapplebur5)
prevapplebur6<-colMeans(prevapplebur6)

#GAMMA-GENERALIZADA

prevapplegam1<-colMeans(prevapplegam1)
prevapplegam2<-colMeans(prevapplegam2)
prevapplegam3<-colMeans(prevapplegam3)
prevapplegam4<-colMeans(prevapplegam4)
prevapplegam5<-colMeans(prevapplegam5)
prevapplegam6<-colMeans(prevapplegam6)

#BAYN

#EXPONENCIAL

prevbaynexp1<-colMeans(prevbaynexp1)
prevbaynexp2<-colMeans(prevbaynexp2)
prevbaynexp3<-colMeans(prevbaynexp3)
prevbaynexp4<-colMeans(prevbaynexp4)
prevbaynexp5<-colMeans(prevbaynexp5)
prevbaynexp6<-colMeans(prevbaynexp6)

#WEIBULL

prevbaynwei1<-colMeans(prevbaynwei1)
prevbaynwei2<-colMeans(prevbaynwei2)
prevbaynwei3<-colMeans(prevbaynwei3)
prevbaynwei4<-colMeans(prevbaynwei4)
prevbaynwei5<-colMeans(prevbaynwei5)
prevbaynwei6<-colMeans(prevbaynwei6)

#BURR

prevbaynbur1<-colMeans(prevbaynbur1)
prevbaynbur2<-colMeans(prevbaynbur2)
prevbaynbur3<-colMeans(prevbaynbur3)
prevbaynbur4<-colMeans(prevbaynbur4)
prevbaynbur5<-colMeans(prevbaynbur5)
prevbaynbur6<-colMeans(prevbaynbur6)

#GAMMA-GENERALIZADA

prevbayngam1<-colMeans(prevbayngam1)
prevbayngam2<-colMeans(prevbayngam2)
prevbayngam3<-colMeans(prevbayngam3)
prevbayngam4<-colMeans(prevbayngam4)
prevbayngam5<-colMeans(prevbayngam5)
prevbayngam6<-colMeans(prevbayngam6)

#BASE

#EXPONENCIAL

prevbaseexp1<-colMeans(prevbaseexp1)
prevbaseexp2<-colMeans(prevbaseexp2)
prevbaseexp3<-colMeans(prevbaseexp3)
prevbaseexp4<-colMeans(prevbaseexp4)
prevbaseexp5<-colMeans(prevbaseexp5)
prevbaseexp6<-colMeans(prevbaseexp6)

#WEIBULL

prevbasewei1<-colMeans(prevbasewei1)
prevbasewei2<-colMeans(prevbasewei2)
prevbasewei3<-colMeans(prevbasewei3)
prevbasewei4<-colMeans(prevbasewei4)
prevbasewei5<-colMeans(prevbasewei5)
prevbasewei6<-colMeans(prevbasewei6)

#BURR

prevbasebur1<-colMeans(prevbasebur1)
prevbasebur2<-colMeans(prevbasebur2)
prevbasebur3<-colMeans(prevbasebur3)
prevbasebur4<-colMeans(prevbasebur4)
prevbasebur5<-colMeans(prevbasebur5)
prevbasebur6<-colMeans(prevbasebur6)

#GAMMA-GENERALIZADA

prevbasegam1<-colMeans(prevbasegam1)
prevbasegam2<-colMeans(prevbasegam2)
prevbasegam3<-colMeans(prevbasegam3)
prevbasegam4<-colMeans(prevbasegam4)
prevbasegam5<-colMeans(prevbasegam5)
prevbasegam6<-colMeans(prevbasegam6)

####################################################################################################
#GRAFICOS BAGGING
####################################################################################################
grafico<-function(x){
      teste<-x+sazapple[6640:6689]
      graf <- cbind(applefinal,teste )
      j<-accuracy(teste,applefinal)
      print(j)
      # Criar um dataframe a partir da matriz grafico
      df <- data.frame(graf)
      # Adicionar uma coluna de tempo ao dataframe
      df$Tempo <- appleAdj$time[6640:6689] # Substitua isso pela sua variável de tempo
      
      # Converter o dataframe para o formato longo
      df_long <- tidyr::pivot_longer(df, -Tempo, names_to = "Linha", values_to = "Valor")
      # Plotar os dados usando ggplot2
      ggplot(df_long, aes(x = Tempo, y = Valor, color = Linha)) +
        geom_line() +
        labs(x = "Tempo", y = "Duração", title = "Gráfico de linhas") +
        guides(color = FALSE) +
        scale_x_datetime(date_breaks = "1 min", date_labels = "%H:%M")
     
}

#APPLE

#EXPONENCIAL
grafico(prevappleexp)
##BLOCO1
grafico(prevappleexp1)
##BLOCO2
grafico(prevappleexp2)
#BLOCO3
grafico(prevappleexp3)
#BLOCO4
grafico(prevappleexp4)
#BLOCO5
grafico(prevappleexp5)
#BLOCO6
grafico(prevappleexp6)


#WEIBULL
grafico(prevapplewei)
#BLOCO1
grafico(prevapplewei1)
##BLOCO2
grafico(prevapplewei2)
#BLOCO3
grafico(prevapplewei3)
#BLOCO4
grafico(prevapplewei4)
#BLOCO5
grafico(prevapplewei5)
#BLOCO6
grafico(prevapplewei6)


#BURR
grafico(prevapplebur)
#BLOCO1
grafico(prevapplebur1)
##BLOCO2
grafico(prevapplebur2)
#BLOCO3
grafico(prevapplebur3)
#BLOCO4
grafico(prevapplebur4)
#BLOCO5
grafico(prevapplebur5)
#BLOCO6
grafico(prevapplebur6)


#GAMMA
grafico(prevapplegam)
#BLOCO1
grafico(prevapplegam1)
##BLOCO2
grafico(prevapplegam2)
#BLOCO3
grafico(prevapplegam3)
#BLOCO4
grafico(prevapplegam4)
#BLOCO5
grafico(prevapplegam5)
#BLOCO6
grafico(prevapplegam6)

#BAYN
grafico<-function(x){
  teste<-x+sazbayn[1941:1990]
  graf <- cbind(baynfinal,teste )
  j<-accuracy(teste,baynfinal)
  print(j)
  # Criar um dataframe a partir da matriz grafico
  df <- data.frame(graf)
  # Adicionar uma coluna de tempo ao dataframe
  df$Tempo <- baynAdj$time[1941:1990] # Substitua isso pela sua variável de tempo
  
  # Converter o dataframe para o formato longo
  df_long <- tidyr::pivot_longer(df, -Tempo, names_to = "Linha", values_to = "Valor")
  # Plotar os dados usando ggplot2
  ggplot(df_long, aes(x = Tempo, y = Valor, color = Linha)) +
    geom_line() +
    labs(x = "Tempo", y = "Duração", title = "Gráfico de linhas") +
    guides(color = FALSE) +
    scale_x_datetime(date_breaks = "1 min", date_labels = "%H:%M")
}
#EXPONENCIAL
grafico(prevbaynexp)
#BLOCO1
grafico(prevbaynexp1)
##BLOCO2
grafico(prevbaynexp2)
#BLOCO3
grafico(prevbaynexp3)
#BLOCO4
grafico(prevbaynexp4)
#BLOCO5
grafico(prevbaynexp5)
#BLOCO6
grafico(prevbaynexp6)

#WEIBULL
grafico(prevbaynwei)
#BLOCO1
grafico(prevbaynwei1)
##BLOCO2
grafico(prevbaynwei2)
#BLOCO3
grafico(prevbaynwei3)
#BLOCO4
grafico(prevbaynwei4)
#BLOCO5
grafico(prevbaynwei5)
#BLOCO6
grafico(prevbaynwei6)

#BURR
grafico(prevbaynbur)
#BLOCO1
grafico(prevbaynbur1)
##BLOCO2
grafico(prevbaynbur2)
#BLOCO3
grafico(prevbaynbur3)
#BLOCO4
grafico(prevbaynbur4)
#BLOCO5
grafico(prevbaynbur5)
#BLOCO6
grafico(prevbaynbur6)

#GAMMA
grafico(prevbayngam)
#BLOCO1
grafico(prevbayngam1)
##BLOCO2
grafico(prevbayngam2)
#BLOCO3
grafico(prevbayngam3)
#BLOCO4
grafico(prevbayngam4)
#BLOCO5
grafico(prevbayngam5)
#BLOCO6
grafico(prevbayngam6)

#BASE
grafico<-function(x){
  teste<-x+sazbase[2147:2196]
  graf <- cbind(basefinal,teste)
  j<-accuracy(teste,basefinal)
  print(j)
  # Criar um dataframe a partir da matriz grafico
  df <- data.frame(graf)
  # Adicionar uma coluna de tempo ao dataframe
  df$Tempo <- baseAdj$time[2147:2196] # Substitua isso pela sua variável de tempo
  
  # Converter o dataframe para o formato longo
  df_long <- tidyr::pivot_longer(df, -Tempo, names_to = "Linha", values_to = "Valor")
  # Plotar os dados usando ggplot2
  ggplot(df_long, aes(x = Tempo, y = Valor, color = Linha)) +
    geom_line() +
    labs(x = "Tempo", y = "Duração", title = "Gráfico de linhas") +
    guides(color = FALSE) +
    scale_x_datetime(date_breaks = "1 min", date_labels = "%H:%M")
}

#EXPONENCIAL
grafico(prevbaseexp)
#BLOCO1
grafico(prevbaseexp1)
##BLOCO2
grafico(prevbaseexp2)
#BLOCO3
grafico(prevbaseexp3)
#BLOCO4
grafico(prevbaseexp4)
#BLOCO5
grafico(prevbaseexp5)
#BLOCO6
grafico(prevbaseexp6)

#WEIBULL
grafico(prevbasewei)
#BLOCO1
grafico(prevbasewei1)
##BLOCO2
grafico(prevbasewei2)
#BLOCO3
grafico(prevbasewei3)
#BLOCO4
grafico(prevbasewei4)
#BLOCO5
grafico(prevbasewei5)
#BLOCO6
grafico(prevbasewei6)

#BURR
grafico(prevbasebur)
#BLOCO1
grafico(prevbasebur1)
##BLOCO2
grafico(prevbasebur2)
#BLOCO3
grafico(prevbasebur3)
#BLOCO4
grafico(prevbasebur4)
#BLOCO5
grafico(prevbasebur5)
#BLOCO6
grafico(prevbasebur6)

#GAMMA
grafico(prevbasegam)
#BLOCO1
grafico(prevbasegam1)
##BLOCO2
grafico(prevbasegam2)
#BLOCO3
grafico(prevbasegam3)
#BLOCO4
grafico(prevbasegam4)
#BLOCO5
grafico(prevbasegam5)
#BLOCO6
grafico(prevbasegam6)

################################################################################################################
## Visualização do Bootstrap
################################################################################################################
# Visualização
# reduzindo a quantidade apenas para facilitar a visualização
# Usar a função lapply para selecionar os 50 primeiros valores de cada vetor da lista
selecao <- lapply(base1, function(x) x[1:50])
# Visualizar a seleção
selecao
# Adicionar a sazonalidade aos dados
# Usar a função sapply para somar o vetor sazonalidade com cada vetor da lista
resultado <- sapply(selecao, function(x) sazbase[1:50] + x)
# Carregar a biblioteca ggplot2
library(ggplot2)
# Criar um dataframe a partir da matriz resultado
df <- data.frame(resultado)
# Adicionar uma coluna de tempo ao dataframe
df$Tempo <- baseAdj$time[1:50]
# Converter o dataframe para o formato longo
df_long <- tidyr::pivot_longer(df, -Tempo, names_to = "Linha", values_to = "Valor")
# Plotar os dados usando ggplot2
t <- ggplot(df_long, aes(x = Tempo, y = Valor, color = Linha)) +
  geom_line() +
  geom_line(data = df_long[df_long$Linha == "X1", ], aes(x = Tempo, y = Valor), color = "black") + # Alabs(x = "Tempo", y = "Duração ajustada", title = "") +
  guides(color = FALSE)
# Personalize os rótulos do eixo x para mostrar o horário inicial e final
t + scale_x_datetime(date_breaks = "1 min", date_labels = "%H:%M")
#ANALISE EXPLORATORIA, HISTOGRAMA, BOX-PLOT
#TABELA COM CCRITERIOS DE ERROS, MENOR ERRO DA PREVISÃO





