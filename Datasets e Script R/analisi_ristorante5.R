library(car)
library(dplyr)
library(imputeTS)
library(forecast)
library(tidyr)
library(fpp2)
library(tseries)
library(lmtest)
library(tidyverse)
library(ggplot2)

# Importiamo i due dataset definitivi: 2019 e 2020 (e anche il dataset con le covariate nuove per la previsione)

definitivo <- definitivo_2019[-c(1:263),]
str(definitivo)
definitivo$newdate <- as.Date(definitivo$newdate)
definitivo$Vendite_6[is.na(definitivo$Vendite_6)] <- mean(definitivo$Vendite_6, na.rm = TRUE)
definitivo$Scontrini_6[is.na(definitivo$Scontrini_6)] <- mean(definitivo$Scontrini_6, na.rm = TRUE)
definitivo$Scontrino_medio_6[is.na(definitivo$Scontrino_medio_6)] <- mean(definitivo$Scontrino_medio_6, na.rm = TRUE)
summary(definitivo)
data_2020 <- definitivo_2020
str(data_2020)
data_2020$newdate <- as.Date(data_2020$newdate)
summary(data_2020)

#### PLOT SERIE STORICHE RISTORANTE 1 ####

par(mfrow=c(1,1))
plot(definitivo$newdate, definitivo$Vendite_6, ylab = 'Vendite', xlab = 'data', type = 'c', main = 'Ristorante6')
plot(definitivo$newdate, definitivo$Scontrino_medio_6, ylab = 'Scontrino medio', xlab = 'data', type = 'c', main = 'Ristorante6') #da mettere in Appendice

#### SARIMAX VENDITE RISTORANTE 1 ####

covariate <- data.matrix(definitivo[,c(24,25,26,27,28,29,31,35,36)]) 
tsdata1 <- ts(definitivo$Vendite_6, frequency = 7 ) 

# StagionalitÓ e stazionarietÓ serie storica (grafici e test):
par(mfrow=c(2,1))
acf(definitivo$Vendite_6)
pacf(definitivo$Vendite_6)
par(mfrow=c(1,1))
lag.plot(definitivo$Vendite_6, lags = 16)
adf.test(definitivo$Vendite_6)

# Stimiamo il miglior modello SARIMAX utilizzando la funzione auto.arima()
autoarima1.1 <- auto.arima(tsdata1,
                           xreg = covariate,
                           trace = FALSE,
                           seasonal = TRUE,
                           stepwise = FALSE,
                           approximation = FALSE)
autoarima1.1

# Ora validiamo il modello controllando i residui:
res <- autoarima1.1$residuals
par(mfrow=c(2,2)) 
acf(res)
pacf(res)
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

# Dopo aver validato il modello, eseguiamo test statistici per studiarne i coefficienti:
coeftest(autoarima1.1)

# Previsione:
covariate_nuove <- data.matrix(covariate_2020[,c(6,7,8,9,10,11,13,17,18)]) 
forecast1 <- forecast(autoarima1.1, h=56, xreg = covariate_nuove)
par(mfrow=c(1,1)) 
plot(forecast1)
forecast1
sum(forecast1$mean) # quanto perdono i ristoranti (levare vendite del 9 10 11 marzo dataset 2020)

#### ANALISI DATI 2020 ####

data <- definitivo_2020
data$newdate <- as.Date(data$newdate)
data$ZONA <- ifelse(is.na(data$ZONA), 'nessun colore', data$ZONA)

# Vediamo le statistiche iniziali del ristorante 1 per il periodo dopo il lockdown
summary(data$Vendite_6)
summary(data$Scontrini_6)
summary(data$Scontrino_medio_6)

# Imputiamo con 0 i valori mancanti (perchŔ ristoranti chiusi per lockdown)
data$Scontrini_6 <- ifelse(is.na(data$Scontrini_6), 0, data$Scontrini_6)
str(data)

ggplot() + geom_rect(data=data, (aes(xmin=newdate-0.5, xmax=newdate+0.5, 
                                     ymin=min(Scontrini_6)-0.5, ymax=max(Scontrini_6)+0.5, fill=factor(ZONA)))) +
  scale_fill_manual(values=c("orange","yellow", "grey","red"), guide=FALSE) +
  geom_line(data = data, aes(x = newdate, y = Scontrini_6), size=0.8) +
  xlab('Data')+
  ylab('Scontrini')

# se vogliamo concentrarci solo sulle zone colorate
data1 <- data[238:397,]

ggplot() + geom_rect(data=data1, (aes(xmin=newdate-0.5, xmax=newdate+0.5, 
                                      ymin=min(Scontrini_6)-0.5, ymax=max(Scontrini_6)+0.5, fill=factor(ZONA)))) +
  scale_fill_manual(values=c("orange","yellow","red"), guide=FALSE) +
  geom_line(data = data1, aes(x = newdate, y = Scontrini_6), size=0.8) +
  xlab('Data')+
  ylab('Scontrini')
# I valori mancanti presenti nel secondo grafico riguardano solamente i giorni in cui il ristorante si trovava in zona rossa, per cui si presume che
# il ristorante sia stato chiuso. Per questo motivo sono stati imputati col valore 0.