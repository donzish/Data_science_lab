#### PROGETTO DATA SCIENCE LAB ####

library(car)
library(dplyr) # scarica pacchetto dplyr se richiesto
library(imputeTS) # scarica per missing values
library(forecast)

# Importiamo il dataset 'ristorazione_newdate.csv' che è stato ripulito precedentemente.
data <- `ristorazione_newdate.(2)`
str(data)

# Pulizia dataset:
data$newdate <- as.Date(data$newdate) # rendiamo la variabile newdate di tipo 'data'
data$Giorno <- weekdays(as.Date(data$newdate)) # così la variabile Giorno non ha più valori mancanti
str(data)

# Ora rinominiamo tutte le variabili (per comodità) in base ai 6 ristoranti:
data <- data %>% 
  rename(
    Vendite_1 = Vendite,
    Scontrini_1 = Scontrini,
    Vendite_2 = Vendite.1,
    Scontrini_2 = Scontrini.1,
    Vendite_3 = Vendite.2,
    Scontrini_3 = Scontrini.2,
    Vendite_4 = Vendite.3,
    Scontrini_4 = Scontrini.3,
    Vendite_5 = Vendite.4,
    Scontrini_5 = Scontrini.4,
    Vendite_6 = Vendite.5,
    Scontrini_6 = Scontrini.5
  )

str(data)

# Aggiungiamo le 6 variabili scontrino medio:
attach(data)
data$Scontrino_medio_1 <- Vendite_1/Scontrini_1
data$Scontrino_medio_2 <- Vendite_2/Scontrini_2
data$Scontrino_medio_3 <- Vendite_3/Scontrini_3
data$Scontrino_medio_4 <- Vendite_4/Scontrini_4
data$Scontrino_medio_5 <- Vendite_5/Scontrini_5
data$Scontrino_medio_6 <- Vendite_6/Scontrini_6

#### ANALISI INIZIALI ####

head(data)
summary(data)
library(vtable)
st(new)
# Il dataset di partenza su cui lavoreremo risulta costituito da 1563 osservazioni (in questo caso sono i giorni di osservazione dell'attività 
# dei ristoranti) e da 17 variabili: 5 riguardanti la data (giorno settimanale, giorno, mese, anno e data completa) e 2 (vendite e scontrini) 
# riguardanti ognuno dei 6 ristoranti.

new <- data %>%
  select(Vendite_1, Vendite_2, Vendite_3, Vendite_4, Vendite_5, Vendite_6, Scontrini_1, Scontrini_2, Scontrini_3, Scontrini_4, Scontrini_5, Scontrini_6, Scontrino_medio_1, Scontrino_medio_2, Scontrino_medio_3, Scontrino_medio_4, Scontrino_medio_5, Scontrino_medio_6)
summary(new) # vediamo le summary solo delle variabili che ci interessano
# Dalle summary è possibile notare che sono presenti diversi valori mancanti nel dataset: in particolare si hanno poche informazioni riguardanti il
# Ristorante3 (1096 valori mancanti). 
# [Vedere quali sono i periodi in cui si registrano valori mancanti per i vari ristoranti; alcuni coincidono? sono giorni consecutivi?]
# Il Ristorante2 risulta caratterizzato dal numero medio di vendite più alto.
# Il Ristorante5 risulta avere mediamente il numero di scontrini più alto.
# ...

# I valori mancanti rendono difficile il calcolo e il confronto delle correlazioni Vendite-Scontrini dei vari ristoranti!
# Vediamo allora degli scatterplot per farci un'idea:
par(mfrow=c(2,3))
plot(data$Vendite_1, data$Scontrini_1)
plot(data$Vendite_2, data$Scontrini_2)
plot(data$Vendite_3, data$Scontrini_3)
plot(data$Vendite_4, data$Scontrini_4)
plot(data$Vendite_5, data$Scontrini_5)
plot(data$Vendite_6, data$Scontrini_6)
# Tutti e 6 i grafici mostrano una forte correlazione tra le due variabili. E' evidente l'assenza di diversi valori per quanto riguarda il Ristorante3.

#### VALORI MANCANTI ####

# Vediamo i dati mancanti delle vendite (quali osservazioni) per avere un'idea:
which(is.na(data$Vendite_1))
which(is.na(data$Vendite_2))
which(is.na(data$Vendite_3))
which(is.na(data$Vendite_4))
which(is.na(data$Vendite_5))
which(is.na(data$Vendite_6))
ggplot_na_distribution(data$Vendite_1)
ggplot_na_distribution(data$Vendite_2)
ggplot_na_distribution(data$Vendite_3)
ggplot_na_distribution(data$Vendite_4)
ggplot_na_distribution(data$Vendite_5)
ggplot_na_distribution(data$Vendite_6)
# Non essendoci troppi valori mancanti (prima del periodo covid) possiamo imputarli usando la media.
# Tagliamo il dataset in due: dataset pre-covid (31/12/2019) e covid. Per i ristoranti 3 e 6 sarà necessario sistemare il dataset, perchè hanno 
# aperto l'attività dopo gli altri ristoranti.
lst <- split(data, data$newdate < as.Date("2020-01-01"))
dataset_2019 <- lst[[2]] 
dataset_2020 <- lst[[1]]

# Ora imputiamo i valori mancanti:
summary(dataset_2019)

dataset_2019$Vendite_1[is.na(dataset_2019$Vendite_1)] <- mean(dataset_2019$Vendite_1, na.rm = TRUE)
dataset_2019$Scontrini_1[is.na(dataset_2019$Scontrini_1)] <- mean(dataset_2019$Scontrini_1, na.rm = TRUE)
dataset_2019$Scontrino_medio_1[is.na(dataset_2019$Scontrino_medio_1)] <- mean(dataset_2019$Scontrino_medio_1, na.rm = TRUE)

dataset_2019$Vendite_4[is.na(dataset_2019$Vendite_4)] <- mean(dataset_2019$Vendite_4, na.rm = TRUE)
dataset_2019$Scontrini_4[is.na(dataset_2019$Scontrini_4)] <- mean(dataset_2019$Scontrini_4, na.rm = TRUE)
dataset_2019$Scontrino_medio_4[is.na(dataset_2019$Scontrino_medio_4)] <- mean(dataset_2019$Scontrino_medio_4, na.rm = TRUE)

dataset_2019$Vendite_5[is.na(dataset_2019$Vendite_5)] <- mean(dataset_2019$Vendite_5, na.rm = TRUE)
dataset_2019$Scontrini_5[is.na(dataset_2019$Scontrini_5)] <- mean(dataset_2019$Scontrini_5, na.rm = TRUE)
dataset_2019$Scontrino_medio_5[is.na(dataset_2019$Scontrino_medio_5)] <- mean(dataset_2019$Scontrino_medio_5, na.rm = TRUE)

summary(dataset_2019)
# Il Ristorante2 non ha valori mancanti, i ristoranti 3 e 6 sono da considerare dopo, perchè hanno aperto in ritardo rispetto agli altri.

#### PLOT SERIE STORICHE INIZIALI ####

# Bozza serie storiche vendite:
par(mfrow=c(1,1))
par(cex.main=1.0, cex.axis=1.0, cex.lab=1.0) # 2.8 
plot(data$newdate, data$Vendite_1, ylab = 'Vendite', xlab = 'data', type = 'b', main = 'Ristorante1')
plot(data$newdate, data$Vendite_2, ylab = 'Vendite', xlab = 'data', type = 'b', main = 'Ristorante2')
plot(data$newdate, data$Vendite_3, ylab = 'Vendite', xlab = 'data', type = 'b', main = 'Ristorante3')
plot(data$newdate, data$Vendite_4, ylab = 'Vendite', xlab = 'data', type = 'b', main = 'Ristorante4')
plot(data$newdate, data$Vendite_5, ylab = 'Vendite', xlab = 'data', type = 'b', main = 'Ristorante5')
plot(data$newdate, data$Vendite_6, ylab = 'Vendite', xlab = 'data', type = 'b', main = 'Ristorante6')
# Stessa cosa per scontrino 
par(mfrow=c(1,1))
par(cex.main=1.0, cex.axis=1.0, cex.lab=1.0) # 2.8 
plot(data$newdate, data$Scontrini_1, ylab = 'Scontrini', xlab = 'data', type = 'b', main = 'Ristorante1')
plot(data$newdate, data$Scontrini_2, ylab = 'Scontrini', xlab = 'data', type = 'b', main = 'Ristorante2')
plot(data$newdate, data$Scontrini_3, ylab = 'Scontrini', xlab = 'data', type = 'b', main = 'Ristorante3')
plot(data$newdate, data$Scontrini_4, ylab = 'Scontrini', xlab = 'data', type = 'b', main = 'Ristorante4')
plot(data$newdate, data$Scontrini_5, ylab = 'Scontrini', xlab = 'data', type = 'b', main = 'Ristorante5')
plot(data$newdate, data$Scontrini_6, ylab = 'Scontrini', xlab = 'data', type = 'b', main = 'Ristorante6')