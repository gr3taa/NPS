
day <- read.csv("day.csv")
hour <- read.csv("hour.csv")
library(dplyr)
library(tidyr)
#controllo quante osservazioni orarie per ogni giorno
blocchi <- rle(hour$dteday)
blocchi$values   
blocchi$lengths

#Trasforma `dati_orari` per avere una colonna per ogni ora
hour_wide <- hour %>%
  pivot_wider(
    names_from = hr, 
    values_from = cnt,
    names_prefix = "cnt_hour_"
  )
#dataset che compatta le righe
hour_wide_sommato <- hour_wide %>%
  group_by(dteday) %>%
  summarise(across(starts_with("cnt_hour"), sum, na.rm = TRUE))
#Unisci `dati_giornalieri` e `dati_orari_wide` per data
day <- day %>%
  left_join(hour_wide_sommato, by = "dteday")

matplot(t(day[,17:40]), type='l')

mese_anno <- paste(day$yr, day$mnth, sep = "_")
rle <- rle(mese_anno)
len <- rle$lengths
color_vector <- rainbow(24)
col <- rep (color_vector, len)
matplot(t(day[,17:40]), type='l', col=col)

colori_nome <- sapply(color_vector, function(x) name_col(x))