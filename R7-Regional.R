# official RKI data taken from https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0

df <- read.csv("RKI_COVID19.csv", stringsAsFactors = F)
df$Meldedatum <- as.Date(df$Meldedatum)
dfTime <- subset(df, Meldedatum>="2020-03-01")
summary(as.factor(dfTime$Bundesland))
targetLand <- c("Bayern", "Berlin")
dif <- as.numeric(by(dfTime$AnzahlFall, dfTime$Meldedatum, sum))

dfDif <- data.frame(datum = sort(unique(dfTime$Meldedatum)), dif = dif)
dfDif$by <- NA
dfDif$be <- NA
for(i in 1:dim(dfDif)[1]){
  dfDif$by[i] <- sum(dfTime$AnzahlFall[(dfTime$Meldedatum==dfDif$datum[i] & dfTime$Bundesland %in% targetLand[1])])
  dfDif$be[i] <- sum(dfTime$AnzahlFall[(dfTime$Meldedatum==dfDif$datum[i] & dfTime$Bundesland %in% targetLand[2])])
}


barplot(dfDif$dif, names.arg = dfDif$datum, las=2, cex.names = 0.6, main = "New cases, daily")

library("tidyverse")
# [50:dim(data)[1],]
dataD <- dfDif %>%
  select(datum, by, be) %>%
  gather(key = "variable", value = "value", -datum)
head(dataD)

ggplot(dataD, aes(x = datum, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Tägliche Fälle") +
  scale_x_date(date_breaks = "2 days", labels = scales::date_format("%d.%m.")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90, vjust=0))

#######
R7_by <- rep(NA, nrow(dfDif))
R7_be <- rep(NA, nrow(dfDif))
for (t in 11:nrow(dfDif)) {
  R7_by[t-1] <- sum(dfDif$by[t-0:6]) / sum(dfDif$by[t-4:10])
  R7_be[t-1] <- sum(dfDif$be[t-0:6]) / sum(dfDif$be[t-4:10])
}


data <- cbind.data.frame(dfDif$datum, R7_be, R7_by)
colnames(data)[1] <- "datum"

dataT <- data %>%
  select(datum, R7_by, R7_be) %>%
  gather(key = "variable", value = "value", -datum)
head(dataT)

ggplot(dataT, aes(x = datum, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Reproduktionszahl R 7 Tage") +
  scale_x_date(date_breaks = "2 days", labels = scales::date_format("%d.%m.")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90, vjust=0))

mean(R7_be, na.rm = T)
mean(R7_by, na.rm = T)
