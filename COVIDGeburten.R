# Geburten während COVID
library(forecast)

# Daten
# Destatis Daten von:
# https://www-genesis.destatis.de/genesis//online?operation=table&code=12612-0002&bypass=true&levelindex=0&levelid=1656223533221#abreadcrumb
# https://www-genesis.destatis.de/genesis/online?operation=abruftabelleBearbeiten&levelindex=1&levelid=1656310768375&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&code=12612-0001&auswahltext=&werteabruf=Werteabruf#abreadcrumb
# https://www-genesis.destatis.de/genesis/online?operation=find&suchanweisung_language=de&query=totgeborene#abreadcrumb

# Lebendgeborene
lebend <- read.csv("Lebendgeburten.csv", skip = 6, sep = ";", header = FALSE)
lebend <- lebend[c(1:387),] # Wenn mehr Monate vorliegen, ist die 387 zu ändern! Derzeit bis März 2022
colnames(lebend) <- c("Jahr", "Monat", "Männlich", "Weiblich", "Gesamt")

lebendJ <- read.csv("LebendgeburtenJahr.csv", skip = 6, sep = ";", header = FALSE)
lebendJ <- lebendJ[c(1:32),]
colnames(lebendJ) <- c("Jahr", "Männlich", "Weiblich", "Gesamt")

# Totgeborene
tot <- read.csv("TotgeboreneJahr.csv", skip = 6, sep = ";", header = FALSE)
tot <- tot[c(1:32),]
colnames(tot) <- c("Jahr", "Gesamt")

# Umwandeln in TimeSeries
tslebend <- ts(as.numeric(lebend$Gesamt), frequency = 12, start = c(1990, 1)) 
png("LebendTS.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(tslebend,
     main = "Lebendgeborene pro Monat",
     ylab = "",
     xlab = "")
dev.off()

dclebend <- stl(tslebend, s.window = "periodic")

png("LebendDecomp.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 23)
plot(dclebend,
     main = "Decomposed: Lebendgeborene")
dev.off()

tslebendJ <- ts(as.numeric(lebendJ$Gesamt), frequency = 1, start = c(1990,1))
png("LebendJahrTS.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(tslebendJ,
     main = "Lebendgeborene pro Jahr",
     ylab = "",
     xlab = "")
dev.off()

tstot <- ts(as.numeric(tot$Gesamt), frequency = 1, start = c(1990,1))
png("TotJahrTS.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(tstot,
     main = "Totgeborene pro Jahr",
     ylab = "",
     xlab = "")
dev.off()

tslebend2012 <- subset(tslebend, start = (12*25+1))
dclebend2012 <- stl(tslebend2012, s.window = "periodic")
png("LebendDecomp2015.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 23)
plot(dclebend2012,
     main = "Decomposed: Lebendgeborene seit 2015")
dev.off()

# seasonal adjusted
tslebend2012sea <- ts(dclebend2012$time.series[,1], frequency = 12, start = c(2015,1))
tslebend2012sa <- tslebend2012 - tslebend2012sea
png("LebendTSsa.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(tslebend2012sa,
     main = "Lebendgeborene saisonal bereinigt",
     ylab = "",
     xlab = "")
dev.off()

# stationary
tslebend2012st <- diff(tslebend2012sa, differences=1)
png("LebendTSstat.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(tslebend2012st,
     main = "Differenz Lebendgeborene saisonal bereinigt",
     ylab = "",
     xlab = "")
abline(h=0, col = "red")
dev.off()

fit <- auto.arima(tslebend, stepwise = FALSE, approximation = FALSE)
fit
png("ARIMARes.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(fit$residuals,
     main = "Abweichungen ARIMA",
     ylab = "",
     xlab = "")
abline(h=0, col = "red")
dev.off()

fit2 <- auto.arima(subset(tslebend2012, end = 75), stepwise = FALSE, approximation = FALSE)
fit2
png("ARIMA-12Res.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(fit2$residuals,
     main = "Abweichungen ARIMA",
     ylab = "",
     xlab = "")
abline(h=0, col = "red")
dev.off()

myforecast <- forecast(fit2, level=c(95), h=12)
png("Forecast.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(myforecast,
     main = "Forecast ARIMA last 12 Month",
     ylab = "",
     xlab = "")
lines(subset(tslebend, start = 375), col = "red")
legend("topleft", legend = c("Forecast", "95% CI", "Real Values"), fill = c(4, "grey", "red"))
dev.off()

png("TotLebend.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)

plot(tstot / tslebendJ,
     main = "Verhältnis Totgeborene/Lebendgeborene",
     ylab = "",
     xlab = "",
     sub = "Geburtsgewicht vom 1.7.1979 bis 31.3.1994 mindestens 1000
Gramm, ab 1.4.1994 mindestens 500 Gramm.")
dev.off()

png("TotLebendDiff.png",
    type = "cairo",
    units = "cm",
    res = 600,
    width = 23,
    height = 15)
plot(diff(tstot / tslebendJ, differences = 1),
     main = "Differenz Verhältnis Totgeborene/Lebendgeborene",
     ylab = "",
     xlab = "",
     sub = "Geburtsgewicht vom 1.7.1979 bis 31.3.1994 mindestens 1000
Gramm, ab 1.4.1994 mindestens 500 Gramm.")
dev.off()
