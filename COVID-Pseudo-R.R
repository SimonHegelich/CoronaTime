### Calculation of R0
# in fact, it is not R0 but the effective R

# get the data
df <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", stringsAsFactors = F)

# example for Germany
ab <- as.numeric(df[df$Country.Region=="Germany",5:dim(df)[2]])
# convert to date format
datum <- colnames(df[5:dim(df)[2]])
datum <- unlist(lapply(datum, substring, 2))
datum <- as.Date(datum, format = "%m.%d.%y")

dfAb <- data.frame(datum,ab)
plot(dfAb, main = "Number of official infections")

# daily differences
dfDif <- data.frame(datum=dfAb$datum[-1],dif=(dfAb$ab[-1]-dfAb$ab[-dim(dfAb)[1]]))
barplot(dfDif$dif, names.arg = dfDif$datum, las=2, cex.names = 0.6, main = "New cases, daily")

# Definition R:
# Bei einer konstanten Generationszeit von 4 Tagen,
# ergibt sich R als Quotient der Anzahl von Neuerkrankungen in zwei 
# aufeinander folgenden Zeitabschnitten von jeweils 4 Tagen
# https://www.rki.de/DE/Content/Infekt/EpidBull/Archiv/2020/Ausgaben/17_20_SARS-CoV2_vorab.pdf?__blob=publicationFile



# time series
library(zoo)
zooDif <- zoo(dfDif$dif, order.by = dfDif$datum)
plot(zooDif, main = "Daily difference")
# decompose time series
tsDif <- ts(dfDif$dif, names = dfDif$datum, frequency = 7)
deco <- decompose(tsDif, type = "additive")
plot(deco)
zooTrend <- zoo(deco$trend, order.by = dfDif$datum)
plot(zooTrend, las=2, xaxt="n", xlab="", ylab="", main="Daily difference, weekly detrended")
axis(1, at = time(zooTrend), labels = index(zooTrend), las=2, cex=0.6)

# rolling mean 4
# generation time is expected to be 4
zooTrend4 <- rollmeanr(zooTrend, 4)
points(zooTrend4, type = "l", col = "red")

R0 <- (coredata(zooTrend)[-c(1:4)]/coredata(zooTrend)[-c((length(coredata(zooTrend))-3):length(coredata(zooTrend)))])[-c(1:30)]
plot(R0, type = "l", xaxt="n", ylab = "Pseudo R0", xlab = "", main = "Reproductrionrate detrended, gen-time 4")
axis(1, at = time(R0), labels = index(zooTrend)[-c(1:34)], las=2, cex.axis=0.7)
abline(h=1, col="red")
abline(v=20, col="lightblue", lty=2)
abline(v=27, col="lightblue", lty=2)

# same time span as in RKI plot
R0 <- (coredata(zooTrend)[-c(1:4)]/coredata(zooTrend)[-c((length(coredata(zooTrend))-3):length(coredata(zooTrend)))])[-c(1:40)]
plot(R0, type = "l", xaxt="n", ylab = "Pseudo R", xlab = "", main = "Reproductrionrate detrended, gen-time 4")
axis(1, at = time(R0), labels = index(zooTrend)[-c(1:44)], las=2, cex.axis=0.7)
abline(h=1, col="red")
abline(v=10, col="lightblue", lty=2)
abline(v=17, col="lightblue", lty=2)