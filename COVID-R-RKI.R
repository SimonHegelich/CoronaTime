# official RKI data
datum <- Sys.Date()
df <- read.csv("RKI_COVID19.csv", stringsAsFactors = F)
df$Meldedatum <- as.Date(df$Meldedatum)
dfTime <- subset(df, Meldedatum>="2020-03-01")
targetAlter <- c("A60-A79", "A80+")
dif <- as.numeric(by(dfTime$AnzahlFall, dfTime$Meldedatum, sum))

dfDif <- data.frame(datum = sort(unique(dfTime$Meldedatum)), dif = dif)
dfDif$alt <- NA
dfDif$jung <- NA
for(i in 1:dim(dfDif)[1]){
  dfDif$alt[i] <- sum(dfTime$AnzahlFall[(dfTime$Meldedatum==dfDif$datum[i] & dfTime$Altersgruppe %in% targetAlter)])
}
dfDif$jung <- dfDif$dif-dfDif$alt

barplot(dfDif$dif, names.arg = dfDif$datum, las=2, cex.names = 0.6, main = "New cases, daily")

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

zooAlt <- zoo(dfDif$alt, order.by = dfDif$datum)
plot(zooAlt, main = "Daily difference")
# decompose time series
tsDif <- ts(dfDif$alt, names = dfDif$datum, frequency = 7)
deco <- decompose(tsDif, type = "additive")
plot(deco)
zooTrendAlt <- zoo(deco$trend, order.by = dfDif$datum)
plot(zooTrendAlt, las=2, xaxt="n", xlab="", ylab="", main="Daily difference, weekly detrended")
axis(1, at = time(zooTrendAlt), labels = index(zooTrend), las=2, cex=0.6)

zooJung <- zoo(dfDif$jung, order.by = dfDif$datum)
plot(zooJung, main = "Daily difference")
# decompose time series
tsDif <- ts(dfDif$jung, names = dfDif$datum, frequency = 7)
deco <- decompose(tsDif, type = "additive")
plot(deco)
zooTrendJung <- zoo(deco$trend, order.by = dfDif$datum)
plot(zooTrendJung, las=2, xaxt="n", xlab="", ylab="", main="Daily difference, weekly detrended")
axis(1, at = time(zooTrendJung), labels = index(zooTrend), las=2, cex=0.6)


R0 <- (coredata(zooTrend)[-c(1:4)]/coredata(zooTrend)[-c((length(coredata(zooTrend))-3):length(coredata(zooTrend)))])
R0Alt <- (coredata(zooTrendAlt)[-c(1:4)]/coredata(zooTrendAlt)[-c((length(coredata(zooTrendAlt))-3):length(coredata(zooTrendAlt)))])
R0Jung <- (coredata(zooTrendJung)[-c(1:4)]/coredata(zooTrendJung)[-c((length(coredata(zooTrendJung))-3):length(coredata(zooTrendJung)))])
plot(R0, type = "l", xaxt="n", ylab = "Pseudo R0", xlab = "", main = "Reproductrionrate detrended, gen-time 4")
axis(1, at = time(R0), labels = index(zooTrend)[-c(1:4)], las=2, cex.axis=0.7)
abline(h=1, col="red")
abline(v=12, col="lightblue", lty=2)
abline(v=19, col="lightblue", lty=2)
abline(v=26, col="lightgreen", lty=2)
lines(R0Alt, lty=1, col="green")
lines(R0Jung,  lty=1, col="blue")

# Vergleich mit RKI
dfRKI <- read.csv("RKI-R.csv", stringsAsFactors = F)
dfTests <- read.csv("RKI-Tests.csv", stringsAsFactors = F)
# Tests als Prozent zu KW 11
tests <- c(0, (dfTests$Tests[-1]-dfTests$Tests[-length(dfTests$Tests)])/dfTests$Tests[-length(dfTests$Tests)])

R0 <- (coredata(zooTrend)[-c(1:4)]/coredata(zooTrend)[-c((length(coredata(zooTrend))-3):length(coredata(zooTrend)))])
R0Alt <- (coredata(zooTrendAlt)[-c(1:4)]/coredata(zooTrendAlt)[-c((length(coredata(zooTrendAlt))-3):length(coredata(zooTrendAlt)))])
R0Jung <- (coredata(zooTrendJung)[-c(1:4)]/coredata(zooTrendJung)[-c((length(coredata(zooTrendJung))-3):length(coredata(zooTrendJung)))])

par(xpd=F)
par(mar=c(4,2,2,8))
plot(R0, type = "l", xaxt="n", ylab = "", xlab = "", main = "Reproductrionrate detrended, gen-time 4", 
     ylim=c(-0.2, max(c(R0, dfRKI$upper), na.rm = T)),  col = "darkolivegreen")
axis(1, at = time(R0), labels = index(zooTrend)[-c(1:4)], las=2, cex.axis=0.7)
abline(h=1, col="darkgoldenrod")
abline(v=5, col="deepskyblue", lty=2)
abline(v=12, col="deepskyblue", lty=2)
abline(v=19, col="deepskyblue", lty=2)
abline(v=26, col="darkorange", lty=2)
lines(R0Alt, lty=1, col="green")
lines(R0Jung,  lty=1, col="blue")
points( c(31:(30+length(dfRKI$R))), dfRKI$R, pch=19, col="deeppink")
arrows(c(31:(30+length(dfRKI$R))), dfRKI$lower, c(31:(30+length(dfRKI$R))), dfRKI$upper, length=0.05, angle=90, code=3)
for(i in 1:length(tests)){
  segments(x0 = 7*i-2, x1 = 7*i+5, y0 = tests[i], lty = 3, col = "purple", lwd = 2)
}
par(xpd=T)
legend("topright", inset = c(-0.2,0.2),
       legend = c("Pseudo-R", "Pseudo-R 60+", "Pseuso-R 60-", "RKI-R with ci", "intervention", "one week after LD", "tests % change"), 
       col = c("darkolivegreen", "green", "blue", "deeppink", "deepskyblue", "darkorange", "purple"),
       pch = c(NA,NA,NA,19, NA, NA, NA), lty = c(1,1,1,NA,2,2, 2),
       cex = 0.7)


print(R0)
