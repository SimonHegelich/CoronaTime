### R Sim
df <- read.csv("RKI_COVID19.csv", stringsAsFactors = F)
head(df)
# Die Reihen sind keine einzelenen Fälle, sondern kulminiert für die entsprechende Gruppe (Landkreis, Meldedatum, Alter...)
# plot(df$AnzahlFall)
sum(df$AnzahlFall)
df$Meldedatum <- as.Date(df$Meldedatum)
df$Refdatum <- as.Date(df$Refdatum)

# Subset: Vor dem 1.03. ist nichts passiert.
#df <- subset(df, Refdatum>="2020-03-01")

noDate <- which(df$Refdatum == df$Meldedatum)
# RKI sagt, ca. 35% der Fälle kein Meldedatum
length(noDate)/dim(df)[1]
# 25%. Aber es sind ja noch kulminierte Fälle
sum(df$AnzahlFall[noDate])/sum(df$AnzahlFall[-noDate])
# Bei 56% der Fälle ist Meldedatum identisch mit Referenzdatum...
# Problem: Nur 62% der Fälle mit gleichen Datum wurden für den Nowcast imputiert
# Bei den anderen Fällen liegt das Meldedatum und das Erkankungsdatum identisch.
# Idee: Schätzen des Erkrankungsdatums und Effekt mit 0.625 multiplizieren.
# Dann schauen, ob sich das Modell groß unterscheidet, wenn man nur das Referenzdatum wählt.

# Durchschnittliche Differenz Meldedatum, Referenzdatum
MeldRef <- df$Meldedatum[-noDate]-df$Refdatum[-noDate] # difftime in Days
# plot(MeldRef)
head(MeldRef)
mean(MeldRef)
# Bei Fällen, wo das Meldedatum nicht identisch ist mit dem RefDatum, ist die Differenz im Durchschnitt 7 Tage
# Passt auch zur Inkubationszeit
# Also ziehen wir bei 0.625 der Fälle mit identischen Datum eine Woche ab.
# Hat den Vorteil, dass der Wochentrend erhalten bleibt.
set.seed(123456)
impu <- sample(length(noDate), length(noDate)*0.625, replace = F)

df$ImpDatum <- df$Refdatum
#Alternative 1: Teil der Werte imputieren
df$ImpDatum[noDate][impu] <- df$ImpDatum[noDate][impu]-7


dif <- as.numeric(by(df$AnzahlFall, df$Meldedatum, sum))

dfDif <- data.frame(datum = sort(unique(df$Meldedatum)), dif = dif)
barplot(dfDif$dif, names.arg = dfDif$datum, las=2, cex.names = 0.6, main = "New cases, daily")

dif <- as.numeric(by(df$AnzahlFall, df$ImpDatum, sum))

dfDifImp <- data.frame(datum = sort(unique(df$ImpDatum)), dif = dif)
barplot(dfDifImp$dif, names.arg = dfDifImp$datum, las=2, cex.names = 0.6, main = "New Infections, daily")
library(zoo)
dfDifImp$trend <- c(NA,NA,NA, rollmean(dfDifImp$dif, 7), NA,NA,NA)
barplot(dfDifImp$trend, names.arg = dfDifImp$datum, las=2, cex.names = 0.6, main = "New Infections, daily")
dfDifImp$roll4 <- c(NA,NA,NA, rollmeanr(dfDifImp$trend, 4))
dfDifImp$R <- c(rep(NA,4), dfDifImp$roll4[-c(1:4)]/dfDifImp$roll4[-c((length(dfDifImp$roll4)-3):length(dfDifImp$roll4))])

par(xpd=F)
par(mar=c(4,4,2,8))
plot(dfDifImp$R, type = "l", xaxt="n", ylab = "R", xlab = "", main = "Vergleich von R-Berechnungen", ylim=c(0,4), col="darkolivegreen")
axis(1, at = time(dfDifImp$R), labels = dfDifImp$datum, las=2, cex.axis=0.7)
abline(h=1, col = "red", lty=1)

# Alternative 2: Alle Werte Imputieren
df$ImpDatum <- df$Refdatum
df$ImpDatum[noDate] <- df$ImpDatum[noDate]-7
dfDifImp$dif2 <- as.numeric(by(df$AnzahlFall, df$ImpDatum, sum))

dfDifImp$trend2 <- c(NA,NA,NA, rollmean(dfDifImp$dif2, 7), NA,NA,NA)
dfDifImp$roll42 <- c(NA,NA,NA, rollmeanr(dfDifImp$trend2, 4))
dfDifImp$R2 <- c(rep(NA,4), dfDifImp$roll42[-c(1:4)]/dfDifImp$roll42[-c((length(dfDifImp$roll42)-3):length(dfDifImp$roll42))])
points(dfDifImp$R2, type = "l", col="deepskyblue")


# Alternative 3: Keine Imputation
df$ImpDatum <- df$Refdatum
dfDifImp$dif3 <- as.numeric(by(df$AnzahlFall, df$ImpDatum, sum))

dfDifImp$trend3 <- c(NA,NA,NA, rollmean(dfDifImp$dif3, 7), NA,NA,NA)
dfDifImp$roll43 <- c(NA,NA,NA, rollmeanr(dfDifImp$trend3, 4))
dfDifImp$R3 <- c(rep(NA,4), dfDifImp$roll43[-c(1:4)]/dfDifImp$roll43[-c((length(dfDifImp$roll43)-3):length(dfDifImp$roll43))])
points(dfDifImp$R3, type = "l", col="goldenrod")

RKIhack <- read.csv("RKI-R-Hack.csv", stringsAsFactors = F)
i <- which(dfDifImp$datum=="2020-03-06")
dfDifImp$RKI <- NA
dfDifImp$RKI[c(i:(i+length(RKIhack$R)-1))] <- RKIhack$R
points(dfDifImp$RKI, type = "p", col="deeppink")

dfRKI <- read.csv("RKI-R.csv", stringsAsFactors = F)
i <- which(dfDifImp$datum=="2020-04-04")
dfDifImp$RKIneu <- NA
dfDifImp$RKIneu[c(i:(i+length(dfRKI$R)-1))] <- dfRKI$R
points(dfDifImp$RKIneu, type = "p", col="deeppink", pch=19)

par(xpd=T)
legend("topright", inset = c(-0.22,0.1),
       legend = c("No-Imputation", "Stong-Imputation", "Weak-Imputation", "RKI-old", "RKI-new"), 
       col = c("goldenrod", "darkolivegreen", "deepskyblue", "deeppink", "deeppink"),
       pch = c(NA,NA,NA,21, 19), lty = c(1,1,1,NA,NA),
       cex = 0.7)

# Nur RKI Zeit
a <- c(1:(which(dfDifImp$datum=="2020-03-06")-1))
par(xpd=F)
par(mar=c(4,4,2,8))
plot(dfDifImp$R[-a], type = "l", xaxt="n", ylab = "R", xlab = "", main = "Vergleich von R-Berechnungen", ylim=c(0,3.3), col="darkolivegreen")
axis(1, at = time(dfDifImp$R[-a]), labels = dfDifImp$datum[-a], las=2, cex.axis=0.7)
abline(h=1, col = "red", lty=1)

points(dfDifImp$R2[-a], type = "l", col="deepskyblue")
points(dfDifImp$R3[-a], type = "l", col="goldenrod")

points(dfDifImp$RKI[-a], type = "p", col="deeppink")
points(dfDifImp$RKIneu[-a], type = "p", col="deeppink", pch=19)

par(xpd=T)
legend("topright", inset = c(-0.22,0.1),
       legend = c("No-Imputation", "Stong-Imputation", "Weak-Imputation", "RKI-old", "RKI-new"), 
       col = c("goldenrod", "darkolivegreen", "deepskyblue", "deeppink", "deeppink"),
       pch = c(NA,NA,NA,21, 19), lty = c(1,1,1,NA,NA),
       cex = 0.7)

# 23.03. keine Infektionen mehr
dfs <- dfDifImp
i <- which(dfs$datum=="2020-03-23")+6
dfs$dif3[i+1] <- dfs$dif3[i+1]-dfs$dif3[i+1]/4
dfs$dif3[i+2] <- dfs$dif3[i+2]-dfs$dif3[i+1]/4*2 
dfs$dif3[i+3] <- dfs$dif3[i+3]-dfs$dif3[i+1]/4*3 
dfs$dif3[c((i+4):length(dfs$dif3))] <- 0

dfs$trend3 <- c(NA,NA,NA, rollmean(dfs$dif3, 7), NA,NA,NA)
dfs$roll43 <- c(NA,NA,NA, rollmeanr(dfs$trend3, 4))
dfs$R3 <- c(rep(NA,4), dfs$roll43[-c(1:4)]/dfs$roll43[-c((length(dfs$roll43)-3):length(dfs$roll43))])

# Fallzahlen verdoppeln sich
dfs2 <- dfDifImp
dfs2$dif3[c((i+1):length(dfs$dif3))] <- dfs2$dif3[c((i+1):length(dfs$dif3))]*2
dfs2$trend3 <- c(NA,NA,NA, rollmean(dfs2$dif3, 7), NA,NA,NA)
dfs2$roll43 <- c(NA,NA,NA, rollmeanr(dfs2$trend3, 4))
dfs2$R3 <- c(rep(NA,4), dfs2$roll43[-c(1:4)]/dfs2$roll43[-c((length(dfs2$roll43)-3):length(dfs2$roll43))])

par(xpd=F)
par(mar=c(4,4,2,8))
plot(dfDifImp$R3[-a], type = "l", xaxt="n", ylab = "R", xlab = "", main = "R-Dynamiken", ylim=c(0,3.3), col="goldenrod")
axis(1, at = time(dfDifImp$R[-a]), labels = dfDifImp$datum[-a], las=2, cex.axis=0.7)
abline(h=1, col = "red", lty=1)

# points(dfDifImp$R2[-a], type = "l", col="deepskyblue")
points(dfs$R3[-a], type = "l", col="deepskyblue")

points(dfs2$R3[-a], type = "l", col="deeppink")
points(dfDifImp$R3[-a], type = "l", col="black")


par(xpd=T)
legend("topright", inset = c(-0.22,0.1),
       legend = c("R", "Intervention", "Double Infection"), 
       col = c("black", "deepskyblue", "deeppink"),
       #pch = c(NA,NA,NA,21, 19), 
       lty = c(1,1,1),
       cex = 0.7)

# Plot Wirkung
a <- c(1:(which(dfDifImp$datum=="2020-03-06")-1))
par(xpd=F)
par(mar=c(4,4,2,8))
plot(dfDifImp$R3[-a], type = "l", xaxt="n", ylab = "R", xlab = "", main = "R und Maßnahmen", ylim=c(0,3.3), col="darkolivegreen")
axis(1, at = time(dfDifImp$R[-a]), labels = dfDifImp$datum[-a], las=2, cex.axis=0.7)
abline(h=1, col = "red", lty=1)

points(dfDifImp$RKI[-a], type = "p", col="deeppink")
points(dfDifImp$RKIneu[-a], type = "p", col="deeppink", pch=19)
abline(v=which(dfDifImp$datum[-a]=="2020-03-09"), col="darkorange", lty=2)
abline(v=which(dfDifImp$datum[-a]=="2020-03-16"), col="darkorange", lty=2)
abline(v=which(dfDifImp$datum[-a]=="2020-03-23"), col="darkorange", lty=2)
abline(v=(which(dfDifImp$datum[-a]=="2020-03-09")+4), col="blue", lty=2)
abline(v=(which(dfDifImp$datum[-a]=="2020-03-16")+4), col="blue", lty=2)
abline(v=(which(dfDifImp$datum[-a]=="2020-03-23")+4), col="blue", lty=2)

par(xpd=T)
legend("topright", inset = c(-0.22,0.1),
       legend = c("R", "RKI-alt", "RKI-neu", "Intervention", "Früheste Wirkung"), 
       col = c( "darkolivegreen", "deeppink", "deeppink", "darkorange", "blue"),
       pch = c(NA,21, 19,NA,NA), lty = c(1,NA,NA,2,2),
       cex = 0.7)
mean(abs(dfDifImp$RKI-dfDifImp$R3), na.rm = T)
