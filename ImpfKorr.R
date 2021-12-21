library(readxl)
download.file("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquotenmonitoring.xlsx?__blob=publicationFile", "Impfquotenmonitoring.xlsx")
iq <- read_excel("Impfquotenmonitoring.xlsx", sheet = 2)

download.file("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Inzidenz_aktualisiert.xlsx?__blob=publicationFile", "Fallzahlen_Inzidenz_aktualisiert.xlsx")

inz <- read_excel("Fallzahlen_Inzidenz_aktualisiert.xlsx", sheet = 5)
BL_iq <- iq[c(3:18),2]
BL_inz <- inz[c(3:18),1]
BL_iq == BL_inz
Bundesland <- BL_inz
Lage <- c("S", "S", "O", "O", "N", "N", "W", "O","N", "W", "W", "W", "O","O", "N", "O")
OW <- c("W", "W", "W", "O", "W", "W", "W", "O","W", "W", "W", "W", "O","O", "W", "O")
Impfquote <- iq[c(3:18),12]
Inzidenz <- inz[c(3:18),dim(inz)[2]] # letzter Wert der nicht NA ist, wäre richtig
df <- cbind(Bundesland, lapply(Impfquote, as.numeric), lapply(Inzidenz, as.numeric), Lage, OW)
colnames(df) <- c("Bundesland", "Impfquote", "Inzidenz", "Lage", "OW")
summary(df)

cor.test(df$Impfquote, df$Inzidenz)
t.test(df$OW=="O", df$Inzidenz)
# ohne Sachsen und Bremen
cor.test(df$Impfquote[-c(5,13)], df$Inzidenz[-c(5,13)])

fit1 = lm(Inzidenz~Impfquote, data = df)
summary(fit1)
fit2 = lm(Inzidenz~Lage, data = df)
summary(fit2)
fit3 = lm(Inzidenz~Lage+Impfquote, data = df)
summary(fit3)
fit4 = lm(Inzidenz~Lage+Impfquote+(Lage*Impfquote), data = df)
summary(fit4)
fit5 = lm(Inzidenz~OW, data = df)
summary(fit5)
fit6 = lm(Inzidenz~Impfquote, data = df[df$OW=="O",])
summary(fit6)
fit7 = lm(Inzidenz~Impfquote, data = df[df$OW=="W",])
summary(fit7)

plot(df$Impfquote, df$Inzidenz,
     main = "Korrelation Impfquote/Inzidenz",
     sub = paste0("Datum 7-Tage Inzidenz: ", inz[2,dim(inz)[2]]),
     xlab = "Impfquote",
     ylab = "Inzidenz",
     pch = 20,
     col = "grey",
     xlim = c(55,85),
     ylim = c(150,1000))
abline(fit1, col = "red")
abline(fit7, col = "green")
abline(fit6, col = "yellow")

text(df$Impfquote, df$Inzidenz, labels=df$Bundesland, cex= 0.7, pos =3)
legend("topright", fill = c("red", "green", "yellow"),
       c("Korrelation gesamt", "Korrelation West", "Korrelation Ost"))

plot(df$Impfquote[df$OW=="W"], df$Inzidenz[df$OW=="W"],
     main = "Korrelation Impfquote/Inzidenz",
     sub = paste0("Datum 7-Tage Inzidenz: ", inz[2,dim(inz)[2]]),
     xlab = "Impfquote",
     ylab = "Inzidenz",
     pch = 20,
     col = "grey",
     ylim = c(100,400))
abline(fit7, col = "green")
legend("topright", fill = c("green"),
       c("Korrelation West"))

text(df$Impfquote[df$OW=="W"], df$Inzidenz[df$OW=="W"], labels=df$Bundesland[df$OW=="W"], 
     pos = 3,
     cex= 0.7)

plot(df$Impfquote[df$OW=="O"], df$Inzidenz[df$OW=="O"],
     main = "Korrelation Impfquote/Inzidenz",
     sub = paste0("Datum 7-Tage Inzidenz: ", inz[2,50]),
     xlab = "Impfquote",
     ylab = "Inzidenz")
abline(fit6)

text(df$Impfquote[df$OW=="O"], df$Inzidenz[df$OW=="O"], labels=df$Bundesland[df$OW=="O"], cex= 0.7)
