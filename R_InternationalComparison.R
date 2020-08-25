# Compare Reproduction Numbers

library(stringr)
library(ggplot2)
df <- read.csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_confirmed_global.csv", stringsAsFactors=F)

# Countries of interest # Some countries may cause errors, like France, because there are multiple "France" entries in Country.Region...
# ListOfCountries <- c("Germany",  "Sweden", "US", "Italy", "Spain", "Norway","Austria")

de <- as.numeric(df[df$Country.Region=="Germany",5:dim(df)[2]])
se <- as.numeric(df[df$Country.Region=="Sweden",5:dim(df)[2]])
us <- as.numeric(df[df$Country.Region=="US",5:dim(df)[2]])
it <- as.numeric(df[df$Country.Region=="Italy",5:dim(df)[2]])
no <- as.numeric(df[df$Country.Region=="Norway",5:dim(df)[2]])
kr <- as.numeric(df[df$Country.Region=="Korea, South",5:dim(df)[2]])

R_Wert_de <- rep(NA, length(de))
R_Wert_se <- rep(NA, length(de))
R_Wert_us <- rep(NA, length(de))
R_Wert_it <- rep(NA, length(de))
R_Wert_no <- rep(NA, length(de))
R_Wert_kr <- rep(NA, length(de))
for (t in 8:length(de)) {
  R_Wert_de[t] <- sum(de[t-0:3]) / sum(de[t-4:7])
  R_Wert_se[t] <- sum(se[t-0:3]) / sum(se[t-4:7])
  R_Wert_us[t] <- sum(us[t-0:3]) / sum(us[t-4:7])
  R_Wert_it[t] <- sum(it[t-0:3]) / sum(it[t-4:7])
  R_Wert_no[t] <- sum(no[t-0:3]) / sum(no[t-4:7])
  R_Wert_kr[t] <- sum(kr[t-0:3]) / sum(kr[t-4:7])
}
Datum <- colnames(df)[5:dim(df)[2]]
Datum <- as.Date(str_sub(Datum, 2), format = "%m.%d.%y")
data <- cbind.data.frame(Datum, R_Wert_de, R_Wert_se, R_Wert_it, R_Wert_us, R_Wert_no, R_Wert_kr)

# Data preparation
library("tidyverse")
dataT <- data[100:dim(data)[1],] %>%
  select(Datum, R_Wert_de, R_Wert_se, R_Wert_it, R_Wert_us, R_Wert_no, R_Wert_kr) %>%
  gather(key = "variable", value = "value", -Datum)
head(dataT)

ggplot(dataT, aes(x = Datum, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  theme_minimal() +
  labs(title = "",
       x = "",
       y = "Reproduktionszahl R") +
  scale_x_date(date_breaks = "2 days", labels = scales::date_format("%d.%m.")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90, vjust=0))


