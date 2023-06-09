
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
TTWO <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="TTWO")
names(TTWO)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

TTWO_descriptive_statistics <- data.frame(open=mean(TTWO$Open),high=mean(TTWO$High),low=mean(TTWO$Low),last=mean(TTWO$Last)) 
TTWO_descriptive_statistics[2,] <- c(median(TTWO$Open),median(TTWO$High),median(TTWO$Low),median(TTWO$Last))
library(moments)
TTWO_descriptive_statistics[3,] <- c(skewness(TTWO$Open),skewness(TTWO$High),skewness(TTWO$Low),skewness(TTWO$Last))
TTWO_descriptive_statistics[4,] <- c(kurtosis(TTWO$Open),kurtosis(TTWO$High),kurtosis(TTWO$Low),kurtosis(TTWO$Last))

rownames(TTWO_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN 

#prvo moraju da se naprave kolone u dataframe-u
TTWO$LogReturn_Open<-0
TTWO$LogReturn_High<-0
TTWO$LogReturn_Low<-0
TTWO$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO)){
  TTWO$LogReturn_Open[i]<-ln(TTWO$Open[i]/TTWO$Open[i-1])
  TTWO$LogReturn_High[i]<-ln(TTWO$High[i]/TTWO$High[i-1])
  TTWO$LogReturn_Low[i]<-ln(TTWO$Low[i]/TTWO$Low[i-1])
  TTWO$LogReturn_Last[i]<-ln(TTWO$Last[i]/TTWO$Last[i-1])
}

# DNEVNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO$NetReturn_Open<-0
TTWO$NetReturn_High<-0
TTWO$NetReturn_Low<-0
TTWO$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO)){
  TTWO$NetReturn_Open[i]<-(TTWO$Open[i]-TTWO$Open[i-1])/TTWO$Open[i-1]
  TTWO$NetReturn_High[i]<-(TTWO$High[i]-TTWO$High[i-1])/TTWO$High[i-1]
  TTWO$NetReturn_Low[i]<-(TTWO$Low[i]-TTWO$Low[i-1])/TTWO$Low[i-1]
  TTWO$NetReturn_Last[i]<-(TTWO$Last[i]-TTWO$Last[i-1])/TTWO$Last[i-1]
}

# ///////////////////////////

# GRUPISANJE PODATAKA PO NEDELJAMA

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
TTWO$week <- week(TTWO$Date)
TTWO$year <- year(TTWO$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
TTWO_weekly <- TTWO %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(TTWO_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## STARO RESENJE (nedeljni) ################

for (i in nrow(TTWO)) {
  TTWO$LogReturn_Open_w<-"/"
  TTWO$LogReturn_High_w<-"/"
  TTWO$LogReturn_Low_w<-"/"
  TTWO$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(TTWO)){
  if(weekdays.POSIXt(TTWO$`Date (GMT)`[i])=="Friday"){
    for(j in 1:5){
      if(weekdays.POSIXt(TTWO$`Date (GMT)`[i-j])=="Friday"){
        TTWO$LogReturn_Open_w[i]<-ln(TTWO$Open[i]/TTWO$Open[j])
        TTWO$LogReturn_High_w[i]<-ln(TTWO$High[i]/TTWO$High[j])
        TTWO$LogReturn_Low_w[i]<-ln(TTWO$Low[i]/TTWO$Low[j])
        TTWO$LogReturn_Last_w[i]<-ln(TTWO$Last[i]/TTWO$Last[j])
      }
    }
  }
}

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

TTWO_weekly$LogReturn_Open<-0
TTWO_weekly$LogReturn_High<-0
TTWO_weekly$LogReturn_Low<-0
TTWO_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_weekly)){
  TTWO_weekly$LogReturn_Open[i]<-ln(TTWO_weekly$weekly_open[i]/TTWO_weekly$weekly_open[i-1])
  TTWO_weekly$LogReturn_High[i]<-ln(TTWO_weekly$weekly_high[i]/TTWO_weekly$weekly_high[i-1])
  TTWO_weekly$LogReturn_Low[i]<-ln(TTWO_weekly$weekly_low[i]/TTWO_weekly$weekly_low[i-1])
  TTWO_weekly$LogReturn_Close[i]<-ln(TTWO_weekly$weekly_close[i]/TTWO_weekly$weekly_close[i-1])
}

# NEDELJNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_weekly$NetReturn_Open<-0
TTWO_weekly$NetReturn_High<-0
TTWO_weekly$NetReturn_Low<-0
TTWO_weekly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_weekly)){
  TTWO_weekly$NetReturn_Open[i]<-(TTWO_weekly$weekly_open[i]-TTWO_weekly$weekly_open[i-1])/TTWO_weekly$weekly_open[i-1]
  TTWO_weekly$NetReturn_High[i]<-(TTWO_weekly$weekly_high[i]-TTWO_weekly$weekly_high[i-1])/TTWO_weekly$weekly_high[i-1]
  TTWO_weekly$NetReturn_Low[i]<-(TTWO_weekly$weekly_low[i]-TTWO_weekly$weekly_low[i-1])/TTWO_weekly$weekly_low[i-1]
  TTWO_weekly$NetReturn_Last[i]<-(TTWO_weekly$weekly_close[i]-TTWO_weekly$weekly_close[i-1])/TTWO_weekly$weekly_close[i-1]
}


# ///////////////////////////

# GRUPISANJE PODATAKA PO MESECIMA

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
TTWO$month <- month(TTWO$Date)
TTWO$year <- year(TTWO$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TTWO_monthly <- TTWO %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
TTWO_monthly

# /////////////////////////


# MESECNI LOG RETURN

############## STARO RESENJE (mesecni) ################

monthly_returns_TTWO_Open <- aggregate(TTWO$LogReturn_Open, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_High <- aggregate(TTWO$LogReturn_High, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_Low <- aggregate(TTWO$LogReturn_Low, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TTWO_Last <- aggregate(TTWO$LogReturn_Last, by = list(format(TTWO$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_TTWO_Open,monthly_returns_TTWO_High,by="Group.1")
low_last<-merge(monthly_returns_TTWO_Low,monthly_returns_TTWO_Last,by="Group.1")
monthly_returns_TTWO<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_TTWO)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_TTWO_Open)
remove(monthly_returns_TTWO_High)
remove(monthly_returns_TTWO_Low)
remove(monthly_returns_TTWO_Last)
remove(open_high)
remove(low_last)

############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_monthly$LogReturn_Open<-0
TTWO_monthly$LogReturn_High<-0
TTWO_monthly$LogReturn_Low<-0
TTWO_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_monthly)){
  TTWO_monthly$LogReturn_Open[i]<-ln(TTWO_monthly$monthly_open[i]/TTWO_monthly$monthly_open[i-1])
  TTWO_monthly$LogReturn_High[i]<-ln(TTWO_monthly$monthly_high[i]/TTWO_monthly$monthly_high[i-1])
  TTWO_monthly$LogReturn_Low[i]<-ln(TTWO_monthly$monthly_low[i]/TTWO_monthly$monthly_low[i-1])
  TTWO_monthly$LogReturn_Close[i]<-ln(TTWO_monthly$monthly_close[i]/TTWO_monthly$monthly_close[i-1])
}

# MESECNI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_monthly$NetReturn_Open<-0
TTWO_monthly$NetReturn_High<-0
TTWO_monthly$NetReturn_Low<-0
TTWO_monthly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_monthly)){
  TTWO_monthly$NetReturn_Open[i]<-(TTWO_monthly$monthly_open[i]-TTWO_monthly$monthly_open[i-1])/TTWO_monthly$monthly_open[i-1]
  TTWO_monthly$NetReturn_High[i]<-(TTWO_monthly$monthly_high[i]-TTWO_monthly$monthly_high[i-1])/TTWO_monthly$monthly_high[i-1]
  TTWO_monthly$NetReturn_Low[i]<-(TTWO_monthly$monthly_low[i]-TTWO_monthly$monthly_low[i-1])/TTWO_monthly$monthly_low[i-1]
  TTWO_monthly$NetReturn_Last[i]<-(TTWO_monthly$monthly_close[i]-TTWO_monthly$monthly_close[i-1])/TTWO_monthly$monthly_close[i-1]
}


# ///////////////////////////

# GRUPISANJE PODATAKA PO GODINAMA

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TTWO_yearly <- TTWO %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
TTWO_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## STARO RESENJE (godisnji) ################

yearly_returns_TTWO_Open <- aggregate(TTWO$LogReturn_Open, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_High <- aggregate(TTWO$LogReturn_High, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_Low <- aggregate(TTWO$LogReturn_Low, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)
yearly_returns_TTWO_Last <- aggregate(TTWO$LogReturn_Last, by = list(format(TTWO$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_TTWO_Open,yearly_returns_TTWO_High,by="Group.1")
low_last<-merge(yearly_returns_TTWO_Low,yearly_returns_TTWO_Last,by="Group.1")
yearly_returns_TTWO<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_TTWO)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_TTWO_Open)
remove(yearly_returns_TTWO_High)
remove(yearly_returns_TTWO_Low)
remove(yearly_returns_TTWO_Last)
remove(open_high)
remove(low_last)

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_yearly$LogReturn_Open<-0
TTWO_yearly$LogReturn_High<-0
TTWO_yearly$LogReturn_Low<-0
TTWO_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_yearly)){
  TTWO_yearly$LogReturn_Open[i]<-ln(TTWO_yearly$yearly_open[i]/TTWO_yearly$yearly_open[i-1])
  TTWO_yearly$LogReturn_High[i]<-ln(TTWO_yearly$yearly_high[i]/TTWO_yearly$yearly_high[i-1])
  TTWO_yearly$LogReturn_Low[i]<-ln(TTWO_yearly$yearly_low[i]/TTWO_yearly$yearly_low[i-1])
  TTWO_yearly$LogReturn_Close[i]<-ln(TTWO_yearly$yearly_close[i]/TTWO_yearly$yearly_close[i-1])
}

# GODISNJI NET RETURN (cisti prinos)

#prvo moraju da se naprave kolone u dataframe-u
TTWO_yearly$NetReturn_Open<-0
TTWO_yearly$NetReturn_High<-0
TTWO_yearly$NetReturn_Low<-0
TTWO_yearly$NetReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TTWO_yearly)){
  TTWO_yearly$NetReturn_Open[i]<-(TTWO_yearly$yearly_open[i]-TTWO_yearly$yearly_open[i-1])/TTWO_yearly$yearly_open[i-1]
  TTWO_yearly$NetReturn_High[i]<-(TTWO_yearly$yearly_high[i]-TTWO_yearly$yearly_high[i-1])/TTWO_yearly$yearly_high[i-1]
  TTWO_yearly$NetReturn_Low[i]<-(TTWO_yearly$yearly_low[i]-TTWO_yearly$yearly_low[i-1])/TTWO_yearly$yearly_low[i-1]
  TTWO_yearly$NetReturn_Last[i]<-(TTWO_yearly$yearly_close[i]-TTWO_yearly$yearly_close[i-1])/TTWO_yearly$yearly_close[i-1]
}

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------

############## STARO RESENJE ################

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_TTWO$Open)
volatility_high <- sd(yearly_returns_TTWO$High)
volatility_low <- sd(yearly_returns_TTWO$Low)
volatility_last <- sd(yearly_returns_TTWO$Last)

yearly_returns_TTWO$Volatility_Open[1] <- "/"
yearly_returns_TTWO$Volatility_Open[1] <- sd(yearly_returns_TTWO$Open)
yearly_returns_TTWO$Volatility_High[1] <- "/"
yearly_returns_TTWO$Volatility_High[1] <- sd(yearly_returns_TTWO$High)
yearly_returns_TTWO$Volatility_Low[1] <- "/"
yearly_returns_TTWO$Volatility_Low[1] <- sd(yearly_returns_TTWO$Low)
yearly_returns_TTWO$Volatility_Last[1] <- "/"
yearly_returns_TTWO$Volatility_Last[1] <- sd(yearly_returns_TTWO$Last)

############## NOVO RESENJE ################

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
TTWO_yearly_volatility <- TTWO %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
TTWO_yearly_volatility

#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(TTWO)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(TTWO[1])<-"Date"

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO <- plot_ly(data = TTWO, type = "candlestick",
                   x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                   increasing = list(fillcolor = "green", line = list(color = "green")),
                   decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO <- fig_TTWO %>% layout(title = "TTWO Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO_lr_d <- plot_ly(data = TTWO, type = "candlestick",
                        x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_d <- fig_TTWO_lr_d %>% layout(title = "TTWO Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
TTWO$Date<- as.character(TTWO$Date)

# Create a candlestick chart
fig_TTWO_lr_w <- plot_ly(data = TTWO, type = "candlestick",
                        x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_w <- fig_TTWO_lr_w %>% layout(title = "TTWO Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_TTWO$Month<- as.character(monthly_returns_TTWO$Month)

# Create a candlestick chart
fig_TTWO_lr_m <- plot_ly(data = monthly_returns_TTWO, type = "candlestick",
                        x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_m <- fig_TTWO_lr_m %>% layout(title = "TTWO Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_TTWO$Year<- as.character(yearly_returns_TTWO$Year)

# Create a candlestick chart
fig_TTWO_lr_y <- plot_ly(data = yearly_returns_TTWO, type = "candlestick",
                        x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_TTWO_lr_y <- fig_TTWO_lr_y %>% layout(title = "TTWO Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# TTWOplay the chart
fig_TTWO_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21, 63, 126 and 252 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
TTWO$MA5 <- rollmean(TTWO$Last, k = 5, fill = NA)
TTWO$MA21 <- rollmean(TTWO$Last, k = 21, fill = NA)
TTWO$MA63 <- rollmean(TTWO$Last, k = 63, fill = NA)
TTWO$MA126 <- rollmean(TTWO$Last, k = 126, fill = NA)
TTWO$MA252 <- rollmean(TTWO$Last, k = 252, fill = NA)

ggplot(TTWO, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(TTWO, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "solid") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "solid") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "solid") +
  geom_line(aes(y = MA126,group = 1), color = "yellow", linetype = "solid") +
  geom_line(aes(y = MA252,group = 1), color = "magenta", linetype = "solid") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#--------------------------------------------------------------------------------------------------------------
# prinose (i log i net) iscrtati na line grafiku sa 5 podgrafika:
# prinos open cene
# prinos high
# prinos low
# prinos close
# prinos candlestick (OVAJ DEO NE MOZE DA SE URADI, NE MOGU DA NADJEM NACIN DA SPOJIM LINECHART SA CANDLESTICK CHARTOM)
#--------------------------------------------------------------------------------------------------------------
#DNEVNI
# Grafikon (Log Return)
plot(TTWO$LogReturn_Open, type="l", col="red", xlab="Dan", ylab="Log return", main="TTWO Open, Close, High i Low Log Return")
lines(TTWO$LogReturn_High, type="l", col="blue")
lines(TTWO$LogReturn_Low, type="l", col="green")
lines(TTWO$LogReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)

##### sa candlestickom ---------NE RADI

# Učitavanje potrebnih paketa
library(ggplot2)
install.packages("reshape2")
library(reshape2)


# Reshapeovanje podataka
TTWO.m <- melt(TTWO[,c("Date", "LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")], id.vars = "Date")

# Kreiranje grafikona sa sve četiri cene i candlestick chart-om
ggplot(TTWO.m, aes(Date, value)) +
  geom_line(data = subset(TTWO.m, variable %in% c("LogReturn_Open", "LogReturn_Last", "LogReturn_High", "LogReturn_Low")), aes(color = variable)) +
  geom_candlestick(data = TTWO, aes(x = Date, open = LogReturn_Open, high = LogReturn_High, low = LogReturn_Low, close = LogReturn_Last), fill = "red", color = "black") +
  scale_color_manual(values = c("red", "blue", "green", "purple")) +
  labs(title = "TTWO Open, Close, High i Low sa Candlestick Chart-om", x = "Datum", y = "Cena") +
  theme(plot.title = element_text(hjust = 0.5))


# Grafikon (Net Return)
plot(TTWO$NetReturn_Open, type="l", col="red", xlab="Dan", ylab="Net return", main="TTWO Open, Close, High i Low Net Return")
lines(TTWO$NetReturn_High, type="l", col="blue")
lines(TTWO$NetReturn_Low, type="l", col="green")
lines(TTWO$NetReturn_Last, type="l", col="purple")

# Legenda za grafikone
legend("topright", legend=c("Open", "High", "Low", "Close"), col=c("red", "blue", "green", "purple"), lty=1)











