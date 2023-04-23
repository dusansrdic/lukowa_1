
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
NWS <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NWS")


# DESKRIPTIVNA STATISTIKA

NWS_descriptive_statistics <- data.frame(open=mean(NWS$Open),high=mean(NWS$High),low=mean(NWS$Low),last=mean(NWS$Last)) 
NWS_descriptive_statistics[2,] <- c(median(NWS$Open),median(NWS$High),median(NWS$Low),median(NWS$Last))
library(moments)
NWS_descriptive_statistics[3,] <- c(skewness(NWS$Open),skewness(NWS$High),skewness(NWS$Low),skewness(NWS$Last))
NWS_descriptive_statistics[4,] <- c(kurtosis(NWS$Open),kurtosis(NWS$High),kurtosis(NWS$Low),kurtosis(NWS$Last))

rownames(NWS_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NWS$LogReturn_Open[i]<-0
NWS$LogReturn_High[i]<-0
NWS$LogReturn_Low[i]<-0
NWS$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NWS)){
  NWS$LogReturn_Open[i]<-ln(NWS$Open[i]/NWS$Open[i-1])
  NWS$LogReturn_High[i]<-ln(NWS$High[i]/NWS$High[i-1])
  NWS$LogReturn_Low[i]<-ln(NWS$Low[i]/NWS$Low[i-1])
  NWS$LogReturn_Last[i]<-ln(NWS$Last[i]/NWS$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(NWS)) {
  NWS$LogReturn_Open_w<-"/"
  NWS$LogReturn_High_w<-"/"
  NWS$LogReturn_Low_w<-"/"
  NWS$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(NWS)){
  if(weekdays.POSIXt(NWS$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(NWS$`Date (GMT)`[i-j])=="Monday"){
        NWS$LogReturn_Open_w[i]<-ln(NWS$Open[i]/NWS$Open[j])
        NWS$LogReturn_High_w[i]<-ln(NWS$High[i]/NWS$High[j])
        NWS$LogReturn_Low_w[i]<-ln(NWS$Low[i]/NWS$Low[j])
        NWS$LogReturn_Last_w[i]<-ln(NWS$Last[i]/NWS$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_NWS_Open <- aggregate(NWS$LogReturn_Open, by = list(format(NWS$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NWS_High <- aggregate(NWS$LogReturn_High, by = list(format(NWS$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NWS_Low <- aggregate(NWS$LogReturn_Low, by = list(format(NWS$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NWS_Last <- aggregate(NWS$LogReturn_Last, by = list(format(NWS$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_NWS_Open,monthly_returns_NWS_High,by="Group.1")
low_last<-merge(monthly_returns_NWS_Low,monthly_returns_NWS_Last,by="Group.1")
monthly_returns_NWS<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_NWS)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_NWS_Open)
remove(monthly_returns_NWS_High)
remove(monthly_returns_NWS_Low)
remove(monthly_returns_NWS_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_NWS_Open <- aggregate(NWS$LogReturn_Open, by = list(format(NWS$`Date (GMT)`, "%Y")), sum)
yearly_returns_NWS_High <- aggregate(NWS$LogReturn_High, by = list(format(NWS$`Date (GMT)`, "%Y")), sum)
yearly_returns_NWS_Low <- aggregate(NWS$LogReturn_Low, by = list(format(NWS$`Date (GMT)`, "%Y")), sum)
yearly_returns_NWS_Last <- aggregate(NWS$LogReturn_Last, by = list(format(NWS$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_NWS_Open,yearly_returns_NWS_High,by="Group.1")
low_last<-merge(yearly_returns_NWS_Low,yearly_returns_NWS_Last,by="Group.1")
yearly_returns_NWS<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_NWS)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_NWS_Open)
remove(yearly_returns_NWS_High)
remove(yearly_returns_NWS_Low)
remove(yearly_returns_NWS_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_NWS$Open)
volatility_high <- sd(yearly_returns_NWS$High)
volatility_low <- sd(yearly_returns_NWS$Low)
volatility_last <- sd(yearly_returns_NWS$Last)

yearly_returns_NWS$Volatility_Open[1] <- "/"
yearly_returns_NWS$Volatility_Open[1] <- sd(yearly_returns_NWS$Open)
yearly_returns_NWS$Volatility_High[1] <- "/"
yearly_returns_NWS$Volatility_High[1] <- sd(yearly_returns_NWS$High)
yearly_returns_NWS$Volatility_Low[1] <- "/"
yearly_returns_NWS$Volatility_Low[1] <- sd(yearly_returns_NWS$Low)
yearly_returns_NWS$Volatility_Last[1] <- "/"
yearly_returns_NWS$Volatility_Last[1] <- sd(yearly_returns_NWS$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NWS)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NWS$Date<- as.character(NWS$Date)

# Create a candlestick chart
fig_NWS <- plot_ly(data = NWS, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS <- fig_NWS %>% layout(title = "NWS Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NWS

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NWS$Date<- as.character(NWS$Date)

# Create a candlestick chart
fig_NWS_lr_d <- plot_ly(data = NWS, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_d <- fig_NWS_lr_d %>% layout(title = "NWS Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NWS_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NWS$Date<- as.character(NWS$Date)

# Create a candlestick chart
fig_NWS_lr_w <- plot_ly(data = NWS, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_w <- fig_NWS_lr_w %>% layout(title = "NWS Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NWS_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_NWS$Month<- as.character(monthly_returns_NWS$Month)

# Create a candlestick chart
fig_NWS_lr_m <- plot_ly(data = monthly_returns_NWS, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_m <- fig_NWS_lr_m %>% layout(title = "NWS Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NWS_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_NWS$Year<- as.character(yearly_returns_NWS$Year)

# Create a candlestick chart
fig_NWS_lr_y <- plot_ly(data = yearly_returns_NWS, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NWS_lr_y <- fig_NWS_lr_y %>% layout(title = "NWS Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NWS_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
NWS$MA5 <- rollmean(NWS$Last, k = 5, fill = NA)
NWS$MA21 <- rollmean(NWS$Last, k = 21, fill = NA)
NWS$MA63 <- rollmean(NWS$Last, k = 63, fill = NA)

ggplot(NWS, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(NWS, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "dashed") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "dashed") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "dashed") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#--------------------------------------------------------------------------------------------------------------
# Using all the gathered information from descriptive measures, returns and moving averages,
# rating companies based on price levels of their stock
#--------------------------------------------------------------------------------------------------------------
