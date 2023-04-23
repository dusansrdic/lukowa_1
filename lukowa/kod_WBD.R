
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
WBD <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="WBD")


# DESKRIPTIVNA STATISTIKA

WBD_descriptive_statistics <- data.frame(open=mean(WBD$Open),high=mean(WBD$High),low=mean(WBD$Low),last=mean(WBD$Last)) 
WBD_descriptive_statistics[2,] <- c(median(WBD$Open),median(WBD$High),median(WBD$Low),median(WBD$Last))
library(moments)
WBD_descriptive_statistics[3,] <- c(skewness(WBD$Open),skewness(WBD$High),skewness(WBD$Low),skewness(WBD$Last))
WBD_descriptive_statistics[4,] <- c(kurtosis(WBD$Open),kurtosis(WBD$High),kurtosis(WBD$Low),kurtosis(WBD$Last))

rownames(WBD_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
WBD$LogReturn_Open[i]<-0
WBD$LogReturn_High[i]<-0
WBD$LogReturn_Low[i]<-0
WBD$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(WBD)){
  WBD$LogReturn_Open[i]<-ln(WBD$Open[i]/WBD$Open[i-1])
  WBD$LogReturn_High[i]<-ln(WBD$High[i]/WBD$High[i-1])
  WBD$LogReturn_Low[i]<-ln(WBD$Low[i]/WBD$Low[i-1])
  WBD$LogReturn_Last[i]<-ln(WBD$Last[i]/WBD$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(WBD)) {
  WBD$LogReturn_Open_w<-"/"
  WBD$LogReturn_High_w<-"/"
  WBD$LogReturn_Low_w<-"/"
  WBD$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(WBD)){
  if(weekdays.POSIXt(WBD$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(WBD$`Date (GMT)`[i-j])=="Monday"){
        WBD$LogReturn_Open_w[i]<-ln(WBD$Open[i]/WBD$Open[j])
        WBD$LogReturn_High_w[i]<-ln(WBD$High[i]/WBD$High[j])
        WBD$LogReturn_Low_w[i]<-ln(WBD$Low[i]/WBD$Low[j])
        WBD$LogReturn_Last_w[i]<-ln(WBD$Last[i]/WBD$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_WBD_Open <- aggregate(WBD$LogReturn_Open, by = list(format(WBD$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_WBD_High <- aggregate(WBD$LogReturn_High, by = list(format(WBD$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_WBD_Low <- aggregate(WBD$LogReturn_Low, by = list(format(WBD$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_WBD_Last <- aggregate(WBD$LogReturn_Last, by = list(format(WBD$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_WBD_Open,monthly_returns_WBD_High,by="Group.1")
low_last<-merge(monthly_returns_WBD_Low,monthly_returns_WBD_Last,by="Group.1")
monthly_returns_WBD<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_WBD)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_WBD_Open)
remove(monthly_returns_WBD_High)
remove(monthly_returns_WBD_Low)
remove(monthly_returns_WBD_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_WBD_Open <- aggregate(WBD$LogReturn_Open, by = list(format(WBD$`Date (GMT)`, "%Y")), sum)
yearly_returns_WBD_High <- aggregate(WBD$LogReturn_High, by = list(format(WBD$`Date (GMT)`, "%Y")), sum)
yearly_returns_WBD_Low <- aggregate(WBD$LogReturn_Low, by = list(format(WBD$`Date (GMT)`, "%Y")), sum)
yearly_returns_WBD_Last <- aggregate(WBD$LogReturn_Last, by = list(format(WBD$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_WBD_Open,yearly_returns_WBD_High,by="Group.1")
low_last<-merge(yearly_returns_WBD_Low,yearly_returns_WBD_Last,by="Group.1")
yearly_returns_WBD<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_WBD)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_WBD_Open)
remove(yearly_returns_WBD_High)
remove(yearly_returns_WBD_Low)
remove(yearly_returns_WBD_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_WBD$Open)
volatility_high <- sd(yearly_returns_WBD$High)
volatility_low <- sd(yearly_returns_WBD$Low)
volatility_last <- sd(yearly_returns_WBD$Last)

yearly_returns_WBD$Volatility_Open[1] <- "/"
yearly_returns_WBD$Volatility_Open[1] <- sd(yearly_returns_WBD$Open)
yearly_returns_WBD$Volatility_High[1] <- "/"
yearly_returns_WBD$Volatility_High[1] <- sd(yearly_returns_WBD$High)
yearly_returns_WBD$Volatility_Low[1] <- "/"
yearly_returns_WBD$Volatility_Low[1] <- sd(yearly_returns_WBD$Low)
yearly_returns_WBD$Volatility_Last[1] <- "/"
yearly_returns_WBD$Volatility_Last[1] <- sd(yearly_returns_WBD$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(WBD)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
WBD$Date<- as.character(WBD$Date)

# Create a candlestick chart
fig_WBD <- plot_ly(data = WBD, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD <- fig_WBD %>% layout(title = "WBD Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_WBD

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
WBD$Date<- as.character(WBD$Date)

# Create a candlestick chart
fig_WBD_lr_d <- plot_ly(data = WBD, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_d <- fig_WBD_lr_d %>% layout(title = "WBD Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_WBD_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
WBD$Date<- as.character(WBD$Date)

# Create a candlestick chart
fig_WBD_lr_w <- plot_ly(data = WBD, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_w <- fig_WBD_lr_w %>% layout(title = "WBD Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_WBD_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_WBD$Month<- as.character(monthly_returns_WBD$Month)

# Create a candlestick chart
fig_WBD_lr_m <- plot_ly(data = monthly_returns_WBD, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_m <- fig_WBD_lr_m %>% layout(title = "WBD Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_WBD_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_WBD$Year<- as.character(yearly_returns_WBD$Year)

# Create a candlestick chart
fig_WBD_lr_y <- plot_ly(data = yearly_returns_WBD, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_WBD_lr_y <- fig_WBD_lr_y %>% layout(title = "WBD Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_WBD_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
WBD$MA5 <- rollmean(WBD$Last, k = 5, fill = NA)
WBD$MA21 <- rollmean(WBD$Last, k = 21, fill = NA)
WBD$MA63 <- rollmean(WBD$Last, k = 63, fill = NA)

ggplot(WBD, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(WBD, aes(x = Date, y = Last,group = 1)) +
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
