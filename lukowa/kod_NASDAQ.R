
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
NASDAQ <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NASDAQ")


# DESKRIPTIVNA STATISTIKA

NASDAQ_descriptive_statistics <- data.frame(open=mean(NASDAQ$Open),high=mean(NASDAQ$High),low=mean(NASDAQ$Low),last=mean(NASDAQ$Last)) 
NASDAQ_descriptive_statistics[2,] <- c(median(NASDAQ$Open),median(NASDAQ$High),median(NASDAQ$Low),median(NASDAQ$Last))
library(moments)
NASDAQ_descriptive_statistics[3,] <- c(skewness(NASDAQ$Open),skewness(NASDAQ$High),skewness(NASDAQ$Low),skewness(NASDAQ$Last))
NASDAQ_descriptive_statistics[4,] <- c(kurtosis(NASDAQ$Open),kurtosis(NASDAQ$High),kurtosis(NASDAQ$Low),kurtosis(NASDAQ$Last))

rownames(NASDAQ_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NASDAQ$LogReturn_Open[i]<-0
NASDAQ$LogReturn_High[i]<-0
NASDAQ$LogReturn_Low[i]<-0
NASDAQ$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NASDAQ)){
  NASDAQ$LogReturn_Open[i]<-ln(NASDAQ$Open[i]/NASDAQ$Open[i-1])
  NASDAQ$LogReturn_High[i]<-ln(NASDAQ$High[i]/NASDAQ$High[i-1])
  NASDAQ$LogReturn_Low[i]<-ln(NASDAQ$Low[i]/NASDAQ$Low[i-1])
  NASDAQ$LogReturn_Last[i]<-ln(NASDAQ$Last[i]/NASDAQ$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(NASDAQ)) {
  NASDAQ$LogReturn_Open_w<-"/"
  NASDAQ$LogReturn_High_w<-"/"
  NASDAQ$LogReturn_Low_w<-"/"
  NASDAQ$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(NASDAQ)){
  if(weekdays.POSIXt(NASDAQ$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(NASDAQ$`Date (GMT)`[i-j])=="Monday"){
        NASDAQ$LogReturn_Open_w[i]<-ln(NASDAQ$Open[i]/NASDAQ$Open[j])
        NASDAQ$LogReturn_High_w[i]<-ln(NASDAQ$High[i]/NASDAQ$High[j])
        NASDAQ$LogReturn_Low_w[i]<-ln(NASDAQ$Low[i]/NASDAQ$Low[j])
        NASDAQ$LogReturn_Last_w[i]<-ln(NASDAQ$Last[i]/NASDAQ$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_NASDAQ_Open <- aggregate(NASDAQ$LogReturn_Open, by = list(format(NASDAQ$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NASDAQ_High <- aggregate(NASDAQ$LogReturn_High, by = list(format(NASDAQ$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NASDAQ_Low <- aggregate(NASDAQ$LogReturn_Low, by = list(format(NASDAQ$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NASDAQ_Last <- aggregate(NASDAQ$LogReturn_Last, by = list(format(NASDAQ$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_NASDAQ_Open,monthly_returns_NASDAQ_High,by="Group.1")
low_last<-merge(monthly_returns_NASDAQ_Low,monthly_returns_NASDAQ_Last,by="Group.1")
monthly_returns_NASDAQ<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_NASDAQ)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_NASDAQ_Open)
remove(monthly_returns_NASDAQ_High)
remove(monthly_returns_NASDAQ_Low)
remove(monthly_returns_NASDAQ_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_NASDAQ_Open <- aggregate(NASDAQ$LogReturn_Open, by = list(format(NASDAQ$`Date (GMT)`, "%Y")), sum)
yearly_returns_NASDAQ_High <- aggregate(NASDAQ$LogReturn_High, by = list(format(NASDAQ$`Date (GMT)`, "%Y")), sum)
yearly_returns_NASDAQ_Low <- aggregate(NASDAQ$LogReturn_Low, by = list(format(NASDAQ$`Date (GMT)`, "%Y")), sum)
yearly_returns_NASDAQ_Last <- aggregate(NASDAQ$LogReturn_Last, by = list(format(NASDAQ$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_NASDAQ_Open,yearly_returns_NASDAQ_High,by="Group.1")
low_last<-merge(yearly_returns_NASDAQ_Low,yearly_returns_NASDAQ_Last,by="Group.1")
yearly_returns_NASDAQ<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_NASDAQ)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_NASDAQ_Open)
remove(yearly_returns_NASDAQ_High)
remove(yearly_returns_NASDAQ_Low)
remove(yearly_returns_NASDAQ_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_NASDAQ$Open)
volatility_high <- sd(yearly_returns_NASDAQ$High)
volatility_low <- sd(yearly_returns_NASDAQ$Low)
volatility_last <- sd(yearly_returns_NASDAQ$Last)

yearly_returns_NASDAQ$Volatility_Open[1] <- "/"
yearly_returns_NASDAQ$Volatility_Open[1] <- sd(yearly_returns_NASDAQ$Open)
yearly_returns_NASDAQ$Volatility_High[1] <- "/"
yearly_returns_NASDAQ$Volatility_High[1] <- sd(yearly_returns_NASDAQ$High)
yearly_returns_NASDAQ$Volatility_Low[1] <- "/"
yearly_returns_NASDAQ$Volatility_Low[1] <- sd(yearly_returns_NASDAQ$Low)
yearly_returns_NASDAQ$Volatility_Last[1] <- "/"
yearly_returns_NASDAQ$Volatility_Last[1] <- sd(yearly_returns_NASDAQ$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NASDAQ)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NASDAQ$Date<- as.character(NASDAQ$Date)

# Create a candlestick chart
fig_NASDAQ <- plot_ly(data = NASDAQ, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ <- fig_NASDAQ %>% layout(title = "NASDAQ Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NASDAQ

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NASDAQ$Date<- as.character(NASDAQ$Date)

# Create a candlestick chart
fig_NASDAQ_lr_d <- plot_ly(data = NASDAQ, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_d <- fig_NASDAQ_lr_d %>% layout(title = "NASDAQ Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NASDAQ_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NASDAQ$Date<- as.character(NASDAQ$Date)

# Create a candlestick chart
fig_NASDAQ_lr_w <- plot_ly(data = NASDAQ, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_w <- fig_NASDAQ_lr_w %>% layout(title = "NASDAQ Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NASDAQ_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_NASDAQ$Month<- as.character(monthly_returns_NASDAQ$Month)

# Create a candlestick chart
fig_NASDAQ_lr_m <- plot_ly(data = monthly_returns_NASDAQ, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_m <- fig_NASDAQ_lr_m %>% layout(title = "NASDAQ Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NASDAQ_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_NASDAQ$Year<- as.character(yearly_returns_NASDAQ$Year)

# Create a candlestick chart
fig_NASDAQ_lr_y <- plot_ly(data = yearly_returns_NASDAQ, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NASDAQ_lr_y <- fig_NASDAQ_lr_y %>% layout(title = "NASDAQ Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NASDAQ_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
NASDAQ$MA5 <- rollmean(NASDAQ$Last, k = 5, fill = NA)
NASDAQ$MA21 <- rollmean(NASDAQ$Last, k = 21, fill = NA)
NASDAQ$MA63 <- rollmean(NASDAQ$Last, k = 63, fill = NA)

ggplot(NASDAQ, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(NASDAQ, aes(x = Date, y = Last,group = 1)) +
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
