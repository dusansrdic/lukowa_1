
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
NTDOY <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NTDOY")


# DESKRIPTIVNA STATISTIKA

NTDOY_descriptive_statistics <- data.frame(open=mean(NTDOY$Open),high=mean(NTDOY$High),low=mean(NTDOY$Low),last=mean(NTDOY$Last)) 
NTDOY_descriptive_statistics[2,] <- c(median(NTDOY$Open),median(NTDOY$High),median(NTDOY$Low),median(NTDOY$Last))
library(moments)
NTDOY_descriptive_statistics[3,] <- c(skewness(NTDOY$Open),skewness(NTDOY$High),skewness(NTDOY$Low),skewness(NTDOY$Last))
NTDOY_descriptive_statistics[4,] <- c(kurtosis(NTDOY$Open),kurtosis(NTDOY$High),kurtosis(NTDOY$Low),kurtosis(NTDOY$Last))

rownames(NTDOY_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NTDOY$LogReturn_Open<-0
NTDOY$LogReturn_High<-0
NTDOY$LogReturn_Low<-0
NTDOY$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NTDOY)){
  NTDOY$LogReturn_Open[i]<-ln(NTDOY$Open[i]/NTDOY$Open[i-1])
  NTDOY$LogReturn_High[i]<-ln(NTDOY$High[i]/NTDOY$High[i-1])
  NTDOY$LogReturn_Low[i]<-ln(NTDOY$Low[i]/NTDOY$Low[i-1])
  NTDOY$LogReturn_Last[i]<-ln(NTDOY$Last[i]/NTDOY$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(NTDOY)) {
  NTDOY$LogReturn_Open_w<-"/"
  NTDOY$LogReturn_High_w<-"/"
  NTDOY$LogReturn_Low_w<-"/"
  NTDOY$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(NTDOY)){
  if(weekdays.POSIXt(NTDOY$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(NTDOY$`Date (GMT)`[i-j])=="Monday"){
        NTDOY$LogReturn_Open_w[i]<-ln(NTDOY$Open[i]/NTDOY$Open[j])
        NTDOY$LogReturn_High_w[i]<-ln(NTDOY$High[i]/NTDOY$High[j])
        NTDOY$LogReturn_Low_w[i]<-ln(NTDOY$Low[i]/NTDOY$Low[j])
        NTDOY$LogReturn_Last_w[i]<-ln(NTDOY$Last[i]/NTDOY$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_NTDOY_Open <- aggregate(NTDOY$LogReturn_Open, by = list(format(NTDOY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NTDOY_High <- aggregate(NTDOY$LogReturn_High, by = list(format(NTDOY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NTDOY_Low <- aggregate(NTDOY$LogReturn_Low, by = list(format(NTDOY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NTDOY_Last <- aggregate(NTDOY$LogReturn_Last, by = list(format(NTDOY$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_NTDOY_Open,monthly_returns_NTDOY_High,by="Group.1")
low_last<-merge(monthly_returns_NTDOY_Low,monthly_returns_NTDOY_Last,by="Group.1")
monthly_returns_NTDOY<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_NTDOY)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_NTDOY_Open)
remove(monthly_returns_NTDOY_High)
remove(monthly_returns_NTDOY_Low)
remove(monthly_returns_NTDOY_Last)
remove(open_high)
remove(low_last)

# GONTDOYNJI LOG RETURN

yearly_returns_NTDOY_Open <- aggregate(NTDOY$LogReturn_Open, by = list(format(NTDOY$`Date (GMT)`, "%Y")), sum)
yearly_returns_NTDOY_High <- aggregate(NTDOY$LogReturn_High, by = list(format(NTDOY$`Date (GMT)`, "%Y")), sum)
yearly_returns_NTDOY_Low <- aggregate(NTDOY$LogReturn_Low, by = list(format(NTDOY$`Date (GMT)`, "%Y")), sum)
yearly_returns_NTDOY_Last <- aggregate(NTDOY$LogReturn_Last, by = list(format(NTDOY$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_NTDOY_Open,yearly_returns_NTDOY_High,by="Group.1")
low_last<-merge(yearly_returns_NTDOY_Low,yearly_returns_NTDOY_Last,by="Group.1")
yearly_returns_NTDOY<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_NTDOY)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_NTDOY_Open)
remove(yearly_returns_NTDOY_High)
remove(yearly_returns_NTDOY_Low)
remove(yearly_returns_NTDOY_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_NTDOY$Open)
volatility_high <- sd(yearly_returns_NTDOY$High)
volatility_low <- sd(yearly_returns_NTDOY$Low)
volatility_last <- sd(yearly_returns_NTDOY$Last)

yearly_returns_NTDOY$Volatility_Open[1] <- "/"
yearly_returns_NTDOY$Volatility_Open[1] <- sd(yearly_returns_NTDOY$Open)
yearly_returns_NTDOY$Volatility_High[1] <- "/"
yearly_returns_NTDOY$Volatility_High[1] <- sd(yearly_returns_NTDOY$High)
yearly_returns_NTDOY$Volatility_Low[1] <- "/"
yearly_returns_NTDOY$Volatility_Low[1] <- sd(yearly_returns_NTDOY$Low)
yearly_returns_NTDOY$Volatility_Last[1] <- "/"
yearly_returns_NTDOY$Volatility_Last[1] <- sd(yearly_returns_NTDOY$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NTDOY)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NTDOY$Date<- as.character(NTDOY$Date)

# Create a candlestick chart
fig_NTDOY <- plot_ly(data = NTDOY, type = "candlestick",
                   x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                   increasing = list(fillcolor = "green", line = list(color = "green")),
                   decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY <- fig_NTDOY %>% layout(title = "NTDOY Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NTDOY$Date<- as.character(NTDOY$Date)

# Create a candlestick chart
fig_NTDOY_lr_d <- plot_ly(data = NTDOY, type = "candlestick",
                        x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_d <- fig_NTDOY_lr_d %>% layout(title = "NTDOY Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NTDOY$Date<- as.character(NTDOY$Date)

# Create a candlestick chart
fig_NTDOY_lr_w <- plot_ly(data = NTDOY, type = "candlestick",
                        x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_w <- fig_NTDOY_lr_w %>% layout(title = "NTDOY Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_NTDOY$Month<- as.character(monthly_returns_NTDOY$Month)

# Create a candlestick chart
fig_NTDOY_lr_m <- plot_ly(data = monthly_returns_NTDOY, type = "candlestick",
                        x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_m <- fig_NTDOY_lr_m %>% layout(title = "NTDOY Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_NTDOY$Year<- as.character(yearly_returns_NTDOY$Year)

# Create a candlestick chart
fig_NTDOY_lr_y <- plot_ly(data = yearly_returns_NTDOY, type = "candlestick",
                        x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                        increasing = list(fillcolor = "green", line = list(color = "green")),
                        decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NTDOY_lr_y <- fig_NTDOY_lr_y %>% layout(title = "NTDOY Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# NTDOYplay the chart
fig_NTDOY_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
NTDOY$MA5 <- rollmean(NTDOY$Last, k = 5, fill = NA)
NTDOY$MA21 <- rollmean(NTDOY$Last, k = 21, fill = NA)
NTDOY$MA63 <- rollmean(NTDOY$Last, k = 63, fill = NA)

ggplot(NTDOY, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(NTDOY, aes(x = Date, y = Last,group = 1)) +
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













