
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
NFLX <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="NFLX")


# DESKRIPTIVNA STATISTIKA

NFLX_descriptive_statistics <- data.frame(open=mean(NFLX$Open),high=mean(NFLX$High),low=mean(NFLX$Low),last=mean(NFLX$Last)) 
NFLX_descriptive_statistics[2,] <- c(median(NFLX$Open),median(NFLX$High),median(NFLX$Low),median(NFLX$Last))
library(moments)
NFLX_descriptive_statistics[3,] <- c(skewness(NFLX$Open),skewness(NFLX$High),skewness(NFLX$Low),skewness(NFLX$Last))
NFLX_descriptive_statistics[4,] <- c(kurtosis(NFLX$Open),kurtosis(NFLX$High),kurtosis(NFLX$Low),kurtosis(NFLX$Last))

rownames(NFLX_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
NFLX$LogReturn_Open[i]<-0
NFLX$LogReturn_High[i]<-0
NFLX$LogReturn_Low[i]<-0
NFLX$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(NFLX)){
  NFLX$LogReturn_Open[i]<-ln(NFLX$Open[i]/NFLX$Open[i-1])
  NFLX$LogReturn_High[i]<-ln(NFLX$High[i]/NFLX$High[i-1])
  NFLX$LogReturn_Low[i]<-ln(NFLX$Low[i]/NFLX$Low[i-1])
  NFLX$LogReturn_Last[i]<-ln(NFLX$Last[i]/NFLX$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(NFLX)) {
  NFLX$LogReturn_Open_w<-"/"
  NFLX$LogReturn_High_w<-"/"
  NFLX$LogReturn_Low_w<-"/"
  NFLX$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(NFLX)){
  if(weekdays.POSIXt(NFLX$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(NFLX$`Date (GMT)`[i-j])=="Monday"){
        NFLX$LogReturn_Open_w[i]<-ln(NFLX$Open[i]/NFLX$Open[j])
        NFLX$LogReturn_High_w[i]<-ln(NFLX$High[i]/NFLX$High[j])
        NFLX$LogReturn_Low_w[i]<-ln(NFLX$Low[i]/NFLX$Low[j])
        NFLX$LogReturn_Last_w[i]<-ln(NFLX$Last[i]/NFLX$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_NFLX_Open <- aggregate(NFLX$LogReturn_Open, by = list(format(NFLX$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NFLX_High <- aggregate(NFLX$LogReturn_High, by = list(format(NFLX$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NFLX_Low <- aggregate(NFLX$LogReturn_Low, by = list(format(NFLX$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_NFLX_Last <- aggregate(NFLX$LogReturn_Last, by = list(format(NFLX$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_NFLX_Open,monthly_returns_NFLX_High,by="Group.1")
low_last<-merge(monthly_returns_NFLX_Low,monthly_returns_NFLX_Last,by="Group.1")
monthly_returns_NFLX<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_NFLX)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_NFLX_Open)
remove(monthly_returns_NFLX_High)
remove(monthly_returns_NFLX_Low)
remove(monthly_returns_NFLX_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_NFLX_Open <- aggregate(NFLX$LogReturn_Open, by = list(format(NFLX$`Date (GMT)`, "%Y")), sum)
yearly_returns_NFLX_High <- aggregate(NFLX$LogReturn_High, by = list(format(NFLX$`Date (GMT)`, "%Y")), sum)
yearly_returns_NFLX_Low <- aggregate(NFLX$LogReturn_Low, by = list(format(NFLX$`Date (GMT)`, "%Y")), sum)
yearly_returns_NFLX_Last <- aggregate(NFLX$LogReturn_Last, by = list(format(NFLX$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_NFLX_Open,yearly_returns_NFLX_High,by="Group.1")
low_last<-merge(yearly_returns_NFLX_Low,yearly_returns_NFLX_Last,by="Group.1")
yearly_returns_NFLX<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_NFLX)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_NFLX_Open)
remove(yearly_returns_NFLX_High)
remove(yearly_returns_NFLX_Low)
remove(yearly_returns_NFLX_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_NFLX$Open)
volatility_high <- sd(yearly_returns_NFLX$High)
volatility_low <- sd(yearly_returns_NFLX$Low)
volatility_last <- sd(yearly_returns_NFLX$Last)

yearly_returns_NFLX$Volatility_Open[1] <- "/"
yearly_returns_NFLX$Volatility_Open[1] <- sd(yearly_returns_NFLX$Open)
yearly_returns_NFLX$Volatility_High[1] <- "/"
yearly_returns_NFLX$Volatility_High[1] <- sd(yearly_returns_NFLX$High)
yearly_returns_NFLX$Volatility_Low[1] <- "/"
yearly_returns_NFLX$Volatility_Low[1] <- sd(yearly_returns_NFLX$Low)
yearly_returns_NFLX$Volatility_Last[1] <- "/"
yearly_returns_NFLX$Volatility_Last[1] <- sd(yearly_returns_NFLX$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(NFLX)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NFLX$Date<- as.character(NFLX$Date)

# Create a candlestick chart
fig_NFLX <- plot_ly(data = NFLX, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX <- fig_NFLX %>% layout(title = "NFLX Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NFLX

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NFLX$Date<- as.character(NFLX$Date)

# Create a candlestick chart
fig_NFLX_lr_d <- plot_ly(data = NFLX, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_d <- fig_NFLX_lr_d %>% layout(title = "NFLX Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NFLX_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
NFLX$Date<- as.character(NFLX$Date)

# Create a candlestick chart
fig_NFLX_lr_w <- plot_ly(data = NFLX, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_w <- fig_NFLX_lr_w %>% layout(title = "NFLX Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NFLX_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_NFLX$Month<- as.character(monthly_returns_NFLX$Month)

# Create a candlestick chart
fig_NFLX_lr_m <- plot_ly(data = monthly_returns_NFLX, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_m <- fig_NFLX_lr_m %>% layout(title = "NFLX Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NFLX_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_NFLX$Year<- as.character(yearly_returns_NFLX$Year)

# Create a candlestick chart
fig_NFLX_lr_y <- plot_ly(data = yearly_returns_NFLX, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_NFLX_lr_y <- fig_NFLX_lr_y %>% layout(title = "NFLX Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_NFLX_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
NFLX$MA5 <- rollmean(NFLX$Last, k = 5, fill = NA)
NFLX$MA21 <- rollmean(NFLX$Last, k = 21, fill = NA)
NFLX$MA63 <- rollmean(NFLX$Last, k = 63, fill = NA)

ggplot(NFLX, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(NFLX, aes(x = Date, y = Last,group = 1)) +
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
