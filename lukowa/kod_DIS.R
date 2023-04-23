
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
DIS <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="DIS")


# DESKRIPTIVNA STATISTIKA

DIS_descriptive_statistics <- data.frame(open=mean(DIS$Open),high=mean(DIS$High),low=mean(DIS$Low),last=mean(DIS$Last)) 
DIS_descriptive_statistics[2,] <- c(median(DIS$Open),median(DIS$High),median(DIS$Low),median(DIS$Last))
library(moments)
DIS_descriptive_statistics[3,] <- c(skewness(DIS$Open),skewness(DIS$High),skewness(DIS$Low),skewness(DIS$Last))
DIS_descriptive_statistics[4,] <- c(kurtosis(DIS$Open),kurtosis(DIS$High),kurtosis(DIS$Low),kurtosis(DIS$Last))

rownames(DIS_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
DIS$LogReturn_Open<-0
DIS$LogReturn_High<-0
DIS$LogReturn_Low<-0
DIS$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(DIS)){
  DIS$LogReturn_Open[i]<-ln(DIS$Open[i]/DIS$Open[i-1])
  DIS$LogReturn_High[i]<-ln(DIS$High[i]/DIS$High[i-1])
  DIS$LogReturn_Low[i]<-ln(DIS$Low[i]/DIS$Low[i-1])
  DIS$LogReturn_Last[i]<-ln(DIS$Last[i]/DIS$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(DIS)) {
  DIS$LogReturn_Open_w<-"/"
  DIS$LogReturn_High_w<-"/"
  DIS$LogReturn_Low_w<-"/"
  DIS$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(DIS)){
  if(weekdays.POSIXt(DIS$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(DIS$`Date (GMT)`[i-j])=="Monday"){
        DIS$LogReturn_Open_w[i]<-ln(DIS$Open[i]/DIS$Open[j])
        DIS$LogReturn_High_w[i]<-ln(DIS$High[i]/DIS$High[j])
        DIS$LogReturn_Low_w[i]<-ln(DIS$Low[i]/DIS$Low[j])
        DIS$LogReturn_Last_w[i]<-ln(DIS$Last[i]/DIS$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_DIS_Open <- aggregate(DIS$LogReturn_Open, by = list(format(DIS$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_DIS_High <- aggregate(DIS$LogReturn_High, by = list(format(DIS$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_DIS_Low <- aggregate(DIS$LogReturn_Low, by = list(format(DIS$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_DIS_Last <- aggregate(DIS$LogReturn_Last, by = list(format(DIS$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_DIS_Open,monthly_returns_DIS_High,by="Group.1")
low_last<-merge(monthly_returns_DIS_Low,monthly_returns_DIS_Last,by="Group.1")
monthly_returns_DIS<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_DIS)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_DIS_Open)
remove(monthly_returns_DIS_High)
remove(monthly_returns_DIS_Low)
remove(monthly_returns_DIS_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_DIS_Open <- aggregate(DIS$LogReturn_Open, by = list(format(DIS$`Date (GMT)`, "%Y")), sum)
yearly_returns_DIS_High <- aggregate(DIS$LogReturn_High, by = list(format(DIS$`Date (GMT)`, "%Y")), sum)
yearly_returns_DIS_Low <- aggregate(DIS$LogReturn_Low, by = list(format(DIS$`Date (GMT)`, "%Y")), sum)
yearly_returns_DIS_Last <- aggregate(DIS$LogReturn_Last, by = list(format(DIS$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_DIS_Open,yearly_returns_DIS_High,by="Group.1")
low_last<-merge(yearly_returns_DIS_Low,yearly_returns_DIS_Last,by="Group.1")
yearly_returns_DIS<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_DIS)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_DIS_Open)
remove(yearly_returns_DIS_High)
remove(yearly_returns_DIS_Low)
remove(yearly_returns_DIS_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_DIS$Open)
volatility_high <- sd(yearly_returns_DIS$High)
volatility_low <- sd(yearly_returns_DIS$Low)
volatility_last <- sd(yearly_returns_DIS$Last)

yearly_returns_DIS$Volatility_Open[1] <- "/"
yearly_returns_DIS$Volatility_Open[1] <- sd(yearly_returns_DIS$Open)
yearly_returns_DIS$Volatility_High[1] <- "/"
yearly_returns_DIS$Volatility_High[1] <- sd(yearly_returns_DIS$High)
yearly_returns_DIS$Volatility_Low[1] <- "/"
yearly_returns_DIS$Volatility_Low[1] <- sd(yearly_returns_DIS$Low)
yearly_returns_DIS$Volatility_Last[1] <- "/"
yearly_returns_DIS$Volatility_Last[1] <- sd(yearly_returns_DIS$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(DIS)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
DIS$Date<- as.character(DIS$Date)

# Create a candlestick chart
fig_DIS <- plot_ly(data = DIS, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS <- fig_DIS %>% layout(title = "DIS Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_DIS

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
DIS$Date<- as.character(DIS$Date)

# Create a candlestick chart
fig_DIS_lr_d <- plot_ly(data = DIS, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_d <- fig_DIS_lr_d %>% layout(title = "DIS Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_DIS_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
DIS$Date<- as.character(DIS$Date)

# Create a candlestick chart
fig_DIS_lr_w <- plot_ly(data = DIS, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_w <- fig_DIS_lr_w %>% layout(title = "DIS Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_DIS_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_DIS$Month<- as.character(monthly_returns_DIS$Month)

# Create a candlestick chart
fig_DIS_lr_m <- plot_ly(data = monthly_returns_DIS, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_m <- fig_DIS_lr_m %>% layout(title = "DIS Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_DIS_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_DIS$Year<- as.character(yearly_returns_DIS$Year)

# Create a candlestick chart
fig_DIS_lr_y <- plot_ly(data = yearly_returns_DIS, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_DIS_lr_y <- fig_DIS_lr_y %>% layout(title = "DIS Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_DIS_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
DIS$MA5 <- rollmean(DIS$Last, k = 5, fill = NA)
DIS$MA21 <- rollmean(DIS$Last, k = 21, fill = NA)
DIS$MA63 <- rollmean(DIS$Last, k = 63, fill = NA)

ggplot(DIS, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(DIS, aes(x = Date, y = Last,group = 1)) +
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













