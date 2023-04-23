
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
EA <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="EA")


# DESKRIPTIVNA STATISTIKA

EA_descriptive_statistics <- data.frame(open=mean(EA$Open),high=mean(EA$High),low=mean(EA$Low),last=mean(EA$Last)) 
EA_descriptive_statistics[2,] <- c(median(EA$Open),median(EA$High),median(EA$Low),median(EA$Last))
library(moments)
EA_descriptive_statistics[3,] <- c(skewness(EA$Open),skewness(EA$High),skewness(EA$Low),skewness(EA$Last))
EA_descriptive_statistics[4,] <- c(kurtosis(EA$Open),kurtosis(EA$High),kurtosis(EA$Low),kurtosis(EA$Last))

rownames(EA_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
EA$LogReturn_Open[i]<-0
EA$LogReturn_High[i]<-0
EA$LogReturn_Low[i]<-0
EA$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(EA)){
  EA$LogReturn_Open[i]<-ln(EA$Open[i]/EA$Open[i-1])
  EA$LogReturn_High[i]<-ln(EA$High[i]/EA$High[i-1])
  EA$LogReturn_Low[i]<-ln(EA$Low[i]/EA$Low[i-1])
  EA$LogReturn_Last[i]<-ln(EA$Last[i]/EA$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(EA)) {
  EA$LogReturn_Open_w<-"/"
  EA$LogReturn_High_w<-"/"
  EA$LogReturn_Low_w<-"/"
  EA$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(EA)){
  if(weekdays.POSIXt(EA$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(EA$`Date (GMT)`[i-j])=="Monday"){
        EA$LogReturn_Open_w[i]<-ln(EA$Open[i]/EA$Open[j])
        EA$LogReturn_High_w[i]<-ln(EA$High[i]/EA$High[j])
        EA$LogReturn_Low_w[i]<-ln(EA$Low[i]/EA$Low[j])
        EA$LogReturn_Last_w[i]<-ln(EA$Last[i]/EA$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_EA_Open <- aggregate(EA$LogReturn_Open, by = list(format(EA$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_EA_High <- aggregate(EA$LogReturn_High, by = list(format(EA$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_EA_Low <- aggregate(EA$LogReturn_Low, by = list(format(EA$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_EA_Last <- aggregate(EA$LogReturn_Last, by = list(format(EA$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_EA_Open,monthly_returns_EA_High,by="Group.1")
low_last<-merge(monthly_returns_EA_Low,monthly_returns_EA_Last,by="Group.1")
monthly_returns_EA<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_EA)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_EA_Open)
remove(monthly_returns_EA_High)
remove(monthly_returns_EA_Low)
remove(monthly_returns_EA_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_EA_Open <- aggregate(EA$LogReturn_Open, by = list(format(EA$`Date (GMT)`, "%Y")), sum)
yearly_returns_EA_High <- aggregate(EA$LogReturn_High, by = list(format(EA$`Date (GMT)`, "%Y")), sum)
yearly_returns_EA_Low <- aggregate(EA$LogReturn_Low, by = list(format(EA$`Date (GMT)`, "%Y")), sum)
yearly_returns_EA_Last <- aggregate(EA$LogReturn_Last, by = list(format(EA$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_EA_Open,yearly_returns_EA_High,by="Group.1")
low_last<-merge(yearly_returns_EA_Low,yearly_returns_EA_Last,by="Group.1")
yearly_returns_EA<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_EA)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_EA_Open)
remove(yearly_returns_EA_High)
remove(yearly_returns_EA_Low)
remove(yearly_returns_EA_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_EA$Open)
volatility_high <- sd(yearly_returns_EA$High)
volatility_low <- sd(yearly_returns_EA$Low)
volatility_last <- sd(yearly_returns_EA$Last)

yearly_returns_EA$Volatility_Open[1] <- "/"
yearly_returns_EA$Volatility_Open[1] <- sd(yearly_returns_EA$Open)
yearly_returns_EA$Volatility_High[1] <- "/"
yearly_returns_EA$Volatility_High[1] <- sd(yearly_returns_EA$High)
yearly_returns_EA$Volatility_Low[1] <- "/"
yearly_returns_EA$Volatility_Low[1] <- sd(yearly_returns_EA$Low)
yearly_returns_EA$Volatility_Last[1] <- "/"
yearly_returns_EA$Volatility_Last[1] <- sd(yearly_returns_EA$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(EA)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
EA$Date<- as.character(EA$Date)

# Create a candlestick chart
fig_EA <- plot_ly(data = EA, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA <- fig_EA %>% layout(title = "EA Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_EA

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
EA$Date<- as.character(EA$Date)

# Create a candlestick chart
fig_EA_lr_d <- plot_ly(data = EA, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_d <- fig_EA_lr_d %>% layout(title = "EA Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_EA_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
EA$Date<- as.character(EA$Date)

# Create a candlestick chart
fig_EA_lr_w <- plot_ly(data = EA, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_w <- fig_EA_lr_w %>% layout(title = "EA Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_EA_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_EA$Month<- as.character(monthly_returns_EA$Month)

# Create a candlestick chart
fig_EA_lr_m <- plot_ly(data = monthly_returns_EA, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_m <- fig_EA_lr_m %>% layout(title = "EA Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_EA_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_EA$Year<- as.character(yearly_returns_EA$Year)

# Create a candlestick chart
fig_EA_lr_y <- plot_ly(data = yearly_returns_EA, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_EA_lr_y <- fig_EA_lr_y %>% layout(title = "EA Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_EA_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
EA$MA5 <- rollmean(EA$Last, k = 5, fill = NA)
EA$MA21 <- rollmean(EA$Last, k = 21, fill = NA)
EA$MA63 <- rollmean(EA$Last, k = 63, fill = NA)

ggplot(EA, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(EA, aes(x = Date, y = Last,group = 1)) +
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
