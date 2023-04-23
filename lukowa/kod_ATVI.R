
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
ATVI <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="ATVI")


# DESKRIPTIVNA STATISTIKA

ATVI_descriptive_statistics <- data.frame(open=mean(ATVI$Open),high=mean(ATVI$High),low=mean(ATVI$Low),last=mean(ATVI$Last)) 
ATVI_descriptive_statistics[2,] <- c(median(ATVI$Open),median(ATVI$High),median(ATVI$Low),median(ATVI$Last))
library(moments)
ATVI_descriptive_statistics[3,] <- c(skewness(ATVI$Open),skewness(ATVI$High),skewness(ATVI$Low),skewness(ATVI$Last))
ATVI_descriptive_statistics[4,] <- c(kurtosis(ATVI$Open),kurtosis(ATVI$High),kurtosis(ATVI$Low),kurtosis(ATVI$Last))

rownames(ATVI_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
ATVI$LogReturn_Open[i]<-0
ATVI$LogReturn_High[i]<-0
ATVI$LogReturn_Low[i]<-0
ATVI$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI)){
  ATVI$LogReturn_Open[i]<-ln(ATVI$Open[i]/ATVI$Open[i-1])
  ATVI$LogReturn_High[i]<-ln(ATVI$High[i]/ATVI$High[i-1])
  ATVI$LogReturn_Low[i]<-ln(ATVI$Low[i]/ATVI$Low[i-1])
  ATVI$LogReturn_Last[i]<-ln(ATVI$Last[i]/ATVI$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(ATVI)) {
  ATVI$LogReturn_Open_w<-"/"
  ATVI$LogReturn_High_w<-"/"
  ATVI$LogReturn_Low_w<-"/"
  ATVI$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(ATVI)){
  if(weekdays.POSIXt(ATVI$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(ATVI$`Date (GMT)`[i-j])=="Monday"){
        ATVI$LogReturn_Open_w[i]<-ln(ATVI$Open[i]/ATVI$Open[j])
        ATVI$LogReturn_High_w[i]<-ln(ATVI$High[i]/ATVI$High[j])
        ATVI$LogReturn_Low_w[i]<-ln(ATVI$Low[i]/ATVI$Low[j])
        ATVI$LogReturn_Last_w[i]<-ln(ATVI$Last[i]/ATVI$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_ATVI_Open <- aggregate(ATVI$LogReturn_Open, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_ATVI_High <- aggregate(ATVI$LogReturn_High, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_ATVI_Low <- aggregate(ATVI$LogReturn_Low, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_ATVI_Last <- aggregate(ATVI$LogReturn_Last, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_ATVI_Open,monthly_returns_ATVI_High,by="Group.1")
low_last<-merge(monthly_returns_ATVI_Low,monthly_returns_ATVI_Last,by="Group.1")
monthly_returns_ATVI<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_ATVI)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_ATVI_Open)
remove(monthly_returns_ATVI_High)
remove(monthly_returns_ATVI_Low)
remove(monthly_returns_ATVI_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_ATVI_Open <- aggregate(ATVI$LogReturn_Open, by = list(format(ATVI$`Date (GMT)`, "%Y")), sum)
yearly_returns_ATVI_High <- aggregate(ATVI$LogReturn_High, by = list(format(ATVI$`Date (GMT)`, "%Y")), sum)
yearly_returns_ATVI_Low <- aggregate(ATVI$LogReturn_Low, by = list(format(ATVI$`Date (GMT)`, "%Y")), sum)
yearly_returns_ATVI_Last <- aggregate(ATVI$LogReturn_Last, by = list(format(ATVI$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_ATVI_Open,yearly_returns_ATVI_High,by="Group.1")
low_last<-merge(yearly_returns_ATVI_Low,yearly_returns_ATVI_Last,by="Group.1")
yearly_returns_ATVI<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_ATVI)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_ATVI_Open)
remove(yearly_returns_ATVI_High)
remove(yearly_returns_ATVI_Low)
remove(yearly_returns_ATVI_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_ATVI$Open)
volatility_high <- sd(yearly_returns_ATVI$High)
volatility_low <- sd(yearly_returns_ATVI$Low)
volatility_last <- sd(yearly_returns_ATVI$Last)

yearly_returns_ATVI$Volatility_Open[1] <- "/"
yearly_returns_ATVI$Volatility_Open[1] <- sd(yearly_returns_ATVI$Open)
yearly_returns_ATVI$Volatility_High[1] <- "/"
yearly_returns_ATVI$Volatility_High[1] <- sd(yearly_returns_ATVI$High)
yearly_returns_ATVI$Volatility_Low[1] <- "/"
yearly_returns_ATVI$Volatility_Low[1] <- sd(yearly_returns_ATVI$Low)
yearly_returns_ATVI$Volatility_Last[1] <- "/"
yearly_returns_ATVI$Volatility_Last[1] <- sd(yearly_returns_ATVI$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(ATVI)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig_ATVI <- plot_ly(data = ATVI, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI <- fig_ATVI %>% layout(title = "ATVI Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_ATVI

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig_ATVI_lr_d <- plot_ly(data = ATVI, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_d <- fig_ATVI_lr_d %>% layout(title = "ATVI Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_ATVI_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig_ATVI_lr_w <- plot_ly(data = ATVI, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_w <- fig_ATVI_lr_w %>% layout(title = "ATVI Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_ATVI_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_ATVI$Month<- as.character(monthly_returns_ATVI$Month)

# Create a candlestick chart
fig_ATVI_lr_m <- plot_ly(data = monthly_returns_ATVI, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_m <- fig_ATVI_lr_m %>% layout(title = "ATVI Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_ATVI_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_ATVI$Year<- as.character(yearly_returns_ATVI$Year)

# Create a candlestick chart
fig_ATVI_lr_y <- plot_ly(data = yearly_returns_ATVI, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_ATVI_lr_y <- fig_ATVI_lr_y %>% layout(title = "ATVI Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_ATVI_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
ATVI$MA5 <- rollmean(ATVI$Last, k = 5, fill = NA)
ATVI$MA21 <- rollmean(ATVI$Last, k = 21, fill = NA)
ATVI$MA63 <- rollmean(ATVI$Last, k = 63, fill = NA)

ggplot(ATVI, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(ATVI, aes(x = Date, y = Last,group = 1)) +
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
