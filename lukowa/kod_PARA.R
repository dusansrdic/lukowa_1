
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
PARA <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="PARA")


# DESKRIPTIVNA STATISTIKA

PARA_descriptive_statistics <- data.frame(open=mean(PARA$Open),high=mean(PARA$High),low=mean(PARA$Low),last=mean(PARA$Last)) 
PARA_descriptive_statistics[2,] <- c(median(PARA$Open),median(PARA$High),median(PARA$Low),median(PARA$Last))
library(moments)
PARA_descriptive_statistics[3,] <- c(skewness(PARA$Open),skewness(PARA$High),skewness(PARA$Low),skewness(PARA$Last))
PARA_descriptive_statistics[4,] <- c(kurtosis(PARA$Open),kurtosis(PARA$High),kurtosis(PARA$Low),kurtosis(PARA$Last))

rownames(PARA_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
PARA$LogReturn_Open[i]<-0
PARA$LogReturn_High[i]<-0
PARA$LogReturn_Low[i]<-0
PARA$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(PARA)){
  PARA$LogReturn_Open[i]<-ln(PARA$Open[i]/PARA$Open[i-1])
  PARA$LogReturn_High[i]<-ln(PARA$High[i]/PARA$High[i-1])
  PARA$LogReturn_Low[i]<-ln(PARA$Low[i]/PARA$Low[i-1])
  PARA$LogReturn_Last[i]<-ln(PARA$Last[i]/PARA$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(PARA)) {
  PARA$LogReturn_Open_w<-"/"
  PARA$LogReturn_High_w<-"/"
  PARA$LogReturn_Low_w<-"/"
  PARA$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(PARA)){
  if(weekdays.POSIXt(PARA$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(PARA$`Date (GMT)`[i-j])=="Monday"){
        PARA$LogReturn_Open_w[i]<-ln(PARA$Open[i]/PARA$Open[j])
        PARA$LogReturn_High_w[i]<-ln(PARA$High[i]/PARA$High[j])
        PARA$LogReturn_Low_w[i]<-ln(PARA$Low[i]/PARA$Low[j])
        PARA$LogReturn_Last_w[i]<-ln(PARA$Last[i]/PARA$Last[j])
      }
    }
  }
}

# MESECNI LOG RETURN

monthly_returns_PARA_Open <- aggregate(PARA$LogReturn_Open, by = list(format(PARA$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_PARA_High <- aggregate(PARA$LogReturn_High, by = list(format(PARA$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_PARA_Low <- aggregate(PARA$LogReturn_Low, by = list(format(PARA$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_PARA_Last <- aggregate(PARA$LogReturn_Last, by = list(format(PARA$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_PARA_Open,monthly_returns_PARA_High,by="Group.1")
low_last<-merge(monthly_returns_PARA_Low,monthly_returns_PARA_Last,by="Group.1")
monthly_returns_PARA<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_PARA)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_PARA_Open)
remove(monthly_returns_PARA_High)
remove(monthly_returns_PARA_Low)
remove(monthly_returns_PARA_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_PARA_Open <- aggregate(PARA$LogReturn_Open, by = list(format(PARA$`Date (GMT)`, "%Y")), sum)
yearly_returns_PARA_High <- aggregate(PARA$LogReturn_High, by = list(format(PARA$`Date (GMT)`, "%Y")), sum)
yearly_returns_PARA_Low <- aggregate(PARA$LogReturn_Low, by = list(format(PARA$`Date (GMT)`, "%Y")), sum)
yearly_returns_PARA_Last <- aggregate(PARA$LogReturn_Last, by = list(format(PARA$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_PARA_Open,yearly_returns_PARA_High,by="Group.1")
low_last<-merge(yearly_returns_PARA_Low,yearly_returns_PARA_Last,by="Group.1")
yearly_returns_PARA<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_PARA)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_PARA_Open)
remove(yearly_returns_PARA_High)
remove(yearly_returns_PARA_Low)
remove(yearly_returns_PARA_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_PARA$Open)
volatility_high <- sd(yearly_returns_PARA$High)
volatility_low <- sd(yearly_returns_PARA$Low)
volatility_last <- sd(yearly_returns_PARA$Last)

yearly_returns_PARA$Volatility_Open[1] <- "/"
yearly_returns_PARA$Volatility_Open[1] <- sd(yearly_returns_PARA$Open)
yearly_returns_PARA$Volatility_High[1] <- "/"
yearly_returns_PARA$Volatility_High[1] <- sd(yearly_returns_PARA$High)
yearly_returns_PARA$Volatility_Low[1] <- "/"
yearly_returns_PARA$Volatility_Low[1] <- sd(yearly_returns_PARA$Low)
yearly_returns_PARA$Volatility_Last[1] <- "/"
yearly_returns_PARA$Volatility_Last[1] <- sd(yearly_returns_PARA$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(PARA)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
PARA$Date<- as.character(PARA$Date)

# Create a candlestick chart
fig_PARA <- plot_ly(data = PARA, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA <- fig_PARA %>% layout(title = "PARA Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_PARA

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
PARA$Date<- as.character(PARA$Date)

# Create a candlestick chart
fig_PARA_lr_d <- plot_ly(data = PARA, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_d <- fig_PARA_lr_d %>% layout(title = "PARA Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_PARA_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
PARA$Date<- as.character(PARA$Date)

# Create a candlestick chart
fig_PARA_lr_w <- plot_ly(data = PARA, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_w <- fig_PARA_lr_w %>% layout(title = "PARA Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_PARA_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
monthly_returns_PARA$Month<- as.character(monthly_returns_PARA$Month)

# Create a candlestick chart
fig_PARA_lr_m <- plot_ly(data = monthly_returns_PARA, type = "candlestick",
                          x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_m <- fig_PARA_lr_m %>% layout(title = "PARA Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_PARA_lr_m

# YEARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
yearly_returns_PARA$Year<- as.character(yearly_returns_PARA$Year)

# Create a candlestick chart
fig_PARA_lr_y <- plot_ly(data = yearly_returns_PARA, type = "candlestick",
                          x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_y <- fig_PARA_lr_y %>% layout(title = "PARA Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_PARA_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
PARA$MA5 <- rollmean(PARA$Last, k = 5, fill = NA)
PARA$MA21 <- rollmean(PARA$Last, k = 21, fill = NA)
PARA$MA63 <- rollmean(PARA$Last, k = 63, fill = NA)

ggplot(PARA, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(PARA, aes(x = Date, y = Last,group = 1)) +
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
