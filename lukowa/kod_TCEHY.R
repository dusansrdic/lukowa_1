
library(SciViews)

# UCITAVANJE TABELE

install.packages("readxl")
library(readxl)
TCEHY <- read_excel("LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022 (1).xlsx", sheet="TCEHY")


# DESKRIPTIVNA STATISTIKA

TCEHY_descriptive_statistics <- data.frame(open=mean(TCEHY$Open),high=mean(TCEHY$High),low=mean(TCEHY$Low),last=mean(TCEHY$Last)) 
TCEHY_descriptive_statistics[2,] <- c(median(TCEHY$Open),median(TCEHY$High),median(TCEHY$Low),median(TCEHY$Last))
library(moments)
TCEHY_descriptive_statistics[3,] <- c(skewness(TCEHY$Open),skewness(TCEHY$High),skewness(TCEHY$Low),skewness(TCEHY$Last))
TCEHY_descriptive_statistics[4,] <- c(kurtosis(TCEHY$Open),kurtosis(TCEHY$High),kurtosis(TCEHY$Low),kurtosis(TCEHY$Last))

rownames(TCEHY_descriptive_statistics) <- c("MEAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
TCEHY$LogReturn_Open[i]<-0
TCEHY$LogReturn_High[i]<-0
TCEHY$LogReturn_Low[i]<-0
TCEHY$LogReturn_Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(TCEHY)){
  TCEHY$LogReturn_Open[i]<-ln(TCEHY$Open[i]/TCEHY$Open[i-1])
  TCEHY$LogReturn_High[i]<-ln(TCEHY$High[i]/TCEHY$High[i-1])
  TCEHY$LogReturn_Low[i]<-ln(TCEHY$Low[i]/TCEHY$Low[i-1])
  TCEHY$LogReturn_Last[i]<-ln(TCEHY$Last[i]/TCEHY$Last[i-1])
}



# NEDELJNI LOG RETURN 

for (i in nrow(TCEHY)) {
  TCEHY$LogReturn_Open_w<-"/"
  TCEHY$LogReturn_High_w<-"/"
  TCEHY$LogReturn_Low_w<-"/"
  TCEHY$LogReturn_Last_w<-"/"
}

for(i in 9:nrow(TCEHY)){
            if(weekdays.POSIXt(TCEHY$`Date (GMT)`[i])=="Monday"){
                              for(j in 1:5){
                                if(weekdays.POSIXt(TCEHY$`Date (GMT)`[i-j])=="Monday"){
                                  TCEHY$LogReturn_Open_w[i]<-ln(TCEHY$Open[i]/TCEHY$Open[j])
                                  TCEHY$LogReturn_High_w[i]<-ln(TCEHY$High[i]/TCEHY$High[j])
                                  TCEHY$LogReturn_Low_w[i]<-ln(TCEHY$Low[i]/TCEHY$Low[j])
                                  TCEHY$LogReturn_Last_w[i]<-ln(TCEHY$Last[i]/TCEHY$Last[j])
                                }
                              }
                            }
}

# MESECNI LOG RETURN

monthly_returns_TCEHY_Open <- aggregate(TCEHY$LogReturn_Open, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TCEHY_High <- aggregate(TCEHY$LogReturn_High, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TCEHY_Low <- aggregate(TCEHY$LogReturn_Low, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_TCEHY_Last <- aggregate(TCEHY$LogReturn_Last, by = list(format(TCEHY$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_TCEHY_Open,monthly_returns_TCEHY_High,by="Group.1")
low_last<-merge(monthly_returns_TCEHY_Low,monthly_returns_TCEHY_Last,by="Group.1")
monthly_returns_TCEHY<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_TCEHY)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_TCEHY_Open)
remove(monthly_returns_TCEHY_High)
remove(monthly_returns_TCEHY_Low)
remove(monthly_returns_TCEHY_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_TCEHY_Open <- aggregate(TCEHY$LogReturn_Open, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)
yearly_returns_TCEHY_High <- aggregate(TCEHY$LogReturn_High, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)
yearly_returns_TCEHY_Low <- aggregate(TCEHY$LogReturn_Low, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)
yearly_returns_TCEHY_Last <- aggregate(TCEHY$LogReturn_Last, by = list(format(TCEHY$`Date (GMT)`, "%Y")), sum)

open_high<-merge(yearly_returns_TCEHY_Open,yearly_returns_TCEHY_High,by="Group.1")
low_last<-merge(yearly_returns_TCEHY_Low,yearly_returns_TCEHY_Last,by="Group.1")
yearly_returns_TCEHY<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_TCEHY)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_TCEHY_Open)
remove(yearly_returns_TCEHY_High)
remove(yearly_returns_TCEHY_Low)
remove(yearly_returns_TCEHY_Last)
remove(open_high)
remove(low_last)

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns
#--------------------------------------------------------------------------------------------------------------

install.packages("quantmod")
library(quantmod)

volatility_open <- sd(yearly_returns_TCEHY$Open)
volatility_high <- sd(yearly_returns_TCEHY$High)
volatility_low <- sd(yearly_returns_TCEHY$Low)
volatility_last <- sd(yearly_returns_TCEHY$Last)

yearly_returns_TCEHY$Volatility_Open[1] <- "/"
yearly_returns_TCEHY$Volatility_Open[1] <- sd(yearly_returns_TCEHY$Open)
yearly_returns_TCEHY$Volatility_High[1] <- "/"
yearly_returns_TCEHY$Volatility_High[1] <- sd(yearly_returns_TCEHY$High)
yearly_returns_TCEHY$Volatility_Low[1] <- "/"
yearly_returns_TCEHY$Volatility_Low[1] <- sd(yearly_returns_TCEHY$Low)
yearly_returns_TCEHY$Volatility_Last[1] <- "/"
yearly_returns_TCEHY$Volatility_Last[1] <- sd(yearly_returns_TCEHY$Last)


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(TCEHY)[1]<-"Date"
  
# RAW PRICES
  
  # CANDLESTICK CHART PATTERN
    
    # Load libraries
    install.packages("plotly")
    library(plotly)
    library(quantmod)
    
    # Convert date to character for better labeling
    TCEHY$Date<- as.character(TCEHY$Date)
    
    # Create a candlestick chart
    fig_TCEHY <- plot_ly(data = TCEHY, type = "candlestick",
                   x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                   increasing = list(fillcolor = "green", line = list(color = "green")),
                   decreasing = list(fillcolor = "red", line = list(color = "red")))
    fig_TCEHY <- fig_TCEHY %>% layout(title = "TCEHY Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
    
    # Display the chart
    fig_TCEHY

# RETURNS
  
  # CANDLESTICK CHART PATTERN
  
    # DAILY
    
    # Load libraries
    install.packages("plotly")
    library(plotly)
    library(quantmod)
    
    # Convert date to character for better labeling
    TCEHY$Date<- as.character(TCEHY$Date)
    
    # Create a candlestick chart
    fig_TCEHY_lr_d <- plot_ly(data = TCEHY, type = "candlestick",
                   x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                   increasing = list(fillcolor = "green", line = list(color = "green")),
                   decreasing = list(fillcolor = "red", line = list(color = "red")))
    fig_TCEHY_lr_d <- fig_TCEHY_lr_d %>% layout(title = "TCEHY Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
    
    # Display the chart
    fig_TCEHY_lr_d
    
    
  # WEEKLY
  
  # Load libraries
  library(plotly)
  library(quantmod)
  
  # Convert date to character for better labeling
  TCEHY$Date<- as.character(TCEHY$Date)
  
  # Create a candlestick chart
  fig_TCEHY_lr_w <- plot_ly(data = TCEHY, type = "candlestick",
                      x = ~Date, open = ~LogReturn_Open_w, high = ~LogReturn_High_w, low = ~LogReturn_Low_w, close = ~LogReturn_Last_w,
                      increasing = list(fillcolor = "green", line = list(color = "green")),
                      decreasing = list(fillcolor = "red", line = list(color = "red")))
  fig_TCEHY_lr_w <- fig_TCEHY_lr_w %>% layout(title = "TCEHY Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
  
  # Display the chart
  fig_TCEHY_lr_w

  # MONTHLY
  
  # Load libraries
  library(plotly)
  library(quantmod)
  
  # Convert date to character for better labeling
  monthly_returns_TCEHY$Month<- as.character(monthly_returns_TCEHY$Month)
  
  # Create a candlestick chart
  fig_TCEHY_lr_m <- plot_ly(data = monthly_returns_TCEHY, type = "candlestick",
                      x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                      increasing = list(fillcolor = "green", line = list(color = "green")),
                      decreasing = list(fillcolor = "red", line = list(color = "red")))
  fig_TCEHY_lr_m <- fig_TCEHY_lr_m %>% layout(title = "TCEHY Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
  
  # Display the chart
  fig_TCEHY_lr_m

  # YEARLY
  
  # Load libraries
  library(plotly)
  library(quantmod)
  
  # Convert date to character for better labeling
  yearly_returns_TCEHY$Year<- as.character(yearly_returns_TCEHY$Year)
  
  # Create a candlestick chart
  fig_TCEHY_lr_y <- plot_ly(data = yearly_returns_TCEHY, type = "candlestick",
                      x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                      increasing = list(fillcolor = "green", line = list(color = "green")),
                      decreasing = list(fillcolor = "red", line = list(color = "red")))
  fig_TCEHY_lr_y <- fig_TCEHY_lr_y %>% layout(title = "TCEHY Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
  
  # Display the chart
  fig_TCEHY_lr_y
  

#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

  library(zoo)
  TCEHY$MA5 <- rollmean(TCEHY$Last, k = 5, fill = NA)
  TCEHY$MA21 <- rollmean(TCEHY$Last, k = 21, fill = NA)
  TCEHY$MA63 <- rollmean(TCEHY$Last, k = 63, fill = NA)
  
  ggplot(TCEHY, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")
  
  ggplot(TCEHY, aes(x = Date, y = Last,group = 1)) +
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  