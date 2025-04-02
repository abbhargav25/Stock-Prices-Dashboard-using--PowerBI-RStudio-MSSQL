
install.packages("stringi")

library(tidyquant) 
library(DBI)
library(odbc)
library(RMySQL)
library(stringi)

nifty50_stocks <- c("TCS.NS", "INFY.NS", "RELIANCE.NS", "HDFCBANK.NS", "ICICIBANK.NS")

stock_data <- tq_get(nifty50_stocks, from = Sys.Date() - (15 * 365), to = Sys.Date())


stock_data

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "BHARGAV-IIMBA\\SQLEXPRESS",
                 Database = "Stocks",
                 Trusted_Connection = "Yes")


query <- "
CREATE TABLE Stock_Prices (
    Stock_Symbol VARCHAR(20),
    Date DATE,
    Open_Price FLOAT,
    High_Price FLOAT,
    Low_Price FLOAT,
    Close_Price FLOAT,
    Volume BIGINT,
    Adjusted_Close FLOAT,
    PRIMARY KEY (Stock_Symbol, Date)
);"
dbExecute(con, query)


dbWriteTable(con, "Stock_Prices", stock_data, append = TRUE, row.names = FALSE)
str(stock_data)

colnames(stock_data)
colnames(stock_data) <- c("Stock_Symbol", "Date", "Open_Price", "High_Price",
                          "Low_Price", "Close_Price", "Volume", "Adjusted_Close")
dbWriteTable(con, "Stock_Prices", stock_data, append = TRUE, row.names = FALSE)






library(TTR)
library(dplyr)
library(ggplot2)
library(DBI)
library(odbc)

stock_data <- stock_data %>% arrange(Date)

stock_data <- stock_data %>%
  filter(!is.na(Close_Price)) %>%  
  mutate(
    SMA_50 = SMA(Close_Price, n = 50),
    SMA_100 = SMA(Close_Price, n = 100)
  )


stock_data <- stock_data %>%
  mutate(
    Signal = ifelse(SMA_50 > SMA_100, 1, 0),
    Position = Signal - lag(Signal, default = 0)  
  )



stock_data <- stock_data %>%
  mutate(Position = as.numeric(Position))


df_pos <- stock_data %>%
  filter(Position == 1 | Position == -1) %>%
  mutate(Position = ifelse(Position == 1, 'Buy', 'Sell'))

str(df_pos)


backtest_results <- data.frame(
  Equity_Name = character(),
  Trade = character(),
  Entry_Time = as.Date(character()),
  Entry_Price = numeric(),
  Quantity = integer(),
  Position_Size = numeric(),
  Exit_Time = as.Date(character()),
  Exit_Price = numeric(),
  PNL = numeric(),
  Percent_PNL = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(df_pos)) {
  position <- df_pos$Position[i]
  
  if (position == 'Buy') {
    qty <- floor(capital / df_pos$Close_Price[i])
    trade_log <- data.frame(
      Equity_Name = df_pos$Stock_Symbol[i],  
      Trade = 'Long Open',
      Entry_Time = df_pos$Date[i],
      Entry_Price = df_pos$Close_Price[i],
      Quantity = qty,
      Position_Size = qty * df_pos$Close_Price[i],
      Exit_Time = as.Date(NA),  
      Exit_Price = NA,
      PNL = NA,
      Percent_PNL = NA,
      stringsAsFactors = FALSE
    )
  } else if (exists("trade_log") && !is.null(trade_log)) {
    trade_log$Trade <- 'Long Closed'
    trade_log$Exit_Time <- df_pos$Date[i]
    trade_log$Exit_Price <- df_pos$Close_Price[i]
    trade_log$PNL <- (trade_log$Exit_Price - trade_log$Entry_Price) * trade_log$Quantity
    trade_log$Percent_PNL <- (trade_log$PNL / trade_log$Position_Size) * 100
    
    if (nrow(trade_log) > 0) {
      backtest_results <- rbind(backtest_results, trade_log)
    }
    trade_log <- NULL  
  }
}

# Check final results
print(backtest_results)


# Connect to SQL Server
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "BHARGAV-IIMBA\\SQLEXPRESS",
                 Database = "Stocks",
                 Trusted_Connection = "Yes")

query <- "
CREATE TABLE Backtest_Results (
    Equity_Name VARCHAR(20),
    Trade VARCHAR(20),
    Entry_Time DATE,
    Entry_Price FLOAT,
    Exit_Time DATE,
    Exit_Price FLOAT,
    Quantity INT,
    Position_Size FLOAT,
    PNL FLOAT,
    Percent_PNL FLOAT
);
"
dbExecute(con, query)

if(nrow(backtest_results) > 0) {
  dbWriteTable(con, "Backtest_Results", backtest_results, append = TRUE, row.names = FALSE)
}
