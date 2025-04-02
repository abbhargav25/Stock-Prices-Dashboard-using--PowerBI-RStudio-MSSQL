use Stocks

select * from Stock_Prices

SELECT TOP 1 [Date], [Close_Price] 
FROM dbo.Stock_Prices
ORDER BY [Date] DESC;

select count([Date]) from Stock_Prices;