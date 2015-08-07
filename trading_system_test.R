require(RUnit);

#-----------------------------------------------------------------------------------
#Stock class tests
#-----------------------------------------------------------------------------------
stock = Stock$new(code="ABC")
dates = as.Date(c('2015-07-01', '2015-07-02', '2015-07-03', '2015-07-04'))
highs = c(10, 11, 13, 7);
lows = c(9, 10, 11, 7);
closes = c(9.5, 10.5, 11.5, 7)

test.stock <- function(){ 
  checkEquals(stock$code, "ABC")
  checkEqualsNumeric(nrow(stock$history), 0)
}

test.stock.addHistory <- function(){  
  stock$addHistory(dates[1], highs[1], lows[1], closes[1]);
  checkEqualsNumeric(nrow(stock$history), 1)
  
  stock$addHistory(dates[2], highs[2], lows[2], closes[2]);
  checkEqualsNumeric(nrow(stock$history), 2)
  
  stock$addHistory(dates[3], highs[3], lows[3], closes[3]);
  checkEqualsNumeric(nrow(stock$history), 3)
}

test.stock.getHistoryBeforeDate <- function(){  
  for(i in 1:3){
    h = stock$getHistoryBeforeDate(dates[i]);
    checkEqualsNumeric(nrow(h), i);
    for(j in 1:i){
      checkEquals(h[j, "date"], dates[j]);
      checkEqualsNumeric(h[j, "high"], highs[j]);
      checkEqualsNumeric(h[j, "low"], lows[j]);
      checkEqualsNumeric(h[j, "close"], closes[j]);  
    }
  }
}

test.stock.getCloseValueAtDate <- function(){
  for(i in 1:3){
    checkEqualsNumeric(stock$getCloseValueAtDate(dates[i]), closes[i]);
  }
}

test.stock.hasHistoryAtDate <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:3){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);  
  }
  
  checkTrue(stock$hasHistoryAtDate(dates[1]))
  checkTrue(stock$hasHistoryAtDate(dates[2]))
  checkTrue(stock$hasHistoryAtDate(dates[3]))
  checkTrue(!stock$hasHistoryAtDate(dates[4]))
}

test.stock.getDateIndex <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:3){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);  
  }
  
  checkEqualsNumeric(1, stock$getDateIndex(dates[1]));
  checkEqualsNumeric(2, stock$getDateIndex(dates[2]));
  checkEqualsNumeric(3, stock$getDateIndex(dates[3]));
  checkEqualsNumeric(0, stock$getDateIndex(dates[4]));
}

#-----------------------------------------------------------------------------------
#Trade class tests
#-----------------------------------------------------------------------------------
test.trade.initialize <- function(){
  checkException(Trade$new(stock=stock))
  checkException(Trade$new(stock=stock, size=10))  
  checkException(Trade$new(stock=stock, size=10, buyDate=dates[1])$sellDate, NULL)
  
  checkEquals(Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=1)$sellDate, NULL)
  checkEqualsNumeric(1, Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=1)$stopPos)
}

trade = Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=8);

test.trade.isOpen <- function() {
  trade = Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=8);
  checkEquals(trade$isOpen(), TRUE)
  trade$sellDate=dates[1]
  checkEquals(trade$isOpen(), FALSE);
}

test.trade.close <- function() {
  trade$close(dates[2]);
  checkEquals(trade$sellDate, dates[2])
  checkEquals(trade$isOpen(), FALSE);
}

test.trade.getProfit <- function() {
  trade = Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=8);
  checkException(trade$getProfit());
  checkEqualsNumeric(trade$getProfit(date=dates[3]), closes[3]-closes[1])
  trade$close(dates[3]);
  checkEqualsNumeric(trade$getProfit(), closes[3]-closes[1])
}

test.trade.isProfittable <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  
  trade = Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=8);
  trade$close(dates[3]);
  checkEquals(trade$isProfittable(), TRUE)
  trade$close(dates[4]);
  checkEquals(trade$isProfittable(), FALSE)
}

test.trade.getBuyValue <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  
  for(i in 1:3){
    trade = Trade$new(stock=stock, size=10, buyDate=dates[i], stopPos=8);
    checkEqualsNumeric(trade$getBuyValue(), closes[i])  
  }
  
}

test.trade.getSellValue <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  
  trade = Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=8);
  checkException(trade$getSellValue());
  
  for(i in 1:2){
    trade = Trade$new(stock=stock, size=10, buyDate=dates[i], stopPos=8);
    trade$close(dates[i+1])
    checkEqualsNumeric(trade$getSellValue(), closes[i+1])  
  }  
}

test.trade.hasReachedStopPosition <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  
  trade = Trade$new(stock=stock, size=10, buyDate=dates[1], stopPos=8);
  checkTrue(!trade$hasReachedStopPosition(dates[2]))
  checkTrue(trade$hasReachedStopPosition(dates[4]))
}

#-----------------------------------------------------------------------------------
#StockTrades class tests
#-----------------------------------------------------------------------------------
test.StockTrades.initialize <- function(){
  stock = Stock$new(code="ABC")
  checkEqualsNumeric(length(StockTrades$new(stock=stock)$trades), 0)
}

test.StockTrades.getLastTrade <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  
  stockTrades = StockTrades$new(stock=stock);
  
  stockTrades$openNewTrade(10, dates[1], stopPos=8);
  t = stockTrades$getLastTrade();
  checkEqualsNumeric(10, t$size);
  checkTrue(dates[1] == t$buyDate);
  checkTrue(is.null(t$sellDate));
  
  t = stockTrades$closeLastTrade(dates[2]);
  t = stockTrades$getLastTrade();
  checkEqualsNumeric(10, t$size);
  checkTrue(dates[1] == t$buyDate);
  checkTrue(dates[2] == t$sellDate);
  
  stockTrades$openNewTrade(10, dates[3], stopPos=8);
  t = stockTrades$getLastTrade();
  checkEqualsNumeric(10, t$size);
  checkTrue(dates[3] == t$buyDate);
  checkTrue(is.null(t$sellDate));
  
  t = stockTrades$closeLastTrade(dates[4]);
  t = stockTrades$getLastTrade();
  checkEqualsNumeric(10, t$size);
  checkTrue(dates[3] == t$buyDate);
  checkTrue(dates[4] == t$sellDate);
}

test.StockTrades.openNewTrade <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  stockTrades = StockTrades$new(stock=stock);
  
  t = stockTrades$openNewTrade(10, dates[1], stopPos=8);
  checkEqualsNumeric(10, t$size);
  checkEqualsNumeric(dates[1], t$buyDate);
  checkEquals(NULL, t$sellDate);
  checkEqualsNumeric(8, t$stopPos)
  
  checkException(stockTrades$openNewTrade(10, dates[2]))
}

test.StockTrades.closeLastTrade <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  stockTrades = StockTrades$new(stock=stock);
  
  checkException(stockTrades$closeLastTrade());
  
  stockTrades$openNewTrade(10, dates[1], stopPos=8);
  t = stockTrades$closeLastTrade(dates[3]);
  checkEqualsNumeric(10, t$size);
  checkEquals(dates[1], t$buyDate);
  checkEquals(dates[3], t$sellDate);
  
  checkException(stockTrades$closeLastTrade(dates[4]));
}

test.StockTrades.isInOpenPosition <- function() {
  stock = Stock$new(code="ABC")
  for(i in 1:4){
    stock$addHistory(dates[i], highs[i], lows[i], closes[i]);
  }
  stockTrades = StockTrades$new(stock=stock);
  
  checkEquals(FALSE, stockTrades$isInOpenPosition());
  
  stockTrades$openNewTrade(10, dates[1], stopPos=8);
  checkEquals(TRUE, stockTrades$isInOpenPosition());
  
  stockTrades$closeLastTrade(dates[2]);
  checkEquals(FALSE, stockTrades$isInOpenPosition());
  
  stockTrades$openNewTrade(10, dates[3], stopPos=8);
  checkEquals(TRUE, stockTrades$isInOpenPosition());
}

# testSuite.trading <- defineTestSuite("trading", dirs=getwd(), testFileRegexp = ".+test\\.R", testFuncRegexp = "^test.+");
# runTestSuite(testSuite.trading)
# printTextProtocol(runTestSuite(testSuite.trading))