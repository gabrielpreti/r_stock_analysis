TradeSystem$methods(
#   entryStrategy = function(stockTrade, date) {
#     longMmSize=20
#     shortMmSize=9
#     
#     stockData = stockTrade$stock$getHistoryBeforeDate(date)
#     dataSize = nrow(stockData)
#     if(dataSize<=longMmSize){
#       return(FALSE);
#     }
#     
#     longMm = SMA(cbind(stockData[, "high"],stockData[, "low"], stockData[, "close"]), n=longMmSize)
#     shortMm = SMA(cbind(stockData[, "high"],stockData[, "low"], stockData[, "close"]), n=shortMmSize)
#     
#     return(shortMm[dataSize-1]<=longMm[dataSize-1] && shortMm[dataSize]>=longMm[dataSize]);
#   },
  
  entryStrategy = function(stockTrade, date) {
    parameters = .self$parameters;
    stockCode = stockTrade$stock$code
    entrySize = parameters[parameters$code==stockCode, "entryDonchianSize"]
    
    stockData = stockTrade$stock$getHistoryBeforeDate(date)
    dataSize = nrow(stockData)
    
    if(dataSize <= entrySize){
      return(FALSE);
    }

    if(is.null(systemMemory[[stockCode]])){
      systemMemory[[stockCode]] <<- list();
    }
    if(is.null(systemMemory[[stockCode]][["entryDonchianChannel"]])){
      systemMemory[[stockCode]][["entryDonchianChannel"]] <<- DonchianChannel(cbind(stockTrade$stock$history[, "high"], stockTrade$stock$history[, "low"]), n=entrySize)
    }
    donchianChannel =  systemMemory[[stockCode]][["entryDonchianChannel"]];
    
    return(stockData[dataSize, "close"] > donchianChannel[dataSize-1, "high"]);
  },
  
  riskStrategy = function(stockTrade, date) {
    stockValue = stockTrade$stock$getCloseValueAtDate(date);
    
    
    posSize = floor((0.2*.self$accountBalance)/stockValue) #Cada posicao equivale a 10% do saldo
    stopPosValue = stockValue - ((0.01*.self$accountBalance)/posSize) #Risco de 2%
    
    return(list(size=posSize, stopPos=stopPosValue))
  },
  
#   exitStrategy = function(stockTrade, date) {
#     longMmSize=15
#     shortMmSize=6
#     
#     stockData = stockTrade$stock$getHistoryBeforeDate(date)
#     dataSize = nrow(stockData);
#     
#     longMm = SMA(cbind(stockData[, "high"],stockData[, "low"], stockData[, "close"]), n=longMmSize)
#     shortMm = SMA(cbind(stockData[, "high"],stockData[, "low"], stockData[, "close"]), n=shortMmSize)
#     
#     return(shortMm[dataSize-1]>longMm[dataSize-1] && shortMm[dataSize]<=longMm[dataSize]);
#   }

  exitStrategy = function(stockTrade, date) {
    parameters = .self$parameters
    stockCode = stockTrade$stock$code
    
    exitSize = parameters[parameters$code==stockCode, "exitDonchianSize"]
    
    stockData = stockTrade$stock$getHistoryBeforeDate(date)
    dataSize = nrow(stockData)
    if(dataSize <= exitSize){
      return(FALSE);
    }
    
    if(is.null(systemMemory[[stockCode]])){
      systemMemory[[stockCode]] <<- list();
    }
    if(is.null(systemMemory[[stockCode]][["exitDonchianChannel"]])){
      systemMemory[[stockCode]][["exitDonchianChannel"]] <<- DonchianChannel(cbind(stockTrade$stock$history[, "high"], stockTrade$stock$history[, "low"]), n=exitSize)
    }
    donchianChannel =  systemMemory[[stockCode]][["exitDonchianChannel"]];
    
    return(stockData[dataSize, "close"] <= donchianChannel[dataSize-1, "low"]);
  }
);