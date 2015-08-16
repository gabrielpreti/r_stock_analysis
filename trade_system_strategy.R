TradeSystem$methods(
  
  entryStrategy = function(stockTrade, date) {
    parameters = .self$parameters;
    stockCode = stockTrade$stock$code
    entrySize = parameters[parameters$code==stockCode, "entryDonchianSize"]
    
    dataSize = stockTrade$stock$getHistorySizeBeforeDate(date)
    
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
    
    closeValue = stockTrade$stock$getCloseValueAtDate(date)
    return(closeValue > donchianChannel[dataSize-1, "high"]);
  },
  
  riskStrategy = function(stockTrade, date) {
    stockValue = stockTrade$stock$getCloseValueAtDate(date);
    
    
    posSize = floor((0.2*.self$accountBalance)/stockValue) #Cada posicao equivale a 10% do saldo
    stopPosValue = stockValue - ((0.01*.self$accountBalance)/posSize) #Risco de 2%
    
    return(list(size=posSize, stopPos=stopPosValue))
  },

  exitStrategy = function(stockTrade, date) {
    parameters = .self$parameters
    stockCode = stockTrade$stock$code
    
    exitSize = parameters[parameters$code==stockCode, "exitDonchianSize"]
    
    dataSize = stockTrade$stock$getHistorySizeBeforeDate(date)
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
    
    closeValue = stockTrade$stock$getCloseValueAtDate(date)
    return(closeValue <= donchianChannel[dataSize-1, "low"]);
  }
);