TradeSystem$methods(
  
  entryStrategy = function(stockTrade, date) {
    parameters = .self$parameters;
    stockCode = stockTrade$stock$code
    entrySize = parameters[parameters$code==stockCode, "entryDonchianSize"]
    smaLongSize = parameters[parameters$code==stockCode, "smaLongSize"]
    smaShortSize = parameters[parameters$code==stockCode, "smaShortSize"]
    
    if(is.na(entrySize) || is.null(entrySize)){
      return(FALSE)
    }
    
    dataSize = stockTrade$stock$getHistorySizeBeforeDate(date)
    
#     if(dataSize <= max(entrySize, smaLongSize)){
    if(dataSize <= entrySize){
      return(FALSE);
    }
    
    if(is.null(systemMemory[[stockCode]])){
      systemMemory[[stockCode]] <<- list();
    }
    if(is.null(systemMemory[[stockCode]][["entryDonchianChannel"]])){
      systemMemory[[stockCode]][["entryDonchianChannel"]] <<- DonchianChannel(cbind(stockTrade$stock$history[, "high"], stockTrade$stock$history[, "low"]), n=entrySize)
    }
    if(is.null(systemMemory[[stockCode]][["smaLong"]])){
      systemMemory[[stockCode]][["smaLong"]] <<- SMA(cbind(stockTrade$stock$history[, "high"],stockTrade$stock$history[, "low"], stockTrade$stock$history[, "close"]), n=smaLongSize)
    }
    if(is.null(systemMemory[[stockCode]][["smaShort"]])){
      systemMemory[[stockCode]][["smaShort"]] <<- SMA(cbind(stockTrade$stock$history[, "high"],stockTrade$stock$history[, "low"], stockTrade$stock$history[, "close"]), n=smaShortSize)
    }
    donchianChannel =  systemMemory[[stockCode]][["entryDonchianChannel"]];
    smaLong =  systemMemory[[stockCode]][["smaLong"]];
    smaShort =  systemMemory[[stockCode]][["smaShort"]];
    
    closeValue = stockTrade$stock$getCloseValueAtDate(date)
    volume = stockTrade$stock$getVolumeAtDate(date)
#     return(volume>=10^7 && closeValue > donchianChannel[dataSize-1, "high"] && smaShort[dataSize-1]>=smaLong[dataSize-1]);
    return(volume>=10^6 && closeValue > donchianChannel[dataSize-1, "high"]);
  },
  
  riskStrategy = function(stockTrade, date) {
    stockValue = stockTrade$stock$getCloseValueAtDate(date);
    
    
    posSize = floor((0.3*.self$accountBalance)/stockValue) #Cada posicao equivale a 30% do saldo
    stopPosValue = stockValue - ((0.01*.self$accountBalance)/posSize) #Risco de 1%
    
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