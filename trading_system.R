library(stringr)
library(TTR)
library("quantmod")
library(lubridate)

source("trade_system_model.R")
source("trade_system_strategy.R")


library(doMC)
library(foreach)
CORES =  detectCores();
registerDoMC(CORES);
#####################################################################
#Função para geração de relatórios
######################################################################
stockReport <- function(fileContent, stockTrade, initialDate){
  pchValues = c(8, 20)
  
  data = fileContent[fileContent$Code==stockTrade$stock$code & fileContent$Date>=initialDate, ]
  a = chartSeries(zoo(data[,c(5, 6, 7, 8, 9)], data[, 1]), name=stockTrade$stock$code)
  
  i=0;
  for(trade in stockTrade$trades){
    i = i + 1;    
    buyXIndex = trade$stock$getDateIndex(trade$buyDate)
    buyYIndex = trade$stock$getCloseValueAtDate(trade$buyDate)
    sellXIndex = trade$stock$getDateIndex(trade$sellDate)
    sellYIndex = trade$stock$getCloseValueAtDate(trade$sellDate)
    
    pchValue = pchValues[(i%%2)+1]
    if(buyYIndex<sellYIndex){
      color = "blue"  
    }else{
      color = "red"
    }
    
    print(addPoints(x=buyXIndex, y=buyYIndex, col=color, pch=pchValue))
    print(addPoints(x=sellXIndex, y=sellYIndex, col=color, pch=pchValue))
  }
}

#####################################################################
#Função para gerar um array de códigos a partir de uma lista de Stocks
######################################################################
getStockCodes <- function(stocks) {
  result = c();
  for(s in stocks){
    result = append(result, s$code);
  }
  return(result);
}

#####################################################################
#Leitura dos dados
######################################################################
FILE_CONTENT = read.table(file="/tmp/cotacoes.txt", header=FALSE, quote="", fill = TRUE, sep=";");
FILE_CONTENT$Date = strptime(str_sub(FILE_CONTENT[, 1], start = 3, end = 10), format="%Y%m%d");
FILE_CONTENT$Code = str_trim(str_sub(FILE_CONTENT[, 1], start = 13, end = 24))
FILE_CONTENT$Type = str_trim(str_sub(FILE_CONTENT[, 1], start = 25, end = 27))
FILE_CONTENT$Name = str_trim(str_sub(FILE_CONTENT[, 1], start = 28, end = 39))
FILE_CONTENT$Open = as.numeric(str_sub(FILE_CONTENT[, 1], start = 57, end = 69))/100
FILE_CONTENT$High = as.numeric(str_sub(FILE_CONTENT[, 1], start = 70, end = 82))/100
FILE_CONTENT$Low = as.numeric(str_sub(FILE_CONTENT[, 1], start = 83, end = 95))/100
FILE_CONTENT$Close = as.numeric(str_sub(FILE_CONTENT[, 1], start = 109, end = 121))/100
FILE_CONTENT$Volume = as.numeric(str_sub(FILE_CONTENT[, 1], start = 171, end = 188))/100
FILE_CONTENT = FILE_CONTENT[, 2:ncol(FILE_CONTENT)]

#####################################################################
#Criação do modelo
######################################################################
# Todas ações preferenciais
STOCKS_TO_ANALYZE = c("ACES4", "BBDC4", "BDLL4", "BELG4", "BOBR4", "BRAP4", "BRIV4", "BRTO4", "BRTP4", "CAFE4", "CESP4", "CMET4", "CMIG4", "CNFB4", "CRIV4", "CSTB4", "CTNM4", "DURA4", "EBCO4",
                      "EBTP4", "ELPL4", "EMBR4", "ESTR4", "FFTL4", "FJTA4", "FRAS4", "GETI4", "GGBR4", "GOAU4", "GOLL4", "GUAR4", "IMBI4", "INEP4", "ITAU4", "ITSA4", "LAME4", "LEVE4", "LIXC4",
                      "MGEL4", "MNDL4", "MTSA4", "MWET4", "MYPK4", "PCAR4", "PETR4", "POMO4", "PRGA4", "PTIP4", "RAPT4", "RCSL4", "RIPI4", "RPSA4", "SAPR4", "SCLO4", "SDIA4", "SHUL4", "SLED4",
                      "STRP4", "TCOC4", "TCSL4", "TEKA4", "TLPP4", "TMCP4", "TNLP4", "TOYB4", "TRFO4", "TRPL4", "TSPP4", "UBBR4", "VCPA4", "WEGE4", "BGIP4");

STOCKS_TO_ANALYZE = c("ELPL4", "PETR4", "TOYB4", "TRPL4", "ITAU4", "ACES4", "BBDC4")

#small caps
# STOCKS_TO_ANALYZE = c("ABCB4", "ABRE3", "ALPA4", "ALSC3", "ALUP11", "AMAR3", "ANIM3", "ARTR3", "ARZZ3", "BBRK3", "BEEF3", "BPHA3", "BRAP4", "BRIN3", "BRPR3", "BRSR6", "BTOW3", "CSMG3", "CVCB3", "CYRE3", "DIRR3", "DTEX3", "ECOR3", "ELPL4", "ENBR3", "EQTL3", "ESTC3", "EVEN3", "EZTC3", "FLRY3", "GETI3", "GETI4", "GFSA3", "GOAU4", "GRND3", "HBOR3", "HGTX3", "HRTP3", "IGTA3", "LEVE3", "LIGT3", "LINX3", "MEAL3", "MGLU3", "MILS3", "MPLU3", "MRFG3", "MRVE3", "MYPK3", "ODPV3", "OIBR3", "OIBR4", "POMO4", "QGEP3", "QUAL3", "RAPT4", "RLOG3", "RSID3", "RUMO3", "SEER3", "SLCE3", "SMLE3", "SMTO3", "STBP11", "SULA11", "TCSA3", "TGMA3", "TOTS3", "TRPL4", "VLID3")

#ibovespa
# STOCKS_TO_ANALYZE = c("ABEV3", "BBAS3", "BBDC3", "BBDC4", "BBSE3", "BRFS3", "BRKM5", "BRML3", "BVMF3", "CCRO3", "CESP6", "CIEL3", "CMIG4", "CPFE3", "CPLE6", "CRUZ3", "CSAN3", "CSNA3", "CTIP3", "ELET3", "ELET6", "EMBR3", "FIBR3", "GGBR4", "GOLL4", "HYPE3", "ITSA4", "ITUB3", "ITUB4", "JBSS3", "KLBN11", "KROT3", "LAME3", "LAME4", "LREN3", "MDIA3", "MULT3", "NATU3", "PCAR4", "PETR3", "PETR4", "PSSA3", "RADL3", "RENT3", "SANB11", "SBSP3", "SUZB5", "TAEE11", "TBLE3", "TIMP3", "UGPA3", "USIM3", "USIM5", "VALE3", "VALE5", "VIVT4", "VVAR11", "WEGE3")

#Só Petrobrás
# STOCKS_TO_ANALYZE = c("PETR4")
# STOCKS_TO_ANALYZE = c("PETR4", "ABCB4")

INITIAL_DATE='2014-01-01'
INITIAL_POSITION=10000

STOCKS = c();
for(stockCode in STOCKS_TO_ANALYZE){
  stockData = FILE_CONTENT[FILE_CONTENT$Code==stockCode & FILE_CONTENT$Date>=INITIAL_DATE, ]  
  if(nrow(stockData)==0)
    next;
  
  stock = Stock$new(code=stockCode);
  for(i in 1:nrow(stockData)){
    stock$addHistory(as.Date(stockData[i, "Date"]), stockData[i, "High"], stockData[i, "Low"], stockData[i, "Close"], stockData[i, "Volume"])
  }
  
  STOCKS = append(STOCKS, stock);
}


#####################################################################
#Otimização dos parâmetros do modelo
######################################################################
optimizeParameters <- function(stock, initialPosition, initialDate=NULL, finalDate=NULL) {
  donchianSelectedEntry=1
  donchianSelectedExit=1
  selectedSmaLong=1
  selectedSmaShort=1
  gains = data.frame(entry=rep(NA, 0), exit=rep(NA, 0), gain=rep(NA, 0))
  bestGain = 0;
  for(entryDonchianSize in 10:20){
    #   for(entryDonchianSize in 10:10){
    for(exitDonchianSize in 2:10){
      #     for(exitDonchianSize in 2:2){
      print(paste("Parameters (entryDonchianSize, exitDonchianSize):", entryDonchianSize, exitDonchianSize))
      #       for(smaLongSize in 20:30){
      for(smaLongSize in 20:20){
        #         for(smaShortSize in 2:10){
        for(smaShortSize in 2:2){
          sys <- TradeSystem$new(stockVector=c(stock), accountInitialPosition=initialPosition);
          sys$setParameters(data.frame(code=stock$code, entryDonchianSize=entryDonchianSize, exitDonchianSize=exitDonchianSize, smaLongSize=smaLongSize, smaShortSize=smaShortSize));
          sys$analyzeStocks(initialDate, finalDate);
          sys$closeAllOpenTrades(finalDate);
          
          currentGain = sys$accountBalance - sys$accountInitialPosition;
          
          nrows = nrow(gains)+1
          gains[nrows, "entryDonchianSize"] = entryDonchianSize;
          gains[nrows, "exitDonchianSize"] = exitDonchianSize;
          gains[nrows, "smaLongSize"] = smaLongSize;
          gains[nrows, "smaShortSize"] = smaShortSize;
          gains[nrows, "gain"] = currentGain
          
          if( currentGain > bestGain ){
            donchianSelectedEntry = entryDonchianSize;
            donchianSelectedExit = exitDonchianSize;
            selectedSmaLong = smaLongSize;
            selectedSmaShort = smaShortSize;
            bestGain = currentGain;
          }
        }
      }
      
    }
  } 
  write.csv(gains, file=paste("/tmp/", stock$code, ".csv", sep="") )
  return(list(stockCode=stock$code, entryDonchianSize=donchianSelectedEntry, exitDonchianSize=donchianSelectedExit, smaLongSize=selectedSmaLong, smaShortSize=selectedSmaShort, gain=bestGain))
}

createOptimizedParametersFrame <- function(stockList, initialPos, initialDate=NULL, finalDate=NULL){
  parameters = mclapply(stockList, optimizeParameters, initialPosition=initialPos, initialDate=initialDate, finalDate=finalDate, mc.silent=FALSE, mc.cores = CORES)  
#        parameters = lapply(stockList, optimizeParameters, initialPosition=initialPos, initialDate=initialDate, finalDate=finalDate)  
  parametersFrame = data.frame(code=rep(NA, 0), entryDonchianSize=rep(NA, 0), exitDonchianSize=rep(NA, 0), smaLongSize=rep(NA, 0), smaShortSize=rep(NA, 0))
  for(p in parameters) {
    #################
    #TODO: testar remover essa condição!!!!!!!
    #################
    if(p$gain<=0){
      print(paste("Gain is negative:", p$gain))
      next;
    }
    
    parametersFrame[nrow(parametersFrame)+1, "code"] = p$stockCode;
    parametersFrame[nrow(parametersFrame), "entryDonchianSize"] = p$entryDonchianSize;
    parametersFrame[nrow(parametersFrame), "exitDonchianSize"] = p$exitDonchianSize;
    parametersFrame[nrow(parametersFrame), "smaLongSize"] = p$smaLongSize;
    parametersFrame[nrow(parametersFrame), "smaShortSize"] = p$smaShortSize;
  }
  return(parametersFrame);
}

mergeParameters = function(parameters1, parameters2){
  for(code in parameters2[, "code"]){
    if(code %in% parameters1[, "code"]){
      parameters1[parameters1$code==code, c("code", "entryDonchianSize", "exitDonchianSize", "smaLongSize", "smaShortSize")] = parameters2[parameters2$code==code, c("code", "entryDonchianSize", "exitDonchianSize", "smaLongSize", "smaShortSize")]
    }else{
      parameters1[nrow(parameters1)+1, c("code", "entryDonchianSize", "exitDonchianSize", "smaLongSize", "smaShortSize")] = parameters2[parameters2$code==code, c("code", "entryDonchianSize", "exitDonchianSize", "smaLongSize", "smaShortSize")]
    }
  }
  
  for(code in parameters1[, "code"]){
    if(!(code %in% parameters2[, "code"])){
      parameters1[parameters1$code==code, c("entryDonchianSize", "smaLongSize", "smaShortSize")] = c(NA, NA, NA);
    }
  }
  return(parameters1);
}

#####################################################################
#Execução do modelo
######################################################################
FINAL_DATE=as.Date('2015-01-01')

PERIODS_IN_MONTHS_TO_ANALYZE = c(1:1)
results = data.frame(training_months=rep(NA, 0), initial_balance=rep(NA, 0), final_balance=rep(NA, 0), time=rep(NA, 0))
for(TRAINING_PERIOD_IN_MONTHS in PERIODS_IN_MONTHS_TO_ANALYZE){
  #####################################################################
  #Análise
  ######################################################################
  print(paste("Analyzing for", TRAINING_PERIOD_IN_MONTHS, " months training ..."))
  start = Sys.time()
  
  currentInitialDate=as.Date(INITIAL_DATE);
  month(currentInitialDate) = month(currentInitialDate) + TRAINING_PERIOD_IN_MONTHS
  currentFinalDate = currentInitialDate;
  month(currentFinalDate) = month(currentFinalDate) + 1
  
  system <- TradeSystem$new(stockVector=STOCKS, accountInitialPosition=INITIAL_POSITION);
  optimizedParameters = data.frame(code=rep(NA, 0), entryDonchianSize=rep(NA, 0), exitDonchianSize=rep(NA, 0), smaLongSize=rep(NA, 0), smaShortSize=rep(NA, 0))
  
  while(currentFinalDate<=FINAL_DATE) {
    trainingPeriodBeginning = currentInitialDate
    month(trainingPeriodBeginning) = month(trainingPeriodBeginning) - TRAINING_PERIOD_IN_MONTHS;
    trainingPeriodEnd = currentInitialDate
    day(trainingPeriodEnd) = day(trainingPeriodEnd) - 1;
    
    print(paste("Analysing from", currentInitialDate, "to", currentFinalDate, "with training period from ", trainingPeriodBeginning, "to", trainingPeriodEnd))
    newParameters = createOptimizedParametersFrame(STOCKS, system$accountInitialPosition, trainingPeriodBeginning, trainingPeriodEnd);
    optimizedParameters = mergeParameters(optimizedParameters, newParameters);
    print("Optimized parameters:")
    print(optimizedParameters);
    
    if(nrow(optimizedParameters)>0) {
      system$flushMemory();
      system$setParameters(optimizedParameters);
      system$analyzeStocks(initialDate=currentInitialDate, finalDate=currentFinalDate, stockCodes=newParameters$code); 
    }
    
    
    month(currentInitialDate) = month(currentInitialDate) +1;
    month(currentFinalDate) = month(currentFinalDate) + 1;
  }
  
  print(optimizedParameters)
  system$closeAllOpenTrades(FINAL_DATE)
  print(paste("Final balance:", system$accountBalance, " % gain:", 100*((system$accountBalance-system$accountInitialPosition)/system$accountInitialPosition)));
  
  results[nrow(results)+1, c("training_months", "initial_balance", "final_balance")] = c(TRAINING_PERIOD_IN_MONTHS, system$accountInitialPosition, system$accountBalance)
  results[nrow(results), "time"] = Sys.time()-start;
  
  #####################################################################
  #Geração de relatórios
  ######################################################################
  
  plot(
    rep(1, nrow(system$balanceHistory)), 
    type="n", 
    xlim=c(0, nrow(system$balanceHistory)), 
    ylim=c(0, max(system$balanceHistory[, "balance"])), 
    main="Saldo",  xaxt="n", xlab="", ylab="");
  labels=seq(1, nrow(system$balanceHistory), by=nrow(system$balanceHistory)/30);
  axis(1, at=labels, lab=strftime(system$balanceHistory[labels, "date"], format="%d/%m/%y"), las=2);
  lines(system$balanceHistory[, "balance"], type="o", cex=0.8)
  
  X11()
  tmp_dev=dev.cur();
  pdf(file=paste("stock_report_", TRAINING_PERIOD_IN_MONTHS, ".pdf", sep=""));
  pdf_dev=dev.cur();
  
  
  i=0;
  for(stockTrade in system$stocks){
    dev.set(tmp_dev) 
    
    error=FALSE;
    tryCatch(
      stockReport(FILE_CONTENT, stockTrade, INITIAL_DATE),
      error=function(e){print(paste("Error in", stockTrade$stock$code)); error=TRUE}
    );
    
    if(!error){
      dev.copy(which=pdf_dev);
    }
    i=i+1
  }
  dev.off(pdf_dev)
  dev.off(tmp_dev)
}





###############################
###############################
STOCKS_TO_ANALYZE=c("ELPL4")
currentInitialDate=as.Date('2014-01-01')
currentFinalDate=as.Date('2015-01-01')
system <- TradeSystem$new(stockVector=STOCKS, accountInitialPosition=INITIAL_POSITION);
optimizedParameters = data.frame(code=STOCKS_TO_ANALYZE, entryDonchianSize=rep(10, 1), exitDonchianSize=rep(2, 1), smaLongSize=rep(10, 1), smaShortSize=rep(2, 1))
system$setParameters(optimizedParameters);
system$analyzeStocks(initialDate=currentInitialDate, finalDate=currentFinalDate, stockCodes=STOCKS_TO_ANALYZE);
system$closeAllOpenTrades(currentFinalDate)
system$accountBalance
