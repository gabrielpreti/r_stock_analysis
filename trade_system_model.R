require(hash)

###########################################################################
###########################################################################
#DEFINING THE MODEL
###########################################################################
###########################################################################



#-----------------------------------------------------------------------------------
#Stock class

# Stock
#   - code
#   - history
#     - date
#     - high
#     - low
#     - close
#-----------------------------------------------------------------------------------
Stock <- setRefClass("Stock", 
                     fields=list(code="character", history="data.frame", historyIndex="hash"), 
                     methods=list(
                       initialize = function(...){
                         callSuper(...);
                         history <<- data.frame(date=rep(as.Date(NA), 0), high=numeric(0), low=numeric(0), close=numeric(0)); 
                       },
                       
                       getHistoryBeforeDate = function(date){
                         dateIndex = historyIndex[[as.character(date)]];
                         if(is.null(dateIndex)){
                            return(history[which(history$date<=date), ]);
                         }else{
                           return(history[1:dateIndex, ])
                         }
                       },
                       
                       getCloseValueAtDate = function(date){
                         dateIndex = historyIndex[[as.character(date)]];
                         if(!is.null(dateIndex)){
                           return(history[dateIndex, "close"])
                         }
                         
                          temp = history[which(history$date<=date), "close"]
                          return(temp[length(temp)][1]);  
                       },
                       
                       addHistory = function(d, h, l, c){
                         nextElementIndex = nrow(history)+1;
                         
                         history[nextElementIndex, "date"] <<- d ;          
                         history[nextElementIndex, c("high", "low", "close")] <<- c(h, l, c);
                         historyIndex <<- hash(keys=as.character(history$date), values=c(1:nrow(history)))
                       },
                       
                       hasHistoryAtDate = function(date){
                         return(!is.null(historyIndex[[as.character(date)]]))
                       },
                       
                       getDateIndex = function(date) {
                         dateIndex = historyIndex[[as.character(date)]];
                         if(is.null(dateIndex)){
                           return(0);
                         }else{
                           return(dateIndex);
                         }
                       }
                       
                     )
)


#-----------------------------------------------------------------------------------
#Trade class

# Trade
#   - stock
#   - size
#   - buyIndex
#   - sellIndex
#-----------------------------------------------------------------------------------
Trade <- setRefClass("Trade", 
                     fields=list(stock="Stock", size="numeric", stopPos="numeric", buyDate="Date", sellDate="Date"),
                     methods=list(
                       initialize = function(...){
                         size <<- 0;
                         callSuper(...);
                         
                         if(size==0){
                           stop("size can not be empty");
                         }
                         
                         if(is.null(buyDate)){
                           stop("buyDate can not be empty");
                         }
                         
                         if(stopPos==0){
                           stop("Stop postion can not be empty");
                         }
                       },
                       
                       isOpen = function() {
                         return(is.null(sellDate) || is.na(sellDate));
                       },
                       
                       close = function(date) {
                         sellDate <<- date;
                       },
                       
                       getProfit = function(date=NA){
                         if(is.null(buyDate)){
                           stop("Trade invalid: buyDate is empty.");
                         }
                         if(is.na(date) && isOpen()){
                           stop("Trade not closed and no date informed to calculate profits.")
                         }
                         
                         if(!is.na(date)){
                           return(stock$getCloseValueAtDate(date) - stock$getCloseValueAtDate(buyDate));
                         }else{
                           return(stock$getCloseValueAtDate(sellDate) - stock$getCloseValueAtDate(buyDate));
                         }
                       },
                       
                       isProfittable = function(date=NA){
                         return(getProfit(date)>0);
                       },
                       
                       getBuyValue = function(){
                         return(stock$getCloseValueAtDate(buyDate));
                       },
                       
                       getSellValue = function(){
                         if(.self$sellDate==0){
                           stop("No sell value: trade not closed")
                         }
                         return(stock$getCloseValueAtDate(.self$sellDate));
                       },
                       
                       hasReachedStopPosition = function(date){
                         if(!isOpen()){
                           stop("Trade is not opened.");
                         }
                         
                         return(stock$getCloseValueAtDate(date)<=stopPos)
                       }
                     )
)

#-----------------------------------------------------------------------------------
#StockTrades class
StockTrades <- setRefClass("StockTrades",
                           fields=list(stock="Stock", trades="vector"),
                           methods= list(
                             
                             getLastTrade = function() {
                               return(trades[length(trades)][[1]])
                             },
                             
                             openNewTrade = function(s, date, stopPos) {
                               if(isInOpenPosition()){
                                 stop("Can't open a new trade with one already opened.");
                               }
                               
                               t = Trade$new(stock=.self$stock, size=s, buyDate=date, stopPos=stopPos)
                               trades <<-append(trades, t);
                               
                               return(t);
                             },
                             
                             closeLastTrade = function(date){
                               if(!isInOpenPosition()){
                                 stop("No open trade to close.");
                               }
                               
                               t = getLastTrade();
                               t$close(date);
                               return(t);
                             },
                             
                             isInOpenPosition = function(){
                               t = getLastTrade();
                               
                               return(!is.null(t) && t$isOpen())
                             },
                             
                             hasReachedStopPosition = function(date){
                               t = getLastTrade();
                               return(!is.null(t) && t$isOpen() && t$hasReachedStopPosition(date))
                             }
                           )
                           
)



#-----------------------------------------------------------------------------------
# TradeSystem
#   - stock
#   - accountBalance
#   - trades
#   - currentIndex
#   + entryStrategy
#   + exitStrategy
#   + stopStrategy
#   + sizingStrategy

#-----------------------------------------------------------------------------------
TradeSystem <- setRefClass("TradeSystem", 
                           
                           fields=list(stocks="vector", accountInitialPosition="numeric", accountBalance = "numeric", balanceHistory="data.frame", parameters="ANY", systemMemory="list"), 
                           
                           methods = list(
                             
                             initialize = function(stockVector, ...) {
                               callSuper(...);
                               accountBalance <<- accountInitialPosition;
                               systemMemory <<- list();
                               
                               if(is.na(stockVector) || is.null(stockVector) || length(stockVector)==0){
                                 stop("No stocks to analyze.");
                               }
                               
                               for(stock in stockVector) {
                                 stocks <<- append(stocks, StockTrades$new(stock=stock));
                               }
                               
                               balanceHistory <<- data.frame(date=rep(as.Date(NA), 0), balance=numeric(0)); 
                             },
                             
                             setParameters = function(par){
                               parameters <<- par
                             },
                             
                             entryStrategy = function(stockTrade, date) {
                               stop("not implemented")
                             }, 
                             
                             exitStrategy = function(stockTrade, date) {
                               stop("not implemented")
                             }, 
                             
                             riskStrategy = function(stockTrade, date) {
                               stop("not implemented")
                             }, 
                             
                             openNewTrade = function(stockTrade, date) {
                               risk = .self$riskStrategy(stockTrade, date)

                               stockValue = stockTrade$stock$getCloseValueAtDate(date);                               
                               if(risk$size<1 || ((risk$size*stockValue) > .self$accountBalance)){
                                 print("Not enough balance to enter position");
                                 return(FALSE);
                               }
                               
                               trade = stockTrade$openNewTrade(risk$size, as.Date(date), risk$stopPos);
                               accountBalance <<- .self$accountBalance - (trade$size * trade$getBuyValue());
                               
#                                print(paste("Buying", trade$size, "positions of", trade$stock$code, "at", trade$getBuyValue()));
                             },
                             
                             closeLastTrade = function(stockTrade, date){
                               trade = stockTrade$closeLastTrade(as.Date(date));
                               accountBalance <<- .self$accountBalance + (trade$size * trade$getSellValue());
                               
#                                print(paste("selling", trade$size, "positions of", trade$stock$code, "at", trade$getSellValue()));
                             },
                             
                             calculateTotalOpenPositions = function(date) {
                               total = 0;
                               for(stockTrade in stocks){
                                 if(stockTrade$isInOpenPosition()){
                                   openTrade = stockTrade$getLastTrade();
                                   total = total + openTrade$size*openTrade$stock$getCloseValueAtDate(date);
                                 }
                               }
                               return(total);
                             },

                             flushMemory = function() {
                               systemMemory <<- list();
                             },
                             
                             analyzeStocks = function(initialDate=NULL, finalDate=NULL, stockCodes=NULL){
                               #Identifica todas as datas, de forma unica e ordenada
                               allDates = c();
                               for(stockTrade in .self$stocks){
                                 allDates = unique(append(allDates, stockTrade$stock$history$date))
                               }
                               allDates= allDates[order(allDates)]
                               
                               #Verifica se foi especificado uma data inicial
                               if(!is.null(initialDate)){
                                 allDates = allDates[which(allDates>=initialDate)]
                               }
                               
                               #Verifica se foi especificado uma data final
                               if(!is.null(finalDate)){
                                 allDates = allDates[which(allDates<=finalDate)]
                               }
                               
#                                print(paste("Analyzing stocks from", head(allDates, 1), "to", tail(allDates, 1)))
                               
                               nrows = nrow(.self$balanceHistory);
                               for(date in allDates){
                                 for(stockTrade in stocks){ 
                                   if(!stockTrade$stock$hasHistoryAtDate(as.Date(date))){
                                     next;
                                   }
                                   
                                   if(stockTrade$isInOpenPosition()){
                                     if(.self$exitStrategy(stockTrade, date) || stockTrade$hasReachedStopPosition(date)){
                                       .self$closeLastTrade(stockTrade, date);
                                     }
                                   }else{
                                     #Se foi especificado um subconjunto de ações, e a ação corrente não está nesse grupo, então não abre trade pra ação.
                                     if(!is.null(stockCodes) && !(stockTrade$stock$code %in% stockCodes)){
                                       next;
                                     }
                                     
                                     if(.self$entryStrategy(stockTrade, date)){
                                       openNewTrade(stockTrade, date);
                                     }
                                   }
                                 }
                                 
                                 nrows = nrows + 1.
                                 .self$balanceHistory[nrows, "date"] = as.Date(date);
                                 .self$balanceHistory[nrows, "balance"] = .self$accountBalance + .self$calculateTotalOpenPositions(date);
                               }
                               
                               #print("Done.");
                             },

                              closeAllOpenTrades = function(){
                                #print("Closing all open trades in the system ...");
                                for(stockTrade in stocks){
                                  if(stockTrade$isInOpenPosition()){
                                    lastDate = tail(stockTrade$stock$history[, "date"], n=1)
                                    .self$closeLastTrade(stockTrade, lastDate);
                                  }
                                }
                              }
                           )
)