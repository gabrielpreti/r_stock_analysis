stocks_to_analyze = c("ACES4", "BBDC4", "BDLL4", "BELG4", "BOBR4", "BRAP4", "BRIV4", "BRTO4", "BRTP4", "CAFE4", "CESP4", "CMET4", "CMIG4", "CNFB4", "CRIV4", "CSTB4", "CTNM4", "DURA4", "EBCO4",
                      "EBTP4", "ELPL4", "EMBR4", "ESTR4", "FFTL4", "FJTA4", "FRAS4", "GETI4", "GGBR4", "GOAU4", "GOLL4", "GUAR4", "IMBI4", "INEP4", "ITAU4", "ITSA4", "LAME4", "LEVE4", "LIXC4",
                      "MGEL4", "MNDL4", "MTSA4", "MWET4", "MYPK4", "PCAR4", "PETR4", "POMO4", "PRGA4", "PTIP4", "RAPT4", "RCSL4", "RIPI4", "RPSA4", "SAPR4", "SCLO4", "SDIA4", "SHUL4", "SLED4",
                      "STRP4", "TCOC4", "TCSL4", "TEKA4", "TLPP4", "TMCP4", "TNLP4", "TOYB4", "TRFO4", "TRPL4", "TSPP4", "UBBR4", "VCPA4", "WEGE4", "BGIP4");
total_gain=0;
for(stockCode in stocks_to_analyze){
  stock_data = file_content[file_content$Code==stockCode, ]
  stock_data = stock_data[stock_data$Date>'2013-01-01', ]
  if(nrow(stock_data)==0)
    next;
  
  donchian_size=10
  donchian_channel<-DonchianChannel(cbind(stock_data[, "High"],stock_data[, "Low"]), n=donchian_size)
  stock_data$donchian_high = donchian_channel[, "high"];
  stock_data$donchian_low = donchian_channel[, "low"];
  
  long_mm_size=26
  short_mm_size=9
  long_mm = SMA(cbind(stock_data[, "High"],stock_data[, "Low"], stock_data[, "Close"]), n=long_mm_size)
  short_mm = SMA(cbind(stock_data[, "High"],stock_data[, "Low"], stock_data[, "Close"]), n=short_mm_size)
  # 
  chartSeries(zoo(stock_data[,c(5, 6, 7, 8, 9)], stock_data[, 1]), name=stockCode)
  #  addTA(donchian_channel[, c(1, 3)], on = 1, col=c("blue", "blue"))
  addTA(cbind(short_mm, long_mm), col=c("red", "green"), lty=c(2, 2), on=1)
  
  
  
  donchian_low_value = NA;
  buyed_value = NA;
  gain = 0;
  first_analysed_index = max(donchian_size, long_mm_size, short_mm_size) + 1;
  for(i in first_analysed_index:nrow(stock_data)){
    mm_buy_condition = short_mm[i-1]<long_mm[i-1] && short_mm[i]>long_mm[i];
    close_up_condition = stock_data[i, "Close"] >= stock_data[i-1, "Close"]
    
    if(is.na(buyed_value) && mm_buy_condition && close_up_condition){
#       print(paste(stock_data[i, "Date"], "Buyed by", stock_data[i, "Close"]))
      buyed_value = stock_data[i, "Close"];
      donchian_low_value = donchian_channel[i-1, "low"];
      next;
    }
    
    donchian_sell_condition = stock_data[i, "Close"] < donchian_low_value;
    mm_sell_condition = short_mm[i-1]>long_mm[i-1] && short_mm[i]<=long_mm[i];
    
    profit_sell_condition = !is.na(buyed_value) && stock_data[i, "Close"]>buyed_value && mm_sell_condition;
    stop_loss_sell_condition = !is.na(buyed_value) && stock_data[i, "Close"]<=buyed_value && donchian_sell_condition
    
    if(profit_sell_condition || stop_loss_sell_condition ){
#       print(paste(stock_data[i, "Date"], "Buyed by", buyed_value, "and sold by", stock_data[i, "Close"]));
      gain = gain + ( stock_data[i, "Close"] - buyed_value );
      buyed_value = NA;
      donchian_low_value = NA;
    }
  }
  
  print(paste("Stock:", stockCode, "Gain:", gain));
  total_gain = total_gain+gain;
}

