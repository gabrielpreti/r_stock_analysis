library("quantmod")
library(TTR)
library(stringr)

stockReport <- function(completeStockData, stockCode){
  data = completeStockData[completeStockData$Code==stockCode, ]
#   dc<-DonchianChannel(cbind(data[, "High"],data[, "Low"]), n=26)
#   macd = MACD(cbind(data[, "High"],data[, "Low"], data[, "Close"]))
#   long_ema = EMA(cbind(data[, "High"],data[, "Low"], data[, "Close"]), n=26)
#   short_ema = EMA(cbind(data[, "High"],data[, "Low"], data[, "Close"]), n=12)
  
  chartSeries(zoo(data[,c(5, 6, 7, 8, 9)], data[, 1]), name=stockCode)
  
 
#   mm_ta = addTA(cbind(short_ema, long_ema), col=c("red", "green"), lty=c(2, 2), on=1)    
#   macd_ta = addTA(macd, col=c("red", "green"))
#   dc_ta = addTA(dc[, c(1, 3)], on = 1, col=c("blue", "blue"))
  
#   return(c(mm_ta, macd_ta, dc_ta))
}


#######################################################################
file_content = read.table(file="/tmp/cotacoes.txt", header=FALSE, quote="", fill = TRUE, sep=";");
file_content$Date = strptime(str_sub(file_content[, 1], start = 3, end = 10), format="%Y%m%d");
file_content$Code = str_trim(str_sub(file_content[, 1], start = 13, end = 24))
file_content$Type = str_trim(str_sub(file_content[, 1], start = 25, end = 27))
file_content$Name = str_trim(str_sub(file_content[, 1], start = 28, end = 39))
file_content$Open = as.numeric(str_sub(file_content[, 1], start = 57, end = 69))/100
file_content$High = as.numeric(str_sub(file_content[, 1], start = 70, end = 82))/100
file_content$Low = as.numeric(str_sub(file_content[, 1], start = 83, end = 95))/100
file_content$Close = as.numeric(str_sub(file_content[, 1], start = 109, end = 121))/100
file_content$Volume = as.numeric(str_sub(file_content[, 1], start = 171, end = 188))/100
file_content = file_content[, 2:ncol(file_content)]


pdf(file="/tmp/stock_report.pdf", );
par(mfrow=c(1, 1));
pdf_dev=dev.cur();

RStudioGD()
screen_dev=dev.list()["RStudioGD"];

#small caps
# stocks = c("ABCB4", "ABRE3", "ALPA4", "ALSC3", "ALUP11", "AMAR3", "ANIM3", "ARTR3", "ARZZ3", "BBRK3", "BEEF3", "BPHA3", "BRAP4", "BRIN3", "BRPR3", "BRSR6", "BTOW3", "CSMG3", "CVCB3", "CYRE3", "DIRR3", "DTEX3", "ECOR3", "ELPL4", "ENBR3", "EQTL3", "ESTC3", "EVEN3", "EZTC3", "FLRY3", "GETI3", "GETI4", "GFSA3", "GOAU4", "GRND3", "HBOR3", "HGTX3", "HRTP3", "IGTA3", "LEVE3", "LIGT3", "LINX3", "MEAL3", "MGLU3", "MILS3", "MPLU3", "MRFG3", "MRVE3", "MYPK3", "ODPV3", "OIBR3", "OIBR4", "POMO4", "QGEP3", "QUAL3", "RAPT4", "RLOG3", "RSID3", "RUMO3", "SEER3", "SLCE3", "SMLE3", "SMTO3", "STBP11", "SULA11", "TCSA3", "TGMA3", "TOTS3", "TRPL4", "VLID3")

#ibovespa
# stocks = c("ABEV3", "BBAS3", "BBDC3", "BBDC4", "BBSE3", "BRFS3", "BRKM5", "BRML3", "BVMF3", "CCRO3", "CESP6", "CIEL3", "CMIG4", "CPFE3", "CPLE6", "CRUZ3", "CSAN3", "CSNA3", "CTIP3", "ELET3", "ELET6", "EMBR3", "FIBR3", "GGBR4", "GOLL4", "HYPE3", "ITSA4", "ITUB3", "ITUB4", "JBSS3", "KLBN11", "KROT3", "LAME3", "LAME4", "LREN3", "MDIA3", "MULT3", "NATU3", "PCAR4", "PETR3", "PETR4", "PSSA3", "RADL3", "RENT3", "SANB11", "SBSP3", "SUZB5", "TAEE11", "TBLE3", "TIMP3", "UGPA3", "USIM3", "USIM5", "VALE3", "VALE5", "VIVT4", "VVAR11", "WEGE3")


stock_candidates = unique(file_content$Code)

DIFFERENCE_THRESHOLD=0.02;
stocks_to_analyze = c();
for(stock in stock_candidates){
  stock_data = file_content[file_content$Code==stock, ]
  if(nrow(stock_data)<30)
    next;
  
  donchian_channel = DonchianChannel(cbind(stock_data[, "High"],stock_data[, "Low"]), n=26)
  
  last_dc_value=donchian_channel[nrow(donchian_channel)-1, 1]  
  stock_close_value=stock_data[nrow(stock_data), "Close"]
  percentual_difference = abs(last_dc_value-stock_close_value)/stock_close_value
  
  if(percentual_difference <= DIFFERENCE_THRESHOLD){
    stocks_to_analyze[length(stocks_to_analyze)+1] = stock;
    print(stock);
    print(stock_close_value);
    print(last_dc_value);
    print("######################")
  }
}

stocks=stocks_to_analyze #qualquer ação cujo fechamento está perto do canal de donchian

for(s in stocks){
  dev.set(screen_dev) ;#joga a saída para a tela (é preciso pois cada análise gerada é um novo gráfico)
  tas = stockReport(file_content, s);
  for(i in tas){
    print(i);
  }
  dev.copy(which=pdf_dev);#copia somente último gráfico para o pdf
}

dev.off(pdf_dev)




##############
acao:"BICB4"
capital=50000
risco=2/100;
entrada=7.55
saida=7.36
qtd_cotas=(capital*risco)/(entrada-saida)
#################################################



