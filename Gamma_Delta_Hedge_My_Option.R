require("pbapply");require("data.table");require("quantmod")
# ***********************************************************************
#               read in options data from TD Ameritrade
# ***********************************************************************
FILES = list.files("/Volumes/3TB/TDAPI/OptionChains/ALL-DF",full.names = TRUE)
ops = pblapply(as.list(FILES),function(file){
  tmp = readRDS(file)
  tmp = subset(tmp,tmp$StockSymbol == "SPY")
  tmp = subset(tmp, tmp$expirationDate == "2021-03-26 13:00:00 PST")
  tmp = tmp[,c("putCall","mark","quoteTimeInLong","netChange","volatility",
               "delta","gamma","theta","vega","rho","strikePrice",
               "daysToExpiration","expirationDate","StockSymbol","StockClose")]
  tmp$quoteTimeInLong = as.Date(tmp$quoteTimeInLong)
  tmp$expirationDate = as.Date(tmp$expirationDate)
  tmp
})
ops2 = ops[lapply(ops,nrow)>0]
ops2 = rbindlist(ops2,use.names=TRUE)
ops2 = ops2[,c("putCall","mark","quoteTimeInLong","netChange","volatility",
               "delta","gamma","strikePrice","daysToExpiration","expirationDate",
               "StockSymbol","StockClose")]
saveRDS(ops2,"spy_ops.rds") # ALL OPTIONS
# ***********************************************************************
# ***********************************************************************
ops2 = readRDS("spy_ops.rds")                       # ALL OPTIONS
ops2$ID = paste0(ops2$strikePrice,"-",ops2$putCall) # create an option ID to easily ID options

# Selling a CALL credit Spread - Want to hedge it with shares
strk1_sell = 396.00 
strk1_Type = "CALL"
strk2_buy  = 397.00
strk2_Type = "CALL"
days2exp = 14

# function
gamma_delta_hedge = function(strk1_sell, strk1_Type, strk2_buy, strk2_Type, days2exp){
  
  ID_SELL = paste0(strk1_sell,"-",strk1_Type)
  ID_BUY = paste0(strk2_buy,"-",strk2_Type)
  
  # select strikes from options data
  tmp = subset(ops2, ops2$ID == ID_SELL | ops2$ID == ID_BUY)
  
  # subset to match the number of days to expiration
  tmp = subset(tmp, tmp$daysToExpiration <= days2exp)
  
  # reorganize columns
  tmp = tmp[,c("quoteTimeInLong","putCall","strikePrice","mark","delta","gamma",
               "StockClose","daysToExpiration","ID")]
  
  # fix issue with premiums at expiration
  fx = subset(tmp, tmp$daysToExpiration == 0)
  if(fx$putCall[1]== "CALL")
  {
    fx$mark[1] = ifelse(fx$strikePrice[1] <= fx$StockClose[1],fx$StockClose[1]-fx$strikePrice[1],0)
  }
  if(fx$putCall[1]== "PUT")
  {
    fx$mark[1] = ifelse(fx$strikePrice[1] >= fx$StockClose[1],fx$strikePrice[1]-fx$StockClose[1],0)
  }
  
  if(fx$putCall[2]== "CALL")
  {
    fx$mark[2] = ifelse(fx$strikePrice[2] <= fx$StockClose[2],fx$StockClose[2]-fx$strikePrice[2],0)
  }
  if(fx$putCall[2]== "PUT")
  {
    fx$mark[2] = ifelse(fx$strikePrice[2] >= fx$StockClose[2],fx$strikePrice[2]-fx$StockClose[2],0)
  }
  
  # drop old values
  tmp = subset(tmp,tmp$daysToExpiration >=1)
  tmp = rbind(tmp,fx)
  
  # short 1st strike
  tmp$delta[is.nan(tmp$delta)]<-0
  tmp$gamma[is.nan(tmp$gamma)]<-0
  strks = unique(tmp$ID)
  s1 = subset(tmp,tmp$ID == ID_SELL)
  s1$mark <- -s1$mark 
  s1$delta <- -s1$delta
  s1$gamma <- -s1$gamma
  s1 = rbind(s1,subset(tmp,tmp$ID == ID_BUY))
  
  # extract unique days 2 expirations
  d = unique(tmp$daysToExpiration)
  d = d[order(d,decreasing = TRUE)]
  
  # calculate delta-gamma-hedge results for 
  short1 = lapply(as.list(d),function(days2exp){
    one = subset(s1, s1$daysToExpiration == days2exp)
    one = one[order(one$mark),] %>% as.data.frame()
    
    delta1 = round(one$delta[1]*100,1)  # delta of the 1st strike
    gamma1 = round(one$gamma[1]*100,1)  # gamma of the 1st strike
    
    # num of (2nd strike) contracts required to hedge gamma
    gamma_H = ifelse(is.nan(round(-gamma1/(one$gamma[2]*100),2)),0,
                     round(-gamma1/(one$gamma[2]*100),2))
    delta2 = round((gamma_H*100)*one$delta[2],1)   # delta of the 2nd strike
    gamma2 = round((gamma_H*100)*one$gamma[2],1)   # gamma of the 2nd strike
    
    # gamma should now be hedged, now we need to hedge delta using shares
    netGamma = round(gamma1 + gamma2,1)
    
    stkShrs  = -(delta1 + delta2)        # delta hedge 
    netDelta = round(stkShrs+delta1+delta2,2)     # delta should now be hedged
    
    prc     = (one$mark[1]*1)+(one$mark[2]*gamma_H) # net premium of options (negative == credit)
    stkVal  = stkShrs*one$strikePrice[1] # value of position
    # combine data
    one <- as.data.frame(cbind(as.character(one$quoteTimeInLong[1]), 
                               one$ID[1],one$putCall[1],one$strikePrice[1], 
                               one$mark[1], one$delta[1],one$gamma[1],one$ID[2],
                               one$putCall[2],one$strikePrice[2], one$mark[2], 
                               one$delta[2],one$gamma[2],prc,
                               gamma1, gamma2,gamma_H,netGamma,
                               delta1,delta2,stkShrs,netDelta,
                               one$StockClose[1],stkVal,
                               one$daysToExpiration[1]))
    # change column names
    colnames(one) <- c("Date","ID1","type1","strike1","premium1","delta1","gamma1",
                       "ID2","type2","strike2","premium2","delta2","gamma2",
                       "netPremium","strk1Gamma","strk2Gamma","strk2Contracts","netGamma",
                       "strk1Delta","strk2Delta","stkShares","netDelta",
                       "stkPrice","stkValue","days2exp")
    
    # fix column classes
    one$strike1       = one$strike1 %>% as.numeric()
    one$strike2       = one$strike2 %>% as.numeric()
    one$premium1      = one$premium1 %>% as.numeric()
    one$premium2      = one$premium2 %>% as.numeric()
    one$delta1        = one$delta1 %>% as.numeric()
    one$delta2        = one$delta2 %>% as.numeric()
    one$gamma1        = one$gamma1 %>% as.numeric()
    one$gamma2        = one$gamma2 %>% as.numeric()
    one$netPremium    = one$netPremium %>% as.numeric()
    one$strk1Gamma    = one$strk1Gamma %>% as.numeric()
    one$strk2Gamma    = one$strk2Gamma %>% as.numeric()
    one$strk2Contracts= one$strk2Contracts %>% as.numeric()
    one$netGamma      = one$netGamma %>% as.numeric()
    one$strk1Delta    = one$strk1Delta %>% as.numeric()
    one$strk2Delta    = one$strk2Delta %>% as.numeric()
    one$stkShares     = one$stkShares %>% as.numeric()
    one$netDelta      = one$netDelta %>% as.numeric()
    one$stkPrice      = one$stkPrice %>% as.numeric()
    one$stkValue      = one$stkValue %>% as.numeric()
    one$days2exp      = one$days2exp %>% as.numeric()
    one
  })
  # row bind results
  short1 = do.call(rbind,short1)
  # complete cases
  short1 <- na.omit(short1)
  # get Daily PnL on Stock
  stkPnL = suppressWarnings(c(0,diff(short1$stkPrice)*short1$stkShares))
  # create column for stock PnL
  short1$stkPnL = stkPnL[1:(length(stkPnL)-1)] 
  # create column for option PnL
  ret1 = c(0,diff(short1$premium1))*100
  ret2 = suppressWarnings(c(0,diff(short1$premium2)*short1$strk2Contracts*100)[1:length(ret1)])
  short1$optPnL <- ret1+ret2
  # create column for gross PnL
  short1$grossPnL <- round(short1$stkPnL + short1$optPnL,2)
  # cumulative sum of PnL
  short1$cSumPnL <- cumsum(short1$grossPnL)
  
  # plot(cumsum(short1$stkPnL),type='l',ylim=c(-50,50)) # stock PnL
  # lines(cumsum(short1$optPnL),type='l',col="red")     # option PnL
  # lines(short1$cSumPnL,type='l',col="green")          # Net PnL
  # legend("bottomleft",
  #        legend = c("Stock Price","Options","Hedged"),
  #        col = c("black","red","green"),
  #        pch = c(1,1,2),
  #        bty = "n",
  #        pt.cex = 1,
  #        cex = .5,
  #        text.col = "black",
  #        horiz = TRUE ,
  # )
  
   short1
}
gamma_delta_hedge_plot = function(df){
plot(cumsum(df$stkPnL),type='l',col="black",
     ylim=c(min(df$stkPnL,df$optPnL)-20,max(df$stkPnL,df$optPnL)+20)) # stock PnL
lines(cumsum(df$optPnL),type='l',col="red")     # option PnL
lines(df$cSumPnL,type='l',col="green")          # Net PnL
legend("bottomleft",
       legend = c("Stock Price","Options","Hedged"),
       col = c("black","red","green"),
       pch = c(1,1,2), 
       bty = "n", 
       pt.cex = 1, 
       cex = .5, 
       text.col = "black", 
       horiz = TRUE , 
       )
}

# test the function : Vertical Put Credit Spread  - Out of the Money
df = gamma_delta_hedge(strk1_sell=392, strk1_Type="PUT", strk2_buy=391, strk2_Type="PUT", days2exp=14)
gamma_delta_hedge_plot(df)

# test the function : Vertical Put Credit Spread  - In  the Money
df = gamma_delta_hedge(strk1_sell=398, strk1_Type="PUT", strk2_buy=397, strk2_Type="PUT", days2exp=14)
gamma_delta_hedge_plot(df)

# test the function : Sell Put & Buy Call 
df = gamma_delta_hedge(strk1_sell=390, strk1_Type="PUT", strk2_buy=393, strk2_Type="CALL", days2exp=14)
gamma_delta_hedge_plot(df)

# test the function : Vertical Call Debit - In the Money
df = gamma_delta_hedge(strk1_sell=393, strk1_Type="CALL", strk2_buy=392, strk2_Type="CALL", days2exp=14)
gamma_delta_hedge_plot(df)

