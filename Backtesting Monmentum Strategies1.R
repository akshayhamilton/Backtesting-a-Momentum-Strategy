library("PerformanceAnalytics")
library("quantmod")
library("pbapply")
library("data.table")
x<-1
e<- new.env()#For new Environment 
tickers<-c("AMZN","BIDU","GLD","GOOGL","GS","IWM","NFLX","MMM","DIA","SPY")  #Making Tickers
getSymbols(tickers, from="2003-01-01",env=e)#Getting the data from Yahoo Finance and sending it to the new environment "e"
PRC<-do.call(merge,eapply(e,Ad))
tail(PRC,3)
#Compensating for missing value of last day
if(last(index(PRC)) != Sys.Date()) {last<-pblapply(as.list(gsub(".Adjusted","",names(PRC))), getQuote)
PRC<-rbind(PRC,xts(coredata(t(rbindlist(last)$Last)),order.by = Sys.Date()))}
tail(PRC)
#Getting a chacter vector of the tickers we have and changing the column names of our data by dropping the ".Adjusted"

NOM<- colnames(PRC)<-gsub(".Adjusted","",names(PRC))

head(PRC) #We can see that our header names have changed
#Calculating Momentum of past 60 days
MoM60<-round(ROC(PRC,n=60,type="discrete"),4)
PRC<-PRC["20030331::"]
#Date Vector/Re balancing dates
indx<- seq(as.Date("2003-03-31"), length.out=300, by='4 weeks') #Daily momentum

#Momentum 4 weeks apart
SELECT<-MoM60[paste(indx)];dim(SELECT)
#Accounting for holidays
indx2<- ifelse(indx %in% index(SELECT)==F, paste(indx+1), paste(indx))
SELECT<-MoM60[paste(indx2)];dim(SELECT) #Now we have more rows

PRC2<-PRC[paste(indx2)];dim(SELECT)

#Combination vector for apply momentum strategies to 4 tickers 

ASSETS4<-combn(NOM,4)
View(ASSETS4)

#Function to calculate the returns of each combination

MOMO = function(x)
  
  
{  y<-ASSETS4[,x] #for getting tickers
s<-SELECT[,y] #for getting the momentum of the combination
SEQ<-as.numeric(apply(s,1,which.max)) #Which asset returned the highest momentum for each row.
Prc2<-round(PRC2[,y],2) #What returns are for each asset in that time frame
RETS<- CalculateReturns(Prc2,"discrete")#Storing the returns
#What Column returned the highest and what were is asset returns for that period

ALL<-do.call(merge, lapply(as.list(1:ncol(RETS)), function(x){Lag(reclass(ifelse(SEQ==x,1,0),match.to = s)*RETS[,x])}))

colnames(ALL)<-names(Prc2)
ALL[is.na(ALL)]<-0 #For replacing all N/A's with a zero.

#Now we want one return vector ins-ted of 4 columns
EQT<-reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
head(EQT)
#rename 
colnames(EQT)<-paste(names(Prc2), collapse = "-")
EQT
}

#apply the above function to all the combinations of assets
STRAT<- pblapply(as.list(1:ncol(ASSETS4)),function(x) MOMO(x))
#Total return of each combination

AAA<- pblapply(STRAT,colSums)

#Extracting the top 10 results
df<-STRAT[order(sapply(AAA,"[[",1))]
df<-df[(length(df)-9):length(df)]
TOP10<- do.call(merge, df)

#descriptive data

table.Stats(TOP10)

#Column sums of the top 10
AAA<-lapply(df,colSums)
AAA[[which.max(AAA)]]

#Exteact Returns
EQT<- df[[which.max(AAA)]]
head(EQT)
charts.PerformanceSummary(EQT,geometric = TRUE)
table.Stats(EQT)
table.Drawdowns(EQT)



#Extracting the prices the sequence and the returns

getMOMO = function(x)
  
  
{  y<-as.character(strsplit(x,"-")[[1]]) #for getting tickers
s<-SELECT[,y] #for getting the momentum of the combination
SEQ<-as.numeric(apply(s,1,which.max)) #Which asset returned the highest momentum for each row.
Prc2<-round(PRC2[,y],2) #What returns are for each asset in that time frame
RETS<- CalculateReturns(Prc2,"discrete")#Storing the returns
#What Column returned the highest and what were is asset returns for that period

ALL<-do.call(merge, lapply(as.list(1:ncol(RETS)), function(x){Lag(reclass(ifelse(SEQ==x,1,0),match.to = s)*RETS[,x])}))

colnames(ALL)<-names(Prc2)
ALL[is.na(ALL)]<-0 #For replacing all N/A's with a zero.

#Now we want one return vector ins-ted of 4 columns
EQT<-reclass(rowSums(ALL),match.to=ALL); EQT[is.na(EQT)]<-0
head(EQT)
#rename 
colnames(EQT)<- "MOMORET"
cbind(Prc2,SEQ,round(EQT,4))

}

DATA<-getMOMO(x="GLD-NFLX-AMZN-BIDU")
View(DATA)

## Plots

chart.RiskReturnScatter(TOP10, add.sharpe = c(1), Rf=(0.03/sqrt(252)), colorset = rich10equal, 
                        xlim = c(0.45,0.55), ylim = c(1.4,1.75))
charts.PerformanceSummary(TOP10, cex.legend=0.45, colorset=rich10equal, geometric = T, main = "TOP 10")
