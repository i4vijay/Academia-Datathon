library(forecast)
library(dplyr)


# Dataset is uploaded that has prices of January month starting from 18th 00:00
pjan18<-read.csv("Jan_18.csv")

# Number of NAs
sapply(pjan18[,2:length(pjan18)], function(x){sum(is.na(x))})

# NAs treatment

  # Converted to time series
  pjan18ts<-ts(pjan18[2:length(pjan18)],start = c(1,1),frequency=288)
   #Considering 18th March as 1st cycle and 00:00 as first time point

  # NAs imputations by Kalman
  pjan18ts_imp<-na.kalman(pjan18ts,model = "auto.arima")

# Check for NAs to verify
sapply(pjan18ts_imp, function(x){sum(is.na(x))})


# Loop to forecast time points, one per iterations


df<-data.frame()
df<-data.frame(Row=1:1440)
m=1

tic=Sys.time()
while(m<=20){
  k=1
  i=1
  l=7
  j=288
  pred=c()
  while (k<=5) {
    price<-window(pjan18ts_imp[,m],start = c(k,i),end = c(l,j))
    model<-nnetar(price)
    mod_pred<-forecast(model,h=1)
    pred<-append(pred,mod_pred$mean[[1]])
    i=i+1
    j=j+1
    if(i>=289)
    {
      i=1
      k=k+1
    }
    if(j>=289){
      j=1
      l=l+1
    }
  }
  df[,m]=pred
  m=m+1
}
tac=Sys.time()

# Execution Time

tac-tic

# Calculation of different metrics

b<-read.csv("org.csv") # Here org.csv contains the values of original prices

b$pred<-pred  # Predicted Values are inserted

# Calculation of MAPE
mape<-100/length(b$Original)*sum(abs((b$Original-b$pred)/b$Original))


# Calculation of Directional Symmetry
d=c()
i=2
while (i<=nrow(b)) {
  if((b$Original[i]-b$Original[i-1])*(b$pred[i]-b$pred[i-1])>0){
    d=append(d,1)
  }
  else{
    d=append(d,0)
  }
  i=i+1
}
ds<-100/(nrow(b)-1)*sum(d)
ds

