library(tidyverse)
library(zoo)
library(xts)
library(readxl)
library(caret)
library(caretEnsemble)
library(randomForest)
library(reshape)
library(glmnet)
library(doParallel)
library(gridExtra)


cl <- makeCluster(6)
registerDoParallel(cl)

#rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rmseplot <- list()
ensembleplot <- list()
tenors <- 50
history <- 400
listhistory <- c(5,10,22,44,66,ifelse(history<126,history,126),ifelse(history<252,history,252))
listccy <- c("MYR","SGD","JPY","AUD","CNH","CAD","CHF","EUR","NZD","GBP","THB")
listccy2 <- c("SGD","MYR","JPY")
rownama <- c("1w","2w","1m","2m","3m","6m","1y")

#currency options
{
currencyall <- c("MYR","SGD","JPY","AUD","CNH","CAD","CHF","EUR","NZD","GBP","THB")
  NZD <- c("AUD","CAD","NZD","EUR","CNH")
  AUD <- c("AUD","CAD","NZD","EUR",'CNH')
  EUR <- c("EUR","GBP","AUD","CHF","NZD")
  CAD <- c("CAD","AUD","EUR","JPY","CNH")
  SGD <- c("SGD","AUD","EUR","JPY","CNH")
  MYR <- c("SGD","JPY","CNH","MYR")
  JPY <- c("JPY","AUD","CHF","EUR","SGD")
  CNH <- c("CNH","SGD","AUD","CAD","JPY")
  THB <- c("THB","CNH","SGD","AUD","MYR")
  CHF <- c("CHF","EUR","JPY","AUD","GBP")
  GBP <- c("GBP","AUD","JPY","CAD","EUR")
}


#preparing data
{
  dat <- read_excel("FX9am.xlsx",1)
  impliedvol <- read_excel("FX9am.xlsx",2)
  impliedvol <- impliedvol[1:nrow(impliedvol)-1,]
  data <- dat[,-1]
  datats <- na.approx(as.ts(data))[,currencyall]
  full_logreturns <- (diff(log(datats)))^2
  sum(is.na(full_logreturns))
  

  logreturns <- as_tibble(full_logreturns)
  logreturns <-  na.omit(logreturns)
  logreturns <- as_tibble(logreturns[1:history,])
  corrreturn <- as_tibble(round(cor(logreturns),2))
  covreturn <- as_tibble(round(cov(logreturns)*1000000,2))
  rownames(corrreturn) <- colnames(corrreturn)
  rownames(covreturn) <- colnames(covreturn)
}

#convert to tenor vols

  volrun1 <- rollapply(logreturns, tenors, sum, by = round(tenors/8), fill=NA, align='right')
  volrun1 <- sqrt(volrun1/(tenors-1))*sqrt(252)*100
  volrun2 <- rollapply(logreturns[1:round(history/2),], tenors, sum, by = round(tenors/10), fill=NA, align='right')
  volrun2 <- sqrt(volrun2/(tenors-1))*sqrt(252)*100
  volrun <- rbind(volrun1,volrun2)
  colnames(datats) <- paste(colnames(datats),rep("_R",ncol(datats)),sep="")
  volrate <-  as_tibble(na.omit(volrun))


valuation <- function(subject){
  
  #prepare currency list
  print(subject)
  currency <- eval(as.symbol(subject))
  x <- currency == subject
  y <- currency != subject
  currency <- c(currency[x],currency[y]) #rearrange to put subject ccy in first column
  fmla <- formula(paste(subject,"~ ."))
  weeklyvols <- volrate[currency] #pulls out cols that contains string in currency

  #splitting test data
    trainIndex <- caret::createDataPartition(pull(weeklyvols,1), p = .95, 
                                      list = FALSE, 
                                      times = 1)
    train <- weeklyvols[trainIndex,]
    test <- weeklyvols[-trainIndex,]
    train_x <- train[-1]
    train_y <- train[1]
    test_y <- test[1]
    test_x <- test[-1]
  
  
  #CaretList
    
    my_control <- trainControl(method = "repeatedcv", # for “cross-validation”
                               number = 5, # number of k-folds
                               repeats = 3,
                               savePredictions = "final",
                               allowParallel = TRUE)
    
    model_list <- caretList(fmla,
                            train,
                            trControl = my_control,
                            methodList = c("svmLinear","ranger","bridge"),
                            tuneList = NULL,
                            continue_on_fail = FALSE )

    resamples <- resamples(model_list)
    ensemble_1 <- caretEnsemble(model_list, 
                                metric = "RMSE",
                                trControl = my_control)
    ensembleplot[[subject]]<<-plot(ensemble_1, main=subject)
    implied <- round(predict(model_list, newdata = impliedvol),2)
    implied2 <- predict(ensemble_1, newdata = impliedvol)
    implied3 <- apply(implied,1,weighted.mean,c(0.10,0.80,0.1))
    print(implied3)
    nam <- paste("imp",subject,sep="")
    (assign(nam,implied3))
}



modelvols <- sapply(listccy,valuation)
rownames(modelvols) = rownama
do.call("grid.arrange", c(ensembleplot, ncol=floor(sqrt(length(ensembleplot)))))
#generating relative pricing


  realizedvols <- function(tenor){
    temp <- apply(logreturns[1:tenor,],2,sum)
    sqrt(temp/(tenor-1))*sqrt(252)*100
  }

historicalvols <- t(sapply(listhistory,realizedvols))
rownames(historicalvols) = rownama
historical <- round(historicalvols[,listccy],2)
histdiff <- impliedvol[listccy] - historical
implieddiff <- impliedvol[listccy] - modelvols
(finalvaluation <- as_tibble(round(0.1*histdiff + 0.9*implieddiff,2)))
heatmap <- melt(cbind(rownama,finalvaluation))
colnames(heatmap) <- c("Tenor","Currency","Value")
heatmap$Tenor <- factor(heatmap$Tenor,levels=rev(rownama))


#plot heatmap
{
heatgrid <- ggplot(heatmap, aes(Currency, Tenor)) +
  geom_tile(aes(fill = Value)) + 
  geom_text(aes(label = round(Value, 1))) +
  scale_fill_gradient2('pi0', low = "blue", mid = "white", high = "red", midpoint = 0)
heatgrid
}

#view realized vols


realized <- round(t(sapply(listhistory,realizedvols)),2)
histSmry = rbind(realized,colMeans(volrate))
rownames(histSmry) = c(rownama,'Avg')
print(as_tibble(histSmry))
print(as_tibble(impliedvol))


cb <- function(df, sep="\t", dec=".", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}


write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(realized)
print(cor(volrate))