### Libraries ####
library(tidyverse)
library(data.table)
library(Rcpp)
library(keras)
library(plotly)

### Data ####
### "~/Documents/ta/untitled folder/30m nn"
### second column of dataClose is the label
dataClose <- fread("~/Documents/ta/untitled folder/30m nn/data/close.csv") 
dataVolatility <- fread("~/Documents/ta/untitled folder/30m nn/data/volatility.csv") 

### Parameters ####
# how far in the past to feed to nn
lookBack <- 52
# numbers of classes for the multiclass nn
classes <- 15
# how much data to train the NN on
trainDataSize <- 20000
# how many predictions the NN makes before it is retrained
timeBeforeRetrain <- 900
# NN parameters
dense1 <- 70
dropout1 <- 0.45
dense2 <- 60
dropout2 <- 0.45


### Utility Funtions ####
# changes percentage price change to catagories
# ie: divider <- c(-0.2, 0, 0.2)
# -0.23 becomes 1, -0.13 becomes 2, 0.1, becomes 3, 0.4 becomes 4
# will not work if the percentage is more than 1 (100%)
perToCat <- function(data, dividers){
  DT <- copy(data) %>% as.data.table()
  names(DT) <- "Close"
  for(i in 1:length(dividers)){
    DT[Close < dividers[i], Close := ..i] 
  }
  DT[Close < 1, Close := 0]
  return(DT)

}

# changes catagories to percentage price change
# the percentage price change is based off the average of the dividers
# the lowest and highest needs to be set manually
catToPer <- function(data, dividers, lowCat, highCat){
  DT <- copy(data) %>% as.data.table() 
  names(DT) <- "Close"
  DT[, Close := Close %>% as.numeric()]
  percentages <- c(lowCat)
  for(i in 2:length(dividers)){
    percentages[i] <- (dividers[i-1] + dividers[i])/2
  }
  for(i in 1:length(dividers)){
    DT[Close == i, Close := ..percentages[i]]
  }
  DT[Close == length(dividers)+1, Close := ..highCat]
  return(DT)
}

# calculates the EV given a vector of probabilities for percentage change
# cvXPred is a m by n matrix of the probabilities of each catagory
# see catToPer and perToCat for info about dividers, lowCat, and highCat
calcEV <- function(cvXPred, dividers, lowCat, highCat){
  pred <- copy(cvXPred) %>% as.data.table()
  percentages <- c(lowCat)
  for(i in 2:length(dividers)){
    percentages[i] <- (dividers[i-1] + dividers[i])/2
  }
  percentages[length(dividers)+1] <- highCat
  
  predEV <- pred[,1] * percentages[1]
  for(i in 2:ncol(cvXPred)){
    predEV <- predEV + pred[,..i] * percentages[i]
  }
  return(predEV)
}

# calculates the optimal f 
# data: 1 col data table or vector of outcomes; every outcome is assumed to have equal probabilities
optimalf <- function(data){
  return(optimize(function(x) prod((1+x*data)), c(0,5), maximum = TRUE))
}


# calculates the percentage profit/loss of predictions
# cvXPred: a m by n matrix of the probabilities of each catagory
# cvYPerChg: a m by 1 matrix of the actual percent change 
# see catToPer and perToCat for info about dividers, lowCat, and highCat
# fees: precentage deducted per trade 
# includeLongs and includesShorts: what type of positions to take
# type = "sep" or seperate": returns the PL of each inidividual period 
# type = "cum" or "cumulative": returns the additive PL over time
# type = "mult" or "multiplicative": returns the multiplicative PL (what a actual portfolio would look like)
# type = "optimalf": returns the multiplicative PL based on optimal f
# returns a vector (m by 1 data.table) of profit/loss 
calcPL <- function(cvXPred, cvYPerChg, alpha, dividers, lowCat, highCat, 
                   fees = 0, includeLongs = TRUE, includeShorts = TRUE,
                   type = "seperate"){
  predEV <- calcEV(cvXPred, dividers, lowCat, highCat)
  if(type == "cum" | type == "cumulative"){
    cppFunction(
      'NumericVector calcResultsCum(NumericVector cvXPred, NumericVector cvYPerChg, double alpha, double fees, bool longs, bool shorts){
        int n = cvXPred.size();
        NumericVector results(n);
        if(cvXPred[0] > alpha){
          results[0] = cvYPerChg[0] - fees;
        }else if(cvXPred[0] < -alpha){
          results[0] = -cvYPerChg[0] - fees;
        }else{
          results[0] = 0;
        }
        for(int i = 1; i < n; i++){
          if(cvXPred[i] > alpha && cvXPred[i-1] < alpha && longs){
            results[i] = results[i-1] + cvYPerChg[i] - fees;
          }else if(cvXPred[i] > alpha && longs){
            results[i] = results[i-1] + cvYPerChg[i];
          }else if(cvXPred[i] < -alpha && cvXPred[i-1] > -alpha && shorts){
            results[i] = results[i-1] - cvYPerChg[i] - fees;
          }else if(cvXPred[i] < -alpha && shorts){
            results[i] = results[i-1] - cvYPerChg[i];
          }else{
            results[i] = results[i-1];
          }
        }
        return results;
      }'
    )
    return(calcResultsCum(predEV %>% as.matrix(), cvYPerChg %>% as.matrix(), alpha, fees, any(includeLongs), any(includeShorts)))
  }else if(type == "sep" | type == "seperate" | type == "optimalf"){
    cppFunction(
      'NumericVector calcResultsSep(NumericVector cvXPred, NumericVector cvYPerChg, double alpha, double fees, bool longs, bool shorts){
        int n = cvXPred.size();
        NumericVector results(n);
        if(cvXPred[0] > alpha){
          results[0] = cvYPerChg[0] - fees;
        }else if(cvXPred[0] < -alpha){
          results[0] = -cvYPerChg[0] - fees;
        }else{
          results[0] = 0;
        }
        for(int i = 1; i < n; i++){
          if(cvXPred[i] > alpha && cvXPred[i-1] < alpha && longs){
            results[i] = cvYPerChg[i] - fees;
          }else if(cvXPred[i] > alpha && longs){
            results[i] = cvYPerChg[i];
          }else if(cvXPred[i] < -alpha && cvXPred[i-1] > -alpha && shorts){
            results[i] = -cvYPerChg[i] - fees;
          }else if(cvXPred[i] < -alpha && shorts){
            results[i] = -cvYPerChg[i];
          }else{
            results[i] = 0;
          }
        }
        return results;
      }'
    )
    if(type == "sep" | type =="seperate"){
      return(calcResultsSep(predEV %>% as.matrix(), cvYPerChg %>% as.matrix(), alpha, fees, any(includeLongs), any(includeShorts)))
    }else{
      PL <- calcResultsSep(predEV %>% as.matrix(), cvYPerChg %>% as.matrix(), alpha, fees, any(includeLongs), any(includeShorts))
      return(cumprod((1 + optimalf(PL)$maximum * PL)))
    }
  }else if(type == "mult" | type == "multiplicative"){
    cppFunction(
      'NumericVector calcResultsMult(NumericVector cvXPred, NumericVector cvYPerChg, double alpha, double fees, bool longs, bool shorts){
        int n = cvXPred.size();
        NumericVector results(n);
        if(cvXPred[0] > alpha){
          results[0] = 1 + cvYPerChg[0] - fees;
        }else if(cvXPred[0] < -alpha){
          results[0] = 1 - cvYPerChg[0] - fees;
        }else{
          results[0] = 1;
        }
        for(int i = 1; i < n; i++){
          if(cvXPred[i] > alpha && cvXPred[i-1] < alpha && longs){
            results[i] = results[i-1] * (1 + cvYPerChg[i] - fees);
          }else if(cvXPred[i] > alpha && longs){
            results[i] = results[i-1] * (1 + cvYPerChg[i]);
          }else if(cvXPred[i] < -alpha && cvXPred[i-1] > -alpha && shorts){
            results[i] = results[i-1] * (1 - cvYPerChg[i] - fees);
          }else if(cvXPred[i] < -alpha && shorts){
            results[i] = results[i-1] * (1 - cvYPerChg[i]);
          }else{
            results[i] = results[i-1];
          }
        }
        return results;
      }'
    )
    return(calcResultsMult(predEV %>% as.matrix(), cvYPerChg %>% as.matrix(), alpha, fees, any(includeLongs), any(includeShorts)))
  }
  return(NULL)
}



# Train and CV Data Selection ####
sortedClose <- dataClose[order(Close)][,2]
# where to divide the percentage changes into catagories
dividers <- sapply(1:(classes-1), function(x)(sortedClose[x/classes*nrow(dataClose)] %>% as.numeric))
# the lowest and highest categories when converting catagorical predictions to percentage change
lowCat <- mean(dataClose[Close < dividers[1], Close] %>% unlist())
highCat <- mean(dataClose[Close > dividers[length(dividers)], Close] %>% unlist())

idx <- 200:86000
time <- (dataClose[idx,1]/1000) %>% as.matrix() %>% as.POSIXct(origin="1970-01-01")
dataX <- cbind(dataClose[idx,3:(lookBack+2)], dataVolatility[idx,3:(lookBack+2)])
dataYRegr <- cbind(dataClose[idx,2])
dataYCat <- dataYRegr %>% perToCat(dividers) %>% as.matrix %>% to_categorical(classes) %>% as.data.table

### Training ####
model <- keras_model_sequential()
model %>%
  layer_batch_normalization(input_shape = lookBack * 2) %>%
  layer_dense(dense1, activation = 'relu') %>%
  layer_dropout(dropout1) %>%
  layer_dense(dense2, activation = 'relu') %>%
  layer_dropout(dropout2) %>% 
  layer_dense(units = classes, activation = 'softmax')

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = 'adam',
  metrics = list("accuracy")
)

pred <- data.table(matrix(nrow = 0, ncol = classes))
for(i in 0:(length(idx)/timeBeforeRetrain - ceiling(trainDataSize/timeBeforeRetrain) - 1) * timeBeforeRetrain){
  print(paste0("Fitting index ", (i+1), ":", (i+trainDataSize)))
  model %>% fit(
    dataX[(i+1):(i+trainDataSize)] %>% as.matrix,
    dataYCat[(i+1):(i+trainDataSize)] %>% as.matrix,
    epochs = 50,
    validation_split = 0.1,
    callbacks = list(callback_early_stopping(patience = 8, restore_best_weights = TRUE))
  )
  pred <- rbind(pred, model %>% predict(dataX[(i+trainDataSize+1):(i+trainDataSize+timeBeforeRetrain)] %>% as.matrix))
  save_model_hdf5(model, paste0("~/Documents/ta/untitled folder/30m nn/model", i, ".h5"))
}

### Plotting ####
createPLPLot <- function(cvXPred, cvYPerChg, dividers, lowCat, highCat, time, 
                         type = "cum", fees = c(0), alpha = c(0), 
                         includeLongs = TRUE, includeShorts = TRUE){
  if(type == "cum" | type == "cumulative"){
    PLOverTime <- plot_ly(
      x = time, 
      y = cumsum(cvYPerChg),
      name = "HODL",
      type = "scatter",
      mode = "lines"
    )
  }else if(type == "sep" | type == "seperate"){
    PLOverTime <- plot_ly(
      x = time, 
      y = (cvYPerChg),
      name = "HODL",
      type = "scatter",
      mode = "lines"
    )
  }else if(type == "mult" | type == "multiplicative"){
    PLOverTime <- plot_ly(
      x = time, 
      y = cumprod(1+cvYPerChg),
      name = "HODL",
      type = "scatter",
      mode = "lines"
    ) %>% layout(
      yaxis = list(type = "log")
    )
  }else if(type == "optimalf"){
    PLOverTime <- plot_ly(
      x = time, 
      y = cumprod((1+optimalf(cvYPerChg)$maximum * cvYPerChg)),
      name = "Always Long",
      type = "scatter",
      mode = "lines"
    ) %>% layout(
      yaxis = list(type = "log")
    )
  }else{
    return(NULL)
  }

  for(i in fees){
    for(j in alpha){
      if(includeLongs){
        PLOverTime <- PLOverTime %>% add_trace(
          y = calcPL(cvXPred, cvYPerChg, j, dividers, lowCat, highCat, i, type = type, 
                     includeShorts = FALSE),
          name = paste0("Longs Only >", j, " Signal Strength"),
          mode = "lines"
        )
      }
      if(includeShorts){
        PLOverTime <- PLOverTime %>% add_trace(
          y = calcPL(cvXPred, cvYPerChg, j, dividers, lowCat, highCat, i, type = type, 
                     includeLongs = FALSE),
          name = paste0("Shorts Only>", j, " Signal Strength"),
          mode = "lines"
        )
      }
      if(includeLongs | includeShorts){
        PLOverTime <- PLOverTime %>% add_trace(
          y = calcPL(cvXPred, cvYPerChg, j, dividers, lowCat, highCat, i, type = type),
          name = paste0("Longs & Shorts >", j, " Signal Strength"),
          mode = "lines"
        )
      }
    }
  }

  return(PLOverTime)
}

plotCum <- createPLPLot(pred, dataYRegr[1:nrow(pred) + trainDataSize] %>% unlist, dividers, lowCat, highCat, time[1:nrow(pred) + trainDataSize], "cum", alpha = c(0, 0.001))

plotMult <- createPLPLot(pred, dataYRegr[1:nrow(pred) + trainDataSize] %>% unlist, dividers, lowCat, highCat, time[1:nrow(pred) + trainDataSize], "mult", alpha = c(0, 0.001))
 
plotOptimalf <- createPLPLot(pred, dataYRegr[1:nrow(pred) + trainDataSize] %>% unlist, dividers, lowCat, highCat, time[1:nrow(pred) + trainDataSize], "optimalf", alpha = c(0, 0.001))

'htmlwidgets::saveWidget(as_widget(plotCum), "~/Documents/ta/untitled folder/30m nn/additive_PL.html")
htmlwidgets::saveWidget(as_widget(plotMult), "~/Documents/ta/untitled folder/30m nn/real_PL.html")
htmlwidgets::saveWidget(as_widget(plotOptimalf), "~/Documents/ta/untitled folder/30m nn/optimalf_PL.html")'















