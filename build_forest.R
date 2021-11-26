install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caret")
install.packages("e1071")
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
file_path <- "/Users/sbeni/suli/Reggelistat/PET-project/heart.csv"
heart_failure <- read.csv(file_path)

output = build_forest(HeartDisease, heart_failure)

build_forest <- function(HeartDisease, heart_failure){
  "
  HeartDisease : ez az oszlop amit becsülni akarunk
    vegyünk figyelme, hogy ugyanaz a paraméter neve, mint a tényleges adat
  heart_failure : input data amiből becsüljük HeartDisease oszlopot
    itt is vegyünk figyelembe, hogy alapvetően ugyanúgy van elnevezve a data,
    mint amit használunk
  "
  
  
###alapbeállítasok
train_control = trainControl(
  method = "cv", # resampling method
  number = 11, #node-ok száma
  search = "grid" #a finomhangolási paraméter
)
heart_failure$HeartDisease = as.factor(heart_failure$HeartDisease)
####

###mtry optimalizá
finom_hangolas <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(HeartDisease~.,
                 data = heart_failure,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = finom_hangolas,
                 trControl = train_control,
                 importance = TRUE,
                 nodesize = 10, #implicit megadja milyen mélységű lehet a fa. minimum size of terminal nodes
                 ntree = 300
                 )
opt_mtry=rf_mtry$bestTune$mtry
###

###maxnodes optimalizálás
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = opt_mtry)
for (maxnodes in c(5: 15)) {
  rf_maxnode <- train(HeartDisease~.,
                      data = heart_failure,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = train_control,
                      importance = TRUE,
                      nodesize = 10,
                      maxnodes = maxnodes, #itt az iteráló elem
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)

opt_maxnode = 5
iter_val = 0
iter = 0
node_mapping = seq(5,15,1)
for (i in seq(2,22,2)){
  iter = iter + 1
  if (max(results_mtry[2][1]$values[i]) > iter_val){
    iter_val = max(results_mtry[2][1]$values[i])
    opt_maxnode = node_mapping[iter]
  }
}
#a results_mtry objectből ilyen módon tudjuk kinyerni az optimális node számot

###

###ntree optimalizálás
store_maxtrees <- list()
for (ntree in c(5,10,20,30,40,50,100,200,300)) {
  
  rf_maxtrees <- train(HeartDisease~.,
                       data = heart_failure,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = train_control,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = opt_maxnode,
                       ntree = ntree # itt iterálunk végig
  )
  store_maxtrees[[toString(ntree)]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)

opt_ntree = 5
iter_val = 0
iter = 0
tree_mapping = seq(5,15,1)
for (i in seq(2,22,2)){
  iter = iter + 1
  if (max(results_tree[2][1]$values[i]) > iter_val){
    iter_val = max(results_tree[2][1]$values[i])
    opt_ntree = tree_mapping[iter]
  }
}
###

###vegso modell létrehozása
vegso <- train(HeartDisease~.,
               heart_failure,
               method = "rf",
               metric = "Accuracy",
               tuneGrid = tuneGrid, #ebben van az mtry optimális értéke
               trControl = train_control,
               importance = TRUE,
               nodesize = 14,
               ntree = opt_ntree,
               maxnodes = opt_maxnode)
###

return(vegso)
}

