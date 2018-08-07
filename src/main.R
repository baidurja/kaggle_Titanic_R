source( './preProcess/preProcess.R' )
source( './model/model.R' )

library( "DataExplorer" )
library( "caTools" )

useANN = 0

if ( useANN )
{
  library( "h2o" )
  library( "neuralnet" )
}

seed = 123

dataset = readRawData( '../data/train.csv' )
test_dataset = readRawData( '../data/test.csv' )

dataset$IsTrainSet <- TRUE
test_dataset$IsTrainSet  <- FALSE

# In order to combine the two datasets together,
#    both test and train sets need to have the
#    same columns.
# Give the test set a survived column
test_dataset$Survived <- NA

# Combine the two datasets into one
titanic.full <- rbind(dataset,test_dataset)

titanic.full$FamilyNum = titanic.full$SibSp + titanic.full$Parch + 1
titanic.full$Fare = replaceMissingValWithMean( titanic.full$Fare )
titanic.full$Embarked = replaceMissingValwithMode( titanic.full$Embarked )
titanic.full$FarePerPerson = titanic.full$Fare / titanic.full$FamilyNum

titanic.full = titleFromName( titanic.full )

titanic.full$HasCabin = as.factor(!is.na(titanic.full$Cabin))
titanic.full$FarePPcat1 = cut( titanic.full$FarePerPerson, c( -1E-10, 40, 80, 513 ))
titanic.full = splitTicket( titanic.full )
titanic.full$TktNum = replaceMissingValWithMean( titanic.full$TktNum )
titanic.full = splitCabin( titanic.full )

titanic.full$FamilyNum = as.factor( titanic.full$FamilyNum )
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.full$Sex <- as.factor(titanic.full$Sex)

quantiles <- boxplot.stats(titanic.full$Age)$stats
# Anything greater than the upper bound 
#    whisker is an outlier
upper.whisker <- quantiles[5]
outlier.filter <- titanic.full$Age < upper.whisker

# Build a linear model to predict age,
#    where outliers of age were filtered out
age.equation <- "Age ~ Pclass + Sex + Title + SibSp + Parch"
age.model <- lm(
  formula = age.equation,
  data = titanic.full[outlier.filter,]
)

age.row <- titanic.full[
  is.na(titanic.full$Age),
  c( "Pclass", "Sex", "Title", "SibSp", "Parch", "FarePerPerson", "Embarked")
  ]
age.predictions  <- predict(
  age.model,
  newdata = age.row
)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.predictions
titanic.full[ titanic.full$Age < 0, "Age" ] = 0.1

titanic.full$Agecat1 = cut( titanic.full$Age, c( 0, 15, 25, 33, 47, 80 ))

##############################
# Separate datasets back out #
##############################

dataset <- titanic.full[titanic.full$IsTrainSet==TRUE,]
test_dataset <- titanic.full[titanic.full$IsTrainSet==FALSE,]

# Classification task
dataset$Survived <- as.factor(dataset$Survived)

# dataset$Survived = changeToCategorical( dataset$Survived )
# # dataset = subset(dataset, !is.na(Age))
# dataset$FamilyNum = dataset$SibSp + dataset$Parch + 1
# dataset$Fare = replaceMissingValWithMean( dataset$Fare )
# dataset$FarePerPerson = dataset$Fare / dataset$FamilyNum
# dataset$FamilyNum = as.factor( dataset$FamilyNum )
# 
# 
# test_dataset$FamilyNum = test_dataset$SibSp + test_dataset$Parch + 1
# test_dataset$Fare = replaceMissingValWithMean( test_dataset$Fare )
# test_dataset$FarePerPerson = test_dataset$Fare / test_dataset$FamilyNum
# test_dataset$FamilyNum = as.factor( test_dataset$FamilyNum )

# pairs( ~ Survived + Pclass + Sex + Age , data = dataset , main = "Simple Scatter Matrix")

#plot_missing( dataset )
#plot_bar( dataset )
#plot_histogram( dataset )
# if ( !useANN )
# {
#   dataset = assignCategorical( dataset ) 
#   test_dataset = assignCategorical( test_dataset ) 
# }
# dataset = handleMissingVal( dataset )
# test_dataset = handleMissingVal( test_dataset )
# if ( useANN )
# {
#   dataset = labelCategoricalAsNumeric( dataset )
#   # dataset = labelCategorical( dataset )
# } else
# {
#   dataset = labelCategorical( dataset )
#   test_dataset = labelCategorical( test_dataset )
# }
# 
# # boxplot( FarePerPerson ~ Pclass, data = dataset )
# 
# dataset$Agecat1 = cut( dataset$Age, c( 0, 15, 25, 33, 47, 80 ))
# dataset$HasCabin = as.factor(!is.na(dataset$Cabin))
# dataset$FarePPcat1 = cut( dataset$FarePerPerson, c( -1E-10, 40, 80, 513 ))
# 
# test_dataset$Agecat1 = cut( test_dataset$Age, c( 0, 15, 25, 33, 47, 80 ))
# test_dataset$HasCabin = as.factor(!is.na(test_dataset$Cabin))
# test_dataset$FarePPcat1 = cut( test_dataset$FarePerPerson, c( -1E-10, 40, 80, 513 ))
# 
# test_dataset = titleFromName( test_dataset )
# dataset = titleFromName( dataset )
# 
# dataset = splitTicket( dataset )
# test_dataset = splitTicket( test_dataset )
# spineplot( dataset$TktPre, dataset$Survived )
# spineplot( test_dataset$TktPre, test_dataset$Pclass )
# 
# dataset = splitCabin( dataset )
# test_dataset = splitCabin( test_dataset )
# 
# newvec = c( levels(dataset$TktPre), levels( test_dataset$TktPre) )
# newlevels = unique(newvec)
# 
# dataset$TktPre <- factor( dataset$TktPre, levels = newlevels )
# test_dataset$TktPre <- factor( test_dataset$TktPre, levels = newlevels )
# 
# spineplot( dataset$FarePPcat1, dataset$Survived )
# spineplot( test_dataset$FarePPcat1, test_dataset$Pclass )
# spineplot( dataset$CabinLevel, dataset$Survived )
# spineplot( test_dataset$CabinLevel, test_dataset$Pclass )
# g = ggplot(test_dataset, aes(x=CabinRoom))
# g + geom_density(aes(color=Pclass)) + facet_wrap(~CabinLevel)

features = c( "Survived", "Pclass", "Sex", "FamilyNum", "Title", "Agecat1", "TktPre", "HasCabin")

datasetToTrain = subset( dataset, select = features )
datasetToTest = subset( test_dataset, select = features )

# set.seed( 1 )
# split = sample.split( datasetToTrain$Survived, SplitRatio = 0.8 )
# train_set = subset( datasetToTrain, split == TRUE )
# test_set = subset( datasetToTrain, split == FALSE )
# 
# 
# train_set_select = train_set
# test_set_select = test_set

# train_set[, 6] = scale( train_set[, 6] )
# test_set[, 6] = scale( test_set[, 6] )
# 
# train_set_select = subset( train_set, select = c( "Survived", "Pclass", "Sex", "Age",
#                                                   "SibSp", "Parch") )
# test_set_select = subset( test_set, select = c( "Survived", "Pclass", "Sex", "Age",
#                                                 "SibSp", "Parch") )

# NN = neuralnet( Survived ~ Pclass + Sex + Age + SibSp + Parch, train_set_select, 
#                 hidden = 10 , linear.output = T )
# 
# plot( NN )

# library( "h2o" )
# h2o.init(nthreads = -1)
# 
# model = h2o.deeplearning( y = "Survived",
#                          training_frame = as.h2o(train_set_select),
#                          activation = 'Rectifier',
#                          hidden = c(5,5),
#                          epochs = 100,
#                          train_samples_per_iteration = -2)
# 
# # Predicting the Test set results
# y_pred = h2o.predict( model, newdata = as.h2o(test_set_select[ -1 ]))
# y_pred = (y_pred > 0.5)
# y_pred = as.vector(y_pred)
# 
# # Making the Confusion Matrix
# cm = table(test_set_select[, 1 ], y_pred)

#library( "e1071" )
#classifier = svm( formula = Survived ~.,
#                  data = train_set_select,
#                  type = 'C-classification',
#                  kernel = 'radial' )

#y_pred_train = predict( classifier, newdata = train_set_select[-1])
#y_pred_test = predict( classifier, newdata = test_set_select[-1])

# svmLinearGrid = train_CV_grid( train_set_select, "svmLinear" )

svmRadialGridFull = train_CV_grid( datasetToTrain, "svmRadial" )
svmRadialGridFull["results"]
svmRadialGridFull["finalModel"]
svmRadialFull = train_svmRadial( datasetToTrain, 1, 0.2 )


trctrl = trainControl( method = "repeatedcv", number = 10, repeats = 3 )
grid = expand.grid( .mtry = 1:2 )
set.seed(123)
gridsearch = train( Survived ~., data = datasetToTrain, method = "rf",
                    trControl=trctrl,
                    tuneLength = 10 )


modellist <- list()
grid = expand.grid( .mtry = 2 )
for (ntree in c( 20, 50, 75, 100 )) {
  set.seed(123)
  fit <- train(Survived~., data=datasetToTrain, method="rf", tuneGrid=grid, trControl=trctrl, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
rfModel = train(Survived~., data=datasetToTrain, method="rf", mtry = 3, ntree=1000)

library(randomForest)
set.seed(seed)
bestmtry <- tuneRF( datasetToTrain[ ,-1 ], datasetToTrain[ , 1 ], stepFactor=1.5, ntreeTry=50,
                    improve = 0.05 )
print(bestmtry)

# rfModel <- randomForest(Survived ~ ., data=datasetToTrain, importance=TRUE, ntree=50,
#                                  mtry = 2 )
rfModel <- randomForest(Survived ~ ., data=datasetToTrain, importance=TRUE, mtry = 3, 
                        nodesize = 20, ntree = 50 )
varImpPlot(rfModel)
plot( rfModel )

# svmRadialGrid = train_CV_grid( train_set_select, "svmRadial" )
# 
# svmRadial_pred_train = predict( svmRadialGrid, newdata = train_set_select[-1])
# svmRadial_pred_test = predict( svmRadialGrid, newdata = test_set_select[-1])
# 
# confusionMatrix( test_set_select$Survived, svmRadial_pred_test )
# 
# svmRadial_final_pred = predict( svmRadialGrid, newdata = datasetToTest )
svmRadialFull_final_pred = predict( svmRadialFull, newdata = datasetToTest )

levels(datasetToTest$Title) = levels( datasetToTrain$Title)
rfModel_final_pred = predict( rfModel, newdata = datasetToTest )

# outDF = data.frame( test_dataset$PassengerId, svmRadial_final_pred)
# write.table( outDF, file = "test_svmRadial.csv", col.names = c("PassengerId","Survived"), 
#              row.names=FALSE, sep = ",")

outDF = data.frame( test_dataset$PassengerId, svmRadialFull_final_pred)
write.table( outDF, file = "test_svmRadialFull_c1_sp2_SPSFTATktpre.csv", col.names = c("PassengerId","Survived"), 
             row.names=FALSE, sep = ",")

outDF = data.frame( test_dataset$PassengerId, rfModel_final_pred)
write.table( outDF, file = "test_rf_mtry3_nt200_nodesz20.csv", col.names = c("PassengerId","Survived"), 
             row.names=FALSE, sep = ",")
#random.forest.importance(Survived~.,dataset)

library(rpart)
treeModel = rpart( Survived~.,data=train_set_select)

treePredictions = predict(treeModel, test_set_select, type = "class")
head(treePredictions)
treeComparison = test_set_select
treeComparison$Predictions = treePredictions
treeComparison[,c("Survived","Predictions")]

disagreement.index = treeComparison$Survived != treeComparison$Predictions
treeComparison[disagreement.index,]
100 * ( 1 - nrow(treeComparison[disagreement.index,]) / nrow(test_set_select) )

treePredictionsFinal = predict(treeModel, datasetToTest, type = "class")
outDF = data.frame( test_dataset$PassengerId, treePredictionsFinal )
write.table( outDF, file = "test_dTree.csv", col.names = c("PassengerId","Survived"), 
             row.names=FALSE, sep = ",")
