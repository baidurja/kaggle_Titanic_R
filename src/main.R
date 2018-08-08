source( './preProcess/preProcess.R' )
source( './model/model.R' )

train = readRawData( '../data/train.csv' )
test = readRawData( '../data/test.csv' )
testOrg = test

# In order to combine the two datasets together,
#    both test and train sets need to have the
#    same columns.
# Give the test set a survived column
test$Survived = NA

# Combine the two datasets into one
merged = rbind( train, test )

merged$FamilyNum = merged$SibSp + merged$Parch + 1

merged$MissingAge = ifelse( is.na( merged$Age), "Y", "N" )

merged$Embarked = replaceMissingValwithMode( merged$Embarked )

merged = titleFromName( merged )

merged$FarePerPerson = merged$Fare / merged$FamilyNum
merged$FarePerPerson = replaceMissingValWithMean( merged$FarePerPerson )




# merged$HasCabin = as.factor(!is.na(merged$Cabin))
# merged$FarePPcat1 = cut( merged$FarePerPerson, c( -1E-10, 40, 80, 513 ))
# merged = splitTicket( merged )
# merged$TktNum = replaceMissingValWithMean( merged$TktNum )
# merged = splitCabin( merged )

merged$Survived = as.factor( merged$Survived )

# merged$FamilyNum = as.factor( merged$FamilyNum )
merged$Pclass = as.factor( merged$Pclass )
merged$Embarked = as.factor( merged$Embarked )
merged$Sex = as.factor( merged$Sex )
merged$MissingAge = as.factor( merged$MissingAge )

featuresForImpute = c( "Survived", "Pclass", "Sex", "FamilyNum", "Age", "FarePerPerson", "SibSp", "Parch", "Title" )

mergedForImpute = merged[ , features ]

#=================================================================
# Impute Missing Ages
#=================================================================

# Caret supports a number of mechanism for imputing (i.e., 
# predicting) missing values. Leverage bagged decision trees
# to impute missing values for the Age feature.

# First, transform all feature to dummy variables.
dummy.vars = dummyVars( ~., data = mergedForImpute[ , -1 ] )
mergedDummy = predict( dummy.vars, mergedForImpute[ , - 1 ] )

# Now, impute!
pp = preProcess( mergedDummy, method = "bagImpute" )
mergedDummyImputed = predict( pp, mergedDummy )
View( mergedDummyImputed )

merged$Age = mergedDummyImputed[ , 7 ]
View( merged )


# merged$Agecat1 = cut( merged$Age, c( 0, 15, 25, 33, 47, 80 ))

##############################
# Separate datasets back out #
##############################

train = merged[ !is.na( merged$Survived ), ]
test = merged[ is.na( merged$Survived ), ]

features = c( "Survived", "Pclass", "Sex", "FamilyNum", "Age", "FarePerPerson", "SibSp", "Parch", "Title" )

datasetToTrain = subset( train, select = features )
datasetToTest = subset( test, select = features )

#Apply model
#=================================================================
# Train Model
#=================================================================

# Set up caret to perform 10-fold cross validation repeated 3 
# times and to use a grid search for optimal model hyperparameter
# values.
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid")


# Leverage a grid search of hyperparameters for xgboost. See 
# the following presentation for more information:
# https://www.slideshare.net/odsc/owen-zhangopen-sourcetoolsanddscompetitions1
tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)
View(tune.grid)

cl <- makeCluster( 4, type = "SOCK")

# Register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)

# Train the xgboost model using 10-fold CV repeated 3 times 
# and a hyperparameter grid search to train the optimal model.
xgbModel <- train( Survived ~ ., 
                   data = datasetToTrain,
                   method = "xgbTree",
                   tuneGrid = tune.grid,
                   trControl = train.control)
stopCluster(cl)

plot( xgbModel )

xgbPred = predict( xgbModel, datasetToTest )

outDF = data.frame( testOrg$PassengerId, xgbPred )
write.table( outDF, file = "test_xgb.csv", col.names = c("PassengerId","Survived"), 
             row.names=FALSE, sep = ",")
