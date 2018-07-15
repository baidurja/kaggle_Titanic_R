source( './preProcess/preProcess.R' )
source( './model/model.R' )

library( "DataExplorer" )
library( "caTools" )

useANN = 1

if ( useANN )
{
  library( "h2o" )  
}


dataset = readRawData( '../data/train.csv' )

#plot_missing( dataset )
#plot_bar( dataset )
#plot_histogram( dataset )

dataset = assignCategorical( dataset )
dataset = handleMissingVal( dataset )
dataset = labelCategorical( dataset )

set.seed( 123 )
split = sample.split( dataset$Survived, SplitRatio = 0.8 )
train_set = subset( dataset, split == TRUE )
test_set = subset( dataset, split == FALSE )

train_set[, 6] = scale( train_set[, 6] )
test_set[, 6] = scale( test_set[, 6] )

train_set_select = subset( train_set, select = c( "Survived", "Pclass", "Sex", "Age",
                                                  "SibSp", "Parch") )
test_set_select = subset( test_set, select = c( "Survived", "Pclass", "Sex", "Age",
                                                "SibSp", "Parch") )

#library( "e1071" )
#classifier = svm( formula = Survived ~.,
#                  data = train_set_select,
#                  type = 'C-classification',
#                  kernel = 'radial' )

#y_pred_train = predict( classifier, newdata = train_set_select[-1])
#y_pred_test = predict( classifier, newdata = test_set_select[-1])

svmLinearGrid = train_CV_grid( train_set_select, "svmLinear" )
svmRadialGrid = train_CV_grid( train_set_select, "svmRadial" )

svmRadial_pred_train = predict( svmRadialGrid, newdata = train_set_select[-1])
svmRadial_pred_test = predict( svmRadialGrid, newdata = test_set_select[-1])

confusionMatrix( test_set_select$Survived, svmRadial_pred_test )


#random.forest.importance(Survived~.,dataset)