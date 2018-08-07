library( "caret" )

train_CV_grid = function ( train_set, modelName )
{
  trctrl = trainControl( method = "repeatedcv", number = 10, repeats = 3 )
  
  if ( modelName == "svmLinear" )
  {
    grid = expand.grid( C = c( 0.01, 0.05, 0.1, 0.25, 0.5, 1, 1.5, 2, 5 ) )
    set.seed(123)
    gridsearch = train( Survived ~., data = train_set, method = "svmLinear",
                             trControl=trctrl,
                             tuneGrid = grid,
                             tuneLength = 10 )
    plot( gridsearch )
  }
  else if ( modelName == "svmRadial" )
  {
    grid = expand.grid( C = c( 0.5, 0.7, 1, 3, 5 ),
                        sigma = c( 1/3, 1/4, 1/5, 1/7.5, 1/10, 1/15, 1/20 ) )
    set.seed(123)
    gridsearch = train( Survived ~., data = train_set, method = "svmRadial",
                           trControl=trctrl,
                           tuneGrid = grid )
    plot( gridsearch )
  }
  else if ( modelName == "randomForest" )
  {
    grid = expand.grid( .mtry = c( 2, 4, 6, 8, 10, 16, 20, 24 ) )
                        
    set.seed(123)
    gridsearch = train( Survived ~., data = train_set, method = "rf",
                        trControl=trctrl,
                        tuneGrid = grid,
                        tuneLength = 10 )
    plot( gridsearch )
  }
  else
  {
    print("Error: model not found!")
  }
  
  return( gridsearch )
}

train_svmRadial = function ( train_set, CIn = 1, sigmaIn = 0.1 )
{
  trctrl = trainControl( method = "none" )
  print(CIn)
  print(sigmaIn)
  
  grid = expand.grid( C = CIn, sigma = sigmaIn )
  set.seed(123)
  gridsearch = train( Survived ~., data = train_set, method = "svmRadial",
                        trControl=trctrl, tuneGrid = grid )
  
  return( gridsearch )
}