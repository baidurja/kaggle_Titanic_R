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
    grid = expand.grid( C = c( 0.01, 0.05, 0.1, 0.25, 0.5, 1, 1.5, 2, 5 ),
                        sigma = c( 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/10, 1/20, 1/50 ) )
    set.seed(123)
    gridsearch = train( Survived ~., data = train_set, method = "svmRadial",
                           trControl=trctrl,
                           tuneGrid = grid,
                           tuneLength = 10 )
    plot( gridsearch )
  }
  
  return( gridsearch )
}