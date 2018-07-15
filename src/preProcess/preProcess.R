readRawData = function( fnm )
{
  dataset = read.csv( fnm, na.strings = c( "", " " ) )
  return( dataset )
}

assignCategorical = function( dataset )
{
  dataset$Pclass = changeToCategorical( dataset$Pclass )
  dataset$SibSp = changeToCategorical( dataset$SibSp )
  dataset$Parch = changeToCategorical( dataset$Parch )
  dataset$Survived = changeToCategorical( dataset$Survived )
  
  return( dataset )
}

handleMissingVal = function( dataset )
{
  dataset$Age = replaceMissingValWithMean( dataset$Age )
  dataset$Embarked = replaceMissingValwithMode( dataset$Embarked )
  
  return ( dataset )
}

labelCategorical = function( dataset )
{
  dataset$Sex = factor( dataset$Sex,
                        levels = c("male", "female"),
                        labels = c(0, 1) )
  dataset$Embarked = factor( dataset$Embarked,
                             levels = c("S", "C", "Q"),
                             labels = c(0, 1, 2))
  
  return ( dataset )
}

labelCategoricalAsNumeric = function( dataset )
{
  dataset$Sex = as.numeric( factor( dataset$Sex,
                        levels = c("male", "female"),
                        labels = c(0, 1) ) )
  dataset$Embarked = as.numeric( factor( dataset$Embarked,
                             levels = c("S", "C", "Q"),
                             labels = c(0, 1, 2) ) )
  
  return ( dataset )
}

#From https://stackoverflow.com/questions/50079695/replace-missing-with-mode-for-factor-column-and-mean-for-numeric-column-in-r
Mode = function( x ) 
{
  ux = unique( x )
  ux[ which.max( tabulate( match( x, ux ) ) ) ]
}

replaceMissingValwithMode = function( datacol )
{
  datacol = replace( datacol, is.na( datacol ), Mode( na.omit( datacol ) ) )
  return ( datacol )
}


replaceMissingValWithMean = function( datacol )
{
  datacol = ifelse( is.na( datacol ),
                    ave( datacol, FUN = function( x )mean( x, na.rm = TRUE ) ),
                    datacol )
  return ( datacol )
  
}

changeToCategorical = function( datacol, asNumeric )
{
   datacol = as.factor( datacol )
   return ( datacol )
}