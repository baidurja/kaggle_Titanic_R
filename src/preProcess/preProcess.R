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
  
  
  return( dataset )
}

handleMissingVal = function( dataset )
{
  dataset$Age = replaceMissingValWithMean( dataset$Age )
  dataset$Embarked = replaceMissingValwithMode( dataset$Embarked )
  dataset$Fare = replaceMissingValWithMean( dataset$Fare )
  
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

changeToCategorical = function( datacol )
{
   datacol = as.factor( datacol )
   return ( datacol )
}

titleFromName = function( dataset )
{
  dataset$Title = gsub('(.*, )|(\\..*)', '', dataset$Name)
  # VIP = c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major",
  #          "Mlle", "Mme","Rev","Sir","the Countess")
  VIP = c( "Don","Dona","Jonkheer","Rev","Lady","Sir","the Countess")
  Military = c( "Capt","Col","Major" )
  dataset$Title[ dataset$Title %in% VIP] = "VIP"
  dataset$Title[ dataset$Title %in% Military] = "Military"
  dataset$Title = as.factor( dataset$Title )
  if (dim(dataset[ dataset$Title=="Ms",])[1] != 0)
  {
    dataset[ dataset$Title=="Ms",]$Title = "Mrs"
  }
  if (dim(dataset[ dataset$Title=="Mme",])[1] != 0)
  {
    dataset[ dataset$Title=="Mme",]$Title = "Mrs"
  }
  if (dim(dataset[ dataset$Title=="Mlle",])[1] != 0)
  {
    dataset[ dataset$Title=="Mlle",]$Title = "Mrs"
  }
  
  return (dataset)
}

splitTicket = function( dataset )
{
  library(stringi)
  library(stringr)
  dataset$TktPre <- NA
  dataset$TktNum <- NA
  # Get the raw TktPre (or NA), TktNum
  dataset[,c('TktPre','TktNum')] <- stri_match(dataset$Ticket, regex='(.* )?([0-9]+)' ) [,-1]
  # Postprocess punctuation out of TktPre
  dataset$TktPre <- str_replace_all(str_to_upper(dataset$TktPre), '[./ ]', '') # you can also include ' ' and '/' in the '[...]' regex to be removed
  dataset$TktPre <- ifelse(is.na(dataset$TktPre), "None", dataset$TktPre)
  dataset$TktPre = as.factor(dataset$TktPre)
  dataset$TktNum = as.numeric(dataset$TktNum)
  
  return (dataset)
}

splitCabin = function( dataset )
{
  CabinLevels <- c("NoCabin", "A","B","C","D","E", "F", "G", "T")
  CabinLevel <- gsub('^([A-G]).*$', '\\1', dataset$Cabin)
  dataset$CabinLevel <- ifelse(is.na(CabinLevel), CabinLevels[1], CabinLevel)
  dataset$CabinLevel <- as.factor(dataset$CabinLevel)
  
  CabinRoom <- gsub('^[^0-9]+([1-9]+).*$', '\\1', dataset$Cabin)
  CabinRoom <- ifelse(CabinRoom=="", "0", CabinRoom)
  suppressWarnings(CabinRoom <- as.integer(CabinRoom))
  dataset$CabinRoom <- ifelse(is.na(CabinRoom), 0, CabinRoom)
  
  return (dataset)
}