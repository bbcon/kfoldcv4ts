#' accuracy.kfold
#'
#' This function it splits the data in k training and test datasets and returns measures of fit for each fold.
#'
#' @param df.VAR data.frame : data of the VAR (without dates)
#' @param k numeric : number of fold cross validations
#' @param n_ahead numeric :horizon of forecast
#' @param lags numeric : lags of the VAR
#' @param var_index numeric : index of the variable to forecast
#'
#' @return a list with fit measures for each fold
#' @export accuracy.kfold(df.VAR=df.VAR, k=5,n_ahead, = 6lags = 2,var_index = 1)
#'
#' @examples
accuracy.kfold = function(df.VAR, k=5,n_ahead,lags,var_index = 1){

  # Setup
  bigT = dim(df.VAR)[1]
  tmp = floor(bigT/k)
  sel_vec = cumsum(rep(tmp,k))-n_ahead

  # Create dataframes for test and for training (k of each)
  list.df_training = list()
  list.df_test = list()
  for(i in 1:k){
    list.df_training[[i]] = df.VAR[1:sel_vec[i],]
    list.df_test[[i]] = df.VAR[(sel_vec[i]+1):(sel_vec[i]+n_ahead),]
  }

  # Get the results from a VAR on the training datasets
  list.VAR = list()
  list.predict = list()
  list.fitted = list()
  for(i in 1:k){
    list.VAR[[i]] = vars::VAR(list.df_training[[i]], p = lags)
    list.predict[[i]] = stats::predict(list.VAR[[i]], n.ahead = n_ahead)
    list.fitted[[i]] = stats::fitted(list.VAR[[i]])
  }

  # Create an object of class = 'forecast' to use forecast::accuracy()
  list.fcst_object = list()
  for(i in 1:k){
    list.fcst_object[[i]] = structure(list(
      mean = list.predict[[i]]$fcst[[var_index]][,"fcst"],
      x = list.df_training[[i]][,var_index],
      fitted = c(rep(NA,lags), list.fitted[[i]][,var_index])
    ), class = 'forecast')
  }

  # Compute accuracy for each k-fold
  list.fcst_accuracy = list()
  for(i in 1:k){
    list.fcst_accuracy[[i]] = forecast::accuracy(list.fcst_object[[i]], list.df_test[[i]][,var_index])
  }
  return(list.fcst_accuracy)
}
