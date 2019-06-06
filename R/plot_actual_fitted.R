#' Plot Actual Fitted
#'
#' Plots actual and fitted values of the dependent variable in a regression and 95% prediction interval. The horizontal axis is a sequence of integers denoting observations.
#'
#' @param linmod Linear model
#' @param MyData The data
#' @param col_name Name of column variable as a string (use quotes).
#' @export

plotActualFitted = function(linmod, MyData, col_name){
  Y= unlist(MyData[,col_name])
  Observation=seq(1,NROW(MyData))
  Pred_and_PI=stats::predict(linmod,MyData,interval="predict")
  Predicted=Pred_and_PI[,1]
  Lower=Pred_and_PI[,2]
  Upper=Pred_and_PI[,3]
  ymax=max(Upper)
  ymin=min(Lower)
  plot(Y~Observation,ylim=c(ymin,ymax),pch=20,
       main="Actual (black), Predicted (red), and 95% PI (blue)")
  lines(Y~Observation)
  lines(Predicted~Observation,col="red")
  lines(Upper~Observation,col="blue")
  lines(Lower~Observation,col="blue")
}
