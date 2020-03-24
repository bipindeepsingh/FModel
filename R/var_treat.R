#' @title Remove multicollinearity from data
#'
#' @description Returns list of variables removed due to multicolleniarity and treated data frame
#' @param data data frame where 1st column is identifier(key) and 2nd column is target variable should be named flag, data should not have missing values and categorical variables should be one hot encoded
#' @param vif_lim maximum acceptable vif value for treating multicollinearity, default set to 10
#' @return data frame (rm_frame) with list of variables removed and data frame (data_treat) containing treated data
#' @export


var_treat<-function(data,vif_lim=10) {


  #########################SD Check########################

  sd_frame<-data.frame()
  j<-0
  for(i in colnames(data[,-c(1,2)])){
    j<-j+1
    sd_frame[j,"var"]<-i
    sd_frame[j,"sd"]<-stats::sd(data[,i],na.rm = TRUE)
  }
  sd_frame<-sd_frame[sd_frame$sd==0,]

  rm_frame<-data.frame()
  j<-length(rownames(rm_frame))
  for(i in sd_frame[,1]){
    j<-j+1
    rm_frame[j,'var']<-i
    rm_frame[j,'step']<-'sd_null'
    data[,i]<-NULL
  }


  ####################VIF Check########################

  al<-stats::alias(linm<-stats::lm(formula = flag ~.,data =data[,-c(1)]))
  al1<-as.data.frame(al$Complete)

  j<-length(rownames(rm_frame))
  for(i in rownames(al1)){
    j<-j+1
    rm_frame[j,'var']<-i
    rm_frame[j,'step']<-'alias'
    data[,i]<-NULL
  }

  linm<-stats::lm(formula = flag ~.,data =data[,-c(1)])
  vif<-data.frame(car::vif(linm))
  vif$col_name<-rownames(vif)
  rownames(vif)<-c(1:length(rownames(vif)))
  names(vif)<-c("vif","var")
  vif<-vif[,c(2,1)]
  vif<-vif[order(-vif$vif),]

  j<-length(rownames(rm_frame))
  while (vif[1,2]>=vif_lim) {
    j<-j+1
    var<-vif[1,1]
    rm_frame[j,'var']<-vif[1,1]
    rm_frame[j,'step']<-paste0('VIF: ',vif[1,2])
    data[,var]<-NULL
    linm<-stats::lm(formula = flag ~.,data =data[,-c(1)])
    vif<-data.frame(car::vif(linm))
    vif$col_name<-rownames(vif)
    rownames(vif)<-c(1:length(rownames(vif)))
    names(vif)<-c("vif","var")
    vif<-vif[,c(2,1)]
    vif<-vif[order(-vif$vif),]
  }


result<-list(data,rm_frame)
return(result)
}
