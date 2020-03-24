#' @title Run logistic regression with variable selection
#'
#' @description Returns list of variables removed from model due to variable selection, model object for logistic regression,confusiton matrix and comparison of key accuracy parameters between test and control set, 20% of data passed by default is used for testing
#' @param data data frame where 1st column is identifier(key) and 2nd column is target variable should be named flag, data should not have missing values and categorical variables should be one hot encoded
#' @param p_lim maximum acceptable p value for variable in logistic regression default 0.06
#' @param t_sample ratio of data to be used for training
#' @return data frame (rm_frame) with list of variables removed and reasons, model object base.glm, data frame conf_mat contains confusion matrix and data frame comp_frame comparing test and training set
#' @export


m_opt<-function(data,p_lim=0.06,t_sample=0.8){

  #########################Sampling###########################

  set.seed(80085)
  index<-sample(1:nrow(data),size = t_sample*nrow(data))
  train<-data[index,]
  test<-data[-index,]

  ##############################Logistic Model###########################
  base.glm<-stats::glm(flag ~.,family=stats::binomial(link='logit'),data=train[,-c(1)])
  base_sum<-summary(base.glm)
  coeff_base<-data.frame(base_sum$coefficients)
  coeff_base$var<-rownames(coeff_base)
  rownames(coeff_base)<-c(1:length(rownames(coeff_base)))
  coeff_base<-coeff_base[,c(5,1:4)]
  names(coeff_base)<-c('var','estimate','std_err','z_value','p_val')
  coeff_base<-coeff_base[-c(1:1),]
  coeff_base<-coeff_base[order(-coeff_base$p_val),]
  roc_base<-pROC::roc(train$flag,base.glm$fitted.values,plot=FALSE)

  j<-length(rownames(rm_frame))
  while (coeff_base[1,5]>=p_lim) {
    j<-j+1
    var<-coeff_base[1,1]
    train[,var]<-NULL
    rm_frame[j,'var']<-coeff_base[1,1]
    rm_frame[j,'step']<-paste0('p_val: ',coeff_base[1,5])
    rm(coeff_base)
    base.glm <- stats::glm(flag ~.,family=stats::binomial(link='logit'),data=train[,-c(1)])
    base_sum<-summary(base.glm)
    coeff_base<-data.frame(base_sum$coefficients)
    coeff_base$var<-rownames(coeff_base)
    rownames(coeff_base)<-c(1:length(rownames(coeff_base)))
    coeff_base<-coeff_base[,c(5,1:4)]
    names(coeff_base)<-c('var','estimate','std_err','z_value','p_val')
    coeff_base<-coeff_base[-c(1:1),]
    coeff_base<-coeff_base[order(-coeff_base$p_val),]
    roc_base<-pROC::roc(train$flag,base.glm$fitted.values,plot=FALSE)
  }

  train$prob<-base.glm$fitted.values
  baseglm_roc<-data.frame(recall=roc_base$sensitivities,specificity=roc_base$specificities,
                          threshold=roc_base$thresholds)

  baseglm_roc$t_select<-baseglm_roc$recall+baseglm_roc$specificity
  baseglm_roc2<-baseglm_roc[baseglm_roc$t_select==max(baseglm_roc$t_select),]

  base_threshold<-baseglm_roc2$threshold

  train$class<-ifelse(train$prob>=base_threshold,1,0 )

  acc_train<-sum(ifelse(train$class==train$flag,1,0))/length(train$trip_id)

  conf_mat_train<-data.frame(data='Train',
                             tp=sum(ifelse(train$flag==1 & train$class==1,1,0)),
                             fp=sum(ifelse(train$flag==0 & train$class==1,1,0)),
                             tn=sum(ifelse(train$flag==0 & train$class==0,1,0)),
                             fn=sum(ifelse(train$flag==1 & train$class==0,1,0)))

  precision_train<-conf_mat_train$tp/(conf_mat_train$tp+conf_mat_train$fp)

  recall_train<-conf_mat_train$tp/(conf_mat_train$tp+conf_mat_train$fn)

  specificity_train<-conf_mat_train$tn/(conf_mat_train$tn+conf_mat_train$fp)

  test$prob<-stats::predict(base.glm,test,type = 'response')
  test$class<-ifelse(test$prob>=base_threshold,1,0 )
  acc_test<-sum(ifelse(test$class==test$flag,1,0))/length(test$trip_id)
  conf_mat_test<-data.frame(data='Test',
                            tp=sum(ifelse(test$flag==1 & test$class==1,1,0)),
                            fp=sum(ifelse(test$flag==0 & test$class==1,1,0)),
                            tn=sum(ifelse(test$flag==0 & test$class==0,1,0)),
                            fn=sum(ifelse(test$flag==1 & test$class==0,1,0)))
  precision_test<-conf_mat_test$tp/(conf_mat_test$tp+conf_mat_test$fp)
  recall_test<-conf_mat_test$tp/(conf_mat_test$tp+conf_mat_test$fn)
  specificity_test<-conf_mat_test$tn/(conf_mat_test$tn+conf_mat_test$fp)

  conf_mat<-rbind(conf_mat_train,conf_mat_test)

  comp_frame<-data.frame(matrix=c('Acc','precision','recall'),
                         train=c(acc_train,precision_train,recall_train),
                         test=c(acc_test,precision_test,recall_test))

result<-list(base.glm,conf_mat,comp_frame)
return(result)


}
