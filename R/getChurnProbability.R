#Create a function that takes as input the dataset and a customer id and
#returns the churn probability for that customer id only.

getChurnProbability<- function(data, CustId){
  if(any(data[,CustomerId]==CustId)){
    churn_prob_model <- glm(data$Exited ~ data$CreditScore + data$Gender + data$Age + data$Age + data$Tenure + data$Balance + data$NumOfProducts + data$HasCrCard + data$IsActiveMember + data$EstimatedSalary, family="binomial")
    churn_pred <- predict(churn_prob_model, data, type="response")
    data <- cbind(data, churn_pred)
    return(data[CustomerId==CustId,churn_pred])
  } else {
    stop("CustomerID does not exist")
  }


}


