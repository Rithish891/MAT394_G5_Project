

library(tidyverse) 
library(corrplot)
library(plyr)
library(pROC)
library(caTools)
library(cowplot)
library(MASS)
library(e1071)
library(car)
library(caret)
library(readr)
library(ROCR)
library(rms)
library(gplots)
library(rpart)
library(ggpubr)
library(randomForest)



# DATA PREPARATION AND READING CSV DATA FILE


  telco <- read.csv("C:/Users/Gautam Varma/Documents/R/WA_Fn-UseC_-Telco-Customer-Churn.csv")
  glimpse(telco)

  options(repr.plot.width = 10, repr.plot.height = 10)
  na_values <- telco %>% summarise_all(funs(sum(is.na(.))/n()))
  na_values <- gather(na_values, key = "variables", value = "numberofvalues_missing")
  ggplot(na_values, aes(x = reorder(variables, numberofvalues_missing), y = numberofvalues_missing)) +
  geom_bar(stat = "identity", fill = "green", aes(color = I('white')), size = 0.3)+ xlab('Features')+ 
  theme_bw()

telco <- telco[complete.cases(telco),] 


# EDA ( EXPLORATORY DATA ANALYSIS)


  #  Correlation 

    telco %>%
    dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
    cor() %>%
    corrplot.mixed(upper = "circle", tl.col = "blue", number.cex = 0.5)
    
    


# CATEGORICAL FEATURES AND THEIR RELATION WRT CHURN

plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()+scale_fill_manual(values = c("Green","tomato")), 
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("Green","tomato")),
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("Green","tomato")),
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("Green","tomato")),
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("Green","tomato")),
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("Green","tomato")))



plot_grid(ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("blue","pink")), 
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("blue","pink")),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("blue","pink")),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("blue","pink")),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("blue","pink")),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("blue","pink")))



plot_grid(ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("grey","black")), 
          ggplot(telco, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("grey","black")),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("grey","black")),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')+scale_fill_manual(values = c("grey","black")))


# NUMERICAL FEATURES AND THEIR RELATION WRT CHURN


  ggplot(telco, aes(y= MonthlyCharges, x = "", fill = Churn)) + geom_boxplot()+ 
  theme_bw()+ geom_jitter(color="black" , size=0.4 , alpha=0.9)
  xlab(" ")

  ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) + geom_boxplot()+ 
  theme_bw()+ geom_jitter(color="black" , size=0.4 , alpha=0.9)
  xlab(" ")

  ggplot(telco, aes(y= tenure, x = "", fill = Churn)) + geom_boxplot()+ 
  theme_bw()+ geom_jitter(color="black" , size=0.4 , alpha=0.9)
  xlab(" ")


#  DENSITY PLOT WRT CHURN FOR NUMERICAL FEATURES:

  mu <- ddply(telco, "Churn", summarise, grp.mean=mean(MonthlyCharges))
  ggplot(telco , aes(x=MonthlyCharges, color=Churn)) +
  geom_density(size=1)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Churn),
             linetype="dashed")


  kaka <- ddply(telco, "Churn", summarise, grp.mean=mean(TotalCharges))
  ggplot(telco , aes(x=TotalCharges, color=Churn)) +
  geom_density(size=1)+
  geom_vline(data=kaka, aes(xintercept=grp.mean, color=Churn),
             linetype="dashed")

        
  tata <- ddply(telco, "Churn", summarise, grp.mean=mean(tenure))
  ggplot(telco , aes(x=tenure, color=Churn)) +
  geom_density(size=1)+
  geom_vline(data=tata, aes(xintercept=grp.mean, color=Churn),
             linetype="dashed")


  
# CHECKING FOR OUTLIERS uSING BOXPLOT 
  
  options(repr.plot.width =4, repr.plot.height = 4)
  boxplot(telco$tenure)$out
  boxplot(telco$Monthlycharges)$out
  boxplot(telco$Totalcharges)$out


# CLEANING THE CATEGORICAL DATA 

  telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

  telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))


  telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'YES', 'NO'))



# CONTINUOUS i.e NUMERICAL FEATURES
  
  cont_features <- c("tenure", "MonthlyCharges", "TotalCharges")
  telco[cont_features] <- sapply(telco[cont_features], as.numeric)
  telco_integer <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
  telco_integer <- data.frame(scale(telco_integer))




  telco <- mutate(telco, tenure_bin = tenure)

  telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
  telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
  telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
  telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
  telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
  telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

  telco$tenure_bin <- as.factor(telco$tenure_bin)
  ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar() + scale_fill_manual(values = c("violet","green","pink","orange","red","black"))


  # CREATE DUMMY DATA SET AND FINNALY DIVIDE THE DATA INTO TEST AND TRAIN DATA


  telco_edit<- telco[,-c(1,6,19,20)]
  dummy<- data.frame(sapply(telco_edit,function(x) data.frame(model.matrix(~x-1,data =telco_edit))[,-1]))
  head(dummy)
  telco_final <- cbind(telco_integer,dummy)
  head(telco_final)

  set.seed(250)
  index = sample.split(telco_final$Churn, SplitRatio = 0.75)
  train = telco_final[index,]
  validation = telco_final[!(index),]



# MODEL1 LOGISTIC REGRESSION:



    model_1 = glm(Churn ~ ., data = train, family = "binomial")
    summary(model_1)

# AIC

    model_2 <- stepAIC(model_1, direction="both")
    summary(model_2)
# VIF

    model_3 <-glm(formula = Churn ~ tenure + MonthlyCharges  + 
                Partner + InternetService.xFiber.optic + InternetService.xNo + 
                + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                tenure_bin.x5.6.years, family = "binomial", data = train)
    summary(model_3)
    vif(model_3)


    
    model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + 
                 Partner + InternetService.xFiber.optic + InternetService.xNo + 
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                 tenure_bin.x5.6.years, family = "binomial", data = train)

    summary(model_4)
    vif(model_4)
    model_final <- model_4
# Model_final is our final model obtained from lOGISTIC REGRESSION


# Model Evaluation using the validation data: (Confusion matrix and ROC):

    predict(model_final, data = train, type = "response") -> train_probability
    predict(model_final, newdata = validation, type = "response") -> test_probability


    train_predict <- factor(ifelse(train_probability >= 0.5, "Yes", "No"))
    train_actual <- factor(ifelse(train$Churn == 1, "Yes", "No"))
    test_predict <- factor(ifelse(test_probability >= 0.5, "Yes", "No"))
    test_actual <- factor(ifelse(validation$Churn == 1, "Yes", "No"))


    confusionMatrix(data = train_predict, reference = train_actual)
    roc <- roc(train$Churn, train_probability, plot= TRUE, print.auc=TRUE)

    confusionMatrix(data = test_predict, reference = test_actual)
    roc <- roc(validation$Churn, test_probability, plot= TRUE, print.auc=TRUE)


#Find the optimal cutoff and adjust the class of prediction:


  pred <- prediction(train_probability, train_actual)
  perf <- performance(pred, "specificity", "sensitivity")

  alpha <- data.frame(cut=perf@alpha.values[[1]], specificity=perf@x.values[[1]], 
                      sensitivity= perf@y.values[[1]])

  optimal_cutoff <- alpha[which.min(abs(alpha$specificity-alpha$sensitivity)),]

  ggplot(data=alpha) +
  geom_line(aes(x = cut, y = specificity, color ="orange"), size = 1.25)+
  geom_line(aes(x = cut, y = sensitivity, color = "black"), size = 1.25) +
  labs(x = "cutoff", y ="value") +
  scale_color_discrete(name = "", labels = c("Specificity", "Sensitivity"))+
  geom_vline(aes(xintercept = optimal_cutoff$cut))+
  geom_text(aes(x= 0.5, y= 0.75),label="optimal_cutoff = 0.3",hjust=1.5, size=3)


    train_pred_c <- factor(ifelse(train_probability >= 0.3, "Yes", "No"))
    confusionMatrix(data = train_pred_c, reference = train_actual)


    predict(model_logit, newdata = test, type = "response") -> test_prob
    test_pred_c <- factor(ifelse(test_probability >= 0.3, "Yes", "No"))
    confusionMatrix(data = test_pred_c, reference = test_actual)



# DECISION TREES:

#Remember that Totalcharges, MonthlyCharges and tenure are highly correlated, which may effect the performance of the 
#decision tree models. So we remove the TotalCharges column then train the model.

  telcotree <- telco
  telcotree$customerID <- NULL
  telcotree %>%
  mutate_if(is.character, as.factor) -> telcotree
  str(telcotree)

set.seed(500)
tree <- sample(0:1, size= nrow(telcotree), prob = c(0.5,0.5), replace = TRUE)
traindata <- telcotree[tree == 0, ]
testdata <- telcotree[tree == 1, ]

dt <- rpart(formula = Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + 
                       MultipleLines + InternetService + OnlineSecurity + TechSupport +
                       OnlineBackup + DeviceProtection + StreamingTV + StreamingMovies + 
                       Contract + PaperlessBilling + tenure +
                       PaymentMethod + MonthlyCharges, data = traindata, 
                     method = "class", parms = list(split = "gini"))


    predict(dt, data = traindata, type = "class") -> traintree_pred1
    predict(dt, data = traindata, type = "prob") -> traintree_prob1
    predict(dt, newdata= testdata, type = "class") -> testtree_pred1
    predict(dt, newdata = testdata, type = "prob") -> testtree_prob1

    confusionMatrix(data = traintree_pred1, reference = traindata$Churn)
    traintree_actual <- ifelse(traindata$Churn == "Yes", 1,0)
    roc <- roc(traintree_actual, traintree_prob1[,2], plot= TRUE, print.auc=TRUE)

    testtree_actual <- ifelse(testdata$Churn == "Yes", 1,0)
    confusionMatrix(data = testtree_pred1, reference = testdata$Churn)
    roc <- roc(testtree_actual, testtree_prob1[,2], plot = TRUE, print.auc = TRUE)


# RANDOM FOREST:
    
  
#It is an ensemble of a large number of Decision Trees, that uses bootstrapped 
#aggregation technique to choose random samples from a dataset to train each tree
    
    
    
    library(randomForest)
    set.seed(123)
    telco_final$Churn <- as.factor(telco_final$Churn)
    
    indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
    train = telco_final[indices,]
    validation = telco_final[!(indices),]
    head()
    
    model.rf <- randomForest(Churn ~ ., data=train, proximity=FALSE,importance = FALSE,
                             ntree=500,mtry=4, do.trace=FALSE)
    
     
    testPred <- predict(model.rf, newdata=validation)
    table(testPred, validation$Churn)
    
    confusionMatrix(validation$Churn, testPred)
    
    varImpPlot(model.rf)

    
    
    predict(model.rf, data = train, type = "class") -> traintree_pred1
    predict(model.rf, data = validation, type = "prob") -> traintree_prob1
    predict(model.rf, newdata= validation, type = "class") -> testtree_pred1
    predict(model.rf, newdata = train, type = "prob") -> testtree_prob1
    
    confusionMatrix(data = traintree_pred1, reference = train$Churn)
   
    roc <- roc(train$Churn, traintree_prob1[,2], plot= TRUE, print.auc=TRUE)

    
   




