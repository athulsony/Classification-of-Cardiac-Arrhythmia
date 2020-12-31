library(psych)
library(caTools)
library(corrplot)
library(caret)
library(class)
library(rpart)
library(rpart.plot)
library(forecast)
library(dummies)
library(ggplot2)
library(scales)
library(randomForest)
library(dplyr)
library(MASS)
library(adabag)
library(neuralnet)
library(pROC)
library(e1071)
library(skimr)
library(cluster)
library(Rtsne)
library(readr)

# -------------------------------------------Reading Data------------------------------------------------------------------

setwd("C:/Users/athul/Downloads")

data <- read.csv('data.csv',stringsAsFactors = F,na.strings = c(' ','?',''))

data$class[data$class!= 1] <- 'Heart Disease'
data$class[data$class== 1] <- 'Normal ECG'

#skim(data)


# -----------------------------------Removing zero constant columns--------------------------------------------------------

z_val <- c(20,68,165,205,265,275,152,140,70,84,132,133,142,144,146,157,158)

data <- data[,-z_val]


# ---------------------------------Converting Variables to Categorical---------------------------------------------------------

bin_var <- c(2,grep('waveExists',colnames(data)),263)

for(i in bin_var){
  data[,i] <- factor(data[,i])
}
#--------------------------------

gower_dist <- daisy(data,metric = 'gower')

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 2
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#--------------------------------Finding out columns containing null values and imputing mean--------------------------------------

n_col <- NA

for(i in 1: dim(data)[2]){
  if(any(is.na(data[,i]))){
    n_col <- c(n_col,i)
  }
}

imputer <- function(x){
  x[is.na(x)] <- mean(x,na.rm=T)
  return(x)
}

data[,n_col[-1]] <- apply(data[,n_col[-1]],2,imputer)


#--------------------------------------------------------EDA----------------------------------------------------------------

data1 <- data[,-bin_var]

colnames(data1) <- 1:198

M <- cor(data1)

ggcorrplot(M,hc.order = T,type = 'lower',outline.color = 'white')

par(mfrow=c(1,3))
boxplot(data$age, main="Distribution of Age")
boxplot(data$height, main="Distribution of Height")
boxplot(data$weight, main="Distribution of Weight")

par(mfrow=c(1,3))
boxplot(data$PRinterval, main="Distribution of PR interval")
boxplot(data$Q.Tinterval, main="Distribution of Q.T interval")
boxplot(data$Pinterval, main="Distribution of P interval")

par(mfrow=c(1,3))
boxplot(data$J, main="Distribution of J")
boxplot(data$P, main="Distribution of P")
boxplot(data$T, main="Distribution of T")

par(mfrow = c(1,2))
hist(data$J,main = 'Histogram of J',xlab = 'J')
hist(data$P,main = 'Histogram of P',xlab = 'P')

par(mfrow = c(1,2))
hist(data$T,main = 'Histogram of T',xlab = 'T')
hist(data$heartrate,main = 'Histogram of Heart Rate',xlab = 'Heart Rate')


pairs.panels(data[,c(1,3:6)],gap = 0,bg = c('red','green')[data$class],pch = 21)

pairs.panels(data[,c(7:11)],gap = 0,bg = c('red','green')[data$class],pch = 21)

pairs.panels(data[,c(12:16)],gap = 0,bg = c('red','green')[data$class],pch = 21)

pairs.panels(data[,c(17:20)],gap = 0,bg = c('red','green')[data$class],pch = 21)


data$gender <- ifelse(data$sex==1,'Male','Female')

data %>% 
  ggplot(aes(x = gender))+
  geom_bar(fill='steelblue')+
  xlab('Gender')+
  ylab('Count')+
  ggtitle('Gender Frequency')+
  theme(plot.title = element_text(hjust = 0.5))

data %>% 
  ggplot(aes(x = class))+
  geom_bar(fill='steelblue')+
  xlab('Class')+
  ylab('Count')+
  ggtitle('Response Variable Class Frequency')+
  theme(plot.title = element_text(hjust = 0.5))

x <- data[data$sex==1,c(2,280)] %>% 
  group_by(sex,class) %>% 
  summarise(Count = n())
x$Prop <- round((x$Count/sum(x$Count)) * 100,0)

x <- x %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

x %>% 
  ggplot(aes(x = "", y = Prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Prop), color = "white")+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  theme_void()+
  ggtitle('Percentage of Males having disease')+
  theme(plot.title = element_text(hjust = 0.5))



y <- data[data$sex==0,c(2,280)] %>% 
  group_by(sex,class) %>% 
  summarise(Count = n())
y$Prop <- round((x$Count/sum(x$Count)) * 100,0)

y <- y %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

y %>% 
  ggplot(aes(x = "", y = Prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Prop), color = "white")+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  theme_void()+
  ggtitle('Percentage of Females having disease')+
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------------------------------PCA--------------------------------------------------------------------

data <- data[,-264]

pca <- prcomp(data[,-bin_var],center=T,scale=T)

#fa.parallel(data[,-bin_var],fa='pc',n.iter = 100,show.legend=F,main = 'Scree plot with parallel analysis')

summary(pca)

screeplot(pca, type = "l", npcs = 80, main = "Screeplot of the first 80 PCs",pch = 16,cex = 0.4)
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),col=c("red"), lty=5, cex=0.6)


cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:50], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 41, col="blue", lty=5)
abline(h = 0.86179, col="blue", lty=5)

# Selecting 41 principal components

data_after_pca <- data.frame(data[,bin_var],pca$x[,1:41])


#--------------------------------------------------KNN Algorithm------------------------------------------------------------------


# creating m dummy varaibles 

knn_dum <- data.frame(y = rep(NA,nrow(data_after_pca)))

for(i in colnames(data_after_pca[,1:64])){
  knn_dum <- cbind(knn_dum,dummy(i ,data = data_after_pca))
}

knn_dum <- knn_dum[-1]

data_knn <- data.frame(knn_dum,data_after_pca[,65:106])

# Partitioning the data

set.seed(100)

split_knn <- sample.split(data_knn$class,SplitRatio = 0.7)

train_set_knn <- data_knn[split_knn,]

test_set_knn <- data_knn[!split_knn,]

# Performing Cross Validation

ctrl <- trainControl(method="repeatedcv",repeats = 3,number = 10)

model_knn <- train(class ~ ., data = train_set_knn, method = "knn", trControl = ctrl, tuneLength = 20)

model_knn

# K v/s Accuracy Plot

plot(model_knn)

paste("Best value of k chosen is ",as.character(model_knn$bestTune))

# Applying the model on test data

output_knn <- data.frame(Actual = test_set_knn$class, Pred = predict(model_knn,newdata = test_set_knn),
           Prob = round(predict(model_knn,newdata = test_set_knn,type = 'prob'),2))

output_knn$Actual_Binary <- ifelse(output_knn$Actual == 'Heart Disease',1,0)

# Confusion Matrix

cfm_knn <- confusionMatrix(output_knn$Pred,output_knn$Actual)

cfm_knn

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(m$table) ,aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

ggplotConfusionMatrix(cfm_knn)

# Plotting Lift Chart

prop_knn <- round(table(test_set_knn$class)[1]/(table(test_set_knn$class)[1] + table(test_set_knn$class)[2]),2)

# Plotting Lift Chart

rlift.df.knn <- data.frame(x = c(1:135),
                          y = cumsum(output_knn$Actual_Binary[order(output_knn$Prob.Heart.Disease, decreasing = T)]),
                          bench = c(1:135)*prop_knn)

ggplot(rlift.df.knn, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-KNN") + xlab('Cases') + ylab('Cumulative')

# Plotting ROC and finding AUC

roc_knn <- roc(output_knn$Actual,output_knn$Prob.Heart.Disease)

plot.roc(roc_knn,print.auc = T,grid = T)

#-----------------------------------------------Naive Bayes------------------------------------------------------------------------

data_nb <- data_after_pca

# Binning numerical varaibles to convert them into categorical

for(i in 66:106){
  bin <- seq(min(data_nb[,i]),max(data_nb[,i]),(max(data_nb[,i])-min(data_nb[,i]))/3)
  data_nb[,i]<- cut(data_nb[,i],bin,labels = 1:3,include.lowest=TRUE,right=FALSE)
}

# Partitioning the data into train test

set.seed(100)

split_nb <- sample.split(data_nb$class,SplitRatio = 0.7)

train_set_nb <- data_nb[split_nb,]

test_set_nb <- data_nb[!split_nb,]

model_nb <- naiveBayes(class~.,data = train_set_nb)

model_nb

output_nb <- data.frame(Actual = test_set_nb$class,
                        Actual_Binary = ifelse(test_set_nb$class=='Heart Disease',1,0),
                        Prob = predict(model_nb,newdata = test_set_nb,type='raw')[,1] )

output_nb$Pred <- ifelse(output_nb$Prob >=0.5,'Heart Disease','Normal ECG')

output_nb$Pred <- factor(output_nb$Pred)

# Confusion Matrix

cfm_nb <- confusionMatrix(output_nb$Pred,output_nb$Actual)

cfm_svm

ggplotConfusionMatrix(cfm_nn)

prop_nb <- round(table(test_set_nb$class)[1]/(table(test_set_nb$class)[1] + table(test_set_nb$class)[2]),2)

# Lift Chart

rlift.df.nb <- data.frame(x = c(1:135),
                          y = cumsum(output_nb$Actual_Binary[order(output_nb$Prob, decreasing = T)]),
                          bench = c(1:135)*prop_nb)

ggplot(rlift.df.nb, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-Naive Bayes") + xlab('Cases') + ylab('Cumulative')

# Plotting ROC and determining AUC

roc_nb <- roc(output_nb$Actual,output_nb$Prob)

plot.roc(roc_nb,print.auc = T,grid = T)


#-------------------------------------------Classification Trees------------------------------------------------------------- 

# Partitioning data into train and test

set.seed(100)

split_dt <- sample.split(data_after_pca$class,SplitRatio = 0.7)

train_set_dt <- data_after_pca[split_dt,]

test_set_dt <- data_after_pca[!split_dt,]

# Performing 10 fold cross validation

control_dt <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3)


tGrid_dt <- expand.grid(cp = seq(0, .025, .0001))

model_dt <- train(class~., data = train_set_dt,method = "rpart", metric = "Accuracy", trControl=control_dt, tuneLength = 100)

# Results of 10 fold cross validation

model_dt

# Optimal value of cp

paste('Optimal Value of cp after cross validation is ',as.character(model_dt$bestTune$cp))

# Model built using the best parameters

model_dt_best <- model_dt$finalModel

# Plotting the best pruned tree model

prp(model_dt_best,split.font = 2,type = 1,extra = 2,varlen = -15)

# Applying the model on test data

output_dt <- data.frame(Actual = test_set_dt$class, Pred = predict(model_dt,newdata = test_set_dt),
                     Prob = round(predict(model_dt,newdata = test_set_dt,type = 'prob'),2))

output_dt$Actual_Binary <- ifelse(output_dt$Actual == 'Heart Disease',1,0)

# Confusion Matrix

cfm_dt <- confusionMatrix(output_dt$Actual,output_dt$Pred)

cfm_dt
  
ggplotConfusionMatrix(cfm_dt)

prop_dt <- round(table(test_set_dt$class)[1]/(table(test_set_dt$class)[1] + table(test_set_dt$class)[2]),2)

# Plotting Lift Chart

rlift.df.dt <- data.frame(x = c(1:135),
                          y = cumsum(output_dt$Actual_Binary[order(output_dt$Prob.Heart.Disease, decreasing = T)]),
                          bench = c(1:135)*prop_dt)

ggplot(rlift.df.dt, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-Decision Tree") + xlab('Cases') + ylab('Cumulative')

# Plotting ROC and finding AUC

roc_dt<- roc(output_dt$Actual,output_dt$Prob.Heart.Disease)

plot.roc(roc_dt,print.auc = T,grid = T)

#--------------------------------------------Random forest----------------------------------------------------------------


# Partioning data into train and test

split_rf <- sample.split(data_after_pca$class,SplitRatio = 0.7)

train_set_rf <- data_after_pca[split_rf,]

test_set_rf <- data_after_pca[!split_rf,]

# Performing 10 fold cross validation

control_rf <- trainControl(method="repeatedcv", number=10, repeats=3,search = 'grid')

set.seed(100)

tunegrid <- expand.grid(.mtry=c(1:15))

model_rf <- train(class~., data=train_set_rf, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control_rf)

# Results of 10 fold cross validation

model_rf

# plotting mtry v/s Accuracy

plot(model_rf)

# Applying the best model to test data

output_rf <- data.frame(Actual = test_set_rf$class,Prob = predict(model_rf,test_set_rf,type = 'prob'))

output_rf$Pred <- ifelse(output_rf$Prob.Heart.Disease >= 0.5,'Heart Disease','Normal ECG')

output_rf$Pred <- factor(output_rf$Pred)

output_rf$Actual_Binary <- ifelse(output_rf$Actual == 'Heart Disease',1,0)

# Confusion Matrix

cfm_rf <- confusionMatrix(output_rf$Pred,output_rf$Actual)

cfm_rf

ggplotConfusionMatrix(cfm_rf)

prop_rf <- round(table(test_set_rf$class)[1]/(table(test_set_rf$class)[1] + table(test_set_rf$class)[2]),2)

# Lift Chart

rlift.df.rf <- data.frame(x = c(1:135),
                       y = cumsum(output_rf$Actual_Binary[order(output_rf$Prob.Heart.Disease, decreasing = T)]),
                       bench = c(1:135)*prop_rf)

ggplot(rlift.df.rf, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-Random Forest") + xlab('Cases') + ylab('Cumulative')

# ROC Chart

roc_rf <- roc(output_rf$Actual,output_rf$Prob.Heart.Disease)

plot.roc(roc_rf,print.auc = T,grid = T)

#--------------------------------------------------Boosted Trees---------------------------------------------------------------

# Partitioning the data into train and test

split_bt <- sample.split(data_after_pca$class,SplitRatio = 0.7)

train_set_bt <- data_after_pca[split_bt,]

test_set_bt <- data_after_pca[!split_bt,]

# Model Training

model_bt <- boosting(class ~ ., data = train_set_bt)

# Applying model to test data

output_bt <- data.frame(Actual = test_set_bt$class,Prob = predict(model_bt,test_set_bt)$prob, Pred =predict(model_bt,test_set_bt)$class)

output_bt$Actual_Binary <- ifelse(output_bt$Actual=='Heart Disease',1,0)

# Confusion Matrix

cfm_bt <- confusionMatrix(output_bt$Pred,output_bt$Actual)

cfm_bt

ggplotConfusionMatrix(cfm_bt)

prop_bt <- round(table(test_set_bt$class)[1]/(table(test_set_bt$class)[1] + table(test_set_bt$class)[2]),2)

# Lift Chart

rlift.df.bt <- data.frame(x = c(1:135),
                          y = cumsum(output_bt$Actual_Binary[order(output_bt$Prob.1, decreasing = T)]),
                          bench = c(1:135)*prop_bt)

ggplot(rlift.df.bt, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart- Boosted Tree") + xlab('Cases') + ylab('Cumulative')

# Plotting ROC chart

roc_bt <- roc(output_bt$Actual,output_bt$Prob.1)

plot.roc(roc_bt,print.auc = T,grid = T)


#--------------------------------------------Logistic Regression-------------------------------------------------------------------

# Chi Square Test of Independance

chi_test_result <- data.frame(Pair = rep(NA,4096),P_Value = rep(NA,4096))

p = 0
for(i in 1:64){
  for(j in 1:64){
    tab <- chisq.test(table(data_after_pca[,i],data_after_pca[,j]))
    chi_test_result[j+p,1] <- paste(as.character(i),'-',as.character(j))
    chi_test_result[j+p,2] <- round(tab$p.value,5)
    q = j+p
  }
  p = q
}

# Displaying results having p value less than 0.05. If p is less than 0.05, we reject the null hypothesis that two
# variables are independent

head(chi_test_result[chi_test_result$P_Value <0.05,])

# Removing collinear categorical columns based on Chi Square Test of Independance

data_lr <- data_after_pca[,c(1:6,10,14:16,18,19,21,23,26,28,31,33,34,36,37,38,41,53,55,61,62,65:106)]

# Removing categorical constant columns

data_lr <- data_lr[,-c(7,10,16)]

set.seed(100)

split_lr <- sample.split(data_lr$class,SplitRatio = 0.7)

train_set_lr <- data_lr[split_lr,]

test_set_lr <- data_lr[!split_lr,]

model_lr <- glm(class~.,data = train_set_lr,family = 'binomial')

summary(model_lr)

model_lr1 <- glm(class~sex+chDI_RRwaveExists+chDI_DD_RRwaveExists+chDI_RPwaveExists+chDI_DD_RPwaveExists+chDI_RTwaveExists+
                   chDIII_RRwaveExists+chDIII_DD_RRwaveExists+chDIII_RTwaveExists+chDIII_DD_RTwaveExists+chAVR_DD_RRwaveExists+
                   chAVR_DD_RPwaveExists+chAVL_DD_RRwaveExists+chAVF_RRwaveExists+chAVF_DD_RPwaveExists+chAVF_RTwaveExists+
                   chV1_RRwaveExists+chV1_DD_RRwaveExists+chV1_RPwaveExists+chV1_DD_RTwaveExists+chV3_DD_RTwaveExists+
                   chV4_DD_RRwaveExists+chV6_RRwaveExists+chV6_DD_RRwaveExists+poly( PC1 ,2 )+poly( PC2 ,2 )+poly( PC3 ,2 )+
                   poly( PC4 ,2 )+poly( PC5 ,2 )+poly( PC6 ,2 )+poly( PC7 ,2 )+poly( PC8 ,2 )+poly( PC9 ,2 )+poly( PC10 ,2 )+
                   poly( PC11 ,2 )+poly( PC12 ,2 )+poly( PC13 ,2 )+poly( PC14 ,2 )+poly( PC15 ,2 )+poly( PC16 ,2 )+
                   poly( PC17 ,2 )+poly( PC18 ,2 )+poly( PC19 ,2 )+poly( PC20 ,2 )+poly( PC21 ,2 )+poly( PC22 ,2 )+
                   poly( PC23 ,2 )+poly( PC24 ,2 )+poly( PC25 ,2 )+poly( PC26 ,2 )+poly( PC27 ,2 )+poly( PC28 ,2 )+
                   poly( PC29 ,2 )+poly( PC30 ,2 )+poly( PC31 ,2 )+poly( PC32 ,2 )+poly( PC33 ,2 )+poly( PC34 ,2 )+
                   poly( PC35 ,2 )+poly( PC36 ,2 )+poly( PC37 ,2 )+poly( PC38 ,2 )+poly( PC39 ,2 )+poly( PC40 ,2 )+
                   poly( PC41 ,2 ),data = train_set_lr,family = 'binomial')
 
summary(model_lr1)

stat_significant_cols <-  data.frame(summary(model_lr1)$coef[summary(model_lr1)$coef[,4] <= .05, 4])

model_lr1 <- glm(class~sex+chDI_RRwaveExists+poly(PC1, 2)+poly(PC2, 2)+poly(PC3, 2)+poly(PC4, 2)+poly(PC6, 2)+
                   poly(PC9, 2)+poly(PC10, 2)+poly(PC11, 2)+poly(PC14, 2)+poly(PC16, 2)+poly(PC19, 2)+poly(PC21, 2)+
                   poly(PC21, 2)+poly(PC23, 2)+poly(PC25, 2)+poly(PC32, 2)+poly(PC33, 2)+poly(PC35, 2)+poly(PC36, 2)+
                   poly(PC38, 2)+poly(PC39, 2)+poly(PC40, 2),data = train_set_lr,family = 'binomial')


output_lr <-  data.frame(Actual = test_set_lr$class, Pred.Prob = round(predict(model_lr1,test_set_lr[,c(1,2,25,26:29,31,34,35,36,39,41,44,46,48,50,57,58,60,61,63,64,65)],type = 'response'),3))

output_lr$Pred <- ifelse(output_lr$Pred.Prob >=0.5,'Heart Disease','Normal ECG')

output_lr$Pred <- factor(output_lr$Pred)

output_lr$Actual_Binary <- ifelse(output_lr$Actual=='Heart Disease',1,0)

# Confusion Matrix

cfm_lr <- confusionMatrix(output_lr$Pred,output_lr$Actual)

cfm_lr

ggplotConfusionMatrix(cfm_lr)

prop_lr <- round(table(test_set_lr$class)[1]/(table(test_set_lr$class)[1] + table(test_set_lr$class)[2]),2)

rlift.df.lr <- data.frame(x = c(1:135),
                       y = cumsum(output_lr$Actual_Binary[order(output_lr$Pred.Prob, decreasing = T)]),
                       bench = c(1:135)*prop_lr)

ggplot(rlift.df.lr, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-Logistic Regression") + xlab('Cases') + ylab('Cumulative')

roc_lr <- roc(output_lr$Actual,output_lr$Pred.Prob)

plot.roc(roc_lr,print.auc = T,grid = T)

#---------------------------------------LDA---------------------------------------------------------------------------------

# Chi Square Test of Independance

chi_test_result <- data.frame(Pair = rep(NA,4096),P_Value = rep(NA,4096))

p = 0
for(i in 1:64){
  for(j in 1:64){
    tab <- chisq.test(table(data_after_pca[,i],data_after_pca[,j]))
    chi_test_result[j+p,1] <- paste(as.character(i),'-',as.character(j))
    chi_test_result[j+p,2] <- tab$p.value
    q = j+p
  }
  p = q
}

head(chi_test_result)

# Removing collinear categorical columns based on Chi Square Test of Independance

data_lda <- data_after_pca[,c(1:6,10,14:16,18,19,21,23,26,28,31,33,34,36,37,38,41,53,55,61,62,65:106)]

# Removing categorical constant columns

data_lda <- data_lda[,-c(7,10,16)]

# Convering to m-1 dummy binary varaibles

for(i in 1:24){
  data_lda[,i] <- as.numeric(as.character(data_lda[,i]))
}

# Partitioning data into train and test

set.seed(100)

split_lda <- sample.split(data_lda$class,SplitRatio = 0.7)

train_set_lda <- data_lda[split_lda,]

test_set_lda <- data_lda[!split_lda,]

model_lda <- lda(class~.,data = train_set_lda)

output_lda <- data.frame(Actual=test_set_lda$class,Pred = predict(model_lda,test_set_lda)$class, Prob = round(predict(model_lda,test_set_lda)$posterior,3))

output_lda$Actual_Binary <- ifelse(output_lda$Actual=='Heart Disease',1,0)

# Confusion Matrix

cfm_lda <- confusionMatrix(output_lda$Pred,output_lda$Actual)

cfm_lda

ggplotConfusionMatrix(cfm_lda)

prop_lda <- round(table(test_set_lda$class)[1]/(table(test_set_lda$class)[1] + table(test_set_lda$class)[2]),2)

# Lift Chart

rlift.df.lda <- data.frame(x = c(1:135),
                       y = cumsum(output_lda$Actual_Binary[order(output_lda$Prob.Heart.Disease, decreasing = T)]),
                       bench = c(1:135)*prop_lda)

ggplot(rlift.df.lda, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-LDA") + xlab('Cases') + ylab('Cumulative')


roc_lda <- roc(output_lda$Actual,output_lda$Prob.Heart.Disease)

plot.roc(roc_lda,print.auc = T,grid = T)

#----------------------------------------------Neural Nets------------------------------------------------------

data_nn <- data_after_pca

for(i in 1:64){
  data_nn[,i] <- as.numeric(as.character(data_nn[,i]))
}

data_nn$class <- ifelse(data$class=='Heart Disease',1,0)

# Normalizing the data

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_nn[,66:106] <- apply(data_nn[,66:106],2,normalize)

set.seed(100)

split_nn <- sample.split(data_nn$class,SplitRatio = 0.7)

train_set_nn <- data_nn[split_nn,]

test_set_nn <- data_nn[!split_nn,]

model_nn <- neuralnet(class~.,data = train_set_nn,err.fct = "ce",linear.output = FALSE,stepmax = 2e+06,hidden = 1)

output_nn <- data.frame(Actual = ifelse(test_set_nn$class ==1,'Heart Disease','Normal ECG'),
                        Actual_Binary = test_set_nn$class,
                        Pred.Prob = compute(model_nn,test_set_nn[,-65])$net.result)

output_nn$Pred <- ifelse(output_nn$Pred.Prob >=0.5,'Heart Disease','Normal ECG')

output_nn$Pred <- factor(output_nn$Pred)

# Confusion Matrix

cfm_nn <- confusionMatrix(output_nn$Pred,output_nn$Actual)

cfm_nn

ggplotConfusionMatrix(cfm_nn)

prop_nn <- round(table(test_set_nn$class)[2]/(table(test_set_nn$class)[1] + table(test_set_nn$class)[2]),2)

# Lift Chart

rlift.df.nn <- data.frame(x = c(1:135),
                           y = cumsum(output_nn$Actual_Binary[order(output_nn$Pred.Prob, decreasing = T)]),
                           bench = c(1:135)*prop_nn)

ggplot(rlift.df.nn, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-Neural Nets") + xlab('Cases') + ylab('Cumulative')

# Plotting ROC and determining AUC

roc_nn <- roc(output_nn$Actual,output_nn$Pred.Prob)

plot.roc(roc_nn,print.auc = T,grid = T)


# ---------------------------------------------------SVM--------------------------------------------------------------------

set.seed(100)

split_svm <- sample.split(data_after_pca$class,SplitRatio = 0.7)

train_set_svm <- data_after_pca[split_svm,]

test_set_svm <- data_after_pca[!split_svm,]

model_svm_lin <- tune.svm(class~.,data = train_set_svm,type = 'C-classification',kernel="linear",
                           gamma = c(0.1,1,10,100), cost = 10^(-3:4), probability = TRUE)

summary(model_svm_lin)

model_svm_poly <- tune.svm(class~.,data = train_set_svm,type = 'C-classification',kernel="polynomial",
               degree=2,gamma = c(0.01,0.1,1,10,100), cost = 10^(-3:4), coef0=c(0.1,0.5,1,2,3,4),probability = TRUE)

summary(model_svm_poly)

model_svm_rbf <- tune.svm(class~.,data = train_set_svm,type = 'C-classification',kernel="radial",
                           gamma = c(0.05,0.1,0.5,50,500), cost = c(0.01,0.1,1,10,100),probability = TRUE)

summary(model_svm_rbf)

model_svm <- model_svm_poly$best.model

summary(model_svm)

pred_svm <- predict(model_svm,test_set_svm,probability = TRUE)

output_svm <- data.frame(Actual = test_set_svm$class,
                        Actual_Binary = ifelse(test_set_svm$class=='Heart Disease',1,0),
                        Prob = attr(pred_svm,"probabilities")[,1] )

output_svm$Pred <- ifelse(output_svm$Prob>=0.5,'Heart Disease','Normal ECG')

output_svm$Pred <- factor(output_svm$Pred)

# Confusion Matrix

cfm_svm <- confusionMatrix(output_svm$Pred,output_svm$Actual)

cfm_svm

ggplotConfusionMatrix(cfm_nn)

prop_svm <- round(table(test_set_svm$class)[1]/(table(test_set_svm$class)[1] + table(test_set_svm$class)[2]),2)

# Lift Chart

rlift.df.svm <- data.frame(x = c(1:135),
                          y = cumsum(output_svm$Actual_Binary[order(output_svm$Prob, decreasing = T)]),
                          bench = c(1:135)*prop_svm)

ggplot(rlift.df.svm, aes(x = x)) + geom_line(aes(y= y), color = "blue") + geom_line(aes(y = bench), color = "red", lty = "dashed") +
  ggtitle("Lift chart-SVM") + xlab('Cases') + ylab('Cumulative')

# Plotting ROC and determining AUC

roc_svm <- roc(output_svm$Actual,output_svm$Prob)

plot.roc(roc_svm,print.auc = T,grid = T)


