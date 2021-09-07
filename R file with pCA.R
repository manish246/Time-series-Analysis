
##Importingthedataset
dataset<-read.csv('Childbirths.csv',fileEncoding="UTF-8-BOM")


##Reshufflingthedataset


dataset<-dataset[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15)]
View(dataset)


#SplittingthedatasetintotheTrainingsetandTestset
library(caTools)
set.seed(123)
split=sample.split(dataset$lowbwt,SplitRatio=0.7)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)


#FeatureScaling
training_set[-16]=scale(training_set[-16])
test_set[-16]=scale(test_set[-16])



#FittingLogisticRegressiontotheTrainingset
classifier=glm(formula=lowbwt~.,family=binomial,data=training_set)


#PredictingtheTestsetresults
prob_pred=predict(classifier,type='response',newdata=test_set[-16])
y_pred=ifelse(prob_pred>0.5,1,0)


#MakingtheConfusionMatrix
cm=table(test_set[,16],y_pred)


confusionMatrix(table(test_set[,16],y_pred))


plot(classifier)









