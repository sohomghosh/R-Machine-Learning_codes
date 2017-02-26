setwd("/home/sohom/Downloads/tech_gig_solve")
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv("test.csv",stringsAsFactors = F)
#table(is.na(train)) FALSE
#table(is.na(test))  FALSE
test$actual_credit_score<-test$actual._credit_score
test$actual._credit_score<-NULL
test$total_sales<-NA
train_test<-rbind(train,test)
train_test$business_type<-as.factor(train_test$business_type)
train_test$city<-as.factor(train_test$city)
train_test$state<-as.factor(train_test$state)
train_test$zip<-NULL
train_test$store_location<-as.factor(train_test$store_location)
train_test$time_zone<-as.factor(train_test$time_zone)
train_test$latitude<-NULL
train_test$longitude<-NULL
train_test$location_employee_code<-as.factor(train_test$location_employee_code)
train_test$credit_score<-NULL
train_test$credit_score_range<-NULL
boxplot(train_test[,17])
train_test$avg_age<-ifelse(train_test$avg_age>60,60,train_test$avg_age)
#apply(train_test$avg_age, function(X) )
train_test[,7]<-ifelse(train_test[,7]>80,80,train_test[,7])
train_test[,8]<-ifelse(train_test[,8]>70,70,train_test[,8])
train_test[,8]<-ifelse(train_test[,8]<15,15,train_test[,8])
train_test[,9]<-ifelse(train_test[,9]>60,60,train_test[,9])
train_test[,9]<-ifelse(train_test[,9]<40,40,train_test[,9])
train_test[,10]<-ifelse(train_test[,10]>60,60,train_test[,10])
train_test[,10]<-ifelse(train_test[,10]<40,40,train_test[,10])
train_test[,11]<-ifelse(train_test[,11]>3.5,3.5,train_test[,11])
train_test[,11]<-ifelse(train_test[,11]<1,1,train_test[,11])
train_test[,12]<-ifelse(train_test[,12]>125000000,125000000,train_test[,12])
train_test[,16]<-ifelse(train_test[,16]<70,70,train_test[,16])
train_test[,15]<-ifelse(train_test[,15]>200,200,train_test[,15])

x[!x %in% boxplot.stats(x)$out]


train_df<-as.data.frame(train_test[1:(nrow(train)-86),])
valid_df<-as.data.frame(train_test[(nrow(train)-85):nrow(train),])

train_df<-as.data.frame(train_test[1:nrow(train),])
test_df<-as.data.frame(train_test[(nrow(train)+1):nrow(train_test),])
train_df$outlet_no<-NULL
valid_df$outlet_no<-NULL
test_outlet_no<-test_df$outlet_no
test_df$outlet_no<-NULL
train_plt<-train_df[,c("avg_age","blue_collar","white_collar","female","male","total_household_size","total_household_income","employee_size","actual_credit_score","total_sales")]
library(corrplot)
M <- cor(train_plt)
corrplot(M,method="number")
#From the plot: total_sales and employee_size are highly correlated: So employee_size is an important variable: 0.96 so a linear regression modle will do
#####***outlier remov
#####***buckets of x_make
plot(train_df$employee_size,train_df$total_sales,xlim = c(0,200),ylim=c(0,9000))

l####inear_model <- lm(train_df$total_sales~train_df$employee_size,data = train_df)
#x=train_df
#x$total_sales<-NULL
#x_valid<-valid_df
#x_valid$total_sales<-NULL

#library(rpart)
#model_tree<-rpart(train_df$total_sales~train_df$employee_size, data=train_df, method="anova",control=) 
#predict_valid<-predict(linear_model,valid_df)

library(h2o)
library(data.table)
library(dplyr)



h2o.server <- h2o.init( nthreads= -1)

## Preprocessing the training data

#Converting all columns to factors
selCols = names(train_df)
#train_1 = train_df[,(selCols) := lapply(.SD, as.factor), .SDcols = selCols]

testHex = as.h2o(test_df)
train_score1=train_df$total_sales
train_1 = cbind(train_df,Y=train_score1)
valid_score1=valid_df$total_sales
valid_1 = cbind(valid_df,Y=valid_score1)
#Converting to H2o Data frame & splitting
train.hex1 = as.h2o(train_1)
validHex1 = as.h2o(valid_1)
features=names(train.hex1)[-16]

gbmF_model_1 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =validHex1 ,
                        max_depth = 3,
                        #distribution = "bernoulli",
                        ntrees =100,
                        learn_rate = 0.05
                        #,nbins_cats = 5891
)

gbmF_model_new = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =NULL ,
                        max_depth = 3,
                        #distribution = "bernoulli",
                        ntrees =100,
                        learn_rate = 0.05
                        #,nbins_cats = 5891
)

test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_new, newdata =testHex ,type="") )
pred1_1 = test_pred_score1
#pred1_new=ifelse(pred1_1<0,0,pred1_1)
ans<-lapply(pred1_1,function(x) ifelse(x<0,mean(train$total_sales),x))
answ<-ans$predict
fl<-cbind(test$outlet_no,answ)
colnames(fl)<-c("outlet_no","total_sales_Actual")
write.csv(file="test_submission_v15.csv",x=fl,row.names = F)

sub_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata = subHex,type="") )
sub1_1 = sub_pred_score1$p1
pred = pred1_1
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))

test_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = testHex1,type="") )
pred1_3 = test_pred_score1$p1
sub_pred_score1 = as.data.frame(h2o.predict(dl_model_1, newdata = subHex,type="") )
sub1_3 = sub_pred_score1$p1
pred = pred1_3 
table(pred>mean(pred),as.vector(testHex1$Y))
auc(roc(pred,factor(as.vector(testHex1$Y))))
    

dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =validHex1 ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
)


rf_model_1 =h2o.randomForest(x=features,y="Y",training_frame =train.hex1,
                             validation_frame =validHex1,
                             ntrees=200,max_depth = 3)
summary(rf_model_1)

rf_model_2 =h2o.randomForest(x=features,y="Y",training_frame =train.hex1,
                             validation_frame = NULL,#validHex1,
                             ntrees=300,max_depth = 3)
summary(rf_model_2)


test_pred_score1 = as.data.frame(h2o.predict(rf_model_2, newdata =testHex ,type="") )
pred1_1 = test_pred_score1
#pred1_new=ifelse(pred1_1<0,0,pred1_1)
ans<-lapply(pred1_1,function(x) ifelse(x<0,mean(train$total_sales),x))
answ<-ans$predict
fl<-cbind(test$outlet_no,answ)
colnames(fl)<-c("outlet_no","total_sales_Actual")
write.csv(file="test_submission_v16.csv",x=fl,row.names = F)



'''
                variable  relative_importance scaled_importance percentage
1           employee_size 7211465572352.000000          1.000000   0.550293
2                    city 3266835644416.000000          0.453006   0.249286
3    total_household_size  911315304448.000000          0.126370   0.069541
4  location_employee_code  706895020032.000000          0.098024   0.053942
5     actual_credit_score  228380000256.000000          0.031669   0.017427
6                  female  185636372480.000000          0.025742   0.014166
7                    male  157275602944.000000          0.021809   0.012001
8  total_household_income  154705051648.000000          0.021453   0.011805
9                 avg_age  117224833024.000000          0.016255   0.008945
10                  state  103296016384.000000          0.014324   0.007882
11         store_location   61735460864.000000          0.008561   0.004711
'''



library(data.table)


library(randomForest)
x=train_df[,cols]
y=train_df[,"total_sales"]
x_valid=valid_df[,cols]
y_valid=valid_df[,"total_sales"]
#rf_model=randomForest(x,y,xtest=x_valid,ytest=y_valid,ntree=100)






testHex = as.h2o(test_df[,cols])
train_score1=train_df$total_sales
train_1 = cbind(train_df[,cols],Y=train_score1)
valid_score1=valid_df$total_sales
valid_1 = cbind(valid_df[,cols],Y=valid_score1)
#Converting to H2o Data frame & splitting
train.hex1 = as.h2o(train_1)
validHex1 = as.h2o(valid_1)
features=names(train.hex1)[-5]

rf_model_1 =h2o.randomForest(x=features,y="Y",training_frame =train.hex1,
                             validation_frame =validHex1,
                             ntrees=400,max_depth = 4,mtries = 2)
summary(rf_model_1)

test_pred_score1 = as.data.frame(h2o.predict(rf_model_1, newdata =testHex ,type="") )
pred1_1 = test_pred_score1
#pred1_new=ifelse(pred1_1<0,0,pred1_1)
ans<-lapply(pred1_1,function(x) ifelse(x<0,min(train$total_sales),x))
answ<-ans$predict
fl<-cbind(test$outlet_no,answ)
colnames(fl)<-c("outlet_no","total_sales_Actual")
write.csv(file="test_submission_v10.csv",x=fl,row.names = F)









dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "Y",
                               training_frame =train.hex1 ,
                               validation_frame =validHex1 ,
                               activation="Rectifier",
                               hidden=60,
                               epochs=40,
                               adaptive_rate =F
); summary(dl_model_2)
test_pred_score1 = as.data.frame(h2o.predict(dl_model_2, newdata =testHex ,type="") )
pred1_1 = test_pred_score1
#pred1_new=ifelse(pred1_1<0,0,pred1_1)
ans<-lapply(pred1_1,function(x) ifelse(x<0,min(train$total_sales),x))
answ<-ans$predict
fl<-cbind(test$outlet_no,answ)
colnames(fl)<-c("outlet_no","total_sales_Actual")
write.csv(file="test_submission_v7.csv",x=fl,row.names = F)




gbmF_model_1 = h2o.gbm( x=features,
                        y = "Y",
                        training_frame =train.hex1 ,
                        validation_frame =validHex1 ,
                        max_depth = 4,
                        #distribution = "bernoulli",
                        ntrees =490,
                        learn_rate = 0.01
                        #,nbins_cats = 5891
);summary(gbmF_model_1)
test_pred_score1 = as.data.frame(h2o.predict(gbmF_model_1, newdata =testHex ,type="") )
pred1_1 = test_pred_score1
#pred1_new=ifelse(pred1_1<0,0,pred1_1)
ans<-lapply(pred1_1,function(x) ifelse(x<0,min(train$total_sales),x))
answ<-ans$predict
fl<-cbind(test$outlet_no,answ)
colnames(fl)<-c("outlet_no","total_sales_Actual")
write.csv(file="test_submission_v12.csv",x=fl,row.names = F)



rf_model_1 =h2o.randomForest(x=features,y="Y",training_frame =train.hex1,
                             validation_frame =validHex1,
                             ntrees=400,max_depth = 4)
summary(rf_model_1)

test_pred_score1 = as.data.frame(h2o.predict(rf_model_1, newdata =testHex ,type="") )
pred1_1 = test_pred_score1
#pred1_new=ifelse(pred1_1<0,0,pred1_1)
ans<-lapply(pred1_1,function(x) ifelse(x<0,min(train$total_sales),x))
answ_2<-ans$predict


fl<-cbind(test$outlet_no,(answ+answ_2)/2)
colnames(fl)<-c("outlet_no","total_sales_Actual")
write.csv(file="test_submission_v14.csv",x=fl,row.names = F)



x=c(1,5,4,2,3,500,5000,8900,9000)
x[!x %in% boxplot.stats(x)$out]
