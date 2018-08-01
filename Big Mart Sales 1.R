#install.packages("data.table")
library(data.table)
library(ggplot2)
library(cowplot) #plot_grid()
library(caret) #dummyVars()
library(corrplot)
library(glmnet)
train = fread("Train_UWu5bXk.csv")
test = fread("Test_u94Q5KV.csv")
#submission = fread("SampleSubmission_TmnO39y.csv")

#Dimensions of Data
dim(train)
dim(test)

#Features of Data
names(train)
names(test)

#Structure of Data
str(train)

#Combine Train and Test
test$Item_Outlet_Sales<-NA
combi = rbind(train,test)
dim(combi)

#-------------------------------Exploratory Data Analysis-------------------
#---------------Univariate Analysis

#Target Variable
#target variable is continuous, visualise it by plotting histogram.
ggplot(combi,aes(x = Item_Outlet_Sales))+
  geom_histogram(binwidth = 100,fill = "darkgreen")+
  xlab("Item_Outlet_Sales")

#Independent Variables (numeric variables)
p1 = ggplot(combi,aes(x = Item_Weight))+
  geom_histogram(binwidth = 0.5,fill = "blue")+
  xlab("Item_Weight")

p2 = ggplot(combi,aes(x = Item_Visibility))+
  geom_histogram(binwidth = 0.005,fill = "blue")+
  xlab("Item_Visibility")

p3 = ggplot(combi,aes(x = Item_MRP))+
  geom_histogram(binwidth = 1,fill = "blue")+
  xlab("Item_MRP")

plot_grid(p1, p2, p3, nrow = 1)
#Independent Variables (categorical variables)
#Item_Fat_Content
combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"]<-"Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="low fat"]<-"Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"]<-"Regular"

ggplot(combi,aes(x = Item_Fat_Content))+
  geom_bar(fill = "coral1")+
  xlab("Item_Fat_Content")

#Item_Type
p4 = ggplot(combi,aes(x = Item_Type))+
  geom_bar(fill = "coral1")+
  xlab("")
#Outlet_Identifier
p5 = ggplot(combi,aes(x = Outlet_Identifier))+
  geom_bar(fill = "coral1")+
  xlab("")

#Outlet_Size
p6 = ggplot(combi,aes(x = Outlet_Size))+
  geom_bar(fill = "coral1")+
  xlab("")

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

#Outlet_Establishment_Year
p7 = ggplot(combi,aes(x = factor(Outlet_Establishment_Year)))+
  geom_bar(fill = "coral1")+
  xlab("Outlet_Establishment_Year")

#Outlet_Type
p8 = ggplot(combi,aes(x = Outlet_Type))+
  geom_bar(fill = "coral1")+
  xlab("")
plot_grid(p7,p8,ncol = 2)


#-----------------------Bivariate Analysis
train<-combi[1:nrow(train)]
## Target Variable vs Independent Numerical Variables

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train,aes(x = Item_Weight,y = Item_Outlet_Sales))+
  geom_point(alpha = 0.3)

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train,aes(x = Item_Visibility,y = Item_Outlet_Sales))+
  geom_point(alpha = 0.3)

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train,aes(x = Item_MRP,y = Item_Outlet_Sales))+
  geom_point(alpha = 0.3)

## Target Variable vs Independent Categorical Variables

# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train,aes(x = Item_Type,y = Item_Outlet_Sales))+
  geom_violin(fill = "magenta")

# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train,aes(x = Item_Fat_Content,y = Item_Outlet_Sales))+
  geom_violin(fill = "magenta")

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train,aes(x = Outlet_Identifier,y = Item_Outlet_Sales))+
  geom_violin(fill = "magenta")

#In the univariate analysis, we came to know about the empty values in 
#Outlet_Size variable. 

ggplot(train,aes(x = Outlet_Size, y = Item_Outlet_Sales))+
  geom_violin(fill = "magenta")
#The distribution of ???Small??? Outlet_Size is almost identical to the distribution of the blank category (first vioin) of Outlet_Size. 
#So, we can substitute the blanks in Outlet_Size with ???Small???.

# Outlet_Location_Type vs Item_Outlet_Sales
p15 = ggplot(train,aes(x = Outlet_Location_Type,y = Item_Outlet_Sales))+
  geom_violin(fill = "magenta")

# Outlet_Type vs Item_Outlet_Sales
p16 = ggplot(train,aes(x = Outlet_Type,y = Item_Outlet_Sales))+
  geom_violin(fill = "magenta")

#.....................Missing Value Treatment......................#

combi$Outlet_Size[which(combi$Outlet_Size=="")]<-"Small"

sum(is.na(combi$Item_Weight))

missing_index = which(is.na(combi$Item_Weight))
for (i in missing_index) {
  item<-combi$Item_Identifier[i]
  combi$Item_Weight[i]<-
    mean(combi$Item_Weight[combi$Item_Identifier==item],na.rm = TRUE)
}
#check
sum(is.na(combi$Item_Weight))

# Replacing 0???s in Item_Visibility variable
#check p10 - visibility cannot be zero

index = which(combi$Item_Visibility==0)
for (i in index) {
  identifier = combi$Item_Identifier[i]
  combi$Item_Visibility[i]<-mean(combi$Item_Visibility[combi$Item_Identifier==identifier],na.rm = TRUE)
}

#check histogram of Item_Visibility again
ggplot(combi,aes(x = Item_Visibility,y = Item_Outlet_Sales))+
  geom_point(alpha = 0.3)




#................Feature Engineering........................

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
# create a new feature 'Item_Type_new'
combi$Item_Type_new <- ifelse(combi$Item_Type %in% perishable, "perishable", 
                              ifelse(combi$Item_Type %in% non_perishable, "non_perishable",
                                     "not_sure"))

#Let???s compare Item_Type with the first 2 characters of Item_Identifier, i.e., ???DR???, ???FD???, and ???NC???. 
#These identifiers most probably stand for drinks, food, and non-consumable.
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))
combi$Item_category<-substr(combi$Item_Identifier, 1, 2)

#change the values of Item_Fat_Content wherever Item_category is ???NC??? 
#because non-consumable items cannot have any fat content
combi$Item_Fat_Content[combi$Item_category=="NC"]<-"Non-Edible"

# years of operation for outlets
combi$Outlet_years<-2018-combi$Outlet_Establishment_Year
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

# Price per unit weight
combi$price_per_unit_wt<-combi$Item_MRP/combi$Item_Weight

#in p11,Item_MRP vs Item_Outlet_Sales plot, 
#we saw Item_MRP was spread across in 4 chunks
## creating new independent variable - Item_MRP_clusters
combi$Item_MRP_clusters<-ifelse(combi$Item_MRP < 69, "1st", 
                                ifelse(combi$Item_MRP >= 69 & combi$Item_MRP < 136, "2nd",
                                       ifelse(combi$Item_MRP >= 136 & combi$Item_MRP < 203, "3rd", "4th")))


#..........................Encoding Categorical Variables.....................#

combi$Outlet_Size_num<-ifelse(combi$Outlet_Size=="Small",0,
                          ifelse(combi$Outlet_Size=="Medium",1,2))
combi$Outlet_Location_Type_num<-ifelse(combi$Outlet_Location_Type=="Tier 3",0,
                                   ifelse(combi$Outlet_Location_Type=="Tier 2",1,2))
# removing categorical variables after label encoding
combi$Outlet_Size<-NULL
combi$Outlet_Location_Type<-NULL

#One hot encoding, each category of a categorical variable is converted into a new binary column (1/0).
dmy<-dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
trsf<-data.table(predict(dmy,newdata = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], trsf)


#....................DATA PREPROCESSING.......................#

#Removing Skewness
#Item_Visibility and price_per_unit_wt are highly skewed
# check p2 and histogram for price_per_unit_wt

combi$Item_Visibility <- log(combi$Item_Visibility+1)
combi$price_per_unit_wt <- log(combi$price_per_unit_wt+1)
#log + 1 as log 0 is undefined

#Scaling numeric predictors
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars) #colnames for numeric featuees
#setdiff(num_vars_names, "Item_Outlet_Sales")-removing Item_outlet_sales(numeric) from num_var_names
#combi_numeric will have only numeric features
combi_numeric <- combi[,setdiff(num_vars_names, "Item_Outlet_Sales"),with = FALSE] 
prep_num<-preProcess(combi_numeric,method = c("center","scale"))
combi_numeric_norm<-predict(prep_num,newdata = combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

#Splitting the combined data combi back to train and test set.
train <- combi[1:nrow(train)]
test <- combi[nrow(train)+1:nrow(combi)]
test[,Item_Outlet_Sales:=NULL]# removing Item_Outlet_Sales as it contains only NA for test dataset

#.................Correlated Variables...............#
cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

#*****************Linear regression******************#
reg_model<-train(Item_Outlet_Sales~.,
             train[, -c("Item_Identifier")],
             method = "lm",
             trControl = trainControl(method = "cv",
                                      number = 5,
                                      verboseIter = TRUE))

y_pred<-predict(reg_model,newdata = test[, -c("Item_Identifier")])
print(reg_model)
# RMSE 1128.651

#*********Regularized Linear Regression(Lasso/Ridge)********#

#Lasso
set.seed(1235)
myGrid<-expand.grid(alpha = 1,
                    lambda = seq(0.001,0.1,by=0.0002))
myControl<-trainControl(method = "cv",number = 5)
lasso_reg_model<-train(x = train[,-c("Item_Identifier","Item_Outlet_Sales")],
                       y = train$Item_Outlet_Sales,
                       method = "glmnet",
                       tuneGrid = myGrid,
                       trControl = myControl)
mean(lasso_reg_model$resample$RMSE) #1129.889

#Ridge
set.seed(1236)
myGrid<-expand.grid(alpha = 0,
                    lambda = seq(0.001,0.1,by=0.0002))

ridge_reg_model<-train(x = train[,-c("Item_Identifier","Item_Outlet_Sales")],
                       y = train$Item_Outlet_Sales,
                       method = "glmnet",
                       tuneGrid = myGrid,
                       trControl = myControl)

mean(ridge_reg_model$resample$RMSE)# 1134.735
#****************Random Forest********************#

set.seed(1237)
my_control = trainControl(method="cv", number=5) # 5-fold CV
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20))
rf_model<-train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")], 
                y = train$Item_Outlet_Sales,
                method='ranger', 
                trControl= my_control, 
                tuneGrid = tgrid,
                num.trees = 400,
                importance = "permutation")
plot(rf_model)
plot(varImp(rf_model))
mean(rf_model$resample$RMSE) #1088.631
