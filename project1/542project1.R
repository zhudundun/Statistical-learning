one_step_lasso = function(r, x, lam){
  xx = sum(x^2)
  xr = sum(r*x)
  b = (abs(xr) -lam/2)/xx
  b = sign(xr)*ifelse(b>0, b, 0)
  return(b)
}



Housing=read.csv("C:/Users/wang/Documents/UIUC/R/Ames_data.csv")
n=2930
ntrain=n*0.7
ntest=n-ntrain
test.id=sample(1:2930,879)
traindata=Housing[-test.id,]
testdata=Housing[test.id,]
save(traindata, file="Ames_traindata.csv")
save(testdata, file="Ames_testdata.csv")



mylasso = function(X, y, lam, n.iter = 50, standardize  = TRUE)
{
  # X: n-by-p design matrix without the intercept
  # y: n-by-1 response vector
  # lam: lambda value
  # n.iter: number of iterations
  # standardize: if True, center and scale X and y. 

  
  
  # YOUR CODE
  # If standardize  = TRUE, center and scale X and Y
  if (standardize==TRUE) {x=scale(x); y=scale(y); xba=mean(x); yba=mean(y); xsd=var(x); ysd=var(y) } 

    
  # Initial values for residual and coefficient vector b
  b = rep(0, p)
  r = y
  
  for(step in 1:n.iter){
    for(j in 1:p){
      
      # YOUR CODE 
      
      # 1) Update the residual vector to be the one
      # in blue on p37 of [lec_W3_VariableSelection.pdf]. 
      r=r + X[, j] * b[j]
      
      
      # 2) Apply one_step_lasso to update beta_j
      
       b[j] = one_step_lasso(r, X[, j], lam)
      
      # 3) Update the current residual vector
      r=r - X[, j] * b[j]
    }
  }
  
  # YOUR CODE: scale back b and add intercept b0
  # For b0, check p13 of [lec_W3_VariableSelection.pdf]. 
  b=b*ysd/xsd
  b0=yba-xba*b
  return(c(b0, b))}
lambda  <- seq(0,200,length=200)
beta_cd <- matrix(0,200,83)
for(l in 1:200){mylasso = function(X, y, lam, n.iter = 50, standardize  = TRUE)
  beta_cd[l,]<-b}
  


Mode <- function(x) {
       ux <- unique(x)
       ux[which.max(tabulate(match(x, ux)))]
   }


remissing=function(x) {
  +      if(is.factor(x)) replace(x, is.na(x), Mode(na.omit(x)))
  +      else if(is.numeric(x)) replace(x, is.na(x), median(x, na.rm=TRUE))
  +      else x }#replace missing values

for (p in 1:79){for (n in 1:2051) remissing(x[n,p])}

install.packages("robustHD")
library("robustHD")

nums <- unlist(lapply(x, is.numeric))
x[ ,nums]
winsorize(x[,nums])

which( colnames(x)=="Street" )
[1] 6
which( colnames(x)=="Utilities" )
[1] 10
which( colnames(x)=="Longitude" )
[1] 81
> which( colnames(x)=="Latitude" )
[1] 82

which( colnames(x)=="Land_Slope" )
[1] 12
> which( colnames(x)=="Condition_2" )
[1] 15
> which( colnames(x)=="Roof_Matl" )
[1] 23
> which( colnames(x)=="Heating" )
[1] 40
> which( colnames(x)=="Pool_QC" )
[1] 73
> which( colnames(x)=="Misc_Feature" )
[1] 75
> which( colnames(x)=="Low_Qual_Fin_SF")
[1] 46
> which( colnames(x)== "Three_season_porch")
[1] 70
> which( colnames(x)=="Pool_Area")
[1] 72
> which( colnames(x)=="Misc_Val")
[1] 76

[1] "(Intercept)"                                "MS_SubClassOne_and_Half_Story_PUD_All_Ages"
[3] "NeighborhoodGreen_Hills"                    "Roof_MatlMembran"                          
[5] "Exterior_1stPreCast"                        "Exterior_2ndOther"                         
[7] "Exterior_2ndPreCast"                        "Mas_Vnr_TypeCBlock"                        
[9] "Kitchen_QualPoor" 

[1] "(Intercept)"                                "MS_SubClassOne_and_Half_Story_PUD_All_Ages"
[3] "NeighborhoodGreen_Hills"                    "Roof_MatlMembran"                          
[5] "Exterior_1stPreCast"                        "Exterior_2ndOther"                         
[7] "Exterior_2ndPreCast"                        "Mas_Vnr_TypeCBlock"                        
[9] "Low_Qual_Fin_SF"                            "Kitchen_AbvGr"                             
[11] "Kitchen_QualPoor"                           "Three_season_porch"                        
[13] "Pool_Area"                                  "Misc_Val" 






install.packages("randomForest")
library("randomForest")
y=Housing[,83]


row.has.na <- apply(x, 1, function(x){any(is.na(x))})
predictors_no_NA <- x[!row.has.na,]
y=y[!row.has.na]

x=traindata[,-82]
> x=x[,-81]
x=x[,-6]
#remove langitude and latitude street

x_train=model.matrix(~ -1 + . , data=Housing[,facs], contrasts.arg = lapply(Housing[,facs], contrasts, contrasts=FALSE))
> x=cbind(x_train,Housing[,-facs])
> View(x2)
> mylasso = glmnet(x[-test.id,], y[-test.id], alpha = 1)



library("Matrix")
> facs <- unlist(lapply(data, is.factor))

> sparseProducts <- sparse.model.matrix(traindata[,83]~ ., data=traindata)
> which( colnames(sparseProducts)=="Sale_Price")
> mylasso = glmnet(sparseProducts[,-310], sparseProducts[,310], alpha = 1)

condenseMe <- function(vector) {
  toCondense <- names(which(prop.table(table(vector)) <0.2))
for (i in 1:2930) {if (vector[i] %in% toCondense) {vector[i]="Other"}}
return(vector)
}
> for (i in 1:45){datafactor[,i]=condenseMe(datafactor[,i])}#change low frequency cell to others


Housing2 <- read.csv("C:/Users/wang/Documents/UIUC/R/Ames_data.csv", header = TRUE, stringsAsFactors = FALSE)
#rename the categorical variable

condenseMe <- function(vector) {
  toCondense <- names(which(prop.table(table(vector)) <0.2))
  for (i in 1:2930) {if (vector[i] %in% toCondense) {vector[i]="Other"}}
  return(vector)#turn low frequency level to "other"
}

catedata=Housing2[,cat]
for (i in 1:45){catedata[,i]=condenseMe(catedata[,i])}
cat <- unlist(lapply(x, is.factor))#find out categorical variable

newdata=cbind(catedata,x[,num])
write.csv(newdata, file = "Ames_newdata.csv")
write.csv(traindata, file = "Ames_traindata.csv")


Housing3 <- read.csv("C:/Users/wang/Documents/UIUC/R/Ames_newdata.csv", header = TRUE, stringsAsFactors = TRUE)
Housing3=Housing3[,-1]

d=matrix(45,5,9)
for (i in 1:45){d[i]=nlevels(catedata[,i])}# check the levels of all categorical variable

Housing4=Housing3[,-46]
Housing4=Housing4[,-9]#remove variable with only 1 or 0 level

library("mice")
imputed_Housing <- mice(x[-cat,], m=1, maxit = 500, method = 'cart', seed = 500)
#impute numerica data

NAdata=complete.cases(x)
newdata=x[NAdata,]#remove the row w/ NA values


