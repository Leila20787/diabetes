# Handling of data frames
# functions

#' function 1 calculates the difference between the max and the min of each column of data.frame
#' @param data
#' @export
DF_entendue=function(data){
  N=ncol(data)
  vct=rnorm(N)
  for  (i in 1:ncol(data)) {
    vct[i]=max(data[i])-min(data[i])
  };
  print(vct) # it returns a vector which contains the extent of each column
}


#'function 2 tests are the elements of column i less than the elements of column j
#' @param data,i,j
#' @export
DF_test_inf=function(data,i,j){
  N=nrow(data)
  test =rnorm(N)
  for (k in 1:N) {
    if(data[k,i] < data[k,j]) test[k]="TRUE"
    else  test[k]="FALSE"
  };
  data<-data.frame(data,test)
}

#'auxiliary function (used in function 3 "DF_qrt") which allows to calculate the limits of a quantile
#' @param x,,p.inf,p.sup
# "x" is a vector
#' @return quantile(x,probs=c(p.inf,p.sup))
#' @export
bornes=function(x,p.inf,p.sup){
  return(quantile(x,probs=c(p.inf,p.sup)))
}

#' function 3 calculates the quantile bounds for each column of data.frame
#' @param data
#' @export
DF_qrt=function(data) {
  m<-sapply(data,bornes,p.inf=0.25,p.sup=0.75) # returns a matrix
}

#' function 4 generates a data.frame
#' @param n,m
#' @return D
#' @export
DF_gd=function(n,m){
  D <- data.frame(lapply(1:m,function(i){runif(n)}))
  colnames(D) <- paste("v",1:m,sep="")
  return(D)
}

#' function 5 is used to modify the elements of a column of a data frame

#' @param v1,V2,data_col,data
# v1: vector contains the indices of the elements that we want to modify
# v2: vector contains the new values
# data_col : the data frame column "data$nom_col"
#' @return data
#' @export
DF_cl=function(v1,v2,data_col,data){
  N=length(v1)
  for (i in 1:N){
    data_col[v1[i]]<-v2[i]
  };
  data=data.frame(data, 'nv_col'=data_col) # returns the data frame plus the new modified vector
  return(data)
}

#' function 6 takes a vector as argument and returns the associated histogram where four vertical lines are drawn (min, median, mean, max)+the density function curve
#' @param data_col
#' @export
info=function(data_col){
  hist(data_col, prob=T)
  densite <- density(data_col)
  lines(densite, col = "red",lwd=3)
  abline(v=quantile(data_col),col="green",lwd=3)
}

# Database manipulation: Diabetes

# This "diabetes" data set is the source of the National Institute for Diabetes and Digestive and Kidney Diseases.
# All patients here are women of at least 21 years of Pima Indian descent
# Pregnancies: number of times pregnant
# Glucose: plasma glucose concentration at 2 hours in an oral glucose tolerance test
# Blood Pressure (Blood Pressure): diastolic blood pressure (mm Hg)
# Skin thickness (Skin Thickness): thickness of the skin fold of the triceps (mm)
# Insulin: serum insulin 2 hours (mIU / L)
# BMI: Body mass index (weight in kg / (height in m) ^ 2)
# Diabetes Pedigree Function: Diabetes Pedigree Function (FGD)
# Age: Age (years)

# the following functions take as argument the data.frame "diabetes"

#' function 7 calculates the number of women who have the positive "diabetes" test and the number of women who have the negative "diabetes" test
#' @param data
#' @export
Diab_test=function(data){
  cpt<-0
  for (i in 1:nrow(data)){
    if (data$Outcome[i] == 1) cpt=cpt+1
  };
  liste1 <- list(cpt,nrow(data)-cpt)
  data <- data.frame(liste1)
  rownames(data) <- "nb"
  colnames(data) <-c("DP","DN")
  print(data) # returns a data.frame
}


#' function 8 returns a data frame contains the number of women who have diabetes and were pregnant more than three times and the number
#'women who have diabetes but were less than three times pregnant
#' @param data
#' @export
Diab_G3=function(data){

  cpt<-0
  cpt1<-0
  for (i in 1:nrow(data)){
    if (data$Pregnancies[i] >= 3 & data$Outcome[i] == 1 ) {
      cpt=cpt+1
    }
    if (data$Pregnancies[i] < 3 & data$Outcome[i] == 1 ) {
      cpt1=cpt1+1
    }
  };
  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("Pregnancies>=3","Pregnancies<3")
  print(data) # returns a data.frame
}

#'function 9 returns a data frame containing the number of women with diabetes with abnormal diastolic blood pressure and the number
#'women who have diabetes with naormal diastolic blood pressure
#' @param data
#' @export
Diab_BP=function(data){
  cpt<-0
  cpt1<-0
  for (i in 1:nrow(data)){
    if (data$BloodPressure[i] >= 90 & data$Outcome[i] == 1 ) {
      cpt=cpt+1
    }
    if (data$BloodPressure[i] < 90 & data$BloodPressure[i] > 0 & data$Outcome[i] == 1 ) {
      cpt1=cpt1+1

    }
  };
  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("BloodPressure>=85","BloodPressure<85")
  print(data) # returns a data frame
}

#' function 10 returns a data frame contains the number of women with diabetes over 30 years old and the number
#' women who have diabetes under 30 years old
#' @param data
#' @export
Diab_Age=function(data){
  cpt<-0
  cpt1<-0
  for (i in 1:nrow(data)){
    if (data$Age[i] >= 30 & data$Outcome[i] == 1 ) {
      cpt=cpt+1
    }
    if (data$Age[i] < 30 & data$Age[i] >0 & data$Outcome[i] == 1 ) {
      cpt1=cpt1+1

    }
  };
  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("AGE >= 30","AGE<30")
  print(data) #  returns a data frame
}
#' function 11 returns a data frame contains the number of women who have diabetes with abnormal glucose levels and the number
#' of women who have diabetes with normal glucose levels
#' @param data
#' @export
Diab_GL=function(data){
  cpt<-0
  cpt1<-0
  for (i in 1:nrow(data)){
    if (data$Glucose[i] >= 126 & data$Outcome[i] == 1 ) {
      cpt=cpt+1
    }
    if (data$Glucose[i] < 126 & data$Glucose[i] >0 & data$Outcome[i] == 1 ) {
      cpt1=cpt1+1

    }
  };
  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("Glucose_A","Glucose_N ")
  print(data) # # returns a data frame
}

#' function 12 returns a data.frame contains the number of women who have diabetes with normal BMI and the number
#' women with diabetes with abnormal BMI
#' @param data
#' @export
Diab_IMC=function(data){
  cpt<-0
  cpt1<-0
  for (i in 1:nrow(data)){

    if (data$BMI[i] < 18.5 & data$BMI[i] > 0 & data$Outcome[i] == 1 ) {cpt1=cpt1+1}
    else if( data$BMI[i] < 29.5 & data$BMI[i] > 0 & data$Outcome[i] == 1 ){cpt=cpt+1 }
    else if(data$BMI[i] > 0 & data$Outcome[i] == 1 ){cpt1=cpt1+1 }

  };

  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("IMC_N","IMC_A")
  print(data)

}

#' function 13 returns a data frame contains the number of women who have diabetes with FGD <0.5 and the number
#' women with diabetes with FGD> 0.5
#' @param data
#' @export
Diab_FGD=function(data){
  cpt<-0
  cpt1<-0

  for (i in 1:nrow(data)){

    if (data$DiabetesPedigreeFunction[i] < 0.5 & data$DiabetesPedigreeFunction[i] >0 & data$Outcome[i] == 1 ) {cpt=cpt+1}

    if( data$DiabetesPedigreeFunction[i] > 0.5 & data$Outcome[i] == 1 ){cpt1=cpt1+1}

  };
  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("FGD <0.5","FGD >0.5")
  print(data) #  returns a data frame

}

#' function 14 returns a data frame contains the number of women who have diabetes with normal skin thickness and the number
#' women who have diabetes with abnormal skin thickness
#' @param data
#' @export
Diab_MG=function(data){
  cpt<-0
  cpt1<-0
  for (i in 1:nrow(data)){

    if (data$SkinThickness[i] < 30 & data$SkinThickness[i]>0 & data$Outcome[i] == 1 ) {cpt=cpt+1}

    if( data$SkinThickness[i] > 30 & data$Outcome[i] == 1 ){cpt1=cpt1+1}

  };
  liste2 <- list(cpt,cpt1)
  data <- data.frame(liste2)
  rownames(data) <- "nb"
  colnames(data) <-c("MG_N","MG_A")
  print(data) # returns a data frame
}
# Note :
# From this we can see that a higher incidence of diabetes tends to be associated with:
# +More pregnancies + More glucose + More skin thickness + More insulin + More BMI
#+ More age.
# Associations with blood pressure and DPF appear weaker.





































