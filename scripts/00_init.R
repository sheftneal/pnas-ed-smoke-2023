library(rgeos)
library(sp)
library(raster)
library(rgdal)
library(sf)
library(fixest)
library(ggplot2)
library(lubridate)
library(multcomp)
library(stringr)
library(splines)
library(treemap)
library(classInt)
library(tidyverse)
library(areaplot)
library(data.table)
library(RCurl)
library(tidycensus)
library(areaplot)
library(viridis)
library(wesanderson)
library(MetBrewer)

#read in icd mappings for 2015+ files
#icdmap <- read_csv("data/clean/icd-codes/icd9toicd10cmgem.csv")



#function for mapping icd9 to icd10
get_icd10 <- function(icd9){icdmap$icd10cm[icdmap$icd9cm %in% icd9]}



sum_na = function(x, ...) if (all(is.na(x))) NaN else sum(x, na.rm = TRUE)
mean_na = function(x, ...) if (all(is.na(x))) NaN else mean(x, na.rm = TRUE)


#add transparency to any color	
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}	




#step 1: create data frame with hist heights:  
get_hst_obj <- function(values, min, max, width, cutoff = 1,type = "count"){
  x_hist_bar <- seq(min,max,width) 
  hist_ht <- data.frame(x_hist_bar- width/2 , x_hist_bar +width/2, NA)       
  for(i in 1:nrow(hist_ht)){hist_ht[i,3]<- sum(values> hist_ht[i,1] & 
                                                 values <= hist_ht[i,2], na.rm = T)}
  hist_ht[1,3] <- hist_ht[1,3] + sum(values < hist_ht[1,1], na.rm = T)
  hist_ht[nrow(hist_ht),3] <- hist_ht[nrow(hist_ht),3] +  
    sum(values > hist_ht[nrow(hist_ht),2], na.rm = T)
  if(type == "share"){hist_ht[,3]<-hist_ht[,3]/sum(hist_ht[,3],na.rm = T)}
  names(hist_ht)=c("left","right","count")
  hist_ht <- subset(hist_ht, left <= quantile(as.matrix(values), cutoff,na.rm = T))
  return(hist_ht)
}
#step 1b: create data frame with hist heights for higher unit counting:  
get_hst_obj2 <- function(values, min, max, width, cutoff = 1,type = "count"){
  x_hist_bar <- seq(min,max,width) 
  hist_ht <- data.frame(x_hist_bar- width/2 , x_hist_bar +width/2, NA)
  values=values %>% dplyr::select(pre_exp, everything())
  for(i in 1:nrow(hist_ht)){
    
    hist_ht[i,3]<- values %>% filter(pre_exp>=hist_ht[i,1] & 
                                       pre_exp<hist_ht[i,2]) %>% dplyr::select(2) %>%
      distinct() %>% nrow()
    
  }
  hist_ht[1,3] <- hist_ht[1,3] + values %>% filter(pre_exp<=hist_ht[1,1]) %>% 
    dplyr::select(2) %>% distinct() %>% nrow()
  hist_ht[nrow(hist_ht),3] <- hist_ht[nrow(hist_ht),3] +  
    values %>% filter(pre_exp>=hist_ht[nrow(hist_ht),1]) %>% 
    dplyr::select(2) %>% distinct() %>% nrow()
  if(type == "share"){hist_ht[,3]<-hist_ht[,3]/sum(hist_ht[,3],na.rm = T)}
  names(hist_ht)=c("left","right","count")
  hist_ht <- subset(hist_ht, right <= quantile(as.matrix(values$pre_exp), cutoff,na.rm = T))
  return(hist_ht)
}



#step 2: plot histogram at bottom of figure
plotHist <- function(hst_obj, col, alpha, bottom, height, norm,border.col = col ){
  
  rect(xleft = hst_obj[,1], xright =  hst_obj[,2], 
       ybottom =bottom, ytop = (hst_obj[,3]/norm)*height + bottom, 
       col = add.alpha(col, alpha), border = border.col ) 		
  
}




#pull distributed lag estimate, calculate sum coefficient + se
get_est <- function(model, lags, varname){
  
  vars <-names(coef(model))[grep(x = names(coef(model)), pattern = varname)][1:(lags+1)]
  est <- sum(model$coefficients[vars])
  se <- as.numeric(summary(glht(model, linfct = paste("`",paste0(vars, collapse = "` + `"),"`=0", sep = ""), vcov = vcov(model, cluster = "county"), coef. = coef(model)))$test$sigma)
  
  lb <- est + qnorm(0.025)*se
  ub <- est + qnorm(0.975)*se
  
  output <-data.frame(est = est, se = se, lb = lb, ub = ub)
  return(output)
}



#write equations for different outcomes
get_eqn <- function(outcome, exposure, fixed_effects){
        fml <- as.formula(paste(outcome, "~", exposure, "|",fixed_effects)) #define regression function
        print(fml) #print it out 
        return(fml) #and return it
                  }


#define a function that returns sum of coefficient + standard error for user specified number of lags

get_bin_response_cumulative <- function(model_object, varname, n_lags, cluster_var){
  
  est =  coef(model_object)[grep(x = names(coef(model_object)), pattern = varname)] %>% sum()
  
  if(n_lags == 0){se <- se(model_object)[grep(x = names(coef(model_object)), pattern = varname)]}
  if(n_lags == 1){se <- as.numeric((summary(glht(model_object, linfct = c(paste(varname," + ",paste("`l(",varname,", 1)`", sep="") ," =0", sep="")),vcov = vcov(model_object, cluster = cluster_var ))))$test$sigma)}
  if(n_lags>1){se <- as.numeric((summary(glht(model_object, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ," =0", sep="")),vcov = vcov(model_object, cluster = cluster_var ))))$test$sigma)}
  
  output <- c(est, se)
  names(output)<-c("est","se")
  
  return(output)
  
}




get_poly_response_cumulative  <- function(model_object, degree, xrange){
  
  
  
  if(degree == 2){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1,")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2,")] %>% sum()
    y <- deg1*xrange + deg2*xrange^2  
    
  }
  
  
  if(degree == 3){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1,")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2,")] %>% sum()
    deg3 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM3,")] %>% sum()
    y <- deg1*xrange + deg2*xrange^2 + deg3*xrange^3 
    
  }
  
  
  

  if(degree == 4){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1,")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2,")] %>% sum()
    deg3 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM3,")] %>% sum()
    deg4 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM4,")] %>% sum()
    y <- deg1*xrange + deg2*xrange^2 + deg3*xrange^3 + deg4*xrange^4
    
  }
  
  
   
  
  return(as.numeric(y))
  
}





get_poly_coef_cumulative <- function(model_object, degree){

  
  if(degree == 1){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1,")] %>% sum()
    
    return(as.numeric(deg1))
    
  }
  
  if(degree == 2){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2")] %>% sum()
    
    return(as.numeric(c(deg1, deg2)))
    
  }
  
  
  
  if(degree == 3){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2")] %>% sum()
    deg3 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM3")] %>% sum()
   
    return(as.numeric(c(deg1, deg2, deg3)))
    
  }
  
  
  
  if(degree == 4){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2")] %>% sum()
    deg3 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM3")] %>% sum()
    deg4 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM4")] %>% sum()
    
    
    return(as.numeric(c(deg1, deg2, deg3, deg4)))
    
  }
  
  
  
  
  if(degree == 5){
    deg1 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM1")] %>% sum()
    deg2 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM2")] %>% sum()
    deg3 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM3")] %>% sum()
    deg4 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM4")] %>% sum()
    deg5 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "smokePM5")] %>% sum()
    
    
    return(as.numeric(c(deg1, deg2, deg3, deg4, deg5)))
    
  }
  
  
  
  
  
}









#used for linear interactions
get_bin_coef_interaction <- function(mod,var){
  
  coef <- data.frame(var = paste("regbin",rep(1:5, 2), sep=""), interaction = c(rep(0, 5), rep(1, 5)),est = NA, se = NA)
  
  coef$est[1] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin1' )][1:7])
  coef$est[6] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin1' )])
  
  coef$est[2] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin2' )][1:7])
  coef$est[7] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin2' )])
  
  coef$est[3] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin3' )][1:7])
  coef$est[8] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin3' )])
  
  coef$est[4] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin4' )][1:7])
  coef$est[9] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin4' )])
  
  coef$est[5] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin5' )][1:7])
  coef$est[10] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin5' )])
  
  n_lags = 6
  cluster_var = "zip"
  varname = "regbin1"
  coef$se[1] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ," =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin2"
  coef$se[2] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ," =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin3"
  coef$se[3] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ," =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin4"
  coef$se[4] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ," =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin5"
  coef$se[5] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ," =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  
  
  
  n_lags = 6
  cluster_var = "zip"
  varname = "regbin1"
  coef$se[6] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":interaction+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":interaction` +"),collapse=""),"`l(",varname,", 6):interaction` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin2"
  coef$se[7] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":interaction+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":interaction` +"),collapse=""),"`l(",varname,", 6):interaction` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin3"
  coef$se[8] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":interaction+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":interaction` +"),collapse=""),"`l(",varname,", 6):interaction` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin4"
  coef$se[9] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":interaction+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":interaction` +"),collapse=""),"`l(",varname,", 6):interaction` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin5"
  coef$se[10] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":interaction+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":interaction` +"),collapse=""),"`l(",varname,", 6):interaction` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  
  
  coef$term = var
  return(coef)
  
}





#used for linear interactions
get_bin_coef_interaction_lh <- function(mod,var){
  
  coef <- data.frame(var = paste("regbin",rep(1:5, 2), sep=""), interaction = c(rep("low", 5), rep("high", 5)),est = NA, se = NA)
  
  coef$est[1] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin1' )][c(1:14)])
  coef$est[6] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin1' )][c(1:7,15:21)])
  
  coef$est[2] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin2' )][1:14])
  coef$est[7] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin2' )][c(1:7,15:21)])
  
  coef$est[3] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin3' )][1:14])
  coef$est[8] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin3' )][c(1:7,15:21)])
  
  coef$est[4] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin4' )][1:14])
  coef$est[9] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin4' )][c(1:7,15:21)])
  
  coef$est[5] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin5' )][1:14])
  coef$est[10] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'regbin5' )][c(1:7,15:21)])
  
  n_lags = 6
  cluster_var = "zip"
  varname = "regbin1"
  coef$se[1] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":low_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":low_group` +"),collapse=""),"`l(",varname,", 6):low_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin2"
  coef$se[2] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":low_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":low_group` +"),collapse=""),"`l(",varname,", 6):low_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin3"
  coef$se[3] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":low_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":low_group` +"),collapse=""),"`l(",varname,", 6):low_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin4"
  coef$se[4] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":low_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":low_group` +"),collapse=""),"`l(",varname,", 6):low_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin5"
  coef$se[5] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":low_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":low_group` +"),collapse=""),"`l(",varname,", 6):low_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  
  
  n_lags = 6
  cluster_var = "zip"
  varname = "regbin1"
  coef$se[6] <- as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":high_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":high_group` +"),collapse=""),"`l(",varname,", 6):high_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin2"
  coef$se[7] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":high_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":high_group` +"),collapse=""),"`l(",varname,", 6):high_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin3"
  coef$se[8] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":high_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":high_group` +"),collapse=""),"`l(",varname,", 6):high_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin4"
  coef$se[9] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":high_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":high_group` +"),collapse=""),"`l(",varname,", 6):high_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  varname = "regbin5"
  coef$se[10] <-  as.numeric((summary(glht(mod, linfct = c(paste(varname," + ",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")`",sep="+"),collapse="") ,paste("`l(",varname,", ",n_lags,")`",sep="") ,"+",varname,":high_group+",paste0(paste0("`l(",varname,", ",1:(n_lags-1),")",sep=":high_group` +"),collapse=""),"`l(",varname,", 6):high_group` =0", sep="")),vcov = vcov(mod, cluster = cluster_var ))))$test$sigma)
  
  
  coef$term = var
  return(coef)
  
}





#used for linear interactions
get_poly_coef_interaction_lh3 <- function(mod,var){
  
  coef <- data.frame(var = paste("smokePM",rep(1:4, 3), sep=""), interaction = c(rep("low", 4), rep("med", 4),rep("high", 4)),est = NA)
  
  coef$est[1] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM1' )][c(1:14)])
  coef$est[5] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM1' )][c(1:7)])
  coef$est[9] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM1' )][c(1:7,15:21)])
  
  coef$est[2] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM2' )][1:14])
  coef$est[6] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM2' )][c(1:7)])
  coef$est[10] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM2' )][c(1:7,15:21)])
  
  coef$est[3] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM3' )][1:14])
  coef$est[7] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM3' )][c(1:7)])
  coef$est[11] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM3' )][c(1:7,15:21)])
  
  coef$est[4] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM4' )][1:14])
  coef$est[8] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM4' )][c(1:7)])
  coef$est[12] <- sum(coef(mod)[grep(x = names(coef(mod)), pattern = 'smokePM4' )][c(1:7,15:21)])
  

  
  coef$term = var
  return(coef)
  
}



      get_bin_response <- function(model_object, xrange){

        x1 =  coef(model_object)[grep(x = names(coef(model_object)), pattern = "regbin1")] %>% sum()
        x2 =  coef(model_object)[grep(x = names(coef(model_object)), pattern = "regbin2")]%>% sum()
        x3 =  coef(model_object)[grep(x = names(coef(model_object)), pattern = "regbin3")]%>% sum()
        x4 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "regbin4")]%>% sum()
        x5 = coef(model_object)[grep(x = names(coef(model_object)), pattern = "regbin5")]%>% sum()

        cs <- data.frame(est = c(x1, x2, x3, x4,x5),
                         se = NA)


        cs$se[1] <- as.numeric((summary(glht(model_object, linfct = c(paste("regbin1 + `l(regbin1, ",1,")` + `l(regbin1, ",2,")`+ `l(regbin1, ",3,")`+ `l(regbin1, ",4,")`+ `l(regbin1, ",5,")`+ `l(regbin1, ",6,")` =0", sep="")),vcov = vcov(model_object, cluster = "zip" ))))$test$sigma)
        cs$se[2] <- as.numeric((summary(glht(model_object, linfct = c(paste("regbin2 + `l(regbin2, ",1,")` + `l(regbin2, ",2,")`+ `l(regbin2, ",3,")`+ `l(regbin2, ",4,")`+ `l(regbin2, ",5,")`+ `l(regbin2, ",6,")` =0", sep="")),vcov = vcov(model_object, cluster = "zip" ))))$test$sigma)
        cs$se[3] <- as.numeric((summary(glht(model_object, linfct = c(paste("regbin3 + `l(regbin3, ",1,")` + `l(regbin3, ",2,")`+ `l(regbin3, ",3,")`+ `l(regbin3, ",4,")`+ `l(regbin3, ",5,")`+ `l(regbin3, ",6,")` =0", sep="")),vcov = vcov(model_object, cluster = "zip" ))))$test$sigma)
        cs$se[4] <- as.numeric((summary(glht(model_object, linfct = c(paste("regbin4 + `l(regbin4, ",1,")` + `l(regbin4, ",2,")`+ `l(regbin4, ",3,")`+ `l(regbin4, ",4,")`+ `l(regbin4, ",5,")`+ `l(regbin4, ",6,")` =0", sep="")),vcov = vcov(model_object, cluster = "zip" ))))$test$sigma)
        cs$se[5] <- as.numeric((summary(glht(model_object, linfct = c(paste("regbin5 + `l(regbin5, ",1,")` + `l(regbin5, ",2,")`+ `l(regbin5, ",3,")`+ `l(regbin5, ",4,")`+ `l(regbin5, ",5,")`+ `l(regbin5, ",6,")` =0", sep="")),vcov = vcov(model_object, cluster = "zip" ))))$test$sigma)


        output <- cs %>% mutate(lb = est + qnorm(0.025)*se, ub = est + qnorm(0.975)*se)
        return(output)

      }


