#### Preamble ####
# Purpose: Build the model to analyze data
# Author: Rayhan Walia
# Data: April 2021
# Contact: rayhan.walia@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
library(stringr)
library(car)
library(janitor)
library(visdat)
library(gridExtra)
library(reshape2)
library(MuMIn)

# Read data
total <- readr::read_csv("outputs/data/data_clean.csv")
rel_total <- readr::read_csv("outputs/data/data_rel.csv")

##VALIDATING MODEL
# create a 50/50 split in the data
set.seed(4)
train <- total[sample(1:nrow(total), 138, replace=F), ]
test <- total[which(!(total$date %in% train$date)),]

mtr <- apply(train[,c(1,3,4,5,6)], 2, mean)
sdtr <- apply(train[,c(1,3,4,5,6)], 2, sd)

mtest <- apply(test[,c(1,3,4,5,6)], 2, mean)
sdtest <- apply(test[,c(1,3,4,5,6)], 2, sd)

#percentage difference
round(abs(mtr-mtest)/mtr*100,2) #all approx less than 10%
round(abs(sdtr-sdtest)/sdtr*100,2)
#creating data frames
train <- data.frame(train)
test <- data.frame(test)

#training model
mod_tr = lm(sptsx~energy+industrial+materials+utilities,data=train)
## next time, add dependence of materials:industrials, since both are manufacturing
## also add energy:utilities, both analyzing production of energies (fuel, electricity)
summary(mod_tr)

##other models
#individual
mod_date <- lm(sptsx~date, data = train)
mod_en <- lm(sptsx~energy, data = train)
mod_ind <- lm(sptsx~industrial, data = train)
mod_mat <- lm(sptsx~materials, data = train)
mod_util <- lm(sptsx~utilities, data = train)

s1 <- summary(mod_date)
s2 <- summary(mod_en)
s3 <- summary(mod_ind)
s4 <- summary(mod_mat)
s5 <- summary(mod_util)

s <- list(s1,s2,s3,s4,s5)

#adding dependencies
mod2 <- lm(sptsx~ materials:industrial+
             energy:utilities, data=train)
summary(mod2) #not needed
vif(mod_tr)

mod3 <- lm(sptsx~ energy+ materials, data = train) #no util and indust, high vif from tr model 
summary(mod3)

mod4 <- lm(sptsx~ materials + industrial + energy + utilities + materials:industrial+
             energy:utilities, data=train) #all with interaction
summary(mod4) 

mod5 <- lm(sptsx~materials + industrial + utilities + materials:industrial+
             energy:utilities, data=train) #all with interaction w/o energy
s1 <- summary(mod5) 

mod6 <- lm(sptsx~ materials + industrial + utilities + energy:materials+
             energy:utilities, data=train) #all with interaction w/o energy w/o mat:ind since insig
s2 <- summary(mod6) #highest adjusted R squared

mod7 <- lm(sptsx~ materials + industrial + utilities + energy:materials+I(materials^2)+
             energy:utilities, data=train) #all with interaction w/o energy w/o mat:ind since insig
summary(mod7) # since materials residuals showed a slight pattern, a higher order was tested
#materials^2 insig

mod8 <- lm(sptsx~ materials + industrial + utilities + energy:materials+materials:industrial+
             energy:utilities, data=train) #all with interaction w/o energy
s3 <- summary(mod8) #with mat:ind




preds <- c(length(coef(mod5))-1, length(coef(mod6))-1)

rsq <-c(s1$adj.r.squared, s2$adj.r.squared)

#aicc
aic_c <- c(AICc(mod5), AICc(mod6))
cbind(preds, aic_c)
#8 lowest AICc, 6 lowest pred bic combo

bic <- c(BIC(mod5), BIC(mod6))
cbind(preds, bic)
#8 lowest bic, 6 lowest pred bic combo

#for table
mod_compare <- cbind(preds, round(rsq,4), round(aic_c,2), round(bic))
mod_compare <- data.frame(mod_compare)
colnames(mod_compare)[2] <- 'Adjusted R-squared'
colnames(mod_compare)[3] <- 'AICc'
colnames(mod_compare)[4] <- 'BIC'

pairs(train[,2:6])
plot(train$sptsx ~ fitted(mod6), main="Y vs Fitted", xlab="Fitted", ylab="S&P500 Price")
lines(lowess(train$sptsx ~ fitted(mod5)), lty=2)
abline(a = 0, b = 1)

#removing energy
mod_no_en <- lm(sptsx~date+ industrial+ materials+ utilities, data = train)
summary(mod_no_en)
#png('rvfit.png')
#dev.off()
#png('rvpred.png')


#png('qq.png')
qqnorm(rstandard(mod_tr))
qqline(rstandard(mod_tr))
#dev.off()
#plotting residuals
par(mfrow=c(2,3))
plot(rstandard(mod_tr)~fitted(mod_tr), xlab="fitted", ylab="Std. Residuals")
abline(a=0,b=0)
for(i in c(2:6)){
  plot(rstandard(mod_tr)~train[,i], xlab=names(train)[i], ylab="Std. Residuals")
  abline(a=0,b=0)
  #plot(rstandard(mod_trans_tr)~tr_transform[,i], xlab=names(tr_transform)[i], ylab="Residuals_tr")
  #abline(a=0,b=0)
}
#all relatively randomly distributed



#histogram of response
y <- abs(train$sptsx-mean(train$sptsx))
ggplot(data=train, aes(x=y))+geom_histogram(aes(y=..density..), bins = 15)+geom_density()

#histogram of residuals
ggplot(data=train, aes(x=rstandard(mod6)))+geom_histogram( bins = 15)+
  labs(x='Standardized residuals')

#plotting individual x against response
p <- list()
p[[1]] <- ggplot(data = train, aes(x = fitted(mod5), y = sptsx))+geom_point()+
  geom_smooth(method = 'lm', se=TRUE, formula = 'y~x')+
  labs(x='Fitted', y='S&P/TSX Price', title= 'y = x')
for(i in c(2:6)){
  if(i == 2){ #don't want to print intercept for date
    p[[i]] = ggplot(data = train, aes_string(x = train[,i], y = train$sptsx))+geom_point()+
      geom_smooth(method = 'lm', se=TRUE, formula = 'y~x')+
      labs(x=names(train)[i], y='S&P/TSX Price', title = paste('y = ', round(s[[i-1]]$coefficients[2],2), 'x', sep=''))
  }
  else{
    p[[i]] = ggplot(data = train, aes_string(x = train[,i], y = train$sptsx))+geom_point()+
      geom_smooth(method = 'lm', se=TRUE, formula = 'y~x')+
      labs(x=names(train)[i], y='S&P/TSX Price', title = paste('y = ', round(s[[i-1]]$coefficients[2],2), 'x + ',
                                                               round(s[[i-1]]$coefficients[1]), sep=''))
  }
}
do.call(grid.arrange, p) #plotting
#energy seeming problematic

#added variable plots
avPlots(mod7)

#chisq test
df_chi <- data.frame(cbind(train$sptsx, fitted(mod6)))
df_chi <- arrange(df_chi, train$sptsx)
chisq.test(df_chi,correct=T)
chisq.test(table(train$sptsx, fitted(mod6)),correct=T)

#checking for transformation
p_tr = powerTransform(cbind(materials
                           )~1,data = train)
summary(p_tr)

#validation
mod_test <- lm(sptsx~ materials + industrial + utilities + energy:materials+
             energy:utilities, data=test) #mat:en insig
summary(mod_test) 
AICc(mod_test)
BIC(mod_test)
mod_test2 <- lm(sptsx~ materials + industrial + utilities +
                 energy:utilities, data=test) 
summary(mod_test2) 

#prediction interval
#xvalues <- data.frame(Food = 20, Decor = 10, Service = 25, East = 0)
#predict(model, newdata = xvalues, interval = "confidence",level=0.95) 


