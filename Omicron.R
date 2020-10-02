#---------------------------------------------------------
#
# FILE NAME: Churn.R
#
# Author: BGL
#
# Date: 2020.10.01
#
# Comments: r script to support IESE Churn Case - EMBA MUC 2021
#
# Modifications
#
#--------------------------------------------------------


#IN CASE YOU NEED TO INSTALL THE PACKAGE FIRST
#install.packages()
library(readxl)
library(tidyverse)
library(ggplot2)
library(gplots)
library(rpart)
library(rpart.plot)
library(reshape2)
library(gtools)
library(caTools)



d <-  read_xlsx("omicron.xlsx", sheet="Data")
meta_d= read_xlsx("omicron.xlsx", sheet="Description")

summary(d)

d

# OTCALL
mod1 <- lm(CHURN ~ OTCALL, data=d)
mod1
summary(mod1)


# ACLENGTH
mod2 <- lm(CHURN ~ ACLENGTH, data=d)
mod2
summary(mod2)

# INTPLAN
mod3 <- lm(CHURN ~ INTPLAN, data=d)
mod3
summary(mod3)

# DATAPLAN
mod4 <- lm(CHURN ~ DATAPLAN, data=d)
mod4
summary(mod4)

# DATAGB
mod5 <- lm(CHURN ~ as.factor(DATAGB), data=d)
mod5
summary(mod5)

# OMMIN
mod6 <- lm(CHURN ~ OMMIN, data=d)
mod6
summary(mod6)

# OMMIN
mod7 <- lm(CHURN ~ OMCALL, data=d)
mod7
summary(mod7)

# NGMIN
mod8 <- lm(CHURN ~ NGMIN, data=d)
mod8
summary(mod8)

# NGCALL
mod9 <- lm(CHURN ~ NGCALL, data=d)
mod9
summary(mod9)

# IMIN
mod10 <- lm(CHURN ~ IMIN, data=d)
mod10
summary(mod10)

# ICALL
mod11 <- lm(CHURN ~ ICALL, data=d)
mod11
summary(mod11)

# CUSCALL
mod12 <- lm(CHURN ~ CUSCALL, data=d)
mod12
summary(mod12)

mod1
mod2
mod3
mod4
mod5
mod6
mod7
mod8
mod9
mod10
mod11
mod12



# Building global model
global_m=lm(CHURN ~ INTPLAN+DATAPLAN+as.factor(DATAGB)+OMMIN+OMCALL+OTMIN+OTCALL+NGMIN+NGCALL+IMIN+ICALL+CUSCALL, data=d)
global_m
summary(global_m)

# corr matrix --> detect colinear "noisy" variables
cor_table=cor(d[,c(2:4,6:15)])

# HEAT MAP
# Negative correlations are shown in blue and the positive ones in red
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor_table, col=col, symm=TRUE)

# Building sub models with relevant variables only
sub_m=lm(CHURN ~ INTPLAN+OMMIN+OTMIN+IMIN+CUSCALL, data=d)
summary(sub_m)

sub_m2=lm(CHURN ~ INTPLAN+CUSCALL, data=d)
summary(sub_m2)

sub_m3=lm(CHURN ~ INTPLAN+OMMIN+CUSCALL, data=d)
summary(sub_m3)

# predictions
pred <- predict(sub_m) 
resid <- residuals(sub_m) 
plot(resid, pch=20)

# BUILIDNG THE CLASSIFICATION TREE

tree=rpart(CHURN ~ INTPLAN+OMMIN+OTMIN+IMIN+CUSCALL,data=d,method='class',
           control=rpart.control(minsplit=1))
tree

prp(tree)

rpart.rules(tree, nn=TRUE)

ggplot(data=d)+
  geom_bar(aes(x=INTPLAN,fill=INTPLAN))

sub_w_datagb=lm(CHURN ~ INTPLAN+as.factor(DATAGB)+OMMIN+CUSCALL, data=d)
summary(sub_w_datagb)


# USING LOGIT REGRESSION FOR CLASSIFICATION
# STEP1, transform predictions into zeros and ones. In our case this is already
# the case because CHURN variable is binary. If not binary then you convert
# the variables using a cuttoff value
# Here is an example for big or small calls OMMIN classification
#
summary(d$OMMIN)
# It is observed that 179,5 is the median. So I will use as cutoff value 180.
d$OMMIN_bin=ifelse(d$OMMIN>=180,1,0)
# checking if variable binary is properly created
summary(d$OMMIN_bin)
#

# AMOUNT OF PEOPLE THAT HAVE THE INTPLAN
sum(d$INTPLAN)
# proportion of people that have the intplan
sum(d$INTPLAN)/nrow(d)




