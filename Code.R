#First unzip the Dataset file and place the xlsx file in the Working directory
#setting wroking directory
setwd('C:/Users/toami/OneDrive/Desktop/Data Science/SEM1/Probability & Stats/CA2')
par(mar=c(1,1,1,1))
#clearing environment
rm(list = ls())

#installing required packages
needed_packages <- c("pastecs", "ggplot2", "semTools", "FSA","dplyr",
                     "tidyr","outliers","ggplot2", "readxl")                                    
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
if(length(not_installed)) install.packages(not_installed)                              
library(pastecs) 
library(semTools)

#loading library
library(dplyr)
library(tidyr)
library(outliers)
library(ggplot2)
library(lm.beta)
library(stargazer)
library(readxl)

#reading xlsx file
student=read_excel("data_academic_performance.xlsx", sheet = "SABER11_SABERPRO")

#DATA CLEANING
#removing blank column
student = subset(student, select = -c(...10) )

#checking for NA values
student[is.na(student)==TRUE]

#Checking for duplicate rows
which(duplicated(student))

#removing duplicates
student=student[!duplicated(student),]

#variables of interest
#Nature of School(SCHOOL_NAT)
#Critical Reading(CR_PRO)
#Citizen Competencies SPRO(CC_PRO)
#English(ENG_PRO)

#summary (CR_PRO)
summary(student$CR_PRO)
sd(student$CR_PRO)
var(student$CR_PRO)
Outlier = boxplot(student$CR_PRO)$out
print(unique(Outlier))

#summary (ENG_PRO)
summary(student$ENG_PRO)
sd(student$ENG_PRO)
var(student$ENG_PRO)
Outlier = boxplot(student$ENG_PRO)$out
print(unique(Outlier))

#summary (CC_PRO)
summary(student$CC_PRO)
sd(student$CC_PRO)
var(student$CC_PRO)
Outlier = boxplot(student$CC_PRO)$out
print(unique(Outlier))

#summary SCHOOL_NAT
summary(student$SCHOOL_NAT)
table(student$SCHOOL_NAT)

#===============================================================================
#NORMALITY CHECK OF VARIABLES OF INTEREST
#NORMALITY TEST of CR_PRO
gg <- ggplot(student, aes(x=student$CR_PRO))
gg <- gg + labs(x="CR_PRO")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",
                         args=list(mean=mean(student$CR_PRO, na.rm=TRUE), 
                                   sd=sd(student$CR_PRO, na.rm=TRUE)))
#Plotting Histogram
gg

#Creating qqplot
qqnorm(student$CR_PRO)
qqline(student$CR_PRO, col=2)

#Summary Statistics
pastecs::stat.desc(student$CR_PRO, basic=F)

#skew
tpskew<-semTools::skew(student$CR_PRO)
tpskew[1]/tpskew[2]

#kurtosis
tpkurt<-semTools::kurtosis(student$CR_PRO)
tpkurt[1]/tpkurt[2]

ztpCR_PRO<- abs(scale(student$CR_PRO))

#how much data fall outside 95% of region
FSA::perc(as.numeric(ztpCR_PRO), 1.96, "gt")

#how much data fall outside 99.9% of region
FSA::perc(as.numeric(ztpCR_PRO), 3.29, "gt")

#-------------------------------------------------------------------------------
#NORMALITY TEST of ENG_PRO
gg <- ggplot(student, aes(x=student$CR_PRO))
gg <- gg + labs(x="ENG_PRO")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",
                         args=list(mean=mean(student$ENG_PRO, na.rm=TRUE), 
                                   sd=sd(student$ENG_PRO, na.rm=TRUE)))
#Plotting Histogram
gg

#Creating qqplot
qqnorm(student$ENG_PRO)
qqline(student$ENG_PRO, col=2)

#Summary Statistics
pastecs::stat.desc(student$ENG_PRO, basic=F)

#skew
tpskew<-semTools::skew(student$ENG_PRO)
tpskew[1]/tpskew[2]

#kurtosis
tpkurt<-semTools::kurtosis(student$ENG_PRO)
tpkurt[1]/tpkurt[2]

ztpENG_PRO<- abs(scale(student$ENG_PRO))

#how much data fall outside 95% of region
FSA::perc(as.numeric(ztpENG_PRO), 1.96, "gt")

#how much data fall outside 99.9% of region
FSA::perc(as.numeric(ztpENG_PRO), 3.29, "gt")

#------------------------------------------------------------------------------
#NORMALITY TEST of CC_PRO
gg <- ggplot(student, aes(x=student$CR_PRO))
gg <- gg + labs(x="CC_PRO")
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gg <- gg + stat_function(fun=dnorm, color="red",
                         args=list(mean=mean(student$CC_PRO, na.rm=TRUE), 
                                   sd=sd(student$CC_PRO, na.rm=TRUE)))
#Plotting Histogram
gg

#Creating qqplot
qqnorm(student$CC_PRO)
qqline(student$CC_PRO, col=2)

#Summary Statistics
pastecs::stat.desc(student$CC_PRO, basic=F)

#skew
tpskew<-semTools::skew(student$CC_PRO)
tpskew[1]/tpskew[2]

#kurtosis
tpkurt<-semTools::kurtosis(student$CC_PRO)
tpkurt[1]/tpkurt[2]

ztpCC_PRO<- abs(scale(student$CC_PRO))

#how much data fall outside 95% of region
FSA::perc(as.numeric(ztpCC_PRO), 1.96, "gt")

#how much data fall outside 99.9% of region
FSA::perc(as.numeric(ztpCC_PRO), 3.29, "gt")
#============================================================================
#CORRELATION CHECK AMONG VARIABLES OF INTEREST
#correlation between CR_PRO and CC_PRO
#Scatterplot
scatter <- ggplot(student, aes(CR_PRO, CC_PRO))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "CR_PRO", y = "CC_PRO") 

#Pearson Correlation
stats::cor.test(student$CR_PRO, student$CC_PRO, method='pearson')

#correlation between ENG_PRO and CC_PRO
#Scatterplot
scatter <- ggplot(student, aes(ENG_PRO, CC_PRO))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "ENG_PRO", y = "CC_PRO") 

#Pearson Correlation
stats::cor.test(student$ENG_PRO, student$CC_PRO, method='pearson')

#correlation between ENG_PRO and CR_PRO
#Scatterplot
scatter <- ggplot(student, aes(ENG_PRO, CR_PRO))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "ENG_PRO", y = "CR_PRO") 

#Pearson Correlation
stats::cor.test(student$ENG_PRO, student$CR_PRO, method='pearson')

#===========================================================================================
#MULTIPLE REGRESSION MODEL 1
#building multiple regression model
model1<-lm(student$CC_PRO~student$ENG_PRO+student$CR_PRO)

#Analysis of Variance Table
anova(model1)

#plots
plot(model1,1)
plot(model1,2)
plot(model1,3)

#Calculate Collinearity
vifmodel1<-car::vif(model1)
vifmodel1

#Calculate tolerance
1/vifmodel1

#summary
summary(model1)

#coefficients
 #Will allow us to isolate the beta co-efficients
lm.beta(model1)

#Tidy output of all the required stats
#For formatting outputs/tables
stargazer(model1, type="text")

#==============================================================================
#MULTIPLE REGRESSION MODEL 2
#building multiple regression model
model2<-lm(student$CC_PRO~student$ENG_PRO+student$CR_PRO+student$SCHOOL_NAT)

#Analysis of Variance Table
anova(model2)

#plots
plot(model2,1)
plot(model2,2)
plot(model2,3)

#Calculate Collinearity
vifmodel2<-car::vif(model2)
vifmodel2

#Calculate tolerance
1/vifmodel2

#summary
summary(model2)

#coefficients
lm.beta(model2)

#Tidy output of all the required stats
stargazer(model2, type="text")
#==============================================================================
#MODEL COMPARISON
anova(model1,model2)
#END
