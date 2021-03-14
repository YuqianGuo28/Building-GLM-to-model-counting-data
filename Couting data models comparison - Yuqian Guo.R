library(readr)
library(ggplot2)
library(MASS)
library(dplyr)
library(moderndive)
library(ISLR)
library(skimr)
library(plotly)
library(tidyr)
library(kableExtra)
library(gridExtra)
library(broom)
library(jtools)
library(janitor)
library(infer)
library(ggpubr)
library(AER)
library(pscl)
library(huxtable)
library(sjPlot)
library(countreg)
library(ggstance)
#Dataset
data(Affairs)
Affairs1<-na.omit(Affairs[,1:9])
#glimpse(Affairs1)

#Exploratory analysis

#Table of summary statistics of number of affairs by gender.
Affairs1%>%
  group_by(gender)%>%
  summarise(n=n(),Mean=round(mean(affairs),digits=2), St.Dev=round(sd(affairs),digits=2),
Min=min(affairs), Q1 = quantile(affairs,0.25), Median=median(affairs),
Q3 = quantile(affairs,0.75), Max=max(affairs))%>%
kable(caption = '\\label{tab:summaffgen} Summary statistics of number of affairs by gender.') %>%
kable_styling(latex_options = "hold_position", font_size = 10)

#Boxplot of numbers of affairs by whether have children or not.
par(mfrow=c(2, 2))
clog <- function(x) log(x + 0.5)
ggplot(Affairs1, aes(x = gender, y = clog(affairs))) +
geom_boxplot() +
labs(x = "Gender", y = "The log value of the number of affairs")

#Table of summary statistics of number of affairs by whether having children or not.
Affairs1%>%
  group_by(children)%>%
  summarise(n=n(),Mean=round(mean(affairs),digits=2), St.Dev=round(sd(affairs),digits=2),Min=min(affairs), Q1 = quantile(affairs,0.25), Median=median(affairs),Q3 = quantile(affairs,0.75), Max=max(affairs))%>%
  kable(caption = '\\label{tab:summaffachi} Summary statistics of number of affairs by whether having children or not.') %>%
  kable_styling(latex_options = "hold_position", font_size = 10)

#Boxplot of numbers of affairs by whether having children or not.
ggplot(Affairs1, aes(x = children, y = clog(affairs))) +
  geom_boxplot() +
  labs(x = "Children", y = "The log value of the number of affairs")
        
#Boxplot of numbers of affairs by rating, religiousness, education and occupation.
Affairs2 <- Affairs1%>%
  mutate(rating = as.factor(rating),religiousness = as.factor(religiousness),education = as.factor(education), occupation = as.factor(occupation))
par(mfrow=c(2, 2))
plot(clog(affairs) ~ rating, data = Affairs2)
plot(clog(affairs) ~ religiousness, data = Affairs2)
plot(clog(affairs) ~ education, data = Affairs2)
plot(clog(affairs) ~ occupation, data = Affairs2)
        
#Spinograms of numbers of affairs by age, years of marriage, education and occoupation.
cfac <- function(x, breaks = NULL) {
  if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
  x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
  levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),sep = "")
  return(x)
  }
par(mfrow=c(1, 2))
plot(cfac(affairs) ~ age, data = Affairs1, breaks = 9)
plot(cfac(affairs) ~ yearsmarried, data = Affairs1, breaks = 8)

#Table of correlation among all the numeric variables.
Affairs1%>%
  select(affairs,age,yearsmarried,religiousness,education,occupation,rating)%>%
  cor()%>%
  kable(caption = '\\label{tab:corr} Correlation among all the numeric variables.', digits = 3) %>%
  kable_styling(latex_options = 'HOLD_position', font_size = 10)

# Formal Analysis
## Variable and model Selection

#fullmodel
posfulmodel <- glm(affairs~.,family=poisson, data=Affairs1)
nbfulmodel <- MASS::glm.nb(affairs~., data=Affairs1)
zinbfulmodel <- zeroinfl(affairs~., data=Affairs1, dist = "negbin",model = TRUE, y = TRUE, x = FALSE)
hurnbfulmodel <- hurdle(affairs ~., data=Affairs1, dist = "negbin",model = TRUE, y = TRUE, x = FALSE)
       
#AIC-based backward selection 
backward.posmodel<- step(posfulmodel,direction = "backward")
backward.nbmodel<- step(nbfulmodel,direction = "backward")
backward.zinbmodel<- step(zinbfulmodel,direction = "backward")
backward.hurnbmodel<- step(hurnbfulmodel,direction = "backward")

#Plotting summary of the basic Poisson model after variable selection.
par(mfrow=c(1,2))
jtools::plot_summs(backward.posmodel)
        
#over-dispersionchecking of Poisson model
dispersiontest(backward.posmodel)

#Frequency distribution for number of affairs.
plot(table(Affairs1$affairs))
        
#modelcomparision
vuong(backward.posmodel,backward.nbmodel)
vuong(backward.nbmodel,backward.zinbmodel) 
vuong(backward.nbmodel,backward.hurnbmodel)
vuong(backward.zinbmodel,backward.hurnbmodel)

#Model comparison of AIC and BIC values
Models <- c('Basic Poisson model', 'Negative binomial model','Zero-inflated NB model','NB hurdle model') 
AIC<- rbind(AIC(backward.posmodel),AIC(backward.nbmodel),AIC(backward.zinbmodel),AIC(backward.hurnbmodel))
BIC<- rbind(BIC(backward.posmodel),BIC(backward.nbmodel),BIC(backward.zinbmodel),BIC(backward.hurnbmodel))
df <- data.frame(A=Models,B=AIC,C=BIC)
knitr::kable(df, col.names = c("Model","AIC value","BIC value"),caption = '\\label{tab:compmod} Model comparison values.', digits = 3) %>%
  kable_styling(latex_options='HOLD_position',font_size = 10)

        
#NB Hurdle model summary after AIC-based backward selection
summary(backward.hurnbmodel)

#Additive NB hurdle model and interation NB hurdle model
hurnbaddmodel<- hurdle(affairs ~ age + yearsmarried + religiousness + rating|age + yearsmarried + religiousness + rating, data=Affairs1, dist = "negbin",model = TRUE, y = TRUE, x = FALSE)
hurnbintmodel<- hurdle(affairs ~ age*yearsmarried + religiousness + rating|age * yearsmarried + religiousness + rating, data=Affairs1, dist = "negbin",model = TRUE, y = TRUE, x = FALSE)
        
#Wald test between NB hurdle model before and after variable selection
waldtest(backward.hurnbmodel,hurnbaddmodel)

## Comparison between additive model and interaction model
Models <- c('NB hurdle(age+yearsmarried+religiousness+rating)','NB hurdle(age*yearsmarried+religiousness+rating)') 
AIC<- rbind(AIC(hurnbaddmodel),AIC(hurnbintmodel))
BIC<- rbind(BIC(hurnbaddmodel),BIC(hurnbintmodel))
df <- data.frame(A=Models,B=AIC,C=BIC)
knitr::kable(df, col.names = c("Model","AIC value","BIC value"),caption = '\\label{tab:addvsint} Model comparison values between model without interaction term and model with interaction term.', digits = 3) %>%
  kable_styling(latex_options='HOLD_position',font_size = 10)

# log-likelihood value
stats::logLik(hurnbaddmodel)
stats::logLik(backward.posmodel)

## Exponentiated coefficients
hurdlePart <- glm(formula = I(affairs>0) ~ age+yearsmarried+religiousness+rating,
                              data    = Affairs1,
                              family  = binomial(link = "logit"))
expCoef <- exp(coef((hurnbaddmodel)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(hurdlePart))
colnames(expCoef) <- c("Count_model","Zero_hurdle_model")
expCoef%>%
  kable(caption = '\\label{tab:mstable7} Exponentiated coefficients of the additive hurdle model(final model)', digits = 3) %>%
  kable_styling(latex_options = 'HOLD_position', font_size = 10)

#Assessing the model fit
par(mfrow = c(3, 2))
rootogram(backward.posmodel,style = "hanging")
rootogram(backward.nbmodel,style = "hanging")
rootogram(backward.zinbmodel,style = "hanging")
rootogram(backward.hurnbmodel,style = "hanging")
rootogram(hurnbaddmodel,style = "hanging")
rootogram(hurnbintmodel,style = "hanging")
