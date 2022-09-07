#Redback tyres

library(ggplot2)
library(reshape2)
library(dplyr)
library(class)
library(MASS)
library(caret)
library(devtools)
library(countreg)
library(forcats)
library("AER")
library(pscl)
library(goft)
library(nortest)
library(Metrics)
library(leaps)
library(car)
library(ggpubr)
library(glmnet)

tyre_data <- read.csv("B2356raw10.csv")
attach(tyre_data)


#EDA graphs

ggplot(tyre_data, aes(x=s, y = kph)) +
  geom_point()
ggsave(file="kph.png")

ggplot(tyre_data, aes(x=s, y = rpm)) +
  geom_point()
ggsave(file="rpm.png")

ggplot(tyre_data, aes(x=s, y = deg)) +
  geom_point()
ggsave(file="deg.png")

ggplot(tyre_data, aes(x=s, y = deg.1)) +
  geom_point()
ggsave(file="deg1.png")

ggplot(tyre_data, aes(x=s, y = cm)) +
  geom_point()
ggsave(file="cm.png")

ggplot(tyre_data, aes(x=s, y = cm.1)) +
  geom_point()
ggsave(file="cm1.png")

ggplot(tyre_data, aes(x=s, y = kPa)) +
  geom_point()
ggsave(file="kPa.png")

ggplot(tyre_data, aes(x=s, y = N)) +
  geom_point()
ggsave(file="N.png")

ggplot(tyre_data, aes(x=s, y = N.1)) +
  geom_point()
ggsave(file="N1.png")

ggplot(tyre_data, aes(x=s, y = N.2)) +
  geom_point()
ggsave(file="N2.png")

ggplot(tyre_data, aes(x=s, y = Nm)) +
  geom_point()
ggsave(file="Nm.png")

ggplot(tyre_data, aes(x=s, y = Nm.1)) +
  geom_point()
ggsave(file="Nm1.png")

ggplot(tyre_data, aes(x=s, y = none)) +
  geom_point()
ggsave(file="none.png")

ggplot(tyre_data, aes(x=s, y = none.1)) +
  geom_point()
ggsave(file="none1.png")

ggplot(tyre_data, aes(x=s, y = deg.c)) +
  geom_point()
ggsave(file="degc.png")

ggplot(tyre_data, aes(x=s, y = deg.c.1)) +
  geom_point()
ggsave(file="degc1.png")

ggplot(tyre_data, aes(x=s, y = deg.c.2)) +
  geom_point()
ggsave(file="degc2.png")

ggplot(tyre_data, aes(x=s, y = deg.c.3)) +
  geom_point()
ggsave(file="degc3.png")

ggplot(tyre_data, aes(x=s, y = deg.c.4)) +
  geom_point()
ggsave(file="degc4.png")

ggplot(tyre_data, aes(x=s, y = SAE)) +
  geom_point()
ggsave(file="SAE.png")


# All Subsets Regression - degc2
leapsDEG <- regsubsets(deg.c.2~s+kph+rpm+deg+deg.1+in.+in.1+kPa+N+N.1+N.2+Nm+Nm.1+none+
                    none.1+deg.c+deg.c.1+SAE, data = tyre_data, nbest=15)
# view results
summary(leapsDEG)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leapsDEG,scale="bic")
# plot statistic by subset size
subsets(leapsDEG, statistic="bic")

# Cross validation for number of predictors
k <- 10
n <- nrow(tyre_data)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 15)

# Regression for degc2 (tire surface temp??) Can we use deg3 and deg4???
options(scipen = 999)
lmdeg = lm(deg.c.2~s+kph+rpm+deg+deg.1+in.+in.1+kPa+N+N.1+N.2+Nm+Nm.1+none+
               none.1+deg.c+deg.c.1+SAE, data = tyre_data)
summary(lmdeg)

# using full model, there are insignificant predictors

lmdegtrim = lm(deg.c.2~s+kph+rpm+deg+deg.1+in.+kPa+N+N.1+N.2+Nm+Nm.1+none+
                 none.1+deg.c+deg.c.1+SAE, data = tyre_data)
summary(lmdegtrim)

# Temp model RSE, 0.8% error rate
sigma(lmdegtrim)/mean(tyre_data$deg.c.2)

plot(tyre_data, pch = 10, col = "blue") #Plot the results
abline(lmspeedtrim)


########################## Pressure analysis ##############################

p<-ggplot(tyre_data, aes(x=kPa)) +
  geom_histogram(position="identity", bins = 100) +
  theme(legend.position="right")+
  labs(x = "kPa", y = "Count") +
  ggtitle("kPa analysis")
p

kPa_mid <- tyre_data %>%
  filter(kPa > 80 & kPa < 85)


ggqqplot(kPa_mid$kPa)

# Correlartion heatmap
cormid <- round(cor(kPa_mid),2)
melted_cormid <- melt(cormid)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormid){
  cormid[upper.tri(cormid)] <- NA
  return(cormid)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormid){
  cormid[lower.tri(cormid)]<- NA
  return(cormid)
}

upper_tri <- get_upper_tri(cormid)
melted_cormid <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormid, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


kpamid_model <- glm(kPa ~ s + kph + rpm + deg
    + deg.1 + cm + cm.1 + N + N.1 + N.2 + Nm + Nm.1 + none + none.1 + deg.c
    + deg.c.1 + deg.c.2 + deg.c.3 + deg.c.4 + SAE,
    family=gaussian(link="identity"), data= kPa_mid)

summary(kpamid_model)
plot(kpamid_model)

# Normal Lasso
x <- model.matrix(kPa ~ s + kph + rpm + deg
                  + deg.1 + cm + cm.1 + N + N.1 + N.2 + Nm + Nm.1 + none + none.1 + deg.c
                  + deg.c.1 + deg.c.2 + deg.c.3 + deg.c.4 + SAE,
                  family=gaussian(link="identity"), data= kPa_mid)[,-1]
y <- kPa_mid$kPa

grid <- 10^seq(10, -2, length = 100)
lasso <- glmnet(x,y, alpha=1, lambda=grid, family = gaussian())
set.seed(1)
cv.out <- cv.glmnet(x,y, alpha=1, lambda = grid, family=gaussian())
plot(cv.out)
cv.out$lambda.min
bestlam <- cv.out$lambda.min
lass.coef <- predict(lasso, type= "coefficients", s=bestlam)
lass.coef

kpamid_model_lasso <- glm(kPa ~ s + kph + deg
                    + N.1 + N.2 + Nm + Nm.1 + deg.c
                    + deg.c.2 + deg.c.3 + deg.c.4,
                    family=gaussian(link="identity"), data= kPa_mid)

summary(kpamid_model_lasso)













# Regression for kPa 
options(scipen = 999)
lmkPa = lm(kPa~s+kph+rpm+deg+deg.1+in.+in.1+N+N.1+N.2+Nm+Nm.1+none+
             none.1+deg.c+deg.c.1+deg.c.2+deg.c.3+deg.c.4+SAE, data = tyre_data)
summary(lmkPa)

# using full model, there are insignificant predictors

lmkPatrim = lm(kPa~s+kph+rpm+deg+deg.1+in.+N+N.1+N.2+Nm+Nm.1+none+
                 none.1+deg.c+deg.c.1+deg.c.2+deg.c.3+deg.c.4+SAE, data = tyre_data)
summary(lmkPatrim)

# Temp model RSE, 6% error rate
sigma(lmdegtrim)/mean(tyre_data$kPa)

