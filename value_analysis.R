library(caret)
library(dplyr)
library(ggplot2)
library(MASS)
options(scipen=999)
data = read.csv('complete.csv')
data = data[complete.cases(data),]
data$Pos = substr(data$Pos,1,2)
data = na.omit(data)
summary(log(data$Value))
hist(log(data$Value))
qqplot(log(data$Value))
par(mfrow = c(1,3))
boxplot(log(data$Value))
hist(log(data$Value))
qqnorm(log(data$Value)); qqline(log(data$Value))
# PART 1: DATA PREPARATION
col = c('Rk', 'Player', 'Nation', 'Pos', 'Squad', 'Comp',
        'Age','MP','Starts','Min','Goals', 'Shots','SoT.',
        'PasTotCmp','PasTotCmp.','Assists','PPA',
        'Sw','GCA','TklWon','Blocks','Int','Clr',
        'Err','Touches','ToAtt','ToSuc.','Carries','Rec',
        'Fld','Crs','Recov','AerWon','Value')
col2 = c('Rk', 'Player', 'Nation', 'Pos', 'Squad', 'Comp')
data = data[, col]
data1 = data

data_reg = subset(data, Min>=500)
data_sub = subset(data, Min<500)
summary(data_reg$Value)
summary(data_sub$Value)
t.test(data_reg$Value, data_sub$Value)
# p_value too small, use H1: minutes played having a significant impact on price

data = subset(data, Min>=500)

# PART 1: COMPARING VALUE BY POSITION

# Split data by position
count = data %>% group_by(Pos) %>% summarise(Total=n(), Max_Value=max(Value))
count$Pos = factor(count$Pos, levels=c('FW','MF','DF','GK'))
ggplot(count, aes(x = Pos, y = Total, fill = Pos)) + geom_bar(stat = "identity")
data_fw = subset(data, Pos=='FW')
data_mf = subset(data, Pos=='MF')
data_df = subset(data, Pos=='DF')
data_gk = subset(data, Pos=='GK')

# Plot the value boxplot
par(mfrow = c(1,4))
boxplot(data_fw$Value, main = 'Forward Value', ylim=c(0,50000000))
summary(data_fw$Value)
boxplot(data_mf$Value, main = 'Midfielder Value', ylim=c(0,50000000))
summary(data_mf$Value)
boxplot(data_df$Value, main = 'Defender Value', ylim=c(0,50000000))
summary(data_df$Value)
boxplot(data_gk$Value, main = 'Goalkeeper Value', ylim=c(0,50000000))
summary(data_gk$Value)
ggplot(count, aes(x = Pos, y = Max_Value, fill = Pos)) + geom_bar(stat = "identity")

# Plot the distribution between each position
hist(data_fw$Value, main='Forward', col='blue', breaks=seq(0,200000000,10000000), xlim=c(0,200000000)); abline(v = mean(data_fw$Value), col = "red")
hist(data_mf$Value, main='Midfielder', col='darkgreen', breaks=seq(0,200000000,10000000), xlim=c(0,200000000)); abline(v = mean(data_mf$Value), col = "red")
hist(data_df$Value, main='Defender', col='orange', breaks=seq(0,200000000,10000000), xlim=c(0,200000000)); abline(v = mean(data_df$Value), col = "red")
hist(data_gk$Value, main='Goalkeeper', col='yellow', breaks=seq(0,200000000,10000000), xlim=c(0,200000000)); abline(v = mean(data_gk$Value), col = "red")


# PART 3: ANALYZING IMPORTANT SKILLS OF A FORWARD
data_fw_test = data_fw[,-seq(1,6)]
nor=function(x){(x - min(x))/(max(x)-min(x))}
data_fw_test[,]=sapply(data_fw_test[,],nor)

corr = cor(data_fw_test, data_fw_test$Value)
corr_fw = as.data.frame(corr)
skills_fw = subset(corr_fw, V1>=0.25)

# Most important: Goals, Shots, Attacking Touches, Ball Control
# Second important: Chance Creating, Passing, Ball Carries

# PART 4: ANALYZING IMPORTANT SKILLS OF A MIDFIELDER
data_mf_test = data_mf[,-seq(1,6)]
nor=function(x){(x - min(x))/(max(x)-min(x))}
data_mf_test[,]=sapply(data_mf_test[,],nor)

corr = cor(data_mf_test, data_mf_test$Value)
corr_mf = as.data.frame(corr)
skills_mf = subset(corr_mf, V1>=0.25)

# Most important: Ball Control, Goals/Assist, Chance Creating
# Second important: Passing, Middle Touches, Ball Carries

# PART 5: ANALYZING IMPORTANT SKILLS OF A DEFENDER
data_df_test = data_df[,-seq(1,6)]
nor=function(x){(x - min(x))/(max(x)-min(x))}
data_df_test[,]=sapply(data_df_test[,],nor)

corr = cor(data_df_test, data_df_test$Value)
corr_df = as.data.frame(corr)
skills_df = subset(corr_mf, V1>=0.25)
# Most important: Ball Control, Chance Creating, Ball Carries
# Second important: Defensive Skills, Attack Contribution, Passing

# PART 6: TESTING LINEAR REGRESSION
set.seed(100)
training.idx = sample(1:nrow(data_fw_test), size=nrow(data_fw_test)*0.8)
train.data = data_fw_test[training.idx,]
test.data = data_fw_test[-training.idx,]
linreg = lm(Value~., data=train.data)
summary(linreg)
prediction = predict(linreg, test.data)
RMSE(prediction, test.data$Value)
sd(test.data$Value)
# 58% variability of FW Value can be described

set.seed(100)
training.idx = sample(1:nrow(data_mf_test), size=nrow(data_mf_test)*0.8)
train.data = data_mf_test[training.idx,]
test.data = data_mf_test[-training.idx,]
linreg = lm(Value~., data=train.data)
summary(linreg)
prediction = predict(linreg, test.data)
RMSE(prediction, test.data$Value)
sd(test.data$Value)
# 53% variability of MF Value can be described

set.seed(100)
training.idx = sample(1:nrow(data_df_test), size=nrow(data_df_test)*0.8)
train.data = data_df_test[training.idx,]
test.data = data_df_test[-training.idx,]
linreg = lm(Value~., data=train.data)
summary(linreg)
prediction = predict(linreg, test.data)
RMSE(prediction, test.data$Value)
sd(test.data$Value)
# 41% variability of DF Value can be described 
# Conclusion: Linear Regression best fit for FW, not very good for MF/DF

# PART 7: OTHER STATISTICAL TESTS

# QQ-plot for checking normality: Log follows normal distribution
qqnorm(log(data_fw$Value)); qqline(log(data_fw$Value))
qqnorm(log(data_mf$Value)); qqline(log(data_mf$Value))
qqnorm(log(data_df$Value)); qqline(log(data_df$Value))
shapiro.test(log(data_mf$Value))
var.test(log(data_fw$Value),log(data_mf$Value))


# PART 8: MULTIPLE LINEAR REGRESSION FOR ALL
data2 = data1[,-seq(1:6)]
data2[,-28] = sapply(data2[,-28], nor)
data2$Value = sapply(data2$Value, log)
corr_data2 = as.data.frame(cor(data2, log(data2$Value)))

set.seed(100)
training.idx = sample(1:nrow(data2), size=nrow(data2)*0.8)
train.data = data2[training.idx,]
test.data = data2[-training.idx,]
linreg = lm(Value~., data=train.data)
summary(linreg)

hist(log(data$Value))

var.test(data_df$Value, data_gk$Value)
t.test(data_fw$Value, data_mf$Value)

cor.test(log(data$Value), data$Sw)
var.test(data_sub$Value, data_reg$Value)
boxplot(data_reg$Value); boxplot(data_sub$Value)

# PART 9: IMPROVED LINEAR REGRESSION
model = lm(log(Value)~., data=data2)
step(model, direction='backward')
summary(step(model, direction='backward'))

boxcox(log(data2$Value)~., data = data2)
# PART 10: COMPARE REGULAR AND IRREGULAR PLAYERS
par(mfrow = c(1,2))
boxplot(data_reg$Value, main='Regular', ylim=c(0,100000000))
boxplot(data_sub$Value, main='Substitution', ylim=c(0,100000000))

# LINEAR REGRESSION WITH BACKWARD ELIMINATION
model5 =lm(log(data2$Value) ~ ., data=data2)
step(model5, direction="backward") 

data_fw = data_fw[,-seq(1,6)]
model = lm(log(data_fw$Value)~., data=data_fw)
step(model, direction='backward')

data_mf = data_mf[,-seq(1,6)]
model = lm(log(data_mf$Value)~., data=data_mf)
step(model, direction='backward')
