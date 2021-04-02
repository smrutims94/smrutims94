d=read.csv("C:\\smruti\\IIM analytics course\\feb14\\IPL_BATS.csv", header=T)
str(d)

#missing value summary
is.na(d)
colSums(is.na(d))

hist(d$Salary, col = 'blue')
d$log.Salary= log(d$Salary)
str(d)

install.packages("tree")
library(tree)

#training data set- run each line
set.seed(3)
train=sample(1:103,80)
train

#Regression tree
IPL.tree= tree(log.Salary~Type+Capped+Matches+Runs_Scored+Bat_Average+Strike_Rate+Hundred+Fifties+Fours+Sixes+Catches, data=d, subset=train)

#plot the tree
plot(IPL.tree, lwd=2)
text(IPL.tree, pretty = 0, col="red", font=1)

#8 fold cross validation
set.seed(2)
IPL.cv= cv.tree(IPL.tree, K=8)
IPL.cv

plot(IPL.cv$size, IPL.cv$dev, type="b", xlab = "Number of Terminal nodes", ylab = "MSE", col="green", lwd=2)

#MSE is min at 6 nodes. Hence treee with 6 terminal nodes is optimal.
#Prune a tree to 6 nodes
IPL.prune= prune.tree(IPL.tree, best=6)

#plot the pruned tree
plot(IPL.prune, lwd=2)
text(IPL.prune, pretty=0, col="red", font=4)

#Use pruned tree for predicton
log.Salary.pred= predict(IPL.prune, newdata = d[-train,])
Salary.pred= exp(log.Salary.pred)

Salary.act= d[-train,"Salary"]

#Calculate RMSE
MSE<-mean((Salary.pred-Salary.act)^2)
RMSE<-sqrt(MSE)
RMSE
#RMSE value is too large. Model is not accurate.

#Plot actual and prediscted salary
plot(Salary.pred/10^6, Salary.act/10^6, xlab = "Predicted Salary(in million dollars)", ylab="Actual salary (in million dollars)")
abline(0,1,col="blue", lwd=2)

#Result of prediction- not accurate. So, 