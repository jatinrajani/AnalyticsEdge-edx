setwd("~/")
pp=read.csv("Untitled1.csv")
pp=read.csv("Untitled 1.csv")
str(pp)
pp$RS=-804.63+1584.91*(pp$SLG)+2737.77(pp$OBP)
pp$RS=-804.63+1584.91*(SLG)+2737.77*(OBP)
pp$RS=-804.63+1584.91*(pp$SLG)+2737.77*(pp$OBP)
str(pp)
model=lm(RS~OBP+SLG+Salary,data=pp)
sumary(model)
summary(model)
max(pp$RS)
(pp$RS)[979.0476]
(pp$Player.Name)[979.0476]
pp$RS
table(pp$RS,pp$Salary)
table(pp$RS,pp$Player.Name)
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012=c(94,88,95,88,93,94,98,94)
regularwins2013=c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
wins2012=c(94,88,95,88,93,94,98,97,93,94)
cor(teamRank,wins2012)
cor(teamRank,regularwins2013)
setwd("~/Mitedx/LinearRegression/NBA")
nba=read.csv("NBA_train.csv")
str(nba)
table(NBA$W,NBA$Playoffs)
table(nba$W,nba$Playoffs)
nba$PTSdiff=nba$PTS-nba$oppPTS
plot(nba$W,nba$PTSdiff)
model=lm(W~PTSdiff,data=nba)
summary(model)
pointsreg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,DATA=nba)
pointsreg=lm(PTS~ X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,data =nba)
summary(pointsreg)
SSE=sum(pointsreg$residuals^2)
SSE
RMSE=sqrt(SSE/nrow(NBA))
RMSE=sqrt(SSE/nrow(nba))
RMSE
mean(nba$PTS)
summary(pointsreg)
pointsreg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL)
pointsreg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL,data=nba)
summary(pointsreg)
SSE=sum((pointsreg$residuals)^2)
SSE
RMSE=sqrt(SSE/NROW(nba))
RMSE
testnba=read.csv("NBA_TEST.csv")
testnba=read.csv("NBA_test.csv")
predictnba=predict(pointsreg,newdata=testnba)
summary(predictnba)
SSE=sum((nba$PTS-predictnba$PTS))
SSE=sum((predictnba-pointsreg$PTS))
