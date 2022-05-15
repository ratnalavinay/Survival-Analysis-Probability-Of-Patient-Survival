

# please download and save the text file in your local
dat = read.table("LungCancer.txt",comment.char = "#",sep="",col.names = c("Treatment","Celltype","Survivaldays","Status","Karnofsky","Mondiag","Age","Pchemo"))
str(dat)

install.packages("survminer")
library(lattice)
library(survival)
library(dplyr)
library(stargazer)
library(survminer)



# frequency of data corresponding to each cell type across different treatment group
table(dat$Treatment,dat$Celltype)


histogram(~Survivaldays|as.factor(Celltype),data=dat)




histogram(~Survivaldays|as.factor(Treatment),data=dat)

# contingency table showing the number of records in each treatment group
table(dat$Treatment)


table(dat$Treatment,dat$Status)

y <- Surv(dat$Survivaldays, dat$Status) 
kmModel <- survfit(Surv(Survivaldays, Status) ~ Treatment,data=dat)
summary(kmModel)


# Kaplan-Meier survival graph
plot(kmModel, xlab="Time", ylab="Survival Probability")



# printing the mean survival days for each group
print(kmModel, print.rmean=TRUE,rmean=d$Survivaldays)



# coxph model (semi-parametric)
cox <- coxph(y ~ Mondiag + Age + Celltype + Treatment + Pchemo + Karnofsky, data=dat)
summary(cox)

# exponential model (parametric)
exp <- survreg(y ~ Mondiag + Age + Celltype + Treatment + Pchemo + Karnofsky, dist="exponential", data=dat)
summary(exp)

# weibull model (parametric)
weibull <- survreg(y ~ Mondiag + Age + Celltype + Treatment + Pchemo + Karnofsky, dist="weibull", data=dat)
summary(weibull)

# stargazer output
stargazer(cox,exp,weibull,type="text",single.row = TRUE)

