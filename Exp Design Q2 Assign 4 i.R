# Experimental Design Hw number 4


# Question 2


Adjunct<-read.table(file.choose(),header=TRUE,sep="\t")
names(Adjunct)
sapply(Adjunct,is.factor) #Subject.Matter and Highest.Degree should be factors

Adjunct$Subject.Matter<-factor(Adjunct$Subject.Matter)
Adjunct$Highest.Degree<-factor(Adjunct$Highest.Degree)

sapply(Adjunct,is.factor)
summary(Adjunct)

dim(Adjunct)
45/4 

#No the study is not balanced; different number of members in each subject matter as well as in each highest degree category 


#b)

library(ggplot2)
ggplot(Adjunct) + aes(x =Subject.Matter , color = Highest.Degree, group = Highest.Degree, y = Earnings) + stat_summary(fun.y = mean, geom = "line") + geom_point()+ theme_bw()+ ggtitle("Adjunct Professors Interactions Plot")

#c) 



model.int.adj<-aov(Earnings~Subject.Matter*Highest.Degree,data=Adjunct)
drop1(model.int.adj, .~., test="F")
ss<-drop1(model.int.adj, .~., test="F")[[2]]
ssE<-sum((model.int.adj$residuals)^2)
ssE
ss[-1]/(ss[-1]+ssE)

eta.p.sq<-ss[-1]/(ss[-1]+ssE) ## size of main effects relative to the size of the interaction effects
eta.p.sq[1:2]/eta.p.sq[3]

#additive model, no interactions: 
model.adj<-aov(Earnings~Subject.Matter+Highest.Degree,data=Adjunct)
drop1(model.adj, .~., test="F")

#d) 
TukeyHSD(model.adj)$Highest.Degree

HSD<-TukeyHSD(model.adj)
library(multcompView)
############################################
#### Get labels for depress main effects
#### using plot.labels from MyFunctions.R
labels<-plot.labels(HSD,"Highest.Degree",threshold=0.05)
## statistics for location on plot
means<-aggregate(Earnings~Highest.Degree,data=Adjunct,mean)
## merge labels and locations
final<-merge(labels,means,by="Highest.Degree")

SizePlotB<-ggplot(Adjunct, aes(x = Highest.Degree, y = Earnings)) + geom_blank()  +  geom_point() + geom_text(data = final, aes(x = Highest.Degree, y = Earnings, label = Letters),vjust=0,hjust=-.75,size=5)+  theme_bw()

(SizePlotB)
