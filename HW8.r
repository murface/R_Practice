# Stat 414
# Matt Murray
# Homework 8
# due 4/16
#############################################
rm(list=ls()) 

#Part 1

#create data frame
w1<-c(58.8,1,262,56,8.7)
w2<-c(19,81.5,331,14,64.4)
w3<-c(39,151,27,21.4,578)
w4<-c(3.1,942,85.6,10,637)
data<-data.frame(w1,w2,w3,w4)

#get mean and var for original data
for(i in 1:4){
    print("mean", quote=FALSE)
    print(mean(data[,i]))
    print("variance", quote=FALSE)
    print(var(data[,i]))
}

print("log-transformed data", quotes=FALSE)
dataL<-data.frame()

#log-transform data
for(i in 1:5){
  for(k in 1:4){
    x<-data[i,k]
    dataL[i,k]<-c(log(x))
  }
}

#get mean and var for log-transformed data
for(i in 1:4){
  print("mean", quote=FALSE)
  print(mean(data[,i]))
  print("variance", quote = FALSE)
  print(var(data[,i]))
}
#################################################
# Part 2
#Bartlett's Test
print(bartlett.test(data))
print(bartlett.test(dataL))

#Levene's Test
#original data
y1<-abs(w1-mean(w1))
y2<-abs(w2-mean(w2))
y3<-abs(w3-mean(w3))
y4<-abs(w4-mean(w4))
y<-c(y1,y2,y3,y4)

g<-factor(rep(1:4,c(5,5,5,5)),
          labels=c("well 1","well 2","well 3","well 4"))
a<-aov(y~g)
print(summary(a))

# log-transformed data
z1<-abs(dataL$V1-mean(dataL$V1))
z2<-abs(dataL$V2-mean(dataL$V2))
z3<-abs(dataL$V3-mean(dataL$V3))
z4<-abs(dataL$V4-mean(dataL$V4))
z<-c(z1,z2,z3,z4)

h<-factor(rep(1:4,c(5,5,5,5)),
          labels=c("well 1","well 2","well 3","well 4"))

b<-aov(z~h)
print(summary(b))
#################################################
# Part 3
# Parametric ANOVA

#original data
o<-c(w1,w2,w3,w4)
out1<-aov(o~h)
print(summary(out1))

#log-transformed data
lt<-c(dataL$V1,dataL$V2,dataL$V3,dataL$V4)
out2<-aov(lt~h)
print(summary(out2))

#Nonparametric
#Kruskal-Wallace Rank Sum Test

#original data
print(kruskal.test(o,h))

#log-transformed data
print(kruskal.test(lt,h))
