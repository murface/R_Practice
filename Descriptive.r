# Import dataSet
dataSet<-read.table("~/Dropbox/Stat.Prob>355/project 1/dataSet.txt",header = T, quote="\"")
############################################################################################
# Package install if necessary
#install.packages("scatterplot3d")
library(scatterplot3d)
############################################################################################
# Clean up data --remove extreme outlier
dataSet<-dataSet[-171,]
############################################################################################
# Variables
wage <-(dataSet$Wage)
experience <-(dataSet$Experience)
education <-(dataSet$Education)
age <-(dataSet$Age)
sex <-(dataSet$Sex)
south <-(dataSet$South)
union <-(dataSet$Union)
race <-(dataSet$Race)
occupation <-(dataSet$Occupation)
sector <-(dataSet$Sector)
married <-(dataSet$Marr)
#############################################################################################
#Parsed Data Sets
##SOUTH
Southern <-dataSet[dataSet$South == 1,]
NotSouthern <-dataSet[dataSet$South == 0,]
##SEX
Men <-dataSet[dataSet$Sex == 0,] 
Women <-dataSet[dataSet$Sex == 1,] 
###RACE
White <-dataSet[dataSet$Race == 3,]
Hispanic <-dataSet[dataSet$Race == 2,]
OtherRace <-dataSet[dataSet$Race == 1,]
###OCCUPATION
Management <-dataSet[dataSet$Occupation == 1,]
Sales <-dataSet[dataSet$Occupation == 2,]
Clerical <-dataSet[dataSet$Occupation == 3,]
Service <-dataSet[dataSet$Occupation == 4,]
Professional <-dataSet[dataSet$Occupation == 5,]
OtherOccupation <-dataSet[dataSet$Occupation == 6,]
###SECTOR
Manufacturing <- dataSet[dataSet$Sector == 1,]
Construction <- dataSet[dataSet$Sector == 2,]
OtherSector <- dataSet[dataSet$Sector == 0,]
#############################################################################################
# Function call-> GetData(dataSet$column, label for boxplot)
GetData = function(x){
  print("###########################################",quote=F)
  print("5 Number Summary",quote=F)
  print(summary(x))
  print("Variance",quote=F)
  print(var(x))
  print("Standard Deviation",quote=F)
  print(sd(x))
  print("Quantiles",quote=F)
  print(quantile(x,c(0.1,0.9)))
}
#############################################################################################
# Print Stats
print("Summary for WAGES",quote=F)
GetData(wage)
print("Summary for EXPERIENCE",quote=F)
GetData(experience)
print("Summary for EDUCATION",quote=F)
GetData(education)
print("Summary for AGE",quote=F)
GetData(age)

# Occupational 5 Number Summaries
print("############################################",quote=F)
print("# Occupational 5 Number Summaries of Wages #",quote=F)
print("############################################",quote=F)
print("Management",quote=F)
print(summary(Management$Wage))
print("Sales",quote=F)
print(summary(Sales$Wage))
print("Clerical",quote=F)
print(summary(Clerical$Wage))
print("Service",quote=F)
print(summary(Service$Wage))
print("Professional",quote=F)
print(summary(Professional$Wage))
print("Other",quote=F)
print(summary(OtherOccupation$Wage))

print("####################################################",quote=F)
print("# Occupational 5 Number Summaries of Men and Women #",quote=F)
print("####################################################",quote=F)
print("Men",quote=F)
print(summary(Men$Wage))
print("Women",quote=F)
print(summary(Women$Wage))

# Wage Summaries for Sutherners and Non-Southerners
print("##################################################################",quote=F)
print("# 5 Number Summaries of Wages of Southerners and Non-Southerners #",quote=F)
print("##################################################################",quote=F)
print("Southerners",quote=F)
print(summary(Southern$Wage))
print("Non-Southerners",quote=F)
print(summary(NotSouthern$Wage))

# Wage Summaries by Sector
print("#########################################",quote=F)
print("# 5 Number Summaries of Wages by Sector #",quote=F)
print("#########################################",quote=F)
print("Manufacturing",quote=F)
print(summary(Manufacturing$Wage))
print("Construction",quote=F)
print(summary(Construction$Wage))
print("Other",quote=F)
print(summary(OtherSector$Wage))

#Covariance
print("#########  Covariance  ##########",quote=F)
print("Wage and Experience",quote=F)
print(cov(wage,experience))
print("Wage and Age",quote=F)
print(cov(wage,age))
print("Wage and Education",quote=F)
print(cov(wage,education))
print("Age and Experience",quote=F)
print(cov(age,experience))
print("Union and Education",quote=F)
print(cov(union,education))
print("Wage and Sex",quote=F)
print(cov(wage,sex))

#Correlation
print("#########  Correlations  ###########",quote=F)
print("Wage and Education",quote=F)
print(cor(wage,education))
print("Wage and Experience",quote=F)
print(cor(wage,experience))
print("Wage and Age",quote=F)
print(cor(wage,age))
print("Age and Experience",quote=F)
print(cor(age,experience))
print("Union and Education",quote=F)
print(cor(union,education))
print("Wage and Sex",quote=F)
print(cor(wage,sex))

#################################################################################################
# Graphics #
#################################################################################################

# Format visual output screen
par(mfrow=c(3,6))

# 3-D scatterplot of wages,education and experience
scatterplot3d(education,experience,wage, pch=20, highlight.3d=TRUE, 
             type="h", main="Wage vs Experience vs Education",angle=75,grid=TRUE)

# Scatter Plot of Education vs Wages
plot(dataSet$Education,dataSet$Wage, main="Education vs. Wages",
     xlab="Years of Education", ylab="Wages in $ per hour", col=heat.colors(dataSet$Education),pch=23)
# regression line for Education vs Wages
EduWageFit<-lm(dataSet$Wage~dataSet$Education)
abline(EduWageFit, col="blue")
print("Education vs Wage Linear Model",quote=FALSE)
print(EduWageFit)

# Scatter Plot of Experience vs Wages
plot(dataSet$Experience,dataSet$Wage, main="Experience vs. Wages",
     xlab="Years of Experience", ylab="Wages in $ per hour", col=heat.colors(dataSet$Experience),pch=25)
# regression line for Experience vs Wages
ExpWageFit<-lm(dataSet$Wage~dataSet$Experience)
abline(ExpWageFit, col="blue")
print("Experience vs Wages Linear Model",quote=FALSE)
print(ExpWageFit)

# Scatter Plot of Experience vs Age
plot(dataSet$Experience,dataSet$Age, main="Experience vs. Age",
     xlab="Years of Experience", ylab="Age in years", col=heat.colors(dataSet$Education),pch=23)
# regression line for Education vs Wages
ExpAgeFit<-lm(dataSet$Age~dataSet$Experience)
abline(ExpAgeFit, col="blue")
print("Experience vs Age Linear Model",quote=FALSE)
print(ExpAgeFit)

# Comparitive Plot of Wages vs Sex
boxplot(wage~sex, data=dataSet, col=terrain.colors(wage),
        main="Wages by Sex", ylab="Wages in $ per hour", names=c("Men","Women"),
        las=2, cex.axis=.7)

# Histograms of Education, Wages, Age, and Experience
hist(wage,seq(0,30,2),xlab="Wages in $ per hour", main= "Wages", col= 'light blue' )
hist(experience,seq(0,60,5),xlab="Experience in Years", main="Experience", col= "light green")
hist(education,seq(0,20),xlab="Education in Years",main="Education",col="gray")
hist(dataSet$Age, main="Age",xlab="Age",col="orange", seq(0,80, 5))

# Pie Chart of Occupations
jobs <- data.frame(table(occupation))
tags <- c("Manager\n   ", "Sales", "Clerical", "Service", "Professional","Other")
# Use frequency to calculate percentage
pct <- round(jobs$Freq/sum(jobs$Freq)*100)
tags <- paste(tags, pct)
tags <- paste(tags,"%",sep="")
pie(jobs$Freq,labels = tags, col=rainbow(length(tags)), main="Occupations")

# pie chart for Race
racial <- data.frame(table(race))
labels <- c("Other", "Hispanic","White")
# Use frequency to calculate percentage
pct <- round(racial$Freq/sum(racial$Freq)*100)
# add percents to labels 
labels <- paste(labels, pct)
# add % symbols to labels 
labels <- paste(labels,"%",sep="")
pie(racial$Freq,labels = labels, col=rainbow(length(labels)),
    main="Race")

# pie chart for Southerners
southPie <- data.frame(table(south))
labels <- c("Not Southern", "Southern")
# Use frequency to calculate percentage
pct <- round(southPie$Freq/sum(southPie$Freq)*100)
# add percents to labels 
labels <- paste(labels, pct)
# add % symbols to labels 
labels <- paste(labels,"%",sep="")
pie(southPie$Freq,labels = labels, col=rainbow(length(labels)),
    main="Geographic Designation")

# Barplot of Sex and Marriage Relationship
counts <- data.frame(table(sex, married))
rownames(counts) <- c("Unmarried Male","Unmarried Female","Married Male","Married Female")
barplot(counts$Freq, main="Sex and Mariage Status",
        xlab="Relationship\n\   Types", ylab="Frequency",
        col=rainbow(4), las=2)
legend("topleft", legend=rownames(counts), cex=1, bty="n", fill=rainbow(4))
axis(side=2, at=seq(0,225,50), labels=seq(0,225,50),las=1)

# Barplot of Sectors and Union Workers
work <- data.frame(table(sector,union))
rownames(work) <- c("Other","Manufacturing","Construction",
                    "Other-Union","Manufacturing-Union","Construction-Union")
barplot(work$Freq, main="Union and Nonunion Workers by Sector", xlab="Worker Breakdown",
        ylab= "Frequency", beside=TRUE, las=1, col=rainbow(6))
legend("topright", legend=rownames(work), cex=1, bty="n", fill=rainbow(6))
axis(side=2, at=seq(0,425,50), labels=seq(0,425,50), las=1)

# Wages vs Occupation Boxplot
jobTags <- c("Management","Sales","Clerical","Service","Professional","Other")
boxplot(wage~occupation, data=dataSet, col=terrain.colors(wage),
        main="Wages by Occupation", ylab="Wages in $ per hour", names=jobTags,
        las=2, cex.axis=.7)

# Wages vs Sector Boxplot
sectorTags <- c("Other","Manufacturing","Construction")
boxplot(wage~sector, data=dataSet, col=heat.colors(wage),
        main="Wages by Sector", ylab="Wages in $ per hour", names=sectorTags,
        las=2, cex.axis=.7)

# Wages vs Geography Boxplot
southTags <- c("Not Southern","Southern")
boxplot(wage~south, data=dataSet, col=terrain.colors(wage),
        main="Wages by Geographic Designation", ylab="Wages in $ per hour", names=southTags,
        las=2, cex.axis=.7)

# Pie chart for Gender
genderPie <- data.frame(table(sex))
genderLabel <- c("Men", "Women")
# Use frequency to calculate percentage
pct <- round(genderPie$Freq/sum(genderPie$Freq)*100)
# add percents to labels 
genderLabel <- paste(genderLabel, pct)
# add % symbols to labels 
genderLabel <- paste(genderLabel,"%",sep="")
pie(genderPie$Freq,labels = genderLabel, col=rainbow(length(genderLabel)),
    main="Gender Frequency")


#####################################################################################################
#EXTRA CREDIT STUFF
#Parse Data for Extra Credit
# Women from the South data
pre1woman <- Women[Women$South == 1,]
# Professional Women from the South data
pre2woman <- pre1woman[pre1woman$Occupation == 5,]
# Single professional women from the South data
PredictWoman <- pre2woman[pre2woman$Marr == 0,]
# Remove all non-necessary columns
PredictWoman = PredictWoman[,c(-2,-3,-5,-8,-9,-10,-11)]
#PredictWoman
print("**********************************************",quote=F)
print("***              EXTRA CREDIT              ***",quote=F)
print("**********************************************",quote=F)
print("A single, professional woman from the south, with 10 years of experience",quote=F)
print("and 12 years of education would make: ",quote=F)
# Experience coefficient = .18502
# Education coefficient = 1.00101
# Age coefficient = -.07208
# Intercept = -4.71871
perHour <- (12*1.00101+10*.18502+40*-.07208-4.71871)
print(perHour)
print("dollars per hour",quote=F)
#plot(linearFit)
linearFit <- lm(wage~education+experience+age,data=PredictWoman)
print(linearFit)