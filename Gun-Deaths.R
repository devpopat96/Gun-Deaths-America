#######################
# CREATED BY: DEVANSH POPAT
#######################
# Load Data File: 
#######################

gun_deaths <- read.csv(file.choose(), header= TRUE)


#######################
# Analyzing the number of gun deaths per month 
#######################

#Using Kable
library(plyr)
library(knitr)
x <- count(gun_deaths,"month") 
kable(x, format = "rst", caption="Gun deaths in the United States (2012-2014),
by month", col.names=c("Month","Frequency"))

#Visualizing
counts <-table(gun_deaths$year,gun_deaths$month)
barplot(counts,main="Gun Deaths by month", xlab = "Months", col=c("red","blue","green"), legend=rownames(counts), names.arg = c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), beside = T)


#######################
# Visualizing the number of gun deaths per intent 
#######################
barplot(rev(sort(table(gun_deaths$intent))))


#######################
#  Visualizing the number of gun deaths by sex
#######################
boxplot(age~sex, data=gun_deaths)

#Average Age of male gun deaths
mean(gun_deaths$age[gun_deaths$sex == "M"], na.rm=TRUE)

#Average Age of female gun deaths
mean(gun_deaths$age[gun_deaths$sex == "F"], na.rm=TRUE)



#######################
#  Estimating number of white males with atleast high school education killed in 2012
#######################
gun_deaths_e <- subset(gun_deaths, race == "White" & sex == "M" & year== 2012 & (education=="BA+" | education=="Some college" | education=="HS/GED"))
nrow(gun_deaths_e)



#######################
#  Estimating the season with most gun deaths
#######################
gun_deaths$season <- "Winter"
gun_deaths$season[gun_deaths$month <=6 & gun_deaths$month >= 4] <-"Spring"
gun_deaths$season[gun_deaths$month <=9 & gun_deaths$month >= 7] <-"Summer"
gun_deaths$season[gun_deaths$month <=12 & gun_deaths$month >= 10] <-"Fall"
barplot(table(gun_deaths$season))


#######################
#  Whitess died because of suicide or homicide? What is the same for Blacks and Hispanics?
#######################
table(subset(gun_deaths,race=="White")$intent)
table(subset(gun_deaths,race=="Black")$intent)
table(subset(gun_deaths,race=="Hispanic")$intent)


#######################
#  What happens to deaths with guns when the police is involved?
#  Estimating the relationship between police involvement and other variables
#######################


#  For categorical variables "month", "year", "intent", "sex", "place", "education"
#  and "season", we use Chi-square test to detect their relationships with "police"
chisq.test(table(gun_deaths$police, gun_deaths$month))
chisq.test(table(gun_deaths$police, gun_deaths$year))
chisq.test(table(gun_deaths$police, gun_deaths$intent))
chisq.test(table(gun_deaths$police, gun_deaths$sex))
chisq.test(table(gun_deaths$police, gun_deaths$place))
chisq.test(table(gun_deaths$police, gun_deaths$education))
chisq.test(table(gun_deaths$police, gun_deaths$season))

#  For the numerical variable "age", we use two-sample t-test:
group0<-gun_deaths$age[gun_deaths$police==0]
group1<-gun_deaths$age[gun_deaths$police==1]

t.test(group0,group1)
######################################################################################################





