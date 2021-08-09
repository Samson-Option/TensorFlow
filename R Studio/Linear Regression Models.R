setwd("D:/RLabs")
mydata<-read.csv("D:/RLabs/Action Learning/convertedtrain.csv",  header=TRUE)
colnames(mydata)
#mydata1[mydata=="#N/A"] <-NA
#sum(is.na(mydata)==1)
#which(grepl(10, mydata$pageviews))
#lr models
levels(mydata$medium)

levels(mydata$isMobile)




#transfer factor to numerical
#treat NA as 0 revenue
#transfer log revenue
mydata$newrevenue<-as.numeric(as.character(mydata$transactionRevenue))
mydata$logrevenue<-log(mydata$newrevenue+1)
mydata$logrevenue[is.na(mydata$logrevenue)] <-0

#hit times
mydata$newhittimes<-as.numeric(as.character(mydata$hit.times))
mydata$newhittimes[is.na(mydata$newhittimes)] <-0
#max number is 500, deleted?
which(grepl(500, mydata$newhittimes))
#df[df == 20] <- NA
#pageviews
mydata$newpageviews<-as.numeric(as.character(mydata$pageviews))
mydata$newpageviews[is.na(mydata$newpageviews)] <-0
#bounces
mydata$newbounces<-as.numeric(as.character(mydata$bounces))
mydata$newbounces[is.na(mydata$newbounces)] <-0



#newvisit
mydata$newvisitandrevenue<-if((mydata$transactionRevenue==0)&&(mydata$newVisits==1)){
  1}else{0}
sum(mydata$newvisitandrevenue)

mydata$Channels<-mydata$channelGrouping
#channel summary
summary(mydata$Channels)

summary(mydata$deviceCategory)
#levels 1. Channels/8 2. continent/5 
#3.subcontinet/23 4.devicecategory/3
#5.operatingsystem/18 6.nameofdate/7
levels(mydata$Channels)
levels(mydata$continent)
levels(mydata$subcontinent)
levels(mydata$deviceCategory)
levels(mydata$operatingSystem)
#levels(mydata$search.engine)
levels(mydata$Name.of.date)
summary(mydata$operatingSystem)
#create dummy variables 
levels(mydata$medium)
#Medium dummy (none)
mydata$Dmediumaffiliate<-ifelse(mydata$medium=="affiliate",1,0)
mydata$Dmediumnotset<-ifelse(mydata$medium=="not set",1,0)
mydata$Dmediumcpc<-ifelse(mydata$medium=="cpc",1,0)
mydata$Dmediumcpm<-ifelse(mydata$medium=="cpm",1,0)
mydata$Dmediumorganic<-ifelse(mydata$medium=="organic",1,0)
mydata$Dmediumreferral<-ifelse(mydata$medium=="referral",1,0)

levels(mydata$mobile)

mydata$mobile<-as.character(mydata$isMobile)
#Is mobie False
mydata$Dmobiletrue<-ifelse(mydata$mobile=="TRUE",1,0)
sum(mydata$Dmobiletrue)


#Date dummy Monday
mydata$DTuesday<-ifelse(mydata$week=="Tuesday",1,0)
mydata$DWednesday<-ifelse(mydata$week=="Wednesday",1,0)
mydata$DThursday<-ifelse(mydata$week=="Thursday",1,0)
mydata$DFriday<-ifelse(mydata$week=="Friday",1,0)
mydata$DSaturday<-ifelse(mydata$week=="Saturday",1,0)
mydata$DSunday<-ifelse(mydata$week=="Sunday",1,0)
summary(mydata$week)

#Channels dummy (other)
summary(mydata$Channels)
mydata$DAffiliates<-ifelse(mydata$Channels=="Affiliates",1,0)
mydata$DDirect<-ifelse(mydata$Channels=="Direct",1,0)
mydata$DDisplay<-ifelse(mydata$Channels=="Display",1,0)
mydata$DOrganicSearch<-ifelse(mydata$Channels=="Organic Search",1,0)
mydata$DOther<-ifelse(mydata$Channels=="Other",1,0)
mydata$DPaidSearch<-ifelse(mydata$Channels=="Paid Search",1,0)
mydata$DReferral<-ifelse(mydata$Channels=="Referral",1,0)
mydata$DSocial<-ifelse(mydata$Channels=="Social",1,0)

#Continent dummy (notset)
summary(mydata$continent)
mydata$DAfrica<-ifelse(mydata$continent=="Africa",1,0)
mydata$DAmericas<-ifelse(mydata$continent=="Americas",1,0)
mydata$DAsia<-ifelse(mydata$continent=="Asia",1,0)
mydata$DEurope<-ifelse(mydata$continent=="Europe",1,0)
mydata$DOceania<-ifelse(mydata$continent=="Oceania",1,0)

#devicecategory desktop
summary(mydata$deviceCategory)
mydata$Dmobile<-ifelse(mydata$deviceCategory=="mobile",1,0)
mydata$Dtablet<-ifelse(mydata$deviceCategory=="tablet",1,0)




mydata<-mydata[!is.na(mydata$fullVisitorId), ]
summary(mydata$fullVisitorId)
options(scipen = 999)
which(grepl(854783508496317056, mydata$fullVisitorId))

colnames(mydata)
#aggregate
agg.visits <-aggregate(x=mydata$visits,by=list(mydata$fullVisitorId), FUN=sum, na.rm=T)
colnames(agg.visits)<-c("id","visits")


agg.hits <-aggregate(x=mydata$newhittimes,by=list(mydata$fullVisitorId), FUN=sum, na.rm=T)
colnames(agg.hits)<-c("id","hits")
merge1<-merge(agg.hits,agg.visits, by="id")


agg.pageviews <-aggregate(x=mydata$newpageviews,by=list(mydata$fullVisitorId), FUN=sum, na.rm=T)
colnames(agg.pageviews)<-c("id","pageviews")
merge2<-merge(merge1,agg.pageviews, by="id")

agg.bounces <-aggregate(x=mydata$newbounces,by=list(mydata$fullVisitorId), FUN=sum, na.rm=T)
colnames(agg.bounces)<-c("id","bounces")
merge3<-merge(merge2,agg.bounces, by="id")

agg.newvisits <-aggregate(x=mydata$newVisit,by=list(mydata$fullVisitorId), FUN=sum, na.rm=T)
colnames(agg.newvisits)<-c("id","newvisits")
merge4<-merge(merge3,agg.newvisits, by="id")

agg.revenue <-aggregate(x=mydata$transactionRevenue,by=list(mydata$fullVisitorId), FUN=sum, na.rm=T)
colnames(agg.revenue)<-c("id","revenue")
merge5<-merge(merge4,agg.revenue, by="id")

merge5$logrevenue<-log(merge5$revenue+1)
merge5$logrevenue[is.na(merge5$logrevenue)] <-0

agg.visitnumber <-aggregate(x=mydata$visitNumber,by=list(mydata$fullVisitorId), FUN=max, na.rm=T)
colnames(agg.visitnumber)<-c("id","visitnumber")
merge6<-merge(merge5,agg.visitnumber, by="id")

mydata$id<-mydata$fullVisitorId

agg.channels <-aggregate(x=mydata$Channels,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.channels)<-c("id","channels")
agg.channels$channels <- gsub(",.*$", "", agg.channels$channels)
merge7<-merge(merge6,agg.channels, by="id")

agg.device<-aggregate(x=mydata$deviceCategory,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.device)<-c("id","device")
agg.device$device <- gsub(",.*$", "", agg.device$device)
merge8<-merge(merge7,agg.device, by="id")

agg.continent<-aggregate(x=mydata$continent,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.continent)<-c("id","continent")
agg.continent$continent <- gsub(",.*$", "", agg.continent$continent)
merge9<-merge(merge8,agg.continent, by="id")

agg.operatingSystem<-aggregate(x=mydata$operatingSystem,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.operatingSystem)<-c("id","operatingSystem")
agg.operatingSystem$operatingSystem <- gsub(",.*$", "", agg.operatingSystem$operatingSystem)
merge10<-merge(merge9,agg.operatingSystem, by="id")



agg.month<-aggregate(x=mydata$month,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.month)<-c("id","month")
agg.month$month <- gsub(",.*$", "", agg.month$month)
merge11.5<-merge(merge10,agg.month, by="id")


agg.medium<-aggregate(x=mydata$medium,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.medium)<-c("id","medium")
agg.medium$medium <- gsub(",.*$", "", agg.medium$medium)
merge11.6<-merge(merge11.5,agg.medium, by="id")


agg.mobile<-aggregate(x=mydata$mobile,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.mobile)<-c("id","mobile")
agg.mobile$mobile <- gsub(",.*$", "", agg.mobile$mobile)
merge11.7<-merge(merge11.6,agg.mobile, by="id")


agg.week<-aggregate(x=mydata$week,by=list(mydata$fullVisitorId), FUN=toString, na.rm=T)
colnames(agg.week)<-c("id","week")
agg.week$week <- gsub(",.*$", "", agg.week$week)
merge12<-merge(merge11.7,agg.week, by="id")





levels(merge12$mobile)

#Is mobie False
merge12$Dmobiletrue<-ifelse(merge12$mobile=="TRUE",1,0)
sum(mydata$Dmobiletrue)


######################
#Date dummy Monday

merge12$DTuesday<-ifelse(merge12$week=="Tuesday",1,0)
merge12$DWednesday<-ifelse(merge12$week=="Wednesday",1,0)
merge12$DThursday<-ifelse(merge12$week=="Thursday",1,0)
merge12$DFriday<-ifelse(merge12$week=="Friday",1,0)
merge12$DSaturday<-ifelse(merge12$week=="Saturday",1,0)
merge12$DSunday<-ifelse(merge12$week=="Sunday",1,0)

sum(merge12$DWednesday)

#Channels dummy affiliate
levels(mydata$Channels)

merge12$DDirect<-ifelse(merge12$channels=="Direct",1,0)
merge12$DDisplay<-ifelse(merge12$channels=="Display",1,0)
merge12$DOrganicSearch<-ifelse(merge12$channels=="Organic Search",1,0)
merge12$DOther<-ifelse(merge12$channels=="(Other)",1,0)
merge12$DPaidSearch<-ifelse(merge12$channels=="Paid Search",1,0)
merge12$DReferral<-ifelse(merge12$channels=="Referral",1,0)
merge12$DSocial<-ifelse(merge12$channels=="Social",1,0)

#Continent dummy africa


merge12$DAmericas<-ifelse(merge12$continent=="Americas",1,0)
merge12$DAsia<-ifelse(merge12$continent=="Asia",1,0)
merge12$DEurope<-ifelse(merge12$continent=="Europe",1,0)
merge12$DOceania<-ifelse(merge12$continent=="Oceania",1,0)

#devicecategory desktop
merge12$Dmobile<-ifelse(merge12$device=="mobile",1,0)
merge12$Dtablet<-ifelse(merge12$device=="tablet",1,0)

#Month dummy
merge12$DFebruary<-ifelse(merge12$month=="February",1,0)
merge12$DMarch<-ifelse(merge12$month=="March",1,0)
merge12$DApril<-ifelse(merge12$month=="April",1,0)
merge12$DMay<-ifelse(merge12$month=="May",1,0)

merge12$DJune<-ifelse(merge12$month=="June",1,0)
merge12$DJuly<-ifelse(merge12$month=="July",1,0)
merge12$DAugust<-ifelse(merge12$month=="August",1,0)
merge12$DSeptember<-ifelse(merge12$month=="September",1,0)
merge12$DOctober<-ifelse(merge12$month=="October",1,0)
merge12$DNovember<-ifelse(merge12$month=="November",1,0)
merge12$DDecember<-ifelse(merge12$month=="December",1,0)



merge12$newvisits<-as.numeric(as.character(merge12$newvisits))

#+DAmericas+DAsia+DEurope+DOceania
finalaggregation <-lm(logrevenue~hits+visits+pageviews+newvisits+bounces+Dmobile+Dtablet
                         +DDirect+DDisplay+DOrganicSearch+DOther+DPaidSearch+DReferral+DSocial
                         +DFebruary+DMarch+DApril+DMay+DJune+DJuly
                         +DAugust+DSeptember+DOctober+DNovember+DDecember+DTuesday+DWednesday+
                           DThursday+DFriday+DSaturday+DSunday+DAmericas+DAsia+DEurope+DOceania, data=merge12)


summary(finalaggregation)
summary(finalaggregation)$r.squared

finalaggregation1 <-lm(logrevenue~visits+newvisits+bounces+Dmobile+Dtablet
                      +DDirect+DDisplay+DOrganicSearch+DOther+DPaidSearch+DReferral+DSocial
                      +DFebruary+DMarch+DApril+DMay+DJune+DJuly
                      +DAugust+DSeptember+DOctober+DNovember+DDecember+DTuesday+DWednesday+
                        DThursday+DFriday+DSaturday+DSunday+DAmericas+DAsia+DEurope+DOceania, data=merge12)


summary(finalaggregation1)
summary(finalaggregation1)$r.squared
hist(merge12$visits)
max(merge12$visits)


aggregressionmydata3<-lm(logrevenue~visits,data=merge12)
summary(aggregressionmydata3)
cor(merge12$hits,merge12$visits)
cor(cbind(merge12$hits,merge12$visits,merge12$pageviews,merge12$newvisits))

cor(merge12$pageviews,merge12$bounces)
#ufdddddddddddsgafhuiaiaiaiaiaDSHNUIDHSahdissadsiakjhfioas
#Marginal Impact 


#final./...................
#summary(mydata$Month)
#which(grepl(9373695132787150848, mydata$fullVisitorId))

set.seed(123)
train.index<-sample(1:nrow(merge12), floor(0.75*nrow(merge12)), replace=FALSE)
# step 2: split the data into train and set
my.train<-merge12[train.index, ]
my.test<-merge12[-train.index,]

#data.reg.training<-lm(Calories~Sodium, data=hotdog[3:54,])  #Use the first 2 lines as the out-of-sample to make predictions> 
#data.outsample<-hotdog[1:2,]   
#predict(aggregressionmerge5, my.test)

aggregressionmydata<-lm(logrevenue~hits+visits+newvisits+visitnumber+bounces+Dmobile+Dtablet
                         +DDirect+DDisplay+DOrganicSearch+DOther+DPaidSearch+DReferral+DSocial
                         +DBlackBerry+DChromeOS+DFirefoxOS+DFreeBSD
                         +DiOS+DLinux+DMacintosh+DNintendo3DS+DNintendoWii+DNintendoWiiU+DNokia+DOpenBSD
                         +DSamsung+DWindows+DWindowsPhone+DXbox+DCaribbean+DCentralAmerica+DCentralAsia+DEasternAfrica+DEasternAsia
                         +DEasternEurope+DMelanesia+DMicronesianRegion+DMiddleAfrica+DNorthernAfrica+DNorthernAmerica+DNorthernEurope+DPolynesia
                         +DSouthAmerica+DSoutheastAsia+DSouthernAfrica+DSouthernAsia+DSouthernEurope+DWesternAfrica
                         +DWesternAsia+DWesternEurope+DFebruary+DMarch+DApril+DMay+DJune+DJuly
                         +DAugust+DSeptember+DOctober+DNovember+DDecember, data=merge12)


summary(aggregressionmydata)
summary(aggregressionmydata)$r.squared

aggregressionmydata2<-lm(logrevenue~hits+visits,data=merge12)
summary(aggregressionmydata2)

#prediction 
summary(mydata$newhittimes)
mydata$newhittimes[mydata$newhittimes == 500] <- NA
mydata$newhittimes[mydata$newhittimes == 489] <- NA
mydata$newhittimes[mydata$newhittimes == 483] <- NA
mydata$newhittimes[mydata$newhittimes == 471] <- NA
mydata$newhittimes[mydata$newhittimes == 437] <- NA
mydata$newhittimes[mydata$newhittimes == 387] <- NA
mydata$newhittimes[mydata$newhittimes == 382] <- NA
mydata$newhittimes[mydata$newhittimes == 378] <- 

#Month dummy
mydata$DFebruary<-ifelse(mydata$Month=="February",1,0)
mydata$DMarch<-ifelse(mydata$Month=="March",1,0)
mydata$DMay<-ifelse(mydata$Month=="May",1,0)
mydata$DApril<-ifelse(mydata$Month=="April",1,0)
mydata$DJune<-ifelse(mydata$Month=="June",1,0)
mydata$DJuly<-ifelse(mydata$Month=="July",1,0)
mydata$DAugust<-ifelse(mydata$Month=="August",1,0)
mydata$DSeptember<-ifelse(mydata$Month=="September",1,0)
mydata$DOctober<-ifelse(mydata$Month=="October",1,0)
mydata$DNovember<-ifelse(mydata$Month=="November",1,0)
mydata$DDecember<-ifelse(mydata$Month=="December",1,0)

#Continent dummy Africa
mydata$DAmericas<-ifelse(mydata$continent=="Americas",1,0)
mydata$DAsia<-ifelse(mydata$continent=="Asia",1,0)
mydata$DEurope<-ifelse(mydata$continent=="Europe",1,0)
mydata$DOceania<-ifelse(mydata$continent=="Oceania",1,0)

predicteddata<-lm(logrevenue~newhittimes+newpageviews+newnewvisit+visitNumber+newbounces
                        +DDirect+DDisplay+DOrganicSearch+DOther+DAmericas+DAsia+DEurope+DOceania+DFebruary+DMarch+DApril+DMay+DJune+DJuly
                        +DAugust+DSeptember+DOctober+DNovember+DDecember+DTuesday+DWednesday+DThursday+DFriday+DSaturday+DSunday, data=mydata)

summary(predicteddata)
summary(predicteddata)$r.squared

set.seed(123)
train.index1<-sample(1:nrow(mydata), floor(0.75*nrow(mydata)), replace=FALSE)
# step 2: split the data into train and set
data.train<-mydata[train.index1, ]
data.test<-mydata[-train.index1,]

trainingregression<-lm(logrevenue~newhittimes+newpageviews+newnewvisit+visitNumber+newbounces+Dmobile+Dtablet
                  +DDirect+DDisplay+DOrganicSearch+DOther+DAmericas+DAsia+DEurope+DOceania+DFebruary+DMarch+DApril+DMay+DJune+DJuly
                  +DAugust+DSeptember+DOctober+DNovember+DDecember+DTuesday+DWednesday+DThursday+DFriday+DSaturday+DSunday, data=data.train)

summary(trainingregression)
summary(trainingregression)$r.squared

write.csv(data.test, file="datatest.csv",row.names = FALSE)

setwd("D:/RLabs")
data25<-read.csv("D:/RLabs/25data.csv",  header=TRUE)
colnames(data25)
data25[data25=="#N/A"] <-NA
sum(is.na(data25)==1)

summary(data25)
levels(data25$rmse)
data25$sum<-as.numeric(as.character(data25$rmse))
data25$sum[is.na(data25$sum)] <-0
sum(data25$sum)

setwd("D:/RLabs")
testdata<-read.csv("D:/RLabs/testassignment.csv",  header=TRUE)
colnames(testdata)
#mydata[mydata=="#N/A"] <-NA

#Date dummy Monday
testdata$DTuesday<-ifelse(testdata$dateofweek=="Tuesday",1,0)
testdata$DWednesday<-ifelse(testdata$dateofweek=="Wednesday",1,0)
testdata$DThursday<-ifelse(testdata$dateofweek=="Thursday",1,0)
testdata$DFriday<-ifelse(testdata$dateofweek=="Friday",1,0)
testdata$DSaturday<-ifelse(testdata$dateofweek=="Saturday",1,0)
testdata$DSunday<-ifelse(testdata$dateofweek=="Sunday",1,0)


#Channels dummy Affiliates
testdata$DDirect<-ifelse(testdata$channels=="Direct",1,0)
testdata$DDisplay<-ifelse(testdata$channels=="Display",1,0)
testdata$DOrganicSearch<-ifelse(testdata$channels=="Organic Search",1,0)
testdata$DOther<-ifelse(testdata$channels=="Other",1,0)
testdata$DPaidSearch<-ifelse(testdata$channels=="Paid Search",1,0)
testdata$DReferral<-ifelse(testdata$channels=="Referral",1,0)
testdata$DSocial<-ifelse(testdata$channels=="Social",1,0)

#Continent dummy Africa
testdata$DAmericas<-ifelse(testdata$continent=="Americas",1,0)
testdata$DAsia<-ifelse(testdata$continent=="Asia",1,0)
testdata$DEurope<-ifelse(testdata$continent=="Europe",1,0)
testdata$DOceania<-ifelse(testdata$continent=="Oceania",1,0)

#devicecategory desktop
testdata$Dmobile<-ifelse(testdata$deviceCategory=="mobile",1,0)
testdata$Dtablet<-ifelse(testdata$deviceCategory=="tablet",1,0)

#Month dummy
testdata$DFebruary<-ifelse(testdata$month=="February",1,0)
testdata$DMarch<-ifelse(testdata$month=="March",1,0)
testdata$DMay<-ifelse(testdata$month=="May",1,0)
testdata$DApril<-ifelse(testdata$month=="April",1,0)
testdata$DJune<-ifelse(testdata$month=="June",1,0)
testdata$DJuly<-ifelse(testdata$month=="July",1,0)
testdata$DAugust<-ifelse(testdata$month=="August",1,0)
testdata$DSeptember<-ifelse(testdata$month=="September",1,0)
testdata$DOctober<-ifelse(testdata$month=="October",1,0)
testdata$DNovember<-ifelse(testdata$month=="November",1,0)
testdata$DDecember<-ifelse(testdata$month=="December",1,0)

testdataregression<-lm(logrevenue~newhittimes+newpageviews+newnewvisit+visitNumber+newbounces
                  +DDirect+DDisplay+DOrganicSearch+DOther+DPaidSearch+DReferral+DSocial+DAmericas+DAsia+DEurope+DOceania+Dmobile+Dtablet+DFebruary+DMarch+DApril+DMay+DJune+DJuly
                  +DAugust+DSeptember+DOctober+DNovember+DDecember+DTuesday+DWednesday+DThursday+DFriday+DSaturday+DSunday, data=mydata)


summary(testdataregression)$r.squared
summary(testdataregression)
write.csv(testdata, file="assignmentdata.csv",row.names = FALSE)

##########################################################
setwd("D:/RLabs")
testx2<-read.csv("D:/RLabs/Action Learning/converted.csv",  header=TRUE)
colnames(testx2)




#test2a$fullVisitorId<-NULL


#test2a$fullVisitorId <-gsub("_.*$","",test2a$sessionId)

finalprediction<-testx2
#aggregate=
a.visits <-aggregate(x=finalprediction$visits,by=list(finalprediction$fullVisitorId), FUN=sum, na.rm=T)

colnames(a.visits)<-c("id","visits")



a.hits <-aggregate(x=finalprediction$hit.times,by=list(finalprediction$fullVisitorId), FUN=sum, na.rm=T)
colnames(a.hits)<-c("id","hits")
mergea1<-merge(a.hits,a.visits, by="id")

a.pageviews <-aggregate(x=finalprediction$pageviews,by=list(finalprediction$fullVisitorId), FUN=sum, na.rm=T)
colnames(a.pageviews)<-c("id","pageviews")
mergea2<-merge(mergea1,a.pageviews, by="id")


a.bounces <-aggregate(x=finalprediction$bounces,by=list(finalprediction$fullVisitorId), FUN=sum, na.rm=T)
colnames(a.bounces)<-c("id","bounces")
mergea3<-merge(mergea2,a.bounces, by="id")

a.newvisits <-aggregate(x=finalprediction$newVisits,by=list(finalprediction$fullVisitorId), FUN=sum, na.rm=T)
colnames(a.newvisits)<-c("id","newvisits")
mergea4<-merge(mergea3,a.newvisits, by="id")





finalprediction$id<-finalprediction$fullVisitorId

a.channels <-aggregate(x=finalprediction$Channels,by=list(finalprediction$fullVisitorId), FUN=toString, na.rm=T)
colnames(a.channels)<-c("id","channels")
a.channels$channels <- gsub(",.*$", "", a.channels$channels)
mergea5<-merge(mergea4,a.channels, by="id")

a.device<-aggregate(x=finalprediction$deviceCategory,by=list(finalprediction$fullVisitorId), FUN=toString, na.rm=T)
colnames(a.device)<-c("id","device")
a.device$device <- gsub(",.*$", "", a.device$device)
mergea6<-merge(mergea5,a.device, by="id")

a.continent<-aggregate(x=finalprediction$continent,by=list(finalprediction$fullVisitorId), FUN=toString, na.rm=T)
colnames(a.continent)<-c("id","continent")
a.continent$continent <- gsub(",.*$", "", a.continent$continent)
mergea7<-merge(mergea6,a.continent, by="id")


a.month<-aggregate(x=finalprediction$month,by=list(finalprediction$fullVisitorId), FUN=toString, na.rm=T)
colnames(a.month)<-c("id","month")
a.month$month <- gsub(",.*$", "", a.month$month)
mergea8<-merge(mergea7,a.month, by="id")

a.week<-aggregate(x=finalprediction$week,by=list(finalprediction$fullVisitorId), FUN=toString, na.rm=T)
colnames(a.week)<-c("id","week")
a.week$week <- gsub(",.*$", "", a.week$week)
mergea9<-merge(mergea8,a.week, by="id")

#dummy

#Date dummy Monday
mergea9$DTuesday<-ifelse(mergea9$week=="Tuesday",1,0)
mergea9$DWednesday<-ifelse(mergea9$week=="Wednesday",1,0)
mergea9$DThursday<-ifelse(mergea9$week=="Thursday",1,0)
mergea9$DFriday<-ifelse(mergea9$week=="Friday",1,0)
mergea9$DSaturday<-ifelse(mergea9$week=="Saturday",1,0)
mergea9$DSunday<-ifelse(mergea9$week=="Sunday",1,0)


#Channels dummy Affiliates
mergea9$DDirect<-ifelse(mergea9$channels=="Direct",1,0)
mergea9$DDisplay<-ifelse(mergea9$channels=="Display",1,0)
mergea9$DOrganicSearch<-ifelse(mergea9$channels=="Organic Search",1,0)
mergea9$DOther<-ifelse(mergea9$channels=="Other",1,0)
mergea9$DPaidSearch<-ifelse(mergea9$channels=="Paid Search",1,0)
mergea9$DReferral<-ifelse(mergea9$channels=="Referral",1,0)
mergea9$DSocial<-ifelse(mergea9$channels=="Social",1,0)

#Continent dummy Africa
mergea9$DAmericas<-ifelse(mergea9$continent=="Americas",1,0)
mergea9$DAsia<-ifelse(mergea9$continent=="Asia",1,0)
mergea9$DEurope<-ifelse(mergea9$continent=="Europe",1,0)
mergea9$DOceania<-ifelse(mergea9$continent=="Oceania",1,0)

#devicecategory desktop
mergea9$Dmobile<-ifelse(mergea9$device=="mobile",1,0)
mergea9$Dtablet<-ifelse(mergea9$device=="tablet",1,0)

#Month dummy
mergea9$DFebruary<-ifelse(mergea9$month=="February",1,0)
mergea9$DMarch<-ifelse(mergea9$month=="March",1,0)
mergea9$DApril<-ifelse(mergea9$month=="April",1,0)
mergea9$DMay<-ifelse(mergea9$month=="May",1,0)

mergea9$DJune<-ifelse(mergea9$month=="June",1,0)
mergea9$DJuly<-ifelse(mergea9$month=="July",1,0)
mergea9$DAugust<-ifelse(mergea9$month=="August",1,0)
mergea9$DSeptember<-ifelse(mergea9$month=="September",1,0)
mergea9$DOctober<-ifelse(mergea9$month=="October",1,0)
mergea9$DNovember<-ifelse(mergea9$month=="November",1,0)
mergea9$DDecember<-ifelse(mergea9$month=="December",1,0)

write.csv(mergea9, file="convertedfinal.csv",row.names = FALSE)

write.csv(mergea9, file="p99.csv",row.names = FALSE)

setwd("D:/RLabs")
test8<-read.csv("D:/RLabs/p8.csv",  header=TRUE)
colnames(test8)

setwd("D:/RLabs")
test9<-read.csv("D:/RLabs/p9.csv",  header=TRUE)
colnames(test9)

pp10<-cbind(test9,test8)

pp10$hits <- NULL
pp10$visits <- NULL
pp10$device <- NULL
pp10$geoNetwork <- NULL
pp10$sessionId <- NULL
pp10$socialEngagementType <- NULL
pp10$totals <- NULL
pp10$trafficSource <- NULL
pp10$visitId <- NULL
pp10$visitNumber <- NULL
pp10$visitStartTime <- NULL
pp10$notid <- NULL
pp10$DTuesday <- NULL
pp10$DWednesday <- NULL
pp10$DThursday <- NULL
pp10$DFriday <- NULL
pp10$DSaturday <- NULL
pp10$DSunday <- NULL
pp10$DDirect <- NULL
pp10$DDisplay <- NULL
pp10$DOrganicSearch <- NULL
pp10$DOther <- NULL
pp10$DPaidSearch <- NULL
pp10$DReferral <- NULL

pp10$DSocial <- NULL
pp10$DAmericas <- NULL
pp10$DAsia <- NULL
pp10$DEurope <- NULL
pp10$DOceania <- NULL
pp10$Dmobile <- NULL
pp10$Dtablet <- NULL
pp10$DFebruary <- NULL
pp10$DMarch <- NULL
pp10$DApril <- NULL
pp10$DMay <- NULL
pp10$DJune <- NULL
pp10$DJuly <- NULL
pp10$DAugust <- NULL
pp10$DSeptember <- NULL
pp10$DOctober <- NULL
pp10$DNovember <- NULL
pp10$DDecember <- NULL

pp10$pageviews <- NULL
pp10$bounces <- NULL
pp10$newvisits <- NULL
pp10$channels <- NULL
pp10$continent <- NULL
pp10$month <- NULL
pp10$week <- NULL

write.csv(pp10, file="pp22.csv",row.names = FALSE)



setwd("D:/RLabs")
converted<-read.csv("D:/RLabs/Action Learning/converted.csv",  header=TRUE)
colnames(converted)