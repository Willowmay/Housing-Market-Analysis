library(ggplot2)
library(dplyr)

Totaldata <- read.csv("~/Downloads/cleanCL.csv")
View(Totaldata)

#Column selection
vars <- c("listingTitle", "listingDate", "listingMonth", "cleanRent", "cleanBeds", "cleanBaths", "cleanSqft", "matchAddress", "matchAddress2", "lat", "lng")
CLdata <- Totaldata[, vars]
View(CLdata)
str(CLdata)

#Change some column names to make more understandable and representative
colnames(CLdata) [4] <-"RentPrice"
colnames(CLdata) [5] <-"Beds"
colnames(CLdata) [6] <-"Baths"
colnames(CLdata) [7] <-"Sqft"
View(CLdata)

#Filter out data without NA
min(CLdata[,5])
which(is.na(CLdata$listingDate))
which(is.na(CLdata$Sqft))
CL_new<- CLdata[complete.cases(CLdata), ]
View(CL_new)

#The spread of rent price
#According to the graph of the rent price distribution, we can see that prices vary drastically. 
ggplot(data=CL_new ,aes(x= RentPrice))+
  geom_histogram(binwidth=200, fill='lightpink1')

#Median rent price is $2035
ggplot(data=CL_new,aes(x='',y=RentPrice))+
  geom_boxplot(outlier.color='dodgerblue',fill='lavender')+
  geom_text(aes(x='',y=median(CL_new$RentPrice),label=median(CL_new$RentPrice)),size=4,hjust=8)+
  xlab(label = '')


#We explored the relationship between Sqft and RentPrice
#Typical housing price with Sqft clusters around $2000 and 1000Sqft, as Sqft increases so does price 
#The majority of sqft is 1000, and rent price $2000  
ggplot(data=CL_new,aes(x=Sqft,y=RentPrice))+
  geom_point(alpha=0.5,size=0.9,color='limegreen')

#Just as we thought: More bedrooms, higher price.
library(dplyr)
CL_new%>%
  group_by(Beds)%>%
  summarize(meanPrice=mean(RentPrice),priceLow=mean(RentPrice)-1.96*sd(RentPrice)/sqrt(n()),priceHigh=mean(RentPrice)+1.96*sd(RentPrice)/sqrt(n()))%>%
  ungroup()%>%
  ggplot(aes(x=Beds,y=meanPrice))+
  geom_errorbar(aes(ymin=priceLow,ymax=priceHigh))+
  geom_line(aes(x=Beds,y=meanPrice,group=1),linetype=3)+
  geom_point(aes(x=Beds,y=meanPrice,group=1),size=1.5)

#Most as what we thought: More bathrooms, higher price. 
CL_new%>%
  group_by(Baths)%>%
  summarize(meanPrice=mean(RentPrice),priceLow=mean(RentPrice)-1.96*sd(RentPrice)/sqrt(n()),priceHigh=mean(RentPrice)+1.96*sd(RentPrice)/sqrt(n()))%>%
  ungroup()%>%
  ggplot(aes(x=Baths,y=meanPrice))+
  geom_errorbar(aes(ymin=priceLow,ymax=priceHigh))+
  geom_line(aes(x=Baths,y=meanPrice,group=1),linetype=3)+
  geom_point(aes(x=Baths,y=meanPrice,group=1),size=1.5)

#Filter out value=0 in the Baths column
#Turned out that those are studios, and many are in top floor (Rooftop deck)
bath0 <- dplyr::filter(CL_new, Baths == 0)
View(bath0)

View(CL_new)

## Linear Regression 
names(CL_new)
model1 = lm(RentPrice~Sqft, CL_new)
summary(model1)

model2=lm(RentPrice~Beds, CL_new)
summary(model2)

model3 = lm(RentPrice~Baths, CL_new)
Summary(model3)


##Multiple Linear Regression

#find correlations between factors
#Display the correlation coefficient
library(corrplot)
factor_Corr <- cor(CL_new[,-c(1:2,8:9)])
corrplot(factor_Corr,method="number")

#Correlation Matrix and Probability value
library(psych)
corr.test(CL_new[,c(3:7,10:11)])

#Multiple Linear Regression
model = lm(RentPrice~Sqft+Beds+Baths,data=CL_new)
summary(model)
anova(model)

lm1 = lm(RentPrice~Sqft+Beds, data= subset(CL_new, matchAddress =="40th Ave NE")) 
summary(lm1)

#Regression Diagnostics
par(mfrow=c(2,2))
plot(model)

model
#Equation of Model: RentPrice=872.388+1.209*Sqft+32.116*Beds+298.614*Baths

#Wrap the prarameters inside a new data frame named newdata
newdata = data.frame(Sqft=1450, Beds=2, Baths=2)
newdata1= data.frame(Sqft=1600, Beds=3, Baths=2)

#Apply the predict function to rentprice lm and new data
predict(model, newdata)
predict(model, newdata1)


#Subset 3 Bedrooms & 1 Bathrooms apartemnts
threeb1b <- dplyr::filter(CL_new, Beds == 3 & Baths == 1)
View(threeb1b)

##Clustering
d = earth.dist(CL_new)
library(geosphere)
geo.dist = function(threeb1b) {
  require(geosphere)
  d <- function(i,z){         # z[11:10] contain long, lat
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),11:10],z[i,11:10])
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(threeb1b),d,threeb1b))
  return(as.dist(dm))
}

km <- kmeans(geo.dist(threeb1b),centers=4)  # k-means, 4 clusters
hc <- hclust(geo.dist(threeb1b))            # hierarchical clustering, dendrogram
clust <- cutree(hc, k=4)              # cut the dendrogram to generate 4 clusters

threeb1b$clust <- cutree(hc,k=4)
View(threeb1b)

# Map with 3B1B geographic location in Seattle
library(maps)
library(ggmap)
seattle = get_map(location="Seattle",zoom=11)
ggmap(seattle) + 
  geom_point(data=threeb1b,aes(x=lng,y=lat),size=3,color=clust)    


