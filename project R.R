download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
#1
summary(ames$Gr.Liv.Area) 
summary(ames$Garage.Cars)     
summary(ames$Garage.Type)        
summary(ames$Garage.Area)      
summary(ames$SalePrice) 

#2
samp20means=rep(NA,1000)
samp70means=rep(NA,1000)
samp100means=rep(NA,1000)


for(i in 1:1000){
   samp20<- sample(ames$SalePrice, 20)
  samp20means[i] <- mean(samp20)
  samp70<- sample(ames$SalePrice, 70)
  samp70means[i] <- mean(samp70)
  samp100<- sample(ames$SalePrice, 100)
  samp100means[i] <- mean(samp100)
  #n=20 t distribution
  #n=70 and 100 normal  
  
}
hist(samp20means)
hist(samp70means)
hist(samp100means)

#3
fitsamp20=fitdist(samp20means,"norm")
conf_20=confint(fitsamp20,"mean",level = 0.95)

fitsamp70=fitdist(samp70means,"norm")
conf_70=confint(fitsamp70,"mean",level = 0.95)

fitsamp100=fitdist(samp100means,"norm")
conf_100=confint(fitsamp100,"mean",level = 0.95)
outside_interval_20 <- sum(samp20means <conf_20[1]| samp20means>conf_20[2])
print(outside_interval_20)
outside_interval_70<- sum(samp70means< conf_70[1]|samp70means>conf_70[2])
print(outside_interval_70)
outside_interval_100<- sum(samp100means< conf_100[1] |samp100means>conf_100[2])
print(outside_interval_100)
#a large number of groups fall outside this interval it may indicate that the sample mean is not a good estimate of the population mean 
#or it could be due to random variation in the samples


#4
onecar=ames$SalePrice[ames$Garage.Cars==1]
twocars=ames$SalePrice[ames$Garage.Cars==2]
sampleonecar=sample(onecar,25)
sampletwocars=sample(twocars,25)

test=t.test(sampleonecar,sampletwocars)



