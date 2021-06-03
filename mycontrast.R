
  mycontrast<-function(data, var, contrast){
    
tmp<-data[,var] # temporary variable

tmp2<-data.frame(iv = tmp, index = 1:length(tmp))
# data frame that stores the original variable with an index

tmp3<-tmp2[order(tmp2$iv), ]
# order the levels of the variable in increasing order or by alphabetic order
# according to the type of variable

tmp3$iv2<-contrast[as.factor(tmp3$iv)] # apply the contrast

tmp4<-tmp3[order(tmp3$index), ] # re-order the levels like before ;-) ! 
tmp4$index<-NULL # useless now
colnames(tmp4)<-c(var,paste0(var,"_c")) # set column names

return(tmp4) # export a data frame
  }
  
# EXAMPLES-------------------------------------------
  
d<-data.frame(iv = as.vector(sapply(c("Alix","Carla", "Bob"),function(x) rep(x,10))))
d2<-data.frame(iv2 = as.vector(sapply(c(1,3,2),function(x) rep(x,4))))
  
mycontrast(data=d, var = "iv", contrast = c(0,-0.5,0.5))
mycontrast(data=d2, var = "iv2", contrast = c(25,0,-25))

# USE IT---------------------------------------------
mydata<-data.frame(y = runif(length(d$iv))*1000, x = d$iv)
# Oh no I want to make a contrast ... !
tmp<-mycontrast(data=mydata, var = "x", contrast = c(0,-0.5,0.5))
# So easy
mydata$x2<-tmp$x_c
# plot with plot
plot(x=jitter(mydata$x2,0.5) , y= mydata$y)
points(aggregate(y ~ x2, mydata, mean), type = "o",pch=21, bg = "blue")
# plot with ggplot
library(ggplot2)
ggplot(mydata, aes(x=x2, y = y))+geom_jitter(width=0.02)+stat_summary(col ="red")
# outlier
boxplot(y~x, mydata)
z<-lm(y~x2, mydata)
library(car)
influencePlot(z)
# etc...
