# library---------
library(tidyverse)
# data-------------
# data("ToothGrowth")
# d<-ToothGrowth
# d$id <- rep(1:10,3)
# d$intra1 <- c(-0.5, 0.5)[d$supp]
# d$intra2 <- d$dose
# d$inter<- rep(c(0,0, 0, 0,0, 
#               1, 1, 1, 1, 1),
#               6)

# function---------------
wdiff <- function(data,intra,inter,id,y){

  # 1) data preparation-------------
data$id<-  d[, id]

beta_list<-list()
# list to fill with the individual beta coefficients for 
# each within subject variable
for(var in 1:length(intra)){
 varloop<-  intra[var]
sub_beta_list<-list()
  for(i in 1:max(d[, "id"])){
  d2 <- data[data$id == i,]
  beta<-lm(as.formula(paste(y, "~", varloop)), d2)
  sub_beta_list[i]<-beta$coefficients[2]
  }
  d3<-data.frame(beta = do.call(rbind, args = sub_beta_list))
  beta_list[intra[var]]<-list(d3)
  }
d4<-data.frame(beta = do.call(cbind, args = beta_list))
d5<-cbind(d4, data[unique(data$id), inter], unique(data$id))
colnames(d5)<- c(intra, inter, "subject")
new_df <- d5
return(new_df)
}
# 2) statistics------------
lm2 <- function(data = NULL, inter = NULL, y = NULL){
  anova_list<-list()
  lm_list<-list()
  if(is.null(inter)){
    # lm ---------
    fma <- as.formula(paste(y,"~1"))
    ma<-lm(fma, data)
    sma<- as.data.frame(summary(ma)$coefficient)
    sma[-4]<-round(sma[-4],2)
    sma[4]<-format.pval(sma[4], eps = .001)
    rownames(sma)<-y
    # pre-----------
    mn <- sum(data[,y]^2)# null model
    pre <-round((mn - unlist(anova(ma)[2]))/mn, 2)
    #export-----
    lm_list[y]<-list(cbind(sma, pre))
  }else{
    for(iv in 1:length(inter)){
      # model comparison
      predictor <-  inter[iv]
      fma <-as.formula(paste(y,"~",paste(inter,collapse = "+")))
      fmc <-as.formula(ifelse(length(inter)>1,
        paste(y,"~", paste(inter[-iv], collapse = "+")),
        paste(y,"~ 1")))
      ma<-lm(fma, data)
      mc<-lm(fmc, data)
      ama<-as.data.frame(anova(mc, ma))
      ama[-6]<-round(ama[2,-6],2)
      ama[6]<-format.pval(ama[6], eps = .001)
      ama$model<- c(fmc, fma)
      ama <- ama[, c(7, 1:6)]
      ama$pre <- c(NA,round((ama[1,3] - ama[2,3])/ ama[1,3],2))
      ama<-rbind(ama, rep("---", length(ama)))
      ama[1, 4:8]<-""
      sma<- as.data.frame(summary(ma)$coefficient)
      sma[-4]<-round(sma[-4],2)
      sma[4]<-format.pval(sma[4], eps = .001)
      rownames(sma)[1]<-y
      #export-----
      lm_list[predictor]<-list(sma)
      anova_list[predictor]<-list(ama)
    }
  }
  # qqnorm-----------
   plot(ma, which = 2, sub = "",
       main = paste(format(fma)))
    qqplot<-recordPlot()
  # residual plot-----------
    plot(ma, which = 3, sub = "",
         main = paste(format(fma)))
  varplot<-recordPlot()
  # res. student----------
  plot(rstudent(ma), ylab = "res. student",
       xlab = "subject", xaxt='n', type="l",
       main = "Studentized residuals")
  axis(1,1:nrow(data),cex.axis = 0.5)
  abline(h=mean(rstudent(ma)), lty = "dashed")
  abline(v=1:nrow(data), lty = "dotted", col = "grey")
  points(rstudent(ma), pch = 21, bg = "white",
         cex=1.8)
  text(rstudent(ma), cex=0.5,
       col = "red")
  rssplot<-recordPlot()
  # leverage-----------------
  plot(hatvalues(ma), ylab = "leverage",
       xlab = "subject", xaxt='n', type="l",
       main = "Leverage")
  axis(1,1:nrow(data),cex.axis = 0.5)
  abline(h=mean(hatvalues(ma)), lty = "dashed")
  abline(v=1:nrow(data), lty = "dotted", col = "grey")
  points(hatvalues(ma), pch = 21, bg = "white",
         cex=1.8)
  text(hatvalues(ma), cex=0.5,
       col = "red")
  leverageplot<-recordPlot()
  # cook---------------
  plot(cooks.distance(ma), ylab = "Cooks distance",
       xlab = "subject", xaxt='n', type="l",
       main = "Cooks distance")
  axis(1,1:nrow(data),cex.axis = 0.5)
  abline(h=mean(cooks.distance(ma)), lty = "dashed")
  abline(v=1:nrow(data), lty = "dotted", col = "grey")
  points(cooks.distance(ma), pch = 21, bg = "white",
         cex=1.8)
  text(cooks.distance(ma), cex=0.5,
       col = "red")
  cookplot<-recordPlot()
  
  myanova <- do.call(rbind, anova_list)
  rownames(myanova)<-NULL
  mylm<-lm_list[1]
  rownames(mylm)<-NULL
  mylm<-unique(mylm)
  return(list(
              leverageplot = leverageplot,
              rssplot = rssplot, cookplot = cookplot,
              varplot = varplot,qqplot = qqplot,
              anova = myanova,lm = mylm
              ))
    }
# lm2(data = newd, inter = "inter", y = "intra1")
# lm2(data = newd, y = "intra1")
# lm2(data = mtcars, y = "mpg") 
# lm2(data = mtcars, y = "mpg", inter="am")  
# lm2(data = mtcars, y = "disp", 
#     inter = c("am", "mpg")) 
# lm2(data = mtcars, y = "disp", 
#     inter = c("am", "mpg", "am:mpg")) 







