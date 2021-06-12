library(beanplot)
library(ggplot2)

matplot(zx) 
stem(zx)
mypar = function(){
  beanplot(boxwex = 0.4, border=NA,
           ll = 0.1, beanlinewd = 2)
}
mybeanplot(zx)
myx=c(0.9,1.1,1.9,2.1)
zx <- replicate (4, rnorm(50))


plot(x= jitter(rep(myx,each =50),0.2),  y = zx, col = "grey")
points(y=zx_means, x=myx, pch = 16, col = c("blue", "red", "blue", "red"), cex = 1.5)

zx_means <- (colMeans(zx, na.rm = TRUE))
mypar = list(boxwex = 0.4, border=NA,
             ll = 0.1, beanlinewd = 2)

beanplot(zx,at=c(0.5,1,2,2.5),xlim = c(0,3),
         col = c(alpha("grey",0.5), "blue", "red"), boxwex = 0.4, border=NA,
         ll = 0.1, beanlinewd = 2)

beanplot(zx,at=c(0.5,1,2,2.5),xlim = c(0,3),
         col = c(alpha("grey",0.5), "blue", "red"),mypar)






boxplot(zx, horizontal = FALSE, outline = FALSE, at = c(1,2,4,5))
points(y=zx_means,x=c(1,2,4,5), pch = 21, col = "blue", lwd = 1)


par(mfrow = c(1, 2), mai = c(0.5, 0.5, 0.5, 0.1))
mu <- 2
si <- 0.6
c <- 500
bimodal <- c(rnorm(c/2, -mu, si), rnorm(c/2, mu, si))
uniform <- runif(c, -4, 4)
normal <- rnorm(c, 0, 1.5)
ylim <- c(-7, 7)
boxplot(bimodal, uniform, normal, ylim = ylim, main = "boxplot",
           names = 1:3)
beanplot(bimodal, uniform, normal, ylim = ylim, main = "beanplot",
         col = c("#CAB2D6", "#33A02C", "#B2DF8A"), border = "#CAB2D6")
