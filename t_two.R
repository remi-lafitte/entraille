data("mtcars")
library(tidyverse)
library(ggplot2)
library(pwr)
library(MESS)
d<-mtcars
y<-d$mpg
x<-d$am
t_two <- function(x,y, m0 = 0, alpha = 0.05, 
                  alternative = "two.sided",
                  equal.variance = FALSE) {
  
  xf<-as.factor(x)
summary<-
  aggregate(y, 
          by = list(x), 
          function(x) c(
    M =  round(mean(x),2),
    V = round(var(x),2),
    SD = round(sd(x),2),
    n = length(x),
    df = length(x)-1
    ))
dat<-as.data.frame(summary$x)

dat$mod<-levels(xf)
M1<- dat$M[1] # mean
M2<- dat$M[2]
v1<- dat$V[1] # variance
v2<- dat$V[2]
n1<- dat$n[1] # size
n2<- dat$n[2]

Sp <- sqrt( (1/n1 + 1/n2) * ((n1-1)*v1 + (n2-1)*v2)/(n1+n2-2) )
# pooled standard deviation
dfp <- n1+n2-2
# pooled df (var equal is true)

if(equal.variance==FALSE) 
{
  S <- sqrt( (v1/n1) + (v2/n2) ) # standard error
  # welch-satterthwaite df
  df <- ( (v1/n1 + v2/n2)^2 )/( (v1/n1)^2/(n1-1) + (v2/n2)^2/(n2-1) )
} else
{
  # pooled standard deviation, scaled by the sample sizes
  S <- Sp
  df <- dfp
}
# Difference de moyenne
Mdiff <- M1-M2 
# Cohen d 
d_cohen<-Mdiff/(sqrt(Sp/dfp))
# intervalle de confiance autour des moyennes
LCL = (Mdiff  - S * qnorm(1 - alpha / 2))
UCL  = (Mdiff  + S * qnorm(1 - alpha / 2))

t_observed <- (Mdiff - m0) / S #  t observe
  p <- if (alternative == "two.sided") {
    2 * pnorm(abs(t_observed), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(t_observed, lower.tail = TRUE)
  } else {
    pnorm(t_observed, lower.tail = FALSE)
  }
  
t_critical <- if (alternative == "two.sided") { # t critique
    qt(alpha/2, df, lower.tail=FALSE)
  } else if (alternative == "less") {
    qt(alpha, df, lower.tail=TRUE)
  } else {
    qt(alpha, df, lower.tail=FALSE)
  }

# post hoc power
xt<-table(xf)

power0<-
MESS::power_t_test(n=min(table(xf)), 
                   sd = Sp, 
                   power=NULL, 
                   ratio=max(xt)/min(xt), 
                   sd.ratio= max(dat$V)/min(dat$V), 
                   delta=Mdiff)
# post hoc power
power<-power0$power

# direction du t observe
direction<-sign(t_observed) 
  
linear_model<- lm(y ~ xf) 
# modele lineaire utile pour detecter valeurs extremes
rst<-rstudent(linear_model)
alpha_rst = qt(1-alpha / length(x), sum(dat$n))
outlier<-data.frame(dv = y, iv = xf, rst = abs(rst))
outlier$col <- ifelse(rst<abs(alpha_rst), "grey50", "black")

 boxplot <-
ggplot(outlier, 
         aes(y = dv,x = iv,label = 1:nrow(outlier)),
         color = col) +
    geom_boxplot(width = 0.2, col = "black",
                   outlier.shape = NA)+
    geom_jitter(aes(col = col),width = 0.03, alpha = 0.5,
                size = 2)+
    scale_color_identity()+
    geom_hline(yintercept = mean(dat$M), lty = "dashed") +
    stat_summary(FUN = "mean", geom = "point", shape = 22, 
                 fill = "black", size = 3)+
    guides(col = F)+
    theme_classic(base_size = 12)+
    labs(x = "boxplot",
         y = "raw data",
         subtitle = paste("M=",
                          paste(round(Mdiff,1) ,round(S * qnorm(1 - alpha / 2),1),
                          sep = "\u00B1"),
                          "+ larger studentized residual = ",
                          round(max(rst),2),
                          " (cutoff = ", round(alpha_rst,2),")"))
   
   
   plot(linear_model, which = 2,  bg = alpha("grey50", 0.5),pch = 21)
   qqplot<-recordPlot()
   plot(linear_model, which = 3,  bg = alpha("grey50", 0.5),pch = 21)
   varplot<-recordPlot()
   
  mini<--5
  maxi<-abs(t_observed) + 5
 t_plot <-
    ggplot(data.frame(x = c(mini, maxi)), aes(x)) +
    stat_function(fun = dt, args =list(df =df),
                  xlim = c(abs(t_critical),4),
                  geom = "area", fill = "pink") +
      # fill alpha area H0
    stat_function(fun = dt, args =list(df =df),
                  col = "red") +
      # curve H0 
    stat_function(fun = dt, args =list(df =df,ncp = abs(t_observed)),
                    xlim = c(mini, abs(t_critical)),
                    geom = "area", fill = alpha("blue",0.3))+
      # fill alpha area non central distribution 
    stat_function(fun = dt, args =list(df =df, ncp = abs(t_observed)),
                   col = "blue", lty = "dashed")+
      # curve alpha area non central distribution 
    geom_vline(xintercept = t_critical,
               col = "green2")+
    geom_vline(xintercept = abs(t_observed),
               col = "blue")+
    labs(x = "t", y = "", 
         subtitle = paste("critical t = ",round(t_critical,3),
                          " + observed t = ",abs(round(t_observed,3))))+
    theme_classic(base_size = 12)+
    theme(panel.background = element_rect(fill = "grey95"))
      
  
  cltxt <- function(lcl, ucl){
    return(paste0("95% CI [", round(lcl,2),
    "; ", round(ucl,2),"]"))
  }
  result <- paste0("t(", round(df,1),") = ",
                   round(t_observed,2),
                   ", p = ", format.pval(p, eps = 0.001),
                   ", d = ", round(d_cohen,2),
                   ", M = ", round(Mdiff,2),", ",
                   cltxt(LCL, UCL)
                   )
MSD<-
  dat %>% 
  mutate(msd = paste0("Modality = ", mod, ", m=", M, ", sd=", SD, ", n=", n)) %>% 
  select(msd)

  
  value <- list(mean = Mdiff, # liste des valeurs a imprimer
                LCL = LCL, 
                UCL = UCL, 
                m0 = m0,
                df = df,
                alpha = alpha,
                beta = 1- power,
                power = power,
                t_observed = t_observed,
                t_critical = t_critical,
                p.value = p, 
                qqplot = qqplot,
                varplot = varplot,
                boxplot = boxplot,
                t_plot = t_plot,
                cohens_d = d_cohen,
                alternative = alternative,
                result = result,
                MSD = list(MSD))

  return(value)
}
args(t_two)
t_two(y = d$disp, d$am)
t_two(y = d$disp, d$am, equal.variance = T)
t_one(mtcars$disp)

