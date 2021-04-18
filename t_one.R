data("mtcars")
library(ggplot2)
library(pwr)
d<-mtcars
x<-d$mpg

t_one <- function(y, m0 = 0, alpha = 0.05, 
                    alternative = "two.sided") {
  M <- mean(y) # moyenne
  df <- length(y)-1 # degre de liberte
  V <- var(y) #  variance
  S <- sqrt(V / df) # erreur standard
  t_observed <- (M - m0) / S #  t observe
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
  
  # intervalle de confiance autour de la moyenne
  LCL <- (M - S * qnorm(1 - alpha / 2)) # limite inferieure
  UCL <- (M + S * qnorm(1 - alpha / 2)) # limite superieure
  
  d_cohen <- (M - m0)/sqrt(V) # d de cohen
  power_posthoc<-pwr.t.test(n = length(x), 
                            d = d_cohen, 
                            sig.level = alpha,
                            type = "one.sample") # post hoc power
  power<-power_posthoc$power
  direction<-sign(t_observed) # direction du t observe
  
  linear_model<- lm(y ~ 1) 
  # modele lineaire utile pour detecter valeurs extremes
  rst<-rstudent(linear_model)
  # library(car)
  # outlierTest(linear_model)
  alpha_rst = qt(1-alpha / length(y), df)
  outlier<-data.frame(dv = y, rst = abs(rst))
  outlier$col <- ifelse(rst<abs(alpha_rst), "green", "red")

   boxplot <-
    ggplot(outlier, 
         aes(y = dv,x = "",
             label = 1:nrow(outlier)),
         color = col) +
      geom_boxplot(width = 0.2, col = "blue",
                   outlier.shape = NA)+
      geom_jitter(aes(col = col),width = 0.02)+
      scale_color_identity()+
    geom_hline(yintercept = M, lty = "dashed") +
    geom_hline(yintercept = c(LCL, UCL), lty = "dotted") +
    guides(col = F)+
    theme_classic(base_size = 12)+
    labs(x = "boxplot",
         y = "raw data",
         subtitle = paste("Boxplot + ",
                          paste("M", "95%CI",
                          sep = "\u00B1"),
                          "+ larger studentized residual = ",
                          round(max(rst),2),
                          " (cutoff = ", round(alpha_rst,2),")"))
  
   qqnorm(residuals(linear_model), ylab="Residuals",
          col = "blue")
   qqline(residuals(linear_model))
   qqplot<-recordPlot()
   
  mini<-ifelse(t_observed > 0, -5, -5 + t_observed)
  maxi<-ifelse(t_observed < 0, 5, 5 + t_observed)
  
  t_plot<-
    ggplot(data.frame(x = c(mini, maxi)), aes(x)) +
    stat_function(fun = dt, args =list(df =df),
                  xlim = c(direction*t_critical,4),
                  geom = "area", fill = "pink") +
    stat_function(fun = dt, args =list(df =df),
                  col = "red") +
    stat_function(fun = dt, args =list(df =df, ncp = t_observed),
                   col = "blue", lty = "dashed")+
    geom_vline(xintercept = t_critical,
               col = "green")+
    geom_vline(xintercept = t_observed,
               col = "blue")+
    labs(x = "t", y = "", 
         subtitle = paste("critical t = ",round(t_critical,3),
                          " + observed t = ", round(t_observed,3)))+
    theme_classic(base_size = 12)
  
  
  result <- paste0("t(", df,") = ",
                   round(t_observed,2),
                   ", p = ", format.pval(p),
                   ", d = ", round(d_cohen,2),
                   ", M = ", round(M,2),
                   ", 95% CI [", round(LCL,2),
                   "; ", round(UCL,2),
                   "], SD = ", round(sqrt(V),2))
  
  value <- list(mean = M, # liste des valeurs a imprimer
                m0 = m0, 
                variance = V,
                sd = sqrt(V),
                df = df,
                alpha = alpha,
                beta = 1- power,
                power = power,
                t_observed = t_observed,
                t_critical = t_critical,
                p.value = p, 
                LCL = LCL, 
                UCL = UCL, 
                qqplot = qqplot,
                boxplot = boxplot,
                t_plot = t_plot,
                cohens_d = d_cohen,
                alternative = alternative,
                result = result)


                  
  
  
  # print(sprintf("P-value = %g",p))
  # print(sprintf("Lower %.2f%% Confidence Limit = %g",
  #               alpha, LCL))
  # print(sprintf("Upper %.2f%% Confidence Limit = %g",
  #               alpha, UCL))
  return(value)
}

t_one(d$mpg)
t_one(mtcars$disp)

