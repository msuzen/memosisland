#' 
#' 
#'  Understanding overfitting: an inaccurate meme in supervised learning
#'  (c) 2017 M.Suzen
#'  Creative Commons Attribution 3.0 Unported License
#'  http://creativecommons.org/licenses/by/3.0/deed.en_GB
#'  



#'
#' Given data.frame with x and ysim, and an R formula with ysim=f(x), 
#' fit a linear model 
#'
get_coefficients <- function(data_portion, model_formula) {
 model <- lm(model_formula, data=data_portion)
 return(model$coefficients)
}

#'
#' Two polynomial models: g and h, 3rd and 5th degree respectively.
#' 
g_fun <- function(x,params) as.numeric(params[1]+x*params[2]+x^2*params[3]+x^3*params[4])
h_fun <- function(x,params) as.numeric(params[1]+x*params[2]+x^2*params[3]+params[4]*x^3+
                                                   params[5]*x^4+params[6]*x^{5}+
                                                   params[7]*x^{6})
g_formula <- ysim ~ I(x) + I(x^2) + I(x^3) 
h_formula <- ysim ~ I(x) + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) 
#'
#' Find the prediction error for a given model_function and model_formula
#'
lm_rmsd <- function(x_train, y_train, x_test, y_test, model_function, model_formula) {
   params <- get_coefficients(data.frame(x=x_train,ysim=y_train), model_formula)
   params[as.numeric(which(is.na(params)))] <- 0 # if there is any co-linearity
   f_hat  <- sapply(x_test, model_function, params=params)
   return(sqrt(sum((f_hat-y_test)^2)/length(f_hat)))
}
 
#' 
#' Generate a synthetic dataset
#' A similar model from Bishop : 
#' 
#' f(x) = sin(2pi*x) + N(0, 0.1)
#'  
set.seed(424242)
f       <- function(x) return(sin(2*pi*x))
fsim    <- function(x) return(sin(2*pi*x)+rnorm(1,0,0.1))
x       <- seq(0,1,1e-2)
y       <- sapply(x,f)
ysim    <- sapply(x,fsim)
simdata <- data.frame(x=x, y=y, ysim=ysim)
#' 
#' Visualise the simulated data
#' 
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
gt <-  theme(
  panel.background = element_blank(), 
  axis.text.x      = element_text(face="bold", color="#000000", size=11),
  axis.text.y      = element_text(face="bold", color="#000000", size=11),
  axis.title.x     = element_text(face="bold", color="#000000", size=11),
  axis.title.y     = element_text(face="bold", color="#000000", size=11)
)  
p1 <- ggplot(data=simdata) + geom_line(aes(x=x,y=y), colour="red", size=1.5) + geom_point(aes(x=x,y=ysim), size=0.5) + gt
grid.newpage()
footnote <- "(c) 2017, Understanding overfitting: an inaccurate meme in supervised learning \n http://memosisland.blogspot.de/"
g <- arrangeGrob(p1, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))

png(file="simdata.png")
grid.draw(g)
dev.off()
#'
#' Demonstration of overtraining with g
#' 
#'
set.seed(424242)
model_function <- g_fun
model_formula  <- g_formula
split_percent  <- seq(0.05,0.95,0.03)
split_len      <- length(split_percent)
data_len       <- length(simdata$ysim) 
splits         <- as.integer(data_len*split_percent)
test_rmsd      <- vector("integer", split_len-1)
train_rmsd     <- vector("integer", split_len-1)
for(i in 2:split_len) {
 train_ix <- sample(1:data_len,splits[i-1]) 
 test_ix  <- (1:data_len)[-train_ix]
 train_rmsd[i-1] <-  lm_rmsd(simdata$x[train_ix], 
                            simdata$ysim[train_ix], 
                            simdata$x[train_ix], 
                            simdata$ysim[train_ix],
                            model_function, 
                            model_formula)
 test_rmsd[i-1] <-  lm_rmsd(simdata$x[train_ix], 
                             simdata$ysim[train_ix], 
                             simdata$x[test_ix], 
                             simdata$ysim[test_ix],
                             model_function, 
                             model_formula)
}

rmsd_df            <- data.frame(test_rmsd=test_rmsd, train_rmsd=train_rmsd, percent=split_percent[-1]) 
rmsd_df2           <- melt(rmsd_df, id=c("percent"))
colnames(rmsd_df2) <- c("percent", "Error_on", "rmsd")
rmsd_df2$test_train <- as.factor(rmsd_df2$Error_on)
p2 <- ggplot(data=rmsd_df2) + geom_point(aes(x=percent, y=rmsd,colour=Error_on)) +
                        geom_smooth(aes(x=percent, y=rmsd, colour=Error_on)) + ggtitle("Detect Overtraining for g(x)") + gt
grid.newpage()
footnote <- "(c) 2017, Understanding overfitting: an inaccurate meme in supervised learning \n http://memosisland.blogspot.de/"
g <- arrangeGrob(p2, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))

png(file="over_training_g.png")
grid.draw(g)
dev.off()

#'
#' Demonstration of overtraining with h
#' 
set.seed(424242)
model_function <- h_fun
model_formula  <- h_formula
split_percent  <- seq(0.05,0.95,0.03)
split_len      <- length(split_percent)
data_len       <- length(simdata$ysim) 
splits         <- as.integer(data_len*split_percent)
test_rmsd      <- vector("integer", split_len-1)
train_rmsd     <- vector("integer", split_len-1)
for(i in 2:split_len) {
  train_ix <- sample(1:data_len,splits[i-1]) 
  test_ix  <- (1:data_len)[-train_ix]
  train_rmsd[i-1] <-  lm_rmsd(simdata$x[train_ix], 
                              simdata$ysim[train_ix], 
                              simdata$x[train_ix], 
                              simdata$ysim[train_ix],
                              model_function, 
                              model_formula)
  test_rmsd[i-1] <-  lm_rmsd(simdata$x[train_ix], 
                             simdata$ysim[train_ix], 
                             simdata$x[test_ix], 
                             simdata$ysim[test_ix],
                             model_function, 
                             model_formula)
}

rmsd_df            <- data.frame(test_rmsd=test_rmsd[-(1:2)], 
                                train_rmsd=train_rmsd[-(1:2)], percent=split_percent[-(1:3)]) 
rmsd_df2           <- melt(rmsd_df, id=c("percent"))
colnames(rmsd_df2) <- c("percent", "Error_on", "rmsd")
rmsd_df2$test_train <- as.factor(rmsd_df2$Error_on)
p2 <- ggplot(data=rmsd_df2) + geom_point(aes(x=percent, y=rmsd,colour=Error_on)) +
  geom_smooth(aes(x=percent, y=rmsd, colour=Error_on)) +  ggtitle("Detect Overtraining for h(x)") + gt
grid.newpage()
footnote <- "(c) 2017, Understanding overfitting: an inaccurate meme in supervised learning \n http://memosisland.blogspot.de/"
g <- arrangeGrob(p2, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))

png(file="over_training_h.png")
grid.draw(g)
dev.off()


#'
#'Plot predictions of g and h on the test set with real data
#'
train_ix <- sample(1:data_len, 50) 
test_ix  <- (1:data_len)[-train_ix]
params_g <- get_coefficients(data.frame(x=simdata$x[train_ix],
                                        ysim=simdata$ysim[train_ix]), 
                             g_formula)
g_hat  <- sapply(simdata$x[test_ix], g_fun, params=params_g)
params_h<- get_coefficients(data.frame(x=simdata$x[train_ix],
                                       Creative Commons Attribution 3.0 Unported License](http://creativecommons.org/licenses/by/3.0/deed.en_GB)
ysim=simdata$ysim[train_ix]), 
                            h_formula)
h_hat  <- sapply(simdata$x[test_ix], h_fun, params=params_h)

fit_df  <- data.frame(
                      g=g_hat, 
                      h=h_hat, 
                      ysim=simdata$ysim[test_ix], 
                      x=simdata$x[test_ix]
                     )
fit_df2           <- melt(fit_df,id.vars = c("x"))
colnames(fit_df2) <- c("x", "model", "y")  
p3 <- ggplot(data=fit_df2) + geom_point(aes(x=x, y=y, colour=model)) + gt +
      ggtitle("Test set predictions (50% split)")
grid.newpage()
footnote <- "(c) 2017, Understanding overfitting: an inaccurate meme in supervised learning \n http://memosisland.blogspot.de/"
g <- arrangeGrob(p3, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))

png(file="test_set_predictions.png")
grid.draw(g)
dev.off()
#' CV for g(x) and h(x)
split_percent  <- seq(0,1,0.1)
split_len      <- length(split_percent)
data_len       <- length(simdata$ysim) 
splits         <- as.integer(data_len*split_percent)
cv_rmsd_g      <- 0
cv_rmsd_h      <- 0
for(i in 2:split_len) { # 10-fold cross validation
  test_ix  <- (splits[i-1]+1):splits[i]
  train_ix   <- (1:data_len)[-test_ix]
  x_train   <- simdata$x[train_ix]
  y_train   <- simdata$ysim[train_ix]
  x_test    <- simdata$x[test_ix]
  y_test    <- simdata$ysim[test_ix]
  cv_rmsd_g <-  lm_rmsd(x_train,
                        y_train,
                        x_test,
                        y_test,
                        g_fun, 
                        g_formula)+cv_rmsd_g
  cv_rmsd_h <-   lm_rmsd(x_train,
                         y_train,
                         x_test,
                         y_test,
                         h_fun, 
                         h_formula)+cv_rmsd_h
}

cat("10-fold CV error G = ", cv_rmsd_g/split_len,"\n") # 0.1304164
cat("10-fold CV error H = ", cv_rmsd_h/split_len,"\n") # 0.1206458