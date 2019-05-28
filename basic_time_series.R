# R basic with time series from web article with python impl
# https://www.ritchievink.com/blog/2018/09/26/algorithm-breakdown-ar-ma-and-arima-models/
library(ggplot2)
library(gridExtra)
set.seed(20190511)
generate_white_noise_graph <- function(n = 100) {
eps = rnorm(n)

eps_df <- data.frame(eps)
eps_df$index <- rownames(eps_df)

ggplot(eps_df) + 
  geom_line(aes(x=index, y=eps, group=1), color = "deepskyblue2") +
  theme_minimal() +
  theme(axis.text.x=element_blank())
}

generate_white_noise_graph()


ma_process_simulation <- function() {
# Simulating MA process 
orders <- c(1,6,11)
plots <- list()
acf_plots <- list()
ma_processes <- list()

for (i in 1:3) {
  q <- orders[i]
  thetas <- runif(q)
  
  ma_processes[[i]] <- data.frame(arima.sim(n=100, list(order=c(0,0,q),ma = thetas)))
  names(ma_processes[[i]])[1] <- "MA_X"
  ma_processes[[i]]$index <- rownames(ma_processes[[i]])
  title <- paste("MA Process(", q, ") = [", paste(round(thetas, 2), collapse=', '), "]")
  
  plots[[i]] <- ggplot(ma_processes[[i]]) + 
    geom_line(aes(x=index, y=MA_X, group=1), color = "deepskyblue4") +
    scale_y_continuous(limits = c(-5, 5)) +
    ggtitle(title) + 
    theme_minimal() +
    theme(axis.text.x=element_blank(), 
          plot.title = element_text(family = "Helvetica", face = "bold", size = 12, colour = "grey"))
  acf_plots[[i]] <- ggAcf(ma_processes[[i]]$MA_X, title=paste("ACF for MA(",q,")"))
}
grid.arrange(plots[[1]],acf_plots[[1]],
             plots[[2]],acf_plots[[2]],
             plots[[3]],acf_plots[[3]], nrow=3, ncol=2)
}
ma_process_simulation()


# Simulating AR process 
orders <- c(1,4,7)
plots <- list()
acf_plots <- list()
ar_processes <- list()

for (i in 1:3) {
  p <- orders[i]
  thetas <- runif(p, max=1/p)
  
  ar_processes[[i]] <- data.frame(arima.sim(n=50, list(order=c(p,0,0),ar = thetas)))
  names(ar_processes[[i]])[1] <- "AR_X"
  ar_processes[[i]]$index <- rownames(ar_processes[[i]])
  title <- paste("AR Process(", p, ") = [", paste(round(thetas, 2), collapse=', '), "]")
  
  plots[[i]] <- ggplot(ar_processes[[i]]) + 
    geom_line(aes(x=index, y=AR_X, group=1), color = "deepskyblue4") +
    scale_y_continuous(limits = c(-5, 5)) +
    ggtitle(title) + 
    theme_minimal() +
    theme(axis.text.x=element_blank(), 
          plot.title = element_text(family = "Helvetica", face = "bold", size = 12, colour = "grey"))
  acf_plots[[i]] <- ggAcf(ar_processes[[i]]$AR_X, title=paste("ACF for AR(",p,")"))
}

grid.arrange(plots[[1]],acf_plots[[1]],
             plots[[2]],acf_plots[[2]],
             plots[[3]],acf_plots[[3]], nrow=3, ncol=2)

