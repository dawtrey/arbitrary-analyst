library(TTR)
library(readr)
library(pracma)

library(ggplot2)
theme_set(theme_bw())

data <- read_csv("data.csv")
close <- unlist(data[,5])
ma <- cbind(movavg(close, 20, "e"), movavg(close, 50, "e"))
x = matrix(nrow = length(ma[,1]), ncol =2, 1)

for(i in 2:length(ma[,1])){
  x[i,1] <- close[i] / close[i-1]
  if(ma[i-1,1]>ma[i-1,2]){
    x[i,2] <- x[i,1]
  }
}

y = matrix(nrow = (length(ma[,1])-50), ncol =2, 1)

for(i in 1:(length(x)/2 -50)){
  y[i,1] <- prod(x[50:(i+50),1])
  y[i,2] <- prod(x[50:(i+50),2])
}

hold <- c("If held:", y[length(y)/2 , 1])
active <- c("If using 20/50 day moving averages", y[length(y)/2, 2])

print(hold)
print(active)

y <- cbind(data[51:nrow(data),1], y)

Y <- data.frame(y)
colnames(Y) <- c("Date", "Hold", "Active")

returns <- ggplot(data = Y, aes(x = Date)) +
  scale_color_manual(name="Strategy",
    values = c("Hold"="blue", "Using 20/50 day EMA indicator"="red"))+
  geom_line(aes(y = Hold, col = "Hold")) +
  geom_line(aes(y = Active, col = "Using 20/50 day EMA indicator")) +
  ylab(label="Returns") +
  xlab("Date") +
  labs(title = "Returns on 1 unit investment | PZC.L | PZ Cussons | 01/01/2000 - 01/01/2019",
       caption = "The Arbitrary Analyst | Raw data source: Yahoo Finance")
  


ggsave(returns, filename = "returns", device = "png", width = 35, height = 18, units = "cm")
write.csv(c(hold, active), file = "results.csv") 
