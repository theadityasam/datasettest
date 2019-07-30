library(openxlsx)
library(ggplot2)
library(animint2)
x <- read.xlsx("1e-2.xlsx")
y <- read.xlsx("1e-3.xlsx")
z <- read.xlsx("1e-4.xlsx")

x <- cbind(x, thold = "e-2")
y <- cbind(y, thold = "e-3")
z <- cbind(z, thold = "e-4")
df <- rbind(x, y, z)
df_systematic <- df[(df$data.name == "systematic"),]
df <- df[!(df$data.name == "systematic"),]
df <- df[!(df$algorithm == "constant"),]


new.df <- data.frame()

for(thold in levels(factor(df$thold))){
  #thold.df <- subset(df, df$thold == paste(thold))
  print(thold)
  thold.df <- df[df$thold == paste(thold),]
  for(dataset in levels(factor(df$data.name))){
    print(dataset)
    temp.df <- thold.df[thold.df$data.name == paste(dataset),]
    for(algorithm in levels(factor(temp.df$algorithm))){
      print(algorithm)
      algo.df <- temp.df[temp.df$algorithm == paste(algorithm),]
      mean.time <- mean(algo.df$seconds)
      median.time <- median(algo.df$seconds)
      auc <- median(algo.df$auc)
      errorid <- tail(names(sort(table(algo.df$errorid))), 1)
      de <- data.frame(data.name = dataset, algorithm = algorithm, mean.time = mean.time, median.time = median.time, 
                       auc = auc, errorid = errorid, threshold = thold)
      new.df <- rbind(new.df, de)
    }
  }
}


p <- ggplot(new.df) + 
  geom_point(aes(y = mean.time, x = algorithm, colour = threshold)) +
  facet_wrap("data.name")
animint(p, out.dir = "/home/asam/animint/animint1/")

q <- ggplot(new.df) + 
  geom_point(aes(y = auc, x = algorithm, colour = threshold, shape = errorid)) +
  scale_size_area() +
  facet_wrap("data.name")
animint(q, out.dir = "/home/asam/animint/animint2/")
