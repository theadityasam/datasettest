library(openxlsx)
library(ggplot2)
library(animint2)
x <- read.xlsx("1e-2.xlsx")
y <- read.xlsx("1e-3.xlsx")
z <- read.xlsx("1e-4.xlsx")
w <- read.xlsx("1e-41e5.xlsx")

x <- cbind(x, thold = "e-2")
y <- cbind(y, thold = "e-3")
z <- cbind(z, thold = "e-4")
w <- cbind(w, thold = "e-4e5")
df <- rbind(x, y, z, w)
 
# df_systematic <- df[(df$data.name == "systematic"),]
# df <- df[!(df$data.name == "systematic"),]
df <- df[!(df$algorithm == "constant"),]



penalty.df <- data.frame()

for(dataset in levels(factor(df$data.name))){
  de <- df[((df$data.name == paste(dataset) & df$algorithm == "penaltyLearning.scale1")),]
  de <- data.frame(data.name = dataset, algorithm = "penaltyLearning.scale1", seconds = mean(de$seconds), auc = mean(de$auc))
  penalty.df <- rbind(penalty.df, de)
}

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
      
      if(algorithm != "penaltyLearning.scale1"){
        mean.time <- mean(algo.df$seconds)
        median.time <- median(algo.df$seconds)
        auc <- median(algo.df$auc)
        baseline.time <- penalty.df[penalty.df$data.name == paste(dataset),]$seconds
        baseline.auc <- penalty.df[penalty.df$data.name == paste(dataset),]$auc
        errorid <- tail(names(sort(table(algo.df$errorid))), 1)
        de <- data.frame(data.name = dataset, algorithm = algorithm, baseline.time = baseline.time,
                         baseline.auc = baseline.auc, mean.time = mean.time, median.time = median.time, 
                         auc = auc, errorid = errorid, threshold = thold)
        new.df <- rbind(new.df, de)
      }
    }
  }
}

order.list <- c()
for(dataset in levels(factor(new.df$data.name))){
  temp.df <- new.df[new.df$data.name == paste(dataset),]
  order.list <- append(order.list, list(mean(temp.df$auc)))
}
names(order.list) <- levels(factor(new.df$data.name))
order.list <- order.list[order(unlist(order.list), decreasing = TRUE)] 
name.order <- names(order.list)

transform.df <- transform(new.df,
                         data.name=factor(data.name,levels=name.order))

p <- ggplot(transform.df) + 
  geom_point(aes(y = mean.time, x = algorithm, colour = threshold), data = new.df) +
  geom_hline(aes(yintercept = baseline.time)) +
  facet_wrap("data.name", scales = "free")
p
animint(p, out.dir = "/home/asam/animint/animint1/")

q <- ggplot(transform.df) + 
  geom_point(aes(y = auc, x = algorithm, colour = threshold, shape = errorid)) +
  geom_hline(aes(yintercept = baseline.auc)) +
  facet_wrap("data.name")
q
animint(q, out.dir = "/home/asam/animint/animint2/")
