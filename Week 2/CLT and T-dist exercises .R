library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)



set.seed(1)
n = 100 #sample size
sides = 6

p = 1/sides
zs = replicate(10000, {
  x = sample(1:sides, n, replace=TRUE)
  (mean(x == 6) - p) / sqrt(p * (1-p)/n)
})

qqnorm(zs)
abline(0,1)
mean(abs(zs) > 2)
