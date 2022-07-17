library(downloader)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)



set.seed(1)
n = 100 #sample size
sides = 6

p = 1/sides

p=0.5
n=30
zs = replicate(10000, {
  x = sample(1:sides, n, replace=TRUE)
  (mean(x == 6) - p) / sqrt(p * (1-p)/n)
})

qqnorm(zs)
abline(0,1)
mean(abs(zs) > 2)


X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

mean(X)
sd(X)

2 * (1 - pnorm(2/sd(X) * sqrt(12)))

## Standard Error (XBAR - YBAR) = standard deviation of x^ 2 / sample size + ...
((sd(X)^2)/12 + (sd(Y)^2)/12) %>% sqrt()

## T-stat
(mean(Y) - mean(X)) / sqrt(var(X)/12 + var(Y)/12)
# or..
t.test(Y,X)

1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)

t.test(Y, X)
