# Week 3 - Introduction to Confidence Intervals
set.seed(1)

chowPopulation = read.csv("femaleControlsPopulation.csv")
chowPopulation = chowPopulation %>% unlist()

N = 30
mu_chow = mean(chowPopulation)
chow = sample(chowPopulation, N)
chowbar = mean(chow)

se = sd(chow) / sqrt(N)
se

(chowbar - mu_chow) / se
pnorm(2) - pnorm(-2)


## Confidence Intervals Exercises
library(rafalib)
library(dplyr)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)

n = 25
dat.s = sample(bwt.smoke, n)
dat.ns = sample(bwt.nonsmoke, n)

t.test(dat.s, dat.ns)

# Compute a 99% confidence interval for 

Q = qnorm(1 - 0.01/2)
Q = qt(1 - 0.01/2, df=48)
set.seed(1)
testes = qt(1 - 0.01/2, df=(2 * n-2))

testes

se = ((sd(dat.s)^2)/25 + (sd(dat.ns)^2)/25) %>% sqrt()

set.seed(1)


## Confidence Interval Exercises #1
# Set the seed at 1 and obtain two samples, each of size N = 25,
# from non-smoking mothers (dat.ns) and smoking mothers (dat.s).
# If instead of CLT, we use the t-distribution approximation,
# what do we add and subtract to obtain a 99% confidence interval
# (use 2*N-2 degrees of freedom)?
N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,48)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )

set.seed(1)
dat.ns = sample(bwt.nonsmoke, 5)
dat.s = sample(bwt.smoke, 5)

t.test(dat.ns, dat.s)

## Power Calculations Exercises
