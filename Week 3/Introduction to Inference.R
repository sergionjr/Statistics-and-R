##----------------------------------------Week 3-----------------------------------------------
# Revisiting T-Statistics
library(dplyr)
library(rafalib)




# Introduction to Inference
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke = filter(babies, smoke==0) %>% select(bwt) %>% unlist()
bwt.smoke = babies$bwt[babies$smoke==1]
#bwt.smoke = filter(babies, smoke==1) %>% select(bwt) %>% unlist()

mean(bwt.nonsmoke) - mean(bwt.smoke) #population difference of mean birth weights is ~8.937oz
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
n = 25


samplebwt.nonsmoke = sample(bwt.nonsmoke, 25)
samplebwt.smoke = sample(bwt.smoke, 25)

obs = abs(mean(samplebwt.nonsmoke) - mean(samplebwt.smoke))
se = var(samplebwt.nonsmoke)/n + var(samplebwt.smoke)/n %>% sqrt()

tstat = obs/se
tstat

## re attempt
set.seed(1)
N = 25
dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)

tval = t.test(dat.ns, dat.s)$statistic
tval

# manual t test
obs = abs(mean(dat.ns) - mean(dat.s))
se = var(dat.ns)/N + var(dat.s)/N
se = se %>% sqrt()

obs/se


### 1 - area under the standard normal curve between -tval, tval
pval = 1 - (pnorm(tval) - pnorm(-tval))
# they technically tell you to abs then apply the negative, so if you get issues with ^ then use
pval = 1-(pnorm(abs(tval))-pnorm(-abs(tval)))

## Note on NULL hypotheses: It is always a statement about the POPULATION rather than the SAMPLE
