# Problem 1
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)

n = 1000
avgs = vector("numeric", n)

for (i in 1:n){
  avgs[i] = mean(sample(x, 50))
}

xbar = mean(x)
xbar
mean(avgs > xbar + 1) #population mean xbar +1 
mean(avgs < xbar - 1)


## Problem 2
library(gapminder)
data(gapminder)
head(gapminder)

gap1952 = gapminder$lifeExp[gapminder$year==1952]

mean(60 >= gap1952 & 40 <= gap1952)

## Problem 3
# When we examined mouse weights, we found that our sample estimates for
# females were closer to the population difference than with males.
# What is a possible explanation for this?
# > The population variance of the females is smaller than that of the males; thus,
# > the sample variable has less variability

## Problem 4
mypar(2,2)

# Male Control
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

# Female Control
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

# Male High-fat
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

# Female High-fat
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)