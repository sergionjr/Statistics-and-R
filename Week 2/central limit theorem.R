library(gapminder)
library(dplyr)
data(gapminder)
head(gapminder)


# returns the sub array for all entries where the
# "year" column is equal to 1952.
xfull = gapminder[gapminder$year==1952,] 

# returns the proportion of the subarray that has
# a life expectancy => 40
mean(xfull$lifeExp <= 40)

# Manual conduction of the ecdf(x) function
prop = function(q){
  mean(x <= q)
}

x = xfull$lifeExp

qs = seq(from=min(x), to=max(x), length=20)

props = sapply(qs, prop)

plot(qs, props)

props = sapply(qs, function(q) mean(x <= q))

plot(ecdf(x))

##--------------------------------------Start of Central Limit Theorem--------------------------------------------

#Notes: 

library(downloader) 
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )


# pnorm()

y = dat$Bodyweight[dat$Sex == "M" & dat$Diet=="chow"]
y_sd = popsd(y)
y_mean = mean(y)

mean((2 *y_sd + y_mean) > y) - mean(2 *y_sd + y_mean < y)

2 * y_sd + y_mean
mean((2 *y_sd + y_mean) > y) - mean((2 *y_sd + y_mean) < y)

y_sd + y_mean
y_sd
y_mean

# This is the probability density function for the cumulative distribution of ONE...
# So pnorm(1) - pnorm(-1) is (+1 standard deviation) - (-1 standard deviation)
pnorm(1) - pnorm(-1)


z = (y - mean(y)) / popsd(y) # y - mean / sd = z
z
mean(abs(z) <= 2 * popsd(y))



## Alternate approach...
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
y = dat$Bodyweight[dat$Sex == "M" & dat$Diet=="chow"]
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=2 )

mean(mean(y) + 3 * popsd(y) > y) - mean(mean(y) - 3 * popsd(y) > y)

qqnorm(z)

## Using replicate
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
## or...
y = dat$Bodyweight[dat$Sex =="M" & dat$Diet=="chow"]
set.seed(1)

## Description: Takes a sample of 25 values from the number vector y, takes the mean from those 25 values
## and stores it in an array.
## Replicate is responsible for applying the mean() function on the right 10,000 times and store the num vector
## into 'avgs'
avgs = replicate(10000, mean(sample(y, 25)))
mypar(1, 2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs)
