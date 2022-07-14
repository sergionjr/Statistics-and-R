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
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

pnorm(dat)
