library(downloader) 
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n = 1000
averages5 = vector("numeric", n)

for (i in 1:n){
  X = sample(x, 5)
  averages5[i] = mean(X)
}

averages5

hist(averages5)

set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

mean(averages50 >= 23) - mean(averages50 >= 25)
#or..
mean(averages50 < 25 & averages50 > 23)

pnorm((25 - 23.9) / 0.43) - pnorm((23 - 23.9) / 0.43)
