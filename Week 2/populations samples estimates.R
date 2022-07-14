##########################################
library(downloader)
library(dplyr)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename = basename(url)
download(url, destfile= filename)
dat = read.csv(filename)
##########################################
dat = na.omit(dat)

dat$Sex=="M" & dat$Diet=="chow"


# = vector("numeric", n)
y = dat$Bodyweight[dat$Diet == "chow"][dat$Sex == "M"]


# Use dplyr to create a vector x with the body weight of all males on the control "chow" diet
# couple different ways to do this...

# Filter (data, row constraints..) -> select(row) -> unlist (generate a separate array)
x = filter(dat, Sex=="M", Diet=="chow") %>% select(Bodyweight) %>% unlist()
mean(x)

# Using $ shorthand
x2 = dat$Bodyweight[dat$Sex=="M" & dat$Diet=="chow"]
mean(x2)

popsd(x2) #population standard deviation for a 1d vector

set.seed(1)
X = sample(x2, 25)
mean(X)

hfmale = dat$Bodyweight[dat$Sex=="M" & dat$Diet=="hf"]
mean(hfmale)
popsd(hfmale)

set.seed(1)
HFMALE = sample(hfmale, 25)
mean(HFMALE)

abs(mean(x) - mean(hfmale)) - abs(mean(X) - mean(HFMALE))

x <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(2)
X <- sample(x,25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
set.seed(2)
Y <- sample(y,25)
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )


