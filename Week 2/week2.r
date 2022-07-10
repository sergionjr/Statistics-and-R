library(dplyr)
dat = read.csv("femaleMiceWeights.csv")
View(dat)

control = filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist()
control

treatment = filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist()
treatment

View(control)
View(treatment)

mean(control)
mean(treatment)

print("High fat diet weight difference vs chow (control)" )
obs = mean(treatment) - mean(control)

population = read.csv("femaleControlsPopulation.csv") %>% unlist()
typeof(population)

sample(population, 12) %>% mean()

##------------------------------------- Random Variables Exercises -----------------------------------

library(downloader)
url = "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename = basename(url)
download(url, destfile=filename)
x = read.csv(filename) %>% unlist() #vector of doubles

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

mean(x)


(sample(x, 5) %>% mean() - mean(x)) %>% abs()

set.seed(5)
(mean(sample(x,5)) - mean(x)) %>% abs()

##------------------------------------ Null Distributions ----------------------------------------------

control = sample(population, 12)
treatment = sample(population, 12)

null_hypothesis = vector("numeric", n)

n = 10000
for (i in 1:n){
  control = sample(population, 12)
  treatment = sample(population, 12)
  
  null_hypothesis[i] = mean(treatment) - mean(control)
}

View(null_hypothesis)
max(null_hypothesis)
hist(null_hypothesis)

sum(null_hypothesis > obs)
mean(null_hypothesis > obs)

##------------------------------------- Null Distribution Exercises ----------------------------------------

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n = 10000
narr = vector("numeric", n)

for (i in 1:n){
  narr[i] = sample(x, 5) %>% mean()
}

xavg = mean(x)

(abs(narr - xavg) > 1) %>% sum() 


