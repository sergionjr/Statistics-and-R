install.packages("rafalib")
library(rafalib)

install.packages("swirl")
library(swirl)


intvector = c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)

mean(intvector)

value <- 0

for (i in 1:25) {
  value <- value + i * i
  print(value)
}

print(25**2)

class(cars)
cars[,2]
mean(cars[,2])
which(cars[,2] == 85)
which(cars == 85)
cars[,1]
