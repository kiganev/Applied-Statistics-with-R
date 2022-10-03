help("mean")
print("Hello!")

getwd()
setwd("d:/R_course")

example(mean)

x <- 10L
is.integer(x)
is.numeric(x)

char_var <- "This is a character variable!"

sqrt(-1 + 0i)
class(x)
class(char_var)

x <- c(1, 2, 3, 4, 5, 6)
class(x)
is.vector(x)
x <- 10.0:20.0
x/3

y <- seq(from = 1, to = 10)
y <- seq(from = 1, to = 10, by = 0.5)

z <- c(1, 2 + 3i)
z

w <- c(5.2, 1L)
w

v <- c(NA, 2L)

# Here we start something else

x <- c(1,2,3)
y <- c(2,3,4,5)

x + y

x <- c("Me", "You")
y <- c("Us", "Them")

x + y 

class(x)
mode(x)

x <- 234:657
x[c(26,36)]

x[-c(5,7)]

x <- c(1,2,3)
y <- rep(x,times = 3)

v1 <- c(100, 150, 1000)
names(v1) <- c("One", "Two", "Three")
v1

names(v1)[2] <- "Icecream"
v1

lst1 <- list(121.5, "vacation", TRUE, c(1,2,3), 
             abs, list("zeta",1+0.5i))

lst1a <- lst1[[1]]

lst1 <- list(float = 121.5, character = "vacation", 
             logical = TRUE, vector = c(1,2,3), 
             func = abs, list = list(name1="zeta",cplx=1+0.5i))

lst1$vector[2]

matrix(0,5,5)

diag(5)

m1 <- matrix(c(1,2,3,4,5,6), nrow = 2, byrow = T)
m1

m2 <- matrix(c(1,2,3,4,5,6), nrow = 3, byrow = T)
m2

m1 %*% m2

m1[,3]

