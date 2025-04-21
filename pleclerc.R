
set.seed(5656)

# 1a
alpha <- 0.05
n1 <- 20
(t.20 <- qt(0.95, df = n1-1))

# 1b
n2 <- 30
(t.30 <- qt(0.95, df = n2-1))

# 1c
library(VGAM)
a <- 0
b <- 4
it <- 1000
rej.count <- 0

for(i in 1:it){
  x <- rlaplace(n2, a, b)
  t20 <- mean(x[1:n1])/(sd(x[1:n1])/sqrt(n1))
  if(t20 >= t.20){
    rej.count <- rej.count + 1
  }
}  
(error.rate <- rej.count / it)

# 2a
n <- 15
a1 <- 10
a2 <- 2
b1 <- 2
b2 <- 10
mean1 <- a1/(a1+b1)
mean2 <- a2/(a2+b2)
mean3 <- a1/(a1+b2)
tcrit.one <- qt(0.95, df = n-1)
tcrit.two <- qt(0.975, df = n-1)
rej.left.10.2 <- 0
rej.left.2.10 <- 0
rej.left.10.10 <- 0
rej.right.10.2 <- 0
rej.right.2.10 <- 0
rej.right.10.10 <- 0
rej.two.10.2 <- 0
rej.two.2.10 <- 0
rej.two.10.10 <- 0

for(i in 1:it){
  b.10.2 <- rbeta(n,a1,b1)
  t.10.2 <- (mean(b.10.2)-mean1) / (sd(b.10.2)/sqrt(n))
  b.2.10 <- rbeta(n,a2,b2)
  t.2.10 <- (mean(b.2.10)-mean2) / (sd(b.2.10)/sqrt(n))
  b.10.10 <- rbeta(n,a1,b2)
  t.10.10 <- (mean(b.10.10)-mean3) / (sd(b.10.10)/sqrt(n))
  if(t.10.2 <= -tcrit.one){
    rej.left.10.2 <- rej.left.10.2 + 1
  }
  if(t.2.10 <= -tcrit.one){
    rej.left.2.10 <- rej.left.2.10 + 1
  }
  if(t.10.10 <= -tcrit.one){
    rej.left.10.10 <- rej.left.10.10 + 1
  }
  if(t.10.2 >= tcrit.one){
    rej.right.10.2 <- rej.right.10.2 + 1
  }
  if(t.2.10 >= tcrit.one){
    rej.right.2.10 <- rej.right.2.10 + 1
  }
  if(t.10.10 >= tcrit.one){
    rej.right.10.10 <- rej.right.10.10 + 1
  }
  if(t.10.2 <= -tcrit.two | t.10.2 >= tcrit.two){
    rej.two.10.2 <- rej.two.10.2 + 1
  }
  if(t.2.10 <= -tcrit.two | t.2.10 >= tcrit.two){
    rej.two.2.10 <- rej.two.2.10 + 1
  }
  if(t.10.10 <= -tcrit.two | t.10.10 >= tcrit.two){
    rej.two.10.10 <- rej.two.10.10 + 1
  }
}
(error.left.10.2 <- rej.left.10.2 / it)
(error.left.2.10 <- rej.left.2.10 / it)
(error.left.10.10 <- rej.left.10.10 / it)

# 2b
(error.right.10.2 <- rej.right.10.2 / it)
(error.right.2.10 <- rej.right.2.10 / it)
(error.right.10.10 <- rej.right.10.10 / it)

# 2c
(error.two.10.2 <- rej.two.10.2 / it)
(error.two.2.10 <- rej.two.2.10 / it)
(error.two.10.10 <- rej.two.10.10 / it)