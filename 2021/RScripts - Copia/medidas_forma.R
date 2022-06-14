## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
library(fGarch)

r <- rsnorm(1000, xi = 5)

layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE),  height = c(1, 8))


par(mar = c(0, 3.1, 1.1, 2.1))
boxplot(r, horizontal = TRUE, ylim = c(-2,5), xaxt = "n" , col = rgb(0.8,0.8,0,0.5) , frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(r,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", xlim = c(-2,5))
abline(v = mean(r),
       lty = 2, col = "red", lwd = 2)
text(0.3, 200, "Média")



## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
library(fGarch)

set.seed(2010)

r <- rsnorm(1000, xi = -5)

layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE),  height = c(1, 8))


par(mar = c(0, 3.1, 1.1, 2.1))
boxplot(r, horizontal = TRUE, ylim = c(-5,2), xaxt = "n" , col = rgb(0.8,0.8,0,0.5) , frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(r,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", xlim = c(-5,2))
abline(v = mean(r),
       lty = 2, col = "red", lwd = 2)
text(-0.3, 200, "Média")



## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------

set.seed(1020)
x <- sample(x = iris$Petal.Width[iris$Species == "setosa"], size = 30, replace = FALSE)

layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE),  height = c(1, 8))

par(mar = c(0, 3.1, 1.1, 2.1))
boxplot(x, horizontal = TRUE, ylim = c(0.1,0.6), xaxt = "n" , col = rgb(0.8,0.8,0,0.5) , frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(x,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", xlim = c(0.1,0.6))
abline(v = mean(x),
       lty = 2, col = "red", lwd = 2)
text(0.3, 15, "Média")







## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="100%", fig.cap='Uma distribuição leptocúrtica.'----

library(rmutil)

set.seed(2010)

m <- rnorm(1000)
p <- rt(1000, df = 8)
l <- rlaplace(1000, s = 0.25)

hist(l,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", breaks = 30, xlim = c(-3,3))



## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="100%", fig.cap='Uma distribuição platicúrtica.'----

hist(p,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", breaks = 30, xlim = c(-3,3))



## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="100%", fig.cap='Uma distribuição mesocúrtica.'----

hist(m,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", breaks = 30, xlim = c(-3,3))



## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------

set.seed(1020)
x <- sample(x = iris$Petal.Length[iris$Species == "setosa"], size = 30, replace = FALSE)

layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE),  height = c(1, 8))

par(mar = c(0, 3.1, 1.1, 2.1))
boxplot(x, horizontal = TRUE, ylim = c(1,2), xaxt = "n" , col = rgb(0.8,0.8,0,0.5) , frame = F)

par(mar=c(4, 3.1, 1.1, 2.1))

hist(x,
     xlab = "x", ylab = "Frequência",
     col = rgb(0,0.25,0.25,0.5), border = "white", main = "", xlim = c(1,2))
abline(v = mean(x),
       lty = 2, col = "red", lwd = 2)
text(1.6, 12, "Média")





## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------
library(SemiPar)
library(knitr)
data(pig.weights)
kable(t(pig.weights$weight[pig.weights$num.weeks == 1][1:5]), col.names = paste("Ovelha",1:5), format.args = list(decimal.mark = ","))




## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
library(SemiPar)
data(pig.weights)
hist(pig.weights$weight[pig.weights$num.weeks == 1],
     xlab = "Peso de 48 ovelhas", ylab = "Frequência",
     col = rgb(0,0,1,0.5), border = "white", main = "")
abline(v = mean(pig.weights$weight[pig.weights$num.weeks == 1]),
       lty = 2, col = "red", lwd = 2)
text(28, 15, "Média = 25 kg\n s = 2,47 kg")




## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
hist(pig.weights$weight[pig.weights$num.weeks == 1] - 8,
     xlab = "Peso de 48 ovelhas depois da tosa",
     ylab = "Frequência",
     col = rgb(1,0,0,0.5), border = "white",
     main = "")
abline(v = mean(pig.weights$weight[pig.weights$num.weeks == 1] - 8),
       lty = 2, col = "red", lwd = 2)
text(20, 15, "Média = 17 kg\n s = 2,47 kg")


## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
hist(pig.weights$weight[pig.weights$num.weeks == 1] - 8,
     xlab = "Peso de 48 ovelhas", ylab = "Frequência",
     col = rgb(1,0,0,0.5), border = "white",
     main = "", xlim = c(8, 34))
hist(pig.weights$weight[pig.weights$num.weeks == 1],
     col = rgb(0,0,1,0.5), border = "white", add = TRUE)
abline(v = c(mean(pig.weights$weight[pig.weights$num.weeks == 1] - 8),
             mean(pig.weights$weight[pig.weights$num.weeks == 1])),
       lty = 2, col = "red", lwd = 2)
legend("topright",
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), border = "white",
       c("Depois da tosa", "Antes da tosa"), bty = "n")


## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
hist(pig.weights$weight[pig.weights$num.weeks == 1]*1.5,
     xlab = "Peso de 48 ovelhas em gestação", ylab = "Frequência",
     col = rgb(0,1,0,0.5), border = "white", main = "")
abline(v = mean(pig.weights$weight[pig.weights$num.weeks == 1]*1.5),
       lty = 2, col = "red", lwd = 2)
text(40, 10, "Média = 37,5 kg\n s = 3,7 kg")


## ----echo=FALSE, message=FALSE, warning=FALSE, results='asis', fig.align='center', out.width="80%"----------------
hist(pig.weights$weight[pig.weights$num.weeks == 1]*1.5,
     xlab = "Peso de 48 ovelhas", ylab = "Frequência",
     col = rgb(0,1,0,0.5), border = "white",
     main = "", xlim = c(18, 52))
hist(pig.weights$weight[pig.weights$num.weeks == 1],
     col = rgb(0,0,1,0.5), border = "white", add = TRUE)
abline(v = c(mean(pig.weights$weight[pig.weights$num.weeks == 1]),
             mean(pig.weights$weight[pig.weights$num.weeks == 1]*1.5)),
       lty = 2, col = "red", lwd = 2)
legend("topright",
       fill = c(rgb(0,0,1,0.5), rgb(0,1,0,0.5)), border = "white",
       c("Antes da gestação", "Na gestação"), bty = "n")












## ----carrega-dados0, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE------------------------------------------
## # install.packages("readxl")
library(readxl)

dados <- read_excel(path = "companhia_mb.xlsx")




## ----carrega-dados2, warning=FALSE, message=FALSE-----------------------------------------------------------------
class(dados) # classe do objeto dados
dim(dados) # dimensão do objeto dados


## ----carrega-dados3, warning=FALSE, message=FALSE-----------------------------------------------------------------
head(dados) # apresenta as primeiras linhas do objeto dados


## ----exetremos, warning=FALSE, message=FALSE----------------------------------------------------------------------
# install.packages("e1071")
library(e1071)

skewness(dados$Idade)
kurtosis(dados$Idade)

skewness(dados$`Salario (x Sal Min)`)
kurtosis(dados$`Salario (x Sal Min)`)

skewness(dados$`N de Filhos`, na.rm = TRUE)
kurtosis(dados$`N de Filhos`, na.rm = TRUE)


