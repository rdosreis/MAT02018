## ----carrega-dados0, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----------------------
## # install.packages("readxl")
## library(readxl)
## 
## dados <- read_excel(path = "companhia_mb.xlsx")




## ----carrega-dados2, warning=FALSE, message=FALSE---------------------------------------------
class(dados) # classe do objeto dados
dim(dados) # dimensão do objeto dados


## ----carrega-dados3, warning=FALSE, message=FALSE---------------------------------------------
head(dados) # apresenta as primeiras linhas do objeto dados


## ----exetremos, warning=FALSE, message=FALSE--------------------------------------------------
min(dados$Idade)
max(dados$Idade)
range(dados$Idade)


## ----amplitude, warning=FALSE, message=FALSE--------------------------------------------------
diff(range(dados$Idade))


## ----extremos2, warning=FALSE, message=FALSE--------------------------------------------------
min(dados$`N de Filhos`, na.rm = TRUE)
max(dados$`N de Filhos`, na.rm = TRUE)


## ----var, warning=FALSE, message=FALSE--------------------------------------------------------

var(dados$Idade)
sd(dados$Idade)
sqrt(var(dados$Idade))



## ----cv, warning=FALSE, message=FALSE---------------------------------------------------------

sd(dados$Idade)/mean(dados$Idade)



## ----quantis, warning=FALSE, message=FALSE----------------------------------------------------

quantile(dados$Idade, 
         probs = c(0.25, 0.5, 0.75))



## ----boxplot, warning=FALSE, message=FALSE, out.width='70%', fig.align='center'---------------

boxplot(dados$Idade,
        ylab = "Idade", col = "gold", border = "purple")



## ----boxplot2, warning=FALSE, message=FALSE, out.width='70%', fig.align='center'--------------

boxplot(Idade ~ `Estado Civil`, data = dados,
        ylab = "Idade", col = "gold", border = "purple")


