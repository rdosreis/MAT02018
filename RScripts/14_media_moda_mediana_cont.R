## ----carrega-dados0, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE-----------------
## # install.packages("readxl")
## library(readxl)
## 
## dados <- read_excel(path = "companhia_mb.xlsx")




## ----carrega-dados2, warning=FALSE, message=FALSE----------------------------------------
class(dados) # classe do objeto dados
dim(dados) # dimensão do objeto dados


## ----carrega-dados3, warning=FALSE, message=FALSE----------------------------------------
head(dados) # apresenta as primeiras linhas do objeto dados


## ----medias, warning=FALSE, message=FALSE------------------------------------------------
mean(dados$Idade)
mean(dados$`Salario (x Sal Min)`)


## ----medias2, warning=FALSE, message=FALSE-----------------------------------------------
mean(dados$`N de Filhos`, na.rm = TRUE)


## ----medianas, warning=FALSE, message=FALSE----------------------------------------------

median(dados$Idade)
median(dados$`Salario (x Sal Min)`)
median(dados$`N de Filhos`, na.rm = TRUE)



## ----freqs, warning=FALSE, message=FALSE-------------------------------------------------

table(dados$`Estado Civil`)



## ----modas, warning=FALSE, message=FALSE-------------------------------------------------

which.max(table(dados$`Região de Procedência`))
which.max(table(dados$`N de Filhos`))


