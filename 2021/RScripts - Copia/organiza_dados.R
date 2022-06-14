## ----carrega-dados0, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----------------------------------------
## # install.packages("readxl")
library(readxl)

dados <- read_excel(path = "companhia_mb.xlsx")




## ----carrega-dados2, warning=FALSE, message=FALSE---------------------------------------------------------------
class(dados) # classe do objeto dados
dim(dados) # dimensão do objeto dados


## ----carrega-dados3, warning=FALSE, message=FALSE---------------------------------------------------------------
head(dados) # apresenta as primeiras linhas do objeto dados


## ----apuracao, warning=FALSE, message=FALSE---------------------------------------------------------------------
table(dados$`Estado Civil`) # apura dados nominais
table(dados$`Grau de Instrução`) # apura dados ordinais
table(dados$`N de Filhos`) # apura dados discretos
dados$Idade # apura dados contínuos

