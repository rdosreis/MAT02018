## ----carrega-dados0, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE--------------------------------------------
## # install.packages("readxl")
## library(readxl)
## 
## dados <- read_excel(path = "companhia_mb.xlsx")




## ----carrega-dados2, warning=FALSE, message=FALSE-------------------------------------------------------------------
class(dados) # classe do objeto dados
dim(dados) # dimensão do objeto dados


## ----carrega-dados3, warning=FALSE, message=FALSE-------------------------------------------------------------------
head(dados) # apresenta as primeiras linhas do objeto dados


## ----freqs, warning=FALSE, message=FALSE----------------------------------------------------------------------------
table(dados$`Estado Civil`)
table(dados$`Grau de Instrução`)
table(dados$`N de Filhos`)


## ----freqs2, warning=FALSE, message=FALSE---------------------------------------------------------------------------
dados$Idade.classes <- cut(x = dados$Idade,
                           breaks = c(20, 29, 39, 49),
                           include.lowest = TRUE,
                           right = FALSE)

table(dados$Idade.classes)


## ----freqs3, warning=FALSE, message=FALSE---------------------------------------------------------------------------
table(dados$`Estado Civil`) / 36
table(
  dados$`Grau de Instrução`)/length(dados$`Grau de Instrução`
                                    )


## ----freqs4, warning=FALSE, message=FALSE---------------------------------------------------------------------------
prop.table(x = table(dados$`N de Filhos`))


## ----porcentagem, warning=FALSE, message=FALSE----------------------------------------------------------------------
prop.table(x = table(dados$Idade.classes)) * 100


## ----porcentagem2, warning=FALSE, message=FALSE---------------------------------------------------------------------
round(x = prop.table(x = table(dados$Idade.classes)) * 100,
      digits = 2)


## ----freqcum, warning=FALSE, message=FALSE--------------------------------------------------------------------------
cumsum(x = table(dados$`Grau de Instrução`))
cumsum(x = prop.table(x = table(dados$`Grau de Instrução`)))


## ----freqcum2, warning=FALSE, message=FALSE-------------------------------------------------------------------------
cumsum(x = prop.table(x = table(dados$`N de Filhos`)) * 100)
cumsum(
  round(
  prop.table(x = table(dados$Idade.classes)) * 100,
  digits = 2))


## ----freqtab, warning=FALSE, message=FALSE--------------------------------------------------------------------------
df.freq <- data.frame(
  Idade = unique(dados$Idade.classes),
  Freq = as.numeric(table(dados$Idade.classes)),
  FreqRel = as.numeric(prop.table(table(dados$Idade.classes))),
  Porcentagem = as.numeric(prop.table(table(dados$Idade.classes)) * 100),
  FreqAcumulada = as.numeric(cumsum(table(dados$Idade.classes))),
  FreqRelAcumulada = as.numeric(cumsum(prop.table(table(dados$Idade.classes)))))


## ----freqtabprint, warning=FALSE, message=FALSE---------------------------------------------------------------------
df.freq


## ----freqtab2, warning=FALSE, message=FALSE-------------------------------------------------------------------------
# install.packages("summarytools")

summarytools::freq(dados$`Grau de Instrução`)















## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-mickey.jpg'))


