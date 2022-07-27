## ----carrega-dados1, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------

# install.packages("readxl")
library(readxl)
library(stringr)
library(janitor)
library(dplyr)
library(ggplot2)

dados <- read_excel(path = here::here("data", "companhia_mb.xlsx"))

set.seed(123)

dados$`Grau de Instrução`[dados$`Grau de Instrução` == "ensino fundamental"] <- sample(x = c("fundamental incompleto", "fundamental completo"), size = length(dados$`Grau de Instrução`[dados$`Grau de Instrução` == "ensino fundamental"]), replace = T)

dados$`Grau de Instrução`[dados$`Grau de Instrução` == "ensino médio"] <- sample(x = c("médio incompleto", "médio completo"), size = length(dados$`Grau de Instrução`[dados$`Grau de Instrução` == "ensino médio"]), replace = T)

dados$`Grau de Instrução`[dados$`Grau de Instrução` == "superior"] <- sample(x = c("superior incompleto", "superior completo"), size = length(dados$`Grau de Instrução`[dados$`Grau de Instrução` == "superior"]), replace = T)

dados$`Estado Civil` <- str_to_title(dados$`Estado Civil`)
dados$`Grau de Instrução` <- str_to_title(dados$`Grau de Instrução`)
dados$`Região de Procedência` <- str_to_title(dados$`Região de Procedência`)

dados$`Grau de Instrução` <- factor(dados$`Grau de Instrução`,
                                    levels = c("Fundamental Incompleto", "Fundamental Completo", "Médio Incompleto", "Médio Completo", "Superior Incompleto", "Superior Completo"))





## ----fig-stripchart, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width='90%'-------
 
p <- ggplot(dados, aes(x = `N de Filhos`)) +
  geom_dotplot(fill = "steelblue") +
  labs(x = "Número de filhos",
       caption = "Fonte: Companhia MB.") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank()) 
p





## ----fig-hist, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width='90%'-------------
 
p <- ggplot(data = dados) +
  geom_histogram(mapping = aes(x = `Salario (x Sal Min)`, y = ..density..),
                 breaks = seq(4, 24, by = 4), fill = "steelblue", color = "white") +
  labs(x = "Salário (x. sal. mínimo)",
       y = "Densidade de frequência",
       title = "Distribuição salarial da seção de orçamentos",
       caption = "Fonte: Companhia MB.") +
  theme_bw()
p



## ----fig-hist2, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width='60%'------------
 
p <- ggplot(data = dados) +
  geom_histogram(mapping = aes(x = `Salario (x Sal Min)`, y = I(36 * ..density..)),
                 breaks = seq(4, 24, by = 4), fill = "steelblue", color = "white") +
  labs(x = "Salário (x. sal. mínimo)",
       y = "Densidade de frequência",
       title = "Distribuição salarial da seção de orçamentos",
       caption = "Fonte: Companhia MB.") +
  theme_bw()
p







## ----fig-freqpoly, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width='90%'---------
 
p <- ggplot(data = dados) +
  geom_freqpoly(mapping = aes(x = `Salario (x Sal Min)`),
                 breaks = seq(4, 24, by = 4), color = I("#DA70D6"), size = 1) +
  scale_x_continuous(breaks = seq(4, 24, by = 4)) +
  labs(x = "Salário (x. sal. mínimo)",
       y = "Frequência",
       title = "Distribuição salarial da seção de orçamentos",
       caption = "Fonte: Companhia MB.") +
  theme_bw()
p



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-girafa.jpg'))


