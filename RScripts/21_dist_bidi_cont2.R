## ----mb, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------------
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(ggplot2)

mb_df <- read_excel(path = here::here("data", "companhia_mb.xlsx"))


## ----sal_grupo, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------

mb_df_arrange <- mb_df %>% 
  dplyr::select("N" ,`Salario (x Sal Min)`, `Grau de Instrução`) %>% 
  rename("ID" = "N") %>% 
  arrange(`Grau de Instrução`)

mb_df_arrange[1:18,] %>%
  kable(#caption = "Tabela de dados brutos.",
        align = 'c',
        format.args = list(decimal.mark = ","), format = "latex") %>% 
  kable_styling() %>%
  row_spec(which(mb_df_arrange[1:18,]$`Grau de Instrução` == "ensino fundamental"), background = "lightsalmon") %>% 
  row_spec(which(mb_df_arrange[1:18,]$`Grau de Instrução` == "ensino médio"), background = "lightblue") %>% 
  row_spec(which(mb_df_arrange[1:18,]$`Grau de Instrução` == "superior"), background = "lightyellow")



## ----sal_grupo2, echo=FALSE, warning=FALSE, message=FALSE-------------------------------------------------------

mb_df_arrange[19:36,] %>% 
  kable(#caption = "Tabela de dados brutos.",
        align = 'c',
        format.args = list(decimal.mark = ","), format = "latex") %>% 
  kable_styling() %>%
  row_spec(which(mb_df_arrange[19:36,]$`Grau de Instrução` == "ensino fundamental"), background = "lightsalmon") %>% 
  row_spec(which(mb_df_arrange[19:36,]$`Grau de Instrução` == "ensino médio"), background = "lightblue") %>% 
  row_spec(which(mb_df_arrange[19:36,]$`Grau de Instrução` == "superior"), background = "lightyellow")



## ----sal_grupo_res, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------

tab <- mb_df_arrange %>% 
  group_by(`Grau de Instrução`) %>% 
  summarize(n = n(),
            `Média` = mean(`Salario (x Sal Min)`),
            `D. Padrão` = sd(`Salario (x Sal Min)`),
            `Variância` = var(`Salario (x Sal Min)`),
            `Min` = min(`Salario (x Sal Min)`),
            `Q1` = quantile(`Salario (x Sal Min)`, 0.25),
            `Q2` = quantile(`Salario (x Sal Min)`, 0.5),
            `Q3` = quantile(`Salario (x Sal Min)`, 0.75),
            `Max` = max(`Salario (x Sal Min)`)
            )

tab2 <- mb_df_arrange %>% 
  summarize(n = n(),
            `Média` = mean(`Salario (x Sal Min)`),
            `D. Padrão` = sd(`Salario (x Sal Min)`),
            `Variância` = var(`Salario (x Sal Min)`),
            `Min` = min(`Salario (x Sal Min)`),
            `Q1` = quantile(`Salario (x Sal Min)`, 0.25),
            `Q2` = quantile(`Salario (x Sal Min)`, 0.5),
            `Q3` = quantile(`Salario (x Sal Min)`, 0.75),
            `Max` = max(`Salario (x Sal Min)`)
            )

tab2$`Grau de Instrução` <- "Global"
tab2 <- tab2[c(names(tab2)[10],names(tab2)[-10])]

tab <- rbind(tab, tab2)

kable(tab,
      caption = "Medidas-resumo para a variável salário, segundo o grau de instrução, na Companhia MB.",
      align = 'c', digits = c(0,0,rep(2,8)),
      format.args = list(decimal.mark = ","), format = "latex") %>% 
  kable_styling() %>%
  row_spec(1, background = "lightsalmon") %>% 
  row_spec(2, background = "lightblue") %>% 
  row_spec(3, background = "lightyellow")


## ----sal_bp, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="100%"---------------------

mb_df_arrange$cor <- NULL
mb_df_arrange$cor <- ifelse(mb_df_arrange$`Grau de Instrução` == "ensino fundamental", "lightsalmon", ifelse(mb_df_arrange$`Grau de Instrução` == "ensino médio", "lightblue", "lightyellow"))


bp <- ggplot(data = mb_df_arrange,
             mapping = aes(x = `Grau de Instrução`,
                           y = `Salario (x Sal Min)`)) +
  geom_boxplot(fill = c("lightsalmon", "lightblue", "lightyellow")) +
  theme_bw()

bp


## ----iris, echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------

set.seed(1000)

iris_ex_1 <- iris %>% 
  filter(Species == "setosa") %>% 
  slice_head(n = 10)

iris_ex_2 <- iris %>% 
  filter(Species == "versicolor") %>% 
  slice_head(n = 10)

iris_ex_3 <- iris %>% 
  filter(Species == "virginica") %>% 
  slice_head(n = 10)

iris_ex <- rbind(iris_ex_1, iris_ex_2)
iris_ex <- rbind(iris_ex, iris_ex_3)

iris_ex %>% 
kable(caption = "Tabela de dados brutos.",
        align = 'c', col.names = c("comprimento da sépala", "largura da sépala", "comprimento da pétala", "largura da pétala", "espécie"),
        format.args = list(decimal.mark = ","), format = "pandoc")


