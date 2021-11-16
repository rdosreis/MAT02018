## ----mb, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------------

library(dplyr)
library(readxl)
library(knitr)

mb_df <- read_excel(path = here::here("data", "companhia_mb.xlsx"))
mb_df %>% 
  select(N ,`Região de Procedência`, `Grau de Instrução`) %>% 
  rename("ID" = "N") %>% 
  # slice_head(n = 10) %>% 
  kable(caption = "Tabela de dados brutos.", format = "pandoc")



## ----tab_dupla, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------
library(kableExtra)

tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  addmargins()

row.names(tab)[4] <- "Total"
colnames(tab)[4] <- "Total"

kable(tab, caption = "Distribuição conjunta das frequências das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))



## ----percent_tot_geral, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------

prop.tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table() %>% 
  addmargins() * 100

row.names(prop.tab)[4] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 0, caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação ao total geral das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))



## ----percent_tot_coluna, echo=FALSE, warning=FALSE, message=FALSE-----------------------------------------------

prop.tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table(margin = 2) %>% 
  addmargins() * 100

row.names(prop.tab)[4] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 0, caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação aos totais de colunas das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))



## ----barras, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="100%"---------------------

library(ggplot2)
library(viridis)
library(reshape2)

mb_gg <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table(margin = 2) %>% melt()

p <- ggplot(data = mb_gg, mapping = aes(x = `Grau de Instrução`, fill = `Região de Procedência`, y = value)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + ylab("Frequências relativas") +
  theme(legend.position = "bottom")
p



## ----gremio, echo=FALSE, warning=FALSE, message=FALSE-----------------------------------------------------------

tab <- matrix(c(10, 9, 4, 17, 7, 5),
              byrow = T, ncol = 3, dimnames = list(c("Sem Maicon", "Com Maicon"),
                                                   c("Vitórias", "Empates", "Derrotas")))

prop.tab <- addmargins(prop.table(addmargins(tab, margin = 1), margin = 1), margin = 2) * 100

row.names(prop.tab)[3] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 2,
      caption = "Resultados dos jogos do Grêmio em 2019.")


