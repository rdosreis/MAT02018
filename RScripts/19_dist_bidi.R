## ----mb, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------

library(dplyr)
library(readxl)
library(knitr)

mb_df <- read_excel(path = here::here("data", "companhia_mb.xlsx"))
mb_df %>% 
  select(N ,`Região de Procedência`, `Grau de Instrução`) %>% 
  rename("ID" = "N") %>% 
  # slice_head(n = 10) %>% 
  kable(caption = "Tabela de dados brutos.", format = "pandoc")



## ----tab_dupla, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------
library(kableExtra)

tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  addmargins()

row.names(tab)[4] <- "Total"
colnames(tab)[4] <- "Total"

kable(tab,
      caption = "Distribuição conjunta das frequências das variáveis grau de instrução e região de procedência.", align = "cccc", format = "latex") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))







## ----percent_tot_geral, echo=FALSE, warning=FALSE, message=FALSE------------------------------

prop.tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table() %>% 
  addmargins() * 100

row.names(prop.tab)[4] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 0,
      caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação ao total geral das variáveis grau de instrução e região de procedência.",
      align = "cccc",
      format = "latex") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))



## ----percent_tot_coluna, echo=FALSE, warning=FALSE, message=FALSE-----------------------------

prop.tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table(margin = 2) %>% 
  addmargins() * 100

row.names(prop.tab)[4] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab,
      digits = 0,
      caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação aos totais de colunas das variáveis grau de instrução e região de procedência.",
      align = "cccc",
      format = "latex") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))



## ----barras, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="90%"----

library(ggplot2)
library(viridis)
library(reshape2)

mb_gg <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table(margin = 1) %>% melt()

mb_gg <- mb_gg %>%
  group_by(`Região de Procedência`) %>%
  mutate(value_pos = cumsum(value) - (0.5 * value)) %>%
  mutate(value_lab = round(value * 100, 1))

p <- ggplot(data = mb_gg,
            mapping = aes(x = `Região de Procedência`,
                          y = value)) +
  geom_bar(mapping = aes(fill = rev(`Grau de Instrução`)),
           stat = "identity") +
  geom_text(mapping = aes(y = value_pos, label = value_lab),
            col = "white", vjust = 0) +
  scale_fill_viridis(discrete = T) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Frequências relativas", fill = "Grau de Instrução") +
  theme_bw() + theme(legend.position = "bottom")
p





## ----gremio, echo=FALSE, warning=FALSE, message=FALSE-----------------------------------------

tab <- matrix(c(10, 9, 4, 17, 7, 5),
              byrow = T, ncol = 3, dimnames = list(c("Sem Maicon", "Com Maicon"),
                                                   c("Vitórias", "Empates", "Derrotas")))

prop.tab <- addmargins(prop.table(addmargins(tab, margin = 1), margin = 1), margin = 2) * 100

row.names(prop.tab)[3] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 2,
      caption = "Resultados dos jogos do Grêmio em 2019.",
      align = "cccc",
      format = "latex", format.args = list(decimal.mark = ","))


