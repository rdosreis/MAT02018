## ----carrega-dados1, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------

# install.packages("readxl")
library(readxl)
library(stringr)

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



## ----fig-barras1, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width='75%'--------------------------

library(ggplot2)

p.barras <- ggplot(data = dados, mapping = aes(x = `Grau de Instrução`)) +
  geom_bar(stat = "count", width = 0.5, aes(fill = I("steelblue"))) +
  labs(x = "Grau de Instrução",
       y = "Frequência",
       title = "Escolaridade dos empregados da seção de orçamentos.",
       caption = "Fonte: Companhia MB.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p.barras







## ----fig-barras2, echo=FALSE, warning=FALSE, message=FALSE, out.width="80%", fig.align='center'--------------------------

p.barras <- ggplot(data = dados, mapping = aes(x = `Grau de Instrução`)) +
  geom_bar(stat = "count",
           width = 0.5,
           aes(y = (..count..)/sum(..count..),
               fill = I("lightsalmon"))) +
  labs(x = "Grau de Instrução",
       y = "Frequência relativa",
       title = "Escolaridade dos empregados da seção de orçamentos.",
       caption = "Fonte: Companhia MB.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p.barras



## ----fig-barras3, echo=FALSE, warning=FALSE, message=FALSE, out.width="80%", fig.align='center'--------------------------

p.barras <- ggplot(data = dados, mapping = aes(x = `Grau de Instrução`)) +
  geom_bar(stat = "count",
           width = 0.5,
           aes(y =  (..count..)/sum(..count..),
               fill = I("#77DD77"))) +
  geom_text(aes(label = scales::percent( (..count..)/sum(..count..)),
                y =  (..count..)/sum(..count..) ), stat = "count", vjust = -.5) +
  scale_y_continuous(limits = c(0, 0.35), labels = scales::percent) +
  labs(x = "Grau de Instrução",
       y = "Porcentagem",
       title = "Escolaridade dos empregados da seção de orçamentos.",
       caption = "Fonte: Companhia MB.") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p.barras



## ----fig-barras4, echo=FALSE, warning=FALSE, message=FALSE, out.width="65%", out.height='65%', fig.align='center'--------

p.barras <- ggplot(data = dados, mapping = aes(x = `Grau de Instrução`)) +
  geom_bar(stat = "count",
           width = 0.5,
           aes(y =  (..count..)/sum(..count..),
               fill = I("#DA70D6"))) +
  geom_text(aes(label = scales::percent( (..count..)/sum(..count..)),
                y =  (..count..)/sum(..count..) ), stat = "count", hjust = -.5) +
  scale_y_continuous(limits = c(0, 0.35), labels = scales::percent) +
  labs(x = "Grau de Instrução",
       y = "Porcentagem",
       title = "Escolaridade dos empregados da seção de orçamentos.",
       caption = "Fonte: Companhia MB.") +
  coord_flip() + 
  theme_bw()

p.barras



## ----fig-barras5, echo=FALSE, warning=FALSE, message=FALSE, out.width="100%", fig.align='center'-------------------------

library(dplyr)

df_barras <- dados %>% 
  group_by(`Grau de Instrução`) %>% 
  summarise(n = n()) %>% 
  mutate(freq = 100 * (n / sum(n)))

p.barras <- ggplot(data = df_barras,
                   mapping = aes(y = `Grau de Instrução`,
                                 x = freq,
                                 label = paste0(round(freq, 0), "%"))) +
  geom_segment(aes(x = 0, y = `Grau de Instrução`, xend = freq, yend = `Grau de Instrução`), color = "grey50") +
        geom_point(size = 7, color = "#FF5349") +
        geom_text(color = "white", size = 2) +
  labs(y = "Grau de Instrução",
       x = "Porcentagem",
       title = "Escolaridade dos empregados da seção de orçamentos.",
       caption = "Fonte: Companhia MB.") +
  theme_bw()

p.barras



## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------

library(janitor)

df_regiao <- dados %>% 
  group_by(`Região de Procedência`) %>% 
  summarise(n = n()) %>% 
  mutate(freq = round(100 * (n / sum(n)), 0)) %>%
  adorn_totals("row") 

knitr::kable(df_regiao,
             col.names = c("Região de Procedência", "Frequência ($n_i$)", "Porcentagem"),
             align = c('l', 'c', 'c'), digits = c(0,0,1))








## ----fig-variacoes-setores, echo=FALSE, warning=FALSE, message=FALSE, out.width="90%", fig.align='center'----------------
# 
# p.setores <- ggplot(data = df_regiao, aes(x = "", y = freq, fill = `Região de Procedência`)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) +
#   theme_void() # remove background, grid, numeric labels


p1 <- ggplot(df_regiao[1:3, ], aes(x = 2, freq, fill = `Região de Procedência`)) +
    geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(freq), "%")), 
              position = position_stack(vjust = 0.5)) +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = "Região de Procedência") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c("#ffd700", "#254290", "#bcbcbc")) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#666666")) +
  xlim(0.5, 2.5)

p2 <- ggplot(dados, aes(y = "", fill = factor(`Região de Procedência`))) +
    geom_bar(width = 1, size = 0.1, color = "white", position = "fill") +
  scale_fill_manual(values = c("#ffd700", "#254290", "#bcbcbc")) +
  # geom_text(data = NULL, aes(label = paste(df_regiao$freq[1:3], "%"),
  #               x =  df_regiao$freq[1:3]), hjust = -.5) +
  scale_x_continuous(labels = scales::percent) +
  labs(fill = "Procedência", x = "") +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank()) 

library(waffle)
library(hrbrthemes)

p3 <- ggplot(data = df_regiao[1:3,], aes(fill = `Região de Procedência`, values = n)) +
  geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
  scale_fill_manual(
    name = "Região de Procedência",
    values = c("#ffd700", "#254290", "#bcbcbc"),
    labels = c("Capital", "Interior", "Outra")
  ) +
  coord_equal() +
  # theme_ipsum_rc(grid = "") +
  theme_enhance_waffle()

library(cowplot)

grid1 <- plot_grid(p1, p2, labels = c('A', 'B'), label_size = 10)
plot_grid(grid1, p3, labels = c('', 'C'), label_size = 10, ncol = 1)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-capediem.jpg'))


