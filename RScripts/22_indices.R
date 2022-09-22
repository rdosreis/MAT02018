## ----serie_arroz, echo=FALSE, warning=FALSE, message=FALSE------------------------------------
library(dplyr)
library(lubridate)
library(readxl)
library(knitr)
library(kableExtra)

arroz_df <- read_excel(path = here::here("data", "CEPEA_20201116161703.xls"), 
                                   skip = 3)
arroz_df$Data <- dmy(arroz_df$Data)

arroz_res <- arroz_df %>% 
  group_by(year(Data), month(Data)) %>% 
  summarize(preco = mean(`À vista R$`)) %>% 
  rename(Ano = `year(Data)`, Mes = `month(Data)`, Preco = preco) %>% 
  filter(Ano %in% c(2020) & Mes %in% 3:6) %>% 
  transform(Preco_kg = Preco/50, Quantidade = c(800, 1000, 900, 1050), Mes = paste("Mês", 0:3)) %>% 
  transform(Valor_total = Quantidade * Preco_kg) %>% 
  select(Mes, Quantidade, Preco_kg, Valor_total)

arroz_res %>% 
  kable(escape = F,
        format = "pandoc",
        align = 'c',
        digits = c(0, 0, 2, 2),
        caption = "Evolução das compras mensais de arroz",
        col.names = c("Período ($t$)", "Quantidade ($kg$)", "Preço ($u.m./kg$)", "Valor total ($u.m.$)"),
        format.args = list(decimal.mark = ",")) %>% 
   footnote(general = "u.m. = unidade monetária.",
           general_title = "Nota: ", 
           footnote_as_chunk = T,
           title_format = c("italic"))



## ----serie_relativos, echo=FALSE, warning=FALSE, message=FALSE--------------------------------

arroz_res %>%
  transform(Quantidade = Quantidade / Quantidade[1] * 100,
            Preco_kg = Preco_kg / Preco_kg[1] * 100,
            Valor_total = Valor_total / Valor_total[1] * 100) %>%
  kable(escape = F,
        format = "pandoc",
        align = 'c',
        digits = c(0, 1, 2, 2),
        format.args = list(decimal.mark = ","),
        caption = "Evolução das compras mensais de arroz (relativos ao mês 0)",
        col.names = c("Período ($t$)", "Quantidade", "Preço", "Valor total"))


## ----cinco_produtos, echo=FALSE, warning=FALSE, message=FALSE---------------------------------

prod <- c("Arroz (kg)", "Leite (L)", "Pão francês (u)", "Cigarro (maço)", "Cerveja (garrafa)")
mes0 <- c(1.98, 1.99, 0.9, 7, 5.99)
mes1 <- c(2.1, 2.08, 0.95, 7.50, 6.99)

cinco_df <- data.frame(prod, mes0, mes1)

cinco_df %>%
  kable(escape = F,
        format = "pandoc",
        align = 'c',
        digits = c(0, 2, 2),
        format.args = list(decimal.mark = ","),
        caption = "Preços vigentes de cinco produtos",
        col.names = c("Produtos", "Mês 0 ($u.m.$)", "Mês 1 ($u.m.$)"))


## ----cinco_relativos, echo=FALSE, warning=FALSE, message=FALSE--------------------------------

cinco_df <- cinco_df %>% 
  transform(rel = mes1/mes0)

cinco_df %>% 
  kable(escape = F,
        format = "pandoc",
        align = 'c',
        digits = c(0, 2, 2, 3),
        format.args = list(decimal.mark = ","),
        caption = "Relativos de preços (cinco produtos)",
        col.names = c("Produtos", "Mês 0 ($u.m.$)", "Mês 1 ($u.m.$)", "$p_1^i/p_0^i$"))

