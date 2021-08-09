## ----mb, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------------
library(dplyr)
library(readxl)
library(knitr)

mb_df <- read_excel(path = here::here("data", "companhia_mb.xlsx"))
mb_df %>% 
  select(N ,`Região de Procedência`, `Grau de Instrução`) %>% 
  rename("ID" = "N") %>% 
  # slice_head(n = 10) %>% 
  kable(caption = "Tabela de dados brutos.")



## ----tab_dupla, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------
library(kableExtra)

tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  addmargins()

row.names(tab)[4] <- "Total"
colnames(tab)[4] <- "Total"

kable(tab, caption = "Distribuição conjunta das frequências das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))


## ----percent_tot_geral, echo=FALSE, warning=FALSE, message=FALSE-------------------------------------------------
prop.tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table() %>% 
  addmargins() * 100

row.names(prop.tab)[4] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 0, caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação ao total geral das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))


## ----percent_tot_coluna, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------
prop.tab <- mb_df %>% 
  select(`Região de Procedência`, `Grau de Instrução`) %>% 
  table() %>% 
  prop.table(margin = 2) %>% 
  addmargins() * 100

row.names(prop.tab)[4] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 0, caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação aos totais de colunas das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))


## ----barras, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%"---------------------
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


## ----gremio, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------------

tab <- matrix(c(10, 9, 4, 17, 7, 5),
              byrow = T, ncol = 3, dimnames = list(c("Sem Maicon", "Com Maicon"),
                                                   c("Vitórias", "Empates", "Derrotas")))

prop.tab <- addmargins(prop.table(addmargins(tab, margin = 1), margin = 1), margin = 2) * 100

row.names(prop.tab)[3] <- "Total"
colnames(prop.tab)[4] <- "Total"

kable(prop.tab, digits = 2,
      caption = "Resultados dos jogos do Grêmio em 2019.")


## ----pio, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------------

idade <- c(35, 40, 41, 44, 45, 48, 50, 50, 50, 52, 
           54, 55, 55, 55, 57, 58, 59, 60, 60, 61,
           63, 65, 67, 71, 77)

pio <- c(15, 17, 16, 18, 15, 19, 19, 18, 17, 16, 
         19, 18, 21, 20, 19, 20, 19, 23, 19, 22,
         23, 24, 23, 24, 22)

pio_df <- data.frame(id = 1:length(pio), idade, pio)

kable(x = pio_df, 
      caption = "Tabela de dados brutos.",
      col.names = c("ID", "Idade", "PIO"),
      align = 'c')


## ----pontos, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%"---------------------

p <- ggplot(data = pio_df,
            mapping = aes(x = idade, y = pio)) +
  geom_point(colour = I("purple4"), size = 2) +
  labs(x = "Idade (anos)", y = "PIO") +
  theme_bw()
p


## ----pos_neg, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%", out.width="100%"----

library(MASS)
set.seed(1000)

rho <- cbind(c(1, .9), c(.9, 1))
x <- as.data.frame(mvrnorm(n = 1000, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p1 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  stat_ellipse(type = "norm", col = "red") +
  labs(title = "Relação direta") +
  theme_bw()

rho <- cbind(c(1, -.9), c(-.9, 1))
x <- as.data.frame(mvrnorm(n = 1000, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p2 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  stat_ellipse(type = "norm", col = "red") +
  labs(title = "Relação inversa") +
  theme_bw()

library(cowplot)

plot_grid(p1, p2, labels = c('', ''), label_size = 5, ncol = 2)


## ----cor_nula, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%", out.width="100%"----

rho <- cbind(c(1, 0), c(0, 1))
x <- as.data.frame(mvrnorm(n = 1000, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", col = "blue") +
  stat_ellipse(type = "norm", col = "red") +
  labs(title = "Ausência de relação") +
  theme_bw()
p


## ----linear, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%", out.width="100%"----

library(MASS)
set.seed(1000)

rho <- cbind(c(1, .9), c(.9, 1))
x <- as.data.frame(mvrnorm(n = 1000, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p1 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(caption = "Relação linear positiva forte") +
  theme_bw()

rho <- cbind(c(1, .5), c(.5, 1))
x <- as.data.frame(mvrnorm(n = 1000, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p2 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(caption = "Relação linear positiva fraca") +
  theme_bw()

x <- data.frame(X = runif(n = 1000, min = 0.1, max = 2))
x$Y <- 1/x$X^2 + rnorm(1000, sd = 1)

p3 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ I(1/x^2), se = FALSE, color = "red") +
  labs(caption = "Relação não-linear negativa forte") +
  theme_bw()

plot_grid(p1, p2, p3, labels = c('', '', ''), label_size = 1, ncol = 3)


## ----exemplos, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%", out.width="100%"----

library(MASS)
set.seed(1000)

rho <- cbind(c(1, 0), c(0, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p1 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = 0") +
  theme_bw()

rho <- cbind(c(1, .5), c(.5, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p2 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = 0,5") +
  theme_bw()

rho <- cbind(c(1, .8), c(.8, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p3 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = 0,8") +
  theme_bw()

rho <- cbind(c(1, 1), c(1, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p4 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = 1") +
  theme_bw()

x <- data.frame(X = rnorm(n = 100))
x$Y <- 0.5* x$X

p5 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = 1") +
  theme_bw()

x <- data.frame(X = rnorm(n = 100))
x$Y <- 2* x$X

p6 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = 1") +
  theme_bw()

rho <- cbind(c(1, -1), c(-1, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p7 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = -1") +
  theme_bw()

rho <- cbind(c(1, -.7), c(-.7, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p8 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = -0,7") +
  theme_bw()

rho <- cbind(c(1, -.3), c(-.3, 1))
x <- as.data.frame(mvrnorm(n = 100, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p9 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  labs(title = "r = -0,3") +
  theme_bw()

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9,
          labels = rep('', 9),
          label_size = 5, ncol = 3)


## ----pio_coef, echo=FALSE, warning=FALSE, message=FALSE----------------------------------------------------------

idade <- c(35, 40, 41, 44, 45, 48, 50, 50, 50, 52, 
           54, 55, 55, 55, 57, 58, 59, 60, 60, 61,
           63, 65, 67, 71, 77)

pio <- c(15, 17, 16, 18, 15, 19, 19, 18, 17, 16, 
         19, 18, 21, 20, 19, 20, 19, 23, 19, 22,
         23, 24, 23, 24, 22)

pio_df <- data.frame(id = 1:length(pio), idade, pio)
pio_df$idade_centrada <- pio_df$idade - mean(pio_df$idade)
pio_df$pio_centrada <- pio_df$pio - mean(pio_df$pio)
pio_df$idade_z <- (pio_df$idade - mean(pio_df$idade))/sd(pio_df$idade)
pio_df$pio_z <- (pio_df$pio - mean(pio_df$pio))/sd(pio_df$pio)
pio_df$zi_zp <- pio_df$idade_z * pio_df$pio_z

aux <- colSums(pio_df)
pio_df <- rbind(pio_df, aux)

aux <- c(rep(NA, 25), "Soma")
pio_df <- cbind(aux, pio_df)

pio_df$idade_z[26] <- NA
pio_df$pio_z[26] <- NA

options(knitr.kable.NA = '')

kable(x = pio_df, 
      escape = FALSE,
      caption = "Cálculo do coeficiente de correlação.",
      col.names = c(" " ,"ID", "Idade ($x$)", "PIO ($y$)", "$x- \\bar{x}$", "$y- \\bar{y}$", "$\\frac{x- \\bar{x}}{s_x} = z_x$", "$\\frac{y- \\bar{y}}{s_y} = z_y$", "$z_x\\cdot z_y$"),
      align = 'c', digits = c(0, 0, 0, 0, 2, 2, 2, 2, 2),
      format.args = list(decimal.mark = ","))


## ----sal_grupo, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------

mb_df_arrange <- mb_df %>% 
  dplyr::select("N" ,`Salario (x Sal Min)`, `Grau de Instrução`) %>% 
  rename("ID" = "N") %>% 
  arrange(`Grau de Instrução`)

mb_df_arrange %>% 
  kable(caption = "Tabela de dados brutos.",
        align = 'c',
        format.args = list(decimal.mark = ",")) %>% 
  kable_styling() %>%
  row_spec(which(mb_df_arrange$`Grau de Instrução` == "ensino fundamental"), background = "lightsalmon") %>% 
  row_spec(which(mb_df_arrange$`Grau de Instrução` == "ensino médio"), background = "lightblue") %>% 
  row_spec(which(mb_df_arrange$`Grau de Instrução` == "superior"), background = "lightyellow")



## ----sal_grupo_res, echo=FALSE, warning=FALSE, message=FALSE-----------------------------------------------------

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
      format.args = list(decimal.mark = ",")) %>% 
  kable_styling() %>%
  row_spec(1, background = "lightsalmon") %>% 
  row_spec(2, background = "lightblue") %>% 
  row_spec(3, background = "lightyellow")


## ----sal_bp, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="100%", out.width="100%"----

mb_df_arrange$cor <- NULL
mb_df_arrange$cor <- ifelse(mb_df_arrange$`Grau de Instrução` == "ensino fundamental", "lightsalmon", ifelse(mb_df_arrange$`Grau de Instrução` == "ensino médio", "lightblue", "lightyellow"))


bp <- ggplot(data = mb_df_arrange,
             mapping = aes(x = `Grau de Instrução`,
                           y = `Salario (x Sal Min)`)) +
  geom_boxplot(fill = c("lightsalmon", "lightblue", "lightyellow")) +
  theme_bw()

bp


## ----iris, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------

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
        format.args = list(decimal.mark = ","))

