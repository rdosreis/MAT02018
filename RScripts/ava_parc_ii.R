library(ggplot2)
library(MASS)
set.seed(1000)


x <- data.frame(X = runif(n = 500, min = 0.1, max = 100))
x$Y <- log(x$X) + rnorm(500, sd = 0.5)

p2 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  theme_bw()

rho <- cbind(c(1, 0), c(0, 1))
x <- as.data.frame(mvrnorm(n = 500, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p1 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  theme_bw()

rho <- cbind(c(1, .5), c(.5, 1))
x <- as.data.frame(mvrnorm(n = 500, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p3 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  theme_bw()

rho <- cbind(c(1, -.3), c(-.3, 1))
x <- as.data.frame(mvrnorm(n = 500, mu = c(0,0), Sigma = rho))
names(x) <- c("X", "Y")

p4 <- ggplot(data = x, mapping = aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  ylim(-3,3) + xlim(-3,3) +
  theme_bw()

library(cowplot)

plot_grid(p2, p3, p4, p1,
          labels = letters[1:4],
          label_size = 12, ncol = 2)



library(dplyr)
library(lubridate)
library(readxl)
library(knitr)
library(kableExtra)

prod <- c("Café",
          "Açúcar",
          "Arroz",
          "Feijão",
          "Batata",
          "Cébola")

mes0 <- c(136, 89, 102, 186, 49, 68)
mes1 <- c(182, 95, 106, 250, 66, 78)

quantidade0 <- c(1, 1.5, 2, 3.5, 1.5, 1)
quantidade1 <- c(2.5, 1, 3.5, 4.5, 5, 3)

vinte_df <- data.frame(prod, mes0, quantidade0, mes1, quantidade1)

vinte_df <- vinte_df %>% 
  transform(pq0 = mes0 * quantidade0,
            pq1 = mes1 * quantidade1) %>% 
  transform(w0 = pq0/sum(pq0),
            w1 = pq1/sum(pq1)) %>% 
  transform(rel = mes1/mes0,
            inv_rel = mes0/mes1) %>% 
  transform(relw0 = rel * w0,
            inv_rel_w1 = inv_rel * w1)

vinte_df <- vinte_df[names(vinte_df)[c(1,2,3,8,4,5,9,10,12,11,13)]]

vinte_df %>% 
  kable(escape = F,
        format = "latex",
        align = c('l', rep('c', 10)),
        digits = c(0, 0, 1, 2, 0, 1, 2, 3, 3, 3, 3),
        format.args = list(decimal.mark = ","),
        caption = "Preços, quantidades, importâncias relativas e relativos de preços de produtos de alimentação",
        col.names = c("Produtos ($i$)", "$p_0^i$ ($u.m.$)", "$q_0^i$", "$w_0^i$", "$p_1^i$ ($u.m.$)", "$q_1^i$", "$w_1^i$", "$p_1/p_0$", "$(p_1/p_0)w_0$", "$p_0/p_1$", "$(p_0/p_1)w_1$"))





dist <- c(2375, 1400, 1250, 2325, 985, 2025)
preco <- c(430, 272, 252, 422, 207, 373)

plot(dist, preco,
     xlab = "Distância (em milhas)",
     ylab = "Preço (em dólares)",
     pch = 16, col = "purple")
abline(v = seq(1000, 2400, by = 200), lty = 2, col = "lightgrey")
abline(h = seq(200, 400, by = 50), lty = 2, col = "lightgrey")

cor(dist, preco)

z_dist <- (dist - mean(dist)) / sd(dist) 
z_preco <- (preco - mean(preco)) / sd(preco) 
sum(z_dist * z_preco)/(length(z_dist) - 1)



prop.tab <- matrix(data = c(30, 35, 35, 100, 60, 25, 15, 100),
                   nrow = 2, byrow = T) %>% 
  prop.table(margin = 2) %>% 
  addmargins(margin = 1)

row.names(prop.tab)[3] <- "Total"
colnames(prop.tab)[4] <- c("Total")

kable(prop.tab, digits = 0, caption = "Distribuição conjunta das frequências relativas (em porcentagem) em relação ao total geral das variáveis grau de instrução e região de procedência.") %>% 
  add_header_above(c("Região de Procedência", "Grau de Instrução" = 3, " "))
