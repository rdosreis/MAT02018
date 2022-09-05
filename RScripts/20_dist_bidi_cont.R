## ----pio, echo=FALSE, warning=FALSE, message=FALSE--------------------------------------------

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
      align = 'c', format = "pandoc")



## ----pontos, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----

p <- ggplot(data = pio_df,
            mapping = aes(x = idade, y = pio)) +
  geom_point(colour = I("purple4"), size = 2) +
  labs(x = "Idade (anos)", y = "PIO") +
  theme_bw()
p



## ----pos_neg, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="80%"----

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



## ----cor_nula, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="60%", out.width="60%"----

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



## ----linear, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.width="100%"----

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


## ----exemplos, echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', out.height="80%", out.width="80%"----

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


## ----pio_coef, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------

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
      format.args = list(decimal.mark = ","), format = "pandoc")


