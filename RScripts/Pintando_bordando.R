## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
plot(hp ~ mpg,
     data = mtcars,
     xlab = "Miles/(US) gallon",
     ylab = "Gross horsepower")

## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
library(ggplot2)
p <- ggplot(data = mtcars,
            mapping = aes(x = mpg, y = hp))
p + geom_point() +
  labs(x = "Miles/(US) gallon", y = "Gross horsepower")

## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
plot(hp ~ mpg,
     data = mtcars,
     xlab = "Miles/(US) gallon",
     ylab = "Gross horsepower")

## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
library(ggplot2)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
cbPalette <- c("#29BF12", "#FFBF00")
p <- ggplot(data = mtcars,
            mapping = aes(x = mpg, y = hp, color = am))
p + geom_point(size = 2) +
  labs(x = "Miles/(US) gallon", y = "Gross horsepower", color = "Transmission") +
  theme_dark() + theme(legend.position = "top") +
  scale_colour_manual(values = cbPalette)

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------
## install.packages("ggplot2")

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------
## library(ggplot2)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
# install.packages("gapminder")
library(gapminder)
gapminder

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
library(dplyr)
gapminder <- gapminder %>%
  mutate(pop_m = pop/1e6)

gapminder07 <- gapminder %>%
  filter(year == 2007)

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
p <- ggplot(data = gapminder07)
p
# print(p)

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp))
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() #<<
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Histograma
p <- ggplot(data = gapminder07,
            mapping = aes(x = lifeExp)) +
  geom_histogram()
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Densidade estimada
p <- ggplot(data = gapminder07,
            mapping = aes(x = lifeExp)) +
  geom_density()
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Densidade estimada (grupos)
p <- ggplot(data = gapminder07,
            mapping = aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Gráfico de barras
p <- ggplot(data = gapminder07,
            mapping = aes(x = continent)) +
  geom_bar()
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Boxplot
p <- ggplot(data = gapminder07,
            mapping = aes(x = continent, y = pop_m)) +
  geom_boxplot()
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Gráfico de violino
p <- ggplot(data = gapminder07,
            mapping = aes(x = continent, y = pop_m)) +
  geom_violin()
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
# Gráfico de linha
p <- ggplot(data = gapminder[which(gapminder$country == "Brazil"),],
            mapping = aes(x = year, y = lifeExp)) +
  geom_line(size = 1)
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="62%"----
# Gráfico de linha
p <- ggplot(data = gapminder[which(gapminder$continent == "Americas"),],
            mapping = aes(x = factor(year), y = lifeExp, group = country, color = country)) +
  geom_line(size = 1) + geom_point()
p

## ----echo=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="65%"----
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, size = pop_m)) +
  geom_point(alpha = 0.3) + geom_text(aes(label = country), color = "gray20")
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="65%"----
p + geom_smooth(method = "lm")

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="65%"----
p + geom_smooth(mapping = aes(x = gdpPercap, y = lifeExp, color = NULL), method = "lm")

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="65%"----
p + geom_smooth(mapping = aes(x = gdpPercap, y = lifeExp, color = NULL), method = "lm", formula = y ~ x + log(x), se = FALSE, color = "red")

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + 
  facet_grid(. ~ year) #<<
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + 
  facet_wrap(. ~ year) #<<
p

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
## gapminder$gdpPercap.cat <- cut(gapminder$gdpPercap,
##                                breaks = c(0, 1005, 3955,
##                                           12235, Inf),
##                                labels = c("Baixa-renda",
##                                           "Renda média-baixa",
##                                           "Renda média-alta",
##                                           "Renda alta"))
## p <- ggplot(data = gapminder,
##             mapping = aes(x = factor(year),
##                           y = lifeExp, fill = factor(year))) +
##   geom_boxplot() +
##   facet_grid(continent ~ gdpPercap.cat) #<<
## p

## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="70%"----
gapminder$gdpPercap.cat <- cut(gapminder$gdpPercap,
                               breaks = c(0, 1005, 3955, 12235, Inf),
                               labels = c("Baixa-renda", "Renda média-baixa", "Renda média-alta", "Renda alta"))
p <- ggplot(data = gapminder,
            mapping = aes(x = factor(year), y = lifeExp, fill = factor(year))) +
  geom_boxplot() + 
  facet_grid(continent ~ gdpPercap.cat) #<<
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp,
                          color = continent, size = pop_m)) +
  geom_point(alpha = 0.3)
p

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
p + labs(x = "Renda per capita (US$)",
         y = "Expectativa de vida (anos)",
         color = "Continente", size = "População/1 milhão")

## ----echo=FALSE, results='hide', fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
p <- p + labs(x = "Renda per capita (US$)",
         y = "Expectativa de vida (anos)",
         color = "Continente", size = "População/1 milhão")

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="100%"----
p + theme(legend.position = "none")

## ----echo=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="100%"----
p + theme(legend.position = "top")

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, fig.align='center', out.width="50%"----
## p + theme(text = element_text(color = "gray20"),
##         legend.position = c("top"), # posição da legenda
##         legend.direction = "horizontal",
##         legend.justification = 0.1, # ponto de ancora para legend.position.
##         legend.text = element_text(size = 11, color = "gray10"),
##         axis.text = element_text(face = "italic"),
##         axis.title.x = element_text(vjust = -1),
##         axis.title.y = element_text(vjust = 2),
##         axis.ticks.y = element_blank(), # element_blank() é como removemos elementos
##         axis.line = element_line(color = "gray40", size = 0.5),
##         axis.line.y = element_blank(),
##         panel.grid.major = element_line(color = "gray50", size = 0.5),
##         panel.grid.major.x = element_blank())

## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="70%"----
p + theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # posição da legenda
        legend.direction = "horizontal",
        legend.justification = 0.1, # ponto de ancora para legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2), 
        axis.ticks.y = element_blank(), # element_blank() é como removemos elementos
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank())

## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="60%"----
p + theme_bw()

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
# install.packages("ggthemes")
library(ggthemes)

## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="100%"----
# Wall Street Journal
p + theme_wsj()

## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, fig.align='center', out.width="100%"----
# The Economist
p + theme_economist()

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------
## ggsave("MeuPrimeiroGGPLOT.pdf")
## ggsave("MeuPrimeiroGGPLOT.png")
## ggsave("MeuPrimeiroGGPLOT.jpg",
##        width = 4, height = 4)

## ----echo=TRUE, results='hide', fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
# Considerações finais: exemplos
# install.packages("jpeg")
# install.packages("grid")
library(jpeg)
library(grid)
img <- readJPEG("~/PintandoEBordando/ArquivosR/images/hans_rosling.jpg")
# start plotting
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp,
                          color = continent, size = pop_m)) + 
  annotation_custom(rasterGrob(img, width = unit(1, "npc"),
                               height = unit(1, "npc")),
                    -Inf, Inf, -Inf, Inf) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(min(gapminder07$lifeExp) * 0.9, max(gapminder07$lifeExp) * 1.05)) +
  geom_point() +
  labs(x = "Renda per capita (US$)",
       y = "Expectativa de vida (anos)",
       color = "Continente", size = "População/1 milhão") +
  theme_bw() +
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # posição da legenda
        legend.direction = "horizontal",
        legend.justification = 0.1, # ponto de ancora para legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2), 
        axis.ticks.y = element_blank(), # element_blank() é como removemos elementos
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  )
p


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='70%', paged.print=FALSE----
# Considerações finais: exemplos
# install.packages("jpeg")
# install.packages("grid")
library(jpeg)
library(grid)
img <- readJPEG("~/PintandoEBordando/ArquivosR/images/hans_rosling.jpg")
# start plotting
p <- ggplot(data = gapminder07,
            mapping = aes(x = gdpPercap, y = lifeExp,
                          color = continent, size = pop_m)) + 
  annotation_custom(rasterGrob(img, width = unit(1, "npc"),
                               height = unit(1, "npc")),
                    -Inf, Inf, -Inf, Inf) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(min(gapminder07$lifeExp) * 0.9, max(gapminder07$lifeExp) * 1.05)) +
  geom_point() +
  labs(x = "Renda per capita (US$)",
       y = "Expectativa de vida (anos)",
       color = "Continente", size = "População/1 milhão") +
  theme_bw() +
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # posição da legenda
        legend.direction = "horizontal",
        legend.justification = 0.1, # ponto de ancora para legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2), 
        axis.ticks.y = element_blank(), # element_blank() é como removemos elementos
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  )
p


## ----echo=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='60%', paged.print=FALSE----
# Considerações finais: exemplos - interatividade
p <- ggplot(data = gapminder07, mapping = aes(x = gdpPercap, y = lifeExp,
                          color = continent, size = pop_m)) + 
  geom_point() + labs(x = "Renda per capita (US$)", y = "Expectativa de vida (anos)",
       color = "Continente", size = "População/1 milhão") +  theme_bw()
p

## ----echo=TRUE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', paged.print=FALSE----
# Considerações finais: exemplos - interatividade
# install.packages("plotly")
library(plotly)
ggplotly(p)

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------
## install.packages("rmarkdown")

## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE-----------------
## library(rmarkdown)

## ---- message=FALSE------------------------------------------------------
summary(gapminder07$pop_m)

## ---- echo=FALSE---------------------------------------------------------
summary(gapminder07$pop_m)

## ---- eval=FALSE---------------------------------------------------------
## summary(gapminder07$pop_m)

## ---- echo=FALSE, results='asis'-----------------------------------------
library(knitr)
mod1 <- lm(lifeExp ~ gdpPercap, data = gapminder07)
kable(summary(mod1)$coef, format = "html")

