## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='95%', paged.print=FALSE----
# Considerações finais: exemplos
# install.packages("jpeg")
# install.packages("grid")
library(jpeg)
library(grid)
library(gapminder)
library(dplyr)

gapminder <- gapminder %>%
  mutate(pop_m = pop/1e6)

gapminder07 <- gapminder %>%
  filter(year == 2007)

img <- readJPEG(here::here("images", "hans_rosling.jpg"))

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






## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='50%', out.height='50%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Statistically-Insignificant-ff.jpg'))


