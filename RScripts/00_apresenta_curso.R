## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'rodrigo_cartoon.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='80%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'hi_my_name_is.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='15%', out.height='20%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'AC.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='15%', out.height='20%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'feijao.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='40%', out.height='20%', paged.print=FALSE----

library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image(here::here('images', 'EC.jpg'), scale = 1)
p2 <- ggdraw() + draw_image(here::here('images', 'MCPN.jpg'), scale = 0.82)

plot_grid(p1, p2)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='40%', out.height='20%', paged.print=FALSE----

p1 <- ggdraw() + draw_image(here::here('images', 'barbeiro_errado.jpg'), scale = 1)
p2 <- ggdraw() + draw_image(here::here('images', 'barbeiro_certo.jpg'), scale = 1)

plot_grid(p1, p2)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='40%', out.height='20%', paged.print=FALSE----

library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image(here::here('images', 'EC.jpg'), scale = 0.95)
p2 <- ggdraw() + draw_image(here::here('images', 'GG2.jpg'), scale = 1)

plot_grid(p1, p2)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='90%', out.height='30%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'happy_sad.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='20%', out.height='25%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'SB.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='40%', out.height='20%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'mediation.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'vio.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='90%', out.height='50%', paged.print=FALSE----

p1 <- ggdraw() + draw_image(here::here('images', 'elsa_errado.jpg'), scale = 0.6)
p2 <- ggdraw() + draw_image(here::here('images', 'logo_elsa.png'), scale = 1)

plot_grid(p1, p2)



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'hospborges.jpg'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', out.height='70%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'cirs.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='30%', paged.print=FALSE----
knitr::include_graphics(here::here('images','Rlogo.png'))


## ---- echo=TRUE, eval=FALSE----------------------------------------------------------------------------
## # draw the histogram with the specified number of bins
## hist(x, breaks = bins, col = 'darkgray', border = 'white')


## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'Descritiva_Inferencia.png'))



## ----echo=FALSE, fig.align='center', message=FALSE, warning=FALSE, out.width='100%', out.height='80%', paged.print=FALSE----

knitr::include_graphics(here::here('images', 'EstatDesc.jpg'))


