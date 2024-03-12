
# multimedia <img src="man/figures/logo.png" align="right" width="200" alt=""/>

`multimedia` is an R package for **multi**-domain **media**tion analysis of
microbiome data. It makes it easy to curate data, specify mediation and outcome
model formulas, and carry out inference for direct and indirect effects. By
specifying causal relationships between different data domains, it can support
principled data integration. You can read more about the package in our
preprint:

[H. Jiang, X. Miao, M. W. Thairu, M. Beebe, D. W. Grupe, R. J. Davidson, J. Handelsman, and K. Sankaran (2024).  multimedia: Multidomain Mediation Analysis of Microbiome Data.]()

<center>
<img src="man/figures/overview_figure-extended.png" width=400/>
</center>

## Installation

You can install the development version from
[GitHub](https://github.com/krisrs1128/multimedia) with:

``` r
# install.packages("devtools")
devtools::install_github("krisrs1128/multimedia")
```

## Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/multimedia/issues) or [email](mailto:ksankaran@wisc.edu).



<!-- 
```{r}
library(hexSticker)
library(showtext)
img <- "~/Downloads/source.jpeg"
font_add_google("Dosis", "dosis")
sticker(img, package = "multimedia", h_color="#a92645", h_fill="#f7f7f7", p_size=35, p_y=1.4, s_x=1, s_y=.75, s_width=.47, p_color="#0c0c0c", dpi=500, p_family = "dosis")
usethis::use_logo("multimedia.png")
``` -->