# multimedia <img src="man/figures/logo.png" align="right" width="200" alt=""/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/krisrs1128/multimedia/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/krisrs1128/multimedia/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/krisrs1128/multimedia/graph/badge.svg?token=XB865GLQ8B)](https://app.codecov.io/github/krisrs1128/multimedia/)
[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/krisrs1128/multimedia/main/)
<!-- badges: end -->

`multimedia` is an R package for **multi**modal **media**tion analysis
of microbiome data. It streamlines data curation, mediation and outcome
model specification, and statistical inference for direct and indirect
effects. By defining causal relationships across data modalities, it can
support principled data integration. You can read more about the package
in our paper:

[Jiang H, Miao X, Thairu MW, Beebe M, Grupe DW, Davidson RJ, Handelsman J, Sankaran K. Multimedia: multimodal mediation analysis of microbiome data. Microbiol Spectr 0:e01131-24.](https://doi.org/10.1128/spectrum.01131-24)

The paper describes the scientific context and interpretation for two
of the vignettes in this package. One gives a multi-omics analysis of
IBD, and the other describes how to simultaneously model 16S profiles
and survey responses in a mindfulness intervention study.

<center>
<img src="man/figures/overview_figure-extended.png" width=400/>
</center>

## Installation

You can install the stable [CRAN
version](https://cran.r-project.org/package=multimedia) with:

    install.packages("multimedia")

You can install the development version from
[GitHub](https://github.com/krisrs1128/multimedia) with:

    # install.packages("devtools")
    devtools::install_github("krisrs1128/multimedia")

## Example

Here is a simple example of estimating direct and indirect effects. The
data are randomly generated (no real effects), but we imagine that the
ASV columns mediate the relationship between `treatment` and `PHQ`. This
is mimics the possibility that the microbiome (ASV = Amplicon Sequence
Variant) mediates the relationship between a treatment and depression
(PHQ = Patient Health Questionnaire). You can find more details in the
`random.Rmd` vignette.

The original data here are a `SummarizedExperiment`. The package can
also take phyloseq objects and data.frames.

    library(multimedia)
    demo_joy()

<pre class="r-output"><code>## class: SummarizedExperiment 
## dim: 5 100 
## metadata(0):
## assays(1): counts
## rownames(5): ASV1 ASV2 ASV3 ASV4 ASV5
## rowData names(0):
## colnames: NULL
## colData names(2): treatment PHQ
</code></pre>

Next, we specify which columns are the treatment, mediators, and
outcomes. Notice that we can use `tidyselect` syntax to match multiple
columns.

    exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
    exper

<pre class="r-output"><code>## <span style='color: #BB00BB;'>[Mediation Data]</span> 
## 100 samples with measurements for, 
## 1 treatment: treatment 
## 5 mediators: ASV1, ASV2, ... 
## 1 outcome: PHQ
</code></pre>

Next, we fit all mediation analysis components and estimate effects. By
default, the package uses linear models – see the vignettes for examples
using sparse regression, random forests, and bayesian hierarchical
models instead.

    model <- multimedia(exper) |>
        estimate(exper)
    model

<pre class="r-output"><code>## <span style='color: #BB00BB;'>[Multimedia Analysis]</span> 
## Treatments: treatment 
## Outcomes: PHQ 
## Mediators: ASV1, ASV2, ... 
##   
## <span style='color: #BB00BB;'>[Models]</span> 
## mediation: A fitted lm_model(). 
## outcome: A fitted lm_model().
</code></pre>

In any mediation analysis, there are several types of effects that could
be interesting, each corresponding to different ways of traveling from
the treatment to the outcome in the mediation analysis causal graph. In
the block below, `direct_effect` captures treatment effects that bypass
the microbiome; `indirect_effect` are effects that are mediated by ASV
relative abundances. Since this example uses a linear model, the effects
are identical for the two indirect settings.

    direct_effect(model, exper)

<pre class="r-output"><code>##   outcome indirect_setting            contrast direct_effect
## 1     PHQ          Control Control - Treatment    0.09314376
## 2     PHQ        Treatment Control - Treatment    0.09314376
</code></pre>

    indirect_overall(model, exper)

<pre class="r-output"><code>##   outcome direct_setting            contrast indirect_effect
## 1     PHQ        Control Control - Treatment      0.02256029
## 2     PHQ      Treatment Control - Treatment      0.02256029
</code></pre>

The package also includes helpers to visualize and perform inference on
these effects. For example,

    boot <- bootstrap(model, exper, c(direct = direct_effect))

    library(ggplot2)
    ggplot(boot$direct) +
        geom_histogram(aes(direct_effect), bins = 20) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_classic() +
        labs(x = "Direct Effect", y = "Frequency", title = "Bootstrap Distribution")

<img src="man/figures/README-unnamed-chunk-6-1.png" width="500" style="display: block; margin: auto;" />

If we want to use a different type of model, we can just modify the
original `multimedia` specification. Below we use a sparse regression
model, which correctly recovers that the direct effects are 0.

    multimedia(exper, glmnet_model(lambda = .1)) |>
        estimate(exper) |>
        direct_effect()

<pre class="r-output"><code>##   outcome indirect_setting            contrast direct_effect
## 1     PHQ          Control Control - Treatment             0
## 2     PHQ        Treatment Control - Treatment             0
</code></pre>

## Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/multimedia/issues) or
[email](mailto:ksankaran@wisc.edu).

    sessionInfo()

<pre class="r-output"><code>## R version 4.4.1 Patched (2024-08-21 r87049)
## Platform: aarch64-apple-darwin20
## Running under: macOS Sonoma 14.5
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/Chicago
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_3.5.1     multimedia_0.2.0  tidyselect_1.2.1  ranger_0.16.0     glmnetUtils_1.1.9 brms_2.21.0       Rcpp_1.0.13      
## 
## loaded via a namespace (and not attached):
##   [1] tensorA_0.36.2.1            jsonlite_1.8.8              shape_1.4.6.1               magrittr_2.0.3              TH.data_1.1-2               estimability_1.5.1          farver_2.1.2                rmarkdown_2.28             
##   [9] fs_1.6.4                    zlibbioc_1.51.1             vctrs_0.6.5                 multtest_2.61.0             memoise_2.0.1               progress_1.2.3              htmltools_0.5.8.1           S4Arrays_1.5.7             
##  [17] usethis_3.0.0               distributional_0.5.0        curl_5.2.2                  Rhdf5lib_1.27.0             SparseArray_1.5.31          rhdf5_2.49.0                StanHeaders_2.32.10         htmlwidgets_1.6.4          
##  [25] plyr_1.8.9                  sandwich_3.1-0              emmeans_1.10.4              zoo_1.8-12                  cachem_1.1.0                igraph_2.0.3                mime_0.12                   lifecycle_1.0.4            
##  [33] iterators_1.0.14            pkgconfig_2.0.3             Matrix_1.7-0                R6_2.5.1                    fastmap_1.2.0               GenomeInfoDbData_1.2.12     MatrixGenerics_1.17.0       shiny_1.9.1                
##  [41] digest_0.6.37               colorspace_2.1-1            patchwork_1.3.0             S4Vectors_0.43.2            pkgload_1.4.0               miniLNM_0.1.0               GenomicRanges_1.57.1        vegan_2.6-8                
##  [49] labeling_0.4.3              fansi_1.0.6                 mgcv_1.9-1                  httr_1.4.7                  abind_1.4-8                 compiler_4.4.1              remotes_2.5.0               withr_3.0.1                
##  [57] backports_1.5.0             inline_0.3.19               highr_0.11                  QuickJSR_1.3.1              pkgbuild_1.4.4              MASS_7.3-61                 DelayedArray_0.31.11        sessioninfo_1.2.2          
##  [65] biomformat_1.33.0           permute_0.9-7               loo_2.8.0                   tools_4.4.1                 ape_5.8                     httpuv_1.6.15               glue_1.7.0                  rhdf5filters_1.17.0        
##  [73] nlme_3.1-166                promises_1.3.0              grid_4.4.1                  checkmate_2.3.2             cluster_2.1.6               reshape2_1.4.4              ade4_1.7-22                 generics_0.1.3             
##  [81] operator.tools_1.6.3        gtable_0.3.5                formula.tools_1.7.1         tidyr_1.3.1                 hms_1.1.3                   data.table_1.16.0           tidygraph_1.3.1             utf8_1.2.4                 
##  [89] XVector_0.45.0              BiocGenerics_0.51.1         foreach_1.5.2               pillar_1.9.0                stringr_1.5.1               posterior_1.6.0             later_1.3.2                 splines_4.4.1              
##  [97] dplyr_1.1.4                 lattice_0.22-6              survival_3.7-0              Biostrings_2.73.1           miniUI_0.1.1.1              knitr_1.48                  gridExtra_2.3               V8_5.0.0                   
## [105] phyloseq_1.49.0             IRanges_2.39.2              SummarizedExperiment_1.35.1 stats4_4.4.1                xfun_0.47                   bridgesampling_1.1-2        Biobase_2.65.1              devtools_2.4.5             
## [113] matrixStats_1.4.1           rstan_2.32.6                stringi_1.8.4               UCSC.utils_1.1.0            yaml_2.3.10                 evaluate_0.24.0             codetools_0.2-20            tibble_3.2.1               
## [121] cli_3.6.3                   RcppParallel_5.1.9          xtable_1.8-4                munsell_0.5.1               GenomeInfoDb_1.41.1         coda_0.19-4.1               parallel_4.4.1              rstantools_2.4.0           
## [129] ellipsis_0.3.2              prettyunits_1.2.0           profvis_0.3.8               urlchecker_1.0.1            bayesplot_1.11.1            Brobdingnag_1.2-9           glmnet_4.1-8                mvtnorm_1.3-1              
## [137] scales_1.3.0                purrr_1.0.2                 crayon_1.5.3                rlang_1.1.4                 multcomp_1.4-26
</code></pre>
