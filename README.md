# multimedia <img src="man/figures/logo.png" align="right" width="200" alt=""/>

`multimedia` is an R package for **multi**modal **media**tion analysis
of microbiome data. It streamlines data curation, mediation and outcome
model specification, and statistical inference for direct and indirect
effects. By defining causal relationships across data modalities, it can
support principled data integration. You can read more about the package
in our preprint:

[H. Jiang, X. Miao, M. W. Thairu, M. Beebe, D. W. Grupe, R. J. Davidson,
J. Handelsman, and K. Sankaran (2024). multimedia: Multimodal Mediation
Analysis of Microbiome Data.](https://www.biorxiv.org/content/10.1101/2024.03.27.587024v1)

The preprint describes the scientific context and interpretation for two
of the vignettes in this package. One gives a multi-omics analysis of
IBD, and the other describes how to simultaneously model 16S profiles
and survey responses in a mindfulness intervention study.

<center>
<img src="man/figures/overview_figure-extended.png" width=400/>
</center>

## Installation

You can install the development version from
[GitHub](https://github.com/krisrs1128/multimedia) with:

    # install.packages("devtools")
    devtools::install_github("krisrs1128/multimedia")

## Example

Here is a simple example of estimating direct and indirect effects. 
The data are randomly generated (no real effects), but we
imagine that the ASV columns mediate the relationship between
`treatment` and `PHQ`. This is mimics the possibility that the
microbiome (ASV = Amplicon Sequence Variant) mediates the relationship
between a treatment and depression (PHQ = Patient Health Questionnaire).
You can find more details in the `random.Rmd` vignette.

The original data here are a `SummarizedExperiment`. The package can
also take phyloseq objects and data.frames.

    library(multimedia)
    library(ggplot2)
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

<pre class="r-output"><code>## <span style='color: #555555;'># A tibble: 2 × 4</span>
##   outcome indirect_setting contrast            direct_effect
##   <span style='color: #555555; font-style: italic;'><chr></span>   <span style='color: #555555; font-style: italic;'><fct></span>            <span style='color: #555555; font-style: italic;'><glue></span>                      <span style='color: #555555; font-style: italic;'><dbl></span>
## <span style='color: #555555;'>1</span> PHQ     Control          Control - Treatment        0.093<span style='text-decoration: underline;'>1</span>
## <span style='color: #555555;'>2</span> PHQ     Treatment        Control - Treatment        0.093<span style='text-decoration: underline;'>1</span>
</code></pre>

    indirect_overall(model, exper)

<pre class="r-output"><code>## <span style='color: #555555;'># A tibble: 2 × 4</span>
##   outcome direct_setting contrast            indirect_effect
##   <span style='color: #555555; font-style: italic;'><chr></span>   <span style='color: #555555; font-style: italic;'><fct></span>          <span style='color: #555555; font-style: italic;'><glue></span>                        <span style='color: #555555; font-style: italic;'><dbl></span>
## <span style='color: #555555;'>1</span> PHQ     Control        Control - Treatment          0.022<span style='text-decoration: underline;'>6</span>
## <span style='color: #555555;'>2</span> PHQ     Treatment      Control - Treatment          0.022<span style='text-decoration: underline;'>6</span>
</code></pre>

The package also includes helpers to visualize and perform inference on
these effects. For example,

    boot <- bootstrap(model, exper, c(direct = direct_effect))
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

<pre class="r-output"><code>## <span style='color: #555555;'># A tibble: 2 × 4</span>
##   outcome indirect_setting contrast            direct_effect
##   <span style='color: #555555; font-style: italic;'><chr></span>   <span style='color: #555555; font-style: italic;'><fct></span>            <span style='color: #555555; font-style: italic;'><glue></span>                      <span style='color: #555555; font-style: italic;'><dbl></span>
## <span style='color: #555555;'>1</span> PHQ     Control          Control - Treatment             0
## <span style='color: #555555;'>2</span> PHQ     Treatment        Control - Treatment             0
</code></pre>

## Help

We welcome questions and comments about the package either through
[github](https://github.com/krisrs1128/multimedia/issues) or
[email](mailto:ksankaran@wisc.edu).
