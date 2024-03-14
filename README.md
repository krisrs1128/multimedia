# multimedia <img src="https://uwmadison.box.com/shared/static/ygim01is2uer51wzyqg8u5l1mrz0kjo9.png" align="right" width="200" alt=""/>

`multimedia` is an R package for **multi**domain **media**tion analysis
of microbiome data. It makes it easy to curate data, specify mediation
and outcome model formulas, and carry out inference for direct and
indirect effects. By specifying causal relationships between different
data domains, it can support principled data integration. You can read
more about the package in our preprint:

[H. Jiang, X. Miao, M. W. Thairu, M. Beebe, D. W. Grupe, R. J. Davidson,
J. Handelsman, and K. Sankaran (2024). multimedia: Multidomain Mediation
Analysis of Microbiome Data.]()

The preprint describes the scientific context and interpretation for two
of the vignettes in this package. One gives a multi-omics analysis of
IBD, and the other describes how to model compositional outcomes in a
study of depression.

<center>
<img src="https://uwmadison.box.com/shared/static/rg8dv79wwiu4h37ftuuz57vmqv55zuy4.png" width=400/>
</center>

## Installation

You can install the development version from
[GitHub](https://github.com/krisrs1128/multimedia) with:

    # install.packages("devtools")
    devtools::install_github("krisrs1128/multimedia")

## Example

Here is a simple example of estimating direct and indirect effects using
the package. The data are randomly generated (no real effects), but we
will proceed as if the ASV columns mediate the relationship between
`treatment` and `PHQ`. This is supposed to mimic the possibility that
the microbiome (ASV = Amplicon Sequence Variant) might mediate the
relationship between a treatment and depression (PHQ = Patient Health
Questionnaire). You can find more details in the `random.Rmd` vignette.

The original data here are a `SummarizedExperiment`. The package can
also take phyloseq objects and plain data.frames.

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
outcomes.

    exper <- mediation_data(demo_joy(), "PHQ", "treatment", starts_with("ASV"))
    exper

<pre class="r-output"><code>## <span style='color: #BB00BB;'>[Mediation Data]</span> 
## 100 samples with measurements for, 
## 1 treatment: treatment 
## 5 mediators: ASV1, ASV2, ... 
## 1 outcome: PHQ
</code></pre>

From here, we can estimate the model and calculate effects. By default,
the package uses a linear model.

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

We can calculate different types of effects using the estimated model.
Since we’re using a linear model, the effects are identical for the two
indirect settings. We could also visualize and carry out inference on
these effects – see the vignettes for more examples.

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
