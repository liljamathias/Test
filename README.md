# Test

Interactive Kaplan-Meier survival curves with hoverable point estimates,
built on `ggplot2` + `plotly` and drop-in ready for Shiny.

## Install dependencies

```r
install.packages(c(
  "survival", "ggplot2", "ggsurvfit", "pammtools",
  "plotly", "shiny", "scales"
))
```

## Use the function standalone

```r
source("R/interactive_km.R")

lung <- survival::lung
lung$status <- as.integer(lung$status == 2)  # 1/2 -> 0/1

interactive_km(lung, time = "time", event = "status", strata = "sex")
```

Hovering a step vertex reveals `Time`, `S(t)`, `95% CI`, and (when
stratified) the `Group` label.

## Run the demo Shiny app

```r
shiny::runApp(".")
```

Pick a dataset (`lung` or `veteran`), choose a stratifying variable, toggle
the risk table. The curve is fully hover-interactive.
