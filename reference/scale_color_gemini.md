# Wrapper function for `ggplot2::scale_color_manual()` that applies GEMINI color palettes to ggplot objects.

Wrapper function for
[`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
that applies GEMINI color palettes to ggplot objects.

## Usage

``` r
scale_color_gemini(palette = 1)
```

## Arguments

- palette:

  (`character` or `numeric`)  
  Name or index of color palette. Run
  [`plot_color_palettes()`](https://gemini-medicine.github.io/Rgemini/reference/plot_color_palettes.md)
  to see options. By default, the first palette ("GEMINI Rainbow") is
  returned.
