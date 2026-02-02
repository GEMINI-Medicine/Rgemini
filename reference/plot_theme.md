# Plot theme for ggplots

Common plotting theme that can be applied to any ggplot object. The
theme is based on
[`ggthemes::theme_foundation()`](http://jrnold.github.io/ggthemes/reference/theme_foundation.md),
with some additional features.

## Usage

``` r
plot_theme(base_size = 12, base_family = "sans", show_grid = NULL, ...)
```

## Arguments

- base_size:

  (`numeric`)  
  Numeric input specifying the base font size, which will be passed to
  [`ggthemes::theme_foundation()`](http://jrnold.github.io/ggthemes/reference/theme_foundation.md).

- base_family:

  (`character`)  
  Base font family (e.g., "sans", "mono", "serif"). Run `windowsFonts()`
  to check for available options. Input will be passed to
  [`ggthemes::theme_foundation()`](http://jrnold.github.io/ggthemes/reference/theme_foundation.md).

- show_grid:

  (`character`)  
  Character inputs specifying whether to show "major" or "minor" grid
  lines. Default is `NULL` to not show any grid lines. Both "major" and
  "minor" grid lines can be plotted by providing a character vector,
  i.e., `show_grid = c("major", "minor")`.

- ...:

    
  Additional arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

ggplot with specified theme

## See also

[`vignette("plotting_theme", package = "Rgemini")`](https://gemini-medicine.github.io/Rgemini/articles/plotting_theme.md)
