#' @title
#' Plot theme for ggplots
#'
#' @description
#' Common plotting theme that can be applied to any ggplot object. The theme is
#' based on `ggthemes::theme_foundation()`, with some additional features.
#'
#' @param base_size (`numeric`)\cr
#' Numeric input specifying the base font size, which will be passed to
#' `ggthemes::theme_foundation()`.
#' @param base_family (`character`)\cr
#' Base font family (e.g., "sans", "mono", "serif"). Run `windowsFonts()` to
#' check for available options. Input will be passed to
#' `ggthemes::theme_foundation()`.
#' @param show_grid (`character`)\cr
#' Character inputs specifying whether to show "major" or "minor" grid lines.
#' Default is `NULL` to not show any grid lines. Both "major" and "minor" grid
#' lines can be plotted by providing a character vector, i.e.,
#' `show_grid = c("major", "minor")`.
#' @param ... \cr
#' Additional arguments passed to `ggplot2::theme()`.
#' @import ggplot2
#' @importFrom ggthemes theme_foundation
#'
#' @return
#'
#' ggplot with specified theme
#' @seealso `vignette("plotting_theme", package = "Rgemini")`
#'
#' @export
#'
plot_theme <- function(
  base_size = 12,
  base_family = "sans",
  show_grid = NULL,
  ...
) {
  res <- (
    theme_foundation(
      base_size = base_size,
      base_family = base_family
    ) +
      theme(
        plot.margin = unit(c(2, 0.05, 0.05, 0.05), "lines"),
        plot.title = element_text(
          face = "bold",
          size = rel(1),
          hjust = 0.5,
          vjust = rel(10),
          margin = unit(c(0, 0, 0, 0), "lines")
        ),
        plot.subtitle = element_text(
          size = rel(0.9),
          hjust = 0.5,
          vjust = rel(8),
          margin = unit(c(0, 0, 0, 0), "lines")
        ),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold", size = rel(1)),
        axis.text = element_text(size = rel(0.8), colour = "grey30"),
        axis.line.x = element_line(colour = "grey10"),
        axis.line.y = element_line(colour = "grey10"),
        panel.grid.major =
          if (any(grepl("maj", show_grid, ignore.case = TRUE))) {
            element_line(color = "grey85", linetype = 2)
          } else {
            element_blank()
          },
        panel.grid.minor =
          if (any(grepl("min", show_grid, ignore.case = TRUE))) {
            element_line(color = "grey85", linetype = 2)
          } else {
            element_blank()
          },
        legend.position = "right",
        legend.justification = "center",
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(.05, "npc"), # length of legend entry line

        legend.spacing = unit(0.05, "npc"),
        legend.title = element_text(face = "bold", hjust = 0, size = rel(1)),
        legend.text = element_text(size = rel(0.95), hjust = 0),

        ## top strip for facet wrap plots
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.text = element_text(face = "bold", size = rel(0.75)),
        ...
      ))

  return(res)
}


#' @title
#' GEMINI colors
#'
#' @description
#' Compilation of commonly used color palettes for plotting purposes.
#'
#' @param palette (`character` or `numeric`)\cr
#' Name or index of color palette. Run `plot_color_palettes()` to see options.
#' By default, the first palette ("GEMINI Rainbow") is returned.
#'
#' @section Note:
#' Vignettes that are colorblind accessible are marked with *. For palettes
#' without *, colorblind friendly combinations can be picked according to the
#' number of colors that are needed (e.g., if users only need 2-3 colors,
#' they should pick the ones that are most easily distinguishable).
#'
#' @seealso `vignette("plotting_functions", package = "Rgemini")`
#'
#' @export
#'
gemini_colors <- function(palette = 1) {
  ## NOTE for developers:
  # When adding new colors, please check for colorblind accessibility using this
  # website: https://www.color-blindness.com/coblis-color-blindness-simulator/
  # and/or by running `colorBlindness::cvdPlot(plot_color_palettes())`

  palettes <- list(
    "GEMINI Glow" = c(
      "#022061", # GEMINI navy
      "#02AFF0", # GEMINI cyan
      "#398A3F",
      "#EDAE49",
      "#A6361C",
      "#7B68EE",
      "#9b9b9b"
    ),
    "Shadowed Spectrum" = c(
      "#3B393D",
      "#5a78a9",
      "#7b9e6e",
      "#BD443B",
      "#EDAE49",
      "#996687"
    ),
    "Mellow Medley" = c(
      "#022061", # GEMINI navy
      "#86b9b0",
      "#c266a7",
      "#6D6D6D",
      "#4db5ff",
      "#DC965A"
    ),
    "Lavender Lagoon" = c(
      "#28797B",
      "#8c79a6",
      "#75a4de",
      "#DDAE7E",
      "#8f3f43",
      "#022061" # GEMINI navy
    ),
    "Ocean Oasis" = c(
      "#022061", # GEMINI navy
      "#4477d1",
      "#9ac4ff",
      "#A5694F",
      "#D49A6A",
      "#F7D683",
      "#00695E",
      "#3bbeaa",
      "#9CD6C9"
    ),
    "Twilight Transition" = c(
      "#022061", # GEMINI navy
      "#1B3671",
      "#354D81",
      "#4E6390",
      "#6779A0",
      "#8190B0",
      "#9AA6C0",
      "#B3BCD0",
      "#c5b8c3",
      "#D4B3B5",
      "#C5999C",
      "#B78083",
      "#A8666A",
      "#9A4D51",
      "#8B3339",
      "#7D1A20",
      "#6E0007"
    )
  )

  if (any(palette == "all")) {
    # return all color palettes
    # (should usually only be used when exploring all palettes)
    cols <- palettes
  } else {
    # return user-specified palette
    if (is.numeric(palette)) {
      # warning if invalid index was provided
      if (palette > length(palettes)) {
        stop(paste0(
          "There are currently only ", length(palettes),
          " color palettes available.
        Please specify a valid input for argument 'palette'."
        ))
      } else {
        cols <- palettes[[palette]]
      }
    } else if (is.character(palette)) {
      # search for entries matching provided input name
      pal_name <- grepl(
        paste0("^", palette), names(palettes),
        ignore.case = TRUE
      )

      if (sum(pal_name) == 0) {
        # warning if invalid name was provided
        stop(paste(
          "No color palette called '", palette, "' was found.
          Please run `plot_color_palettes()` to explore available palettes."
        ))
      } else {
        cols <- palettes[pal_name][[1]]
      }
    }
  }

  return(cols)
}


#' @title
#' Plot GEMINI color palettes
#'
#' @description
#' Plots all available GEMINI color palettes.
#'
#' @param plot_palettes (`character` or `numeric`)\cr
#' Which palettes to plot. By default, plots all palettes.
#'
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @seealso `vignette("plotting_theme", package = "Rgemini")`
#'
#' @export
#'
plot_color_palettes <- function(plot_palettes = "all") {
  palettes <- gemini_colors(plot_palettes)

  ## plotting function
  plot_pal <- function(palette, pal_name, idx) {
    ## plot palette
    sub_fig <- ggplot(
      data.frame(x = as.factor(seq(length(palette), 1))),
      aes(x = 1, y = 1, fill = x)
    ) +
      geom_col(position = position_stack(reverse = TRUE), show.legend = FALSE) +
      scale_fill_manual(values = palette) +
      coord_flip() +
      theme_void(base_size = 10) +
      theme(plot.margin = margin(l = .05, r = .05, b = .1, unit = "npc"))

    if (!is.null(idx)) {
      sub_fig <- sub_fig + ggtitle(paste0("     ", idx, ") ", pal_name))
    }

    return(sub_fig)
  }

  ## create subfigure for each palette
  # if plotting multiple palettes
  if (is.list(palettes)) {
    sub_figs <- lapply(
      seq_along(palettes),
      function(idx, name = names(palettes)) {
        plot_pal(palettes[[idx]], name[idx], idx)
      }
    )

    fig <- suppressWarnings(
      ggarrange(plotlist = sub_figs, ncol = 1)
    )
  } else {
    # for a single palette
    fig <- plot_pal(palettes, plot_palettes, NULL)
  }

  return(fig)
}

#' @title
#' Wrapper function for `ggplot2::scale_fill_manual()` that applies GEMINI color
#' palettes to ggplot objects.
#' @import ggplot2
#' @inheritParams gemini_colors
#' @export
#'
scale_fill_gemini <- function(palette = 1) {
  scale_fill_manual(values = gemini_colors(palette))
}

#' @title
#' Wrapper function for `ggplot2::scale_color_manual()` that applies GEMINI
#' color palettes to ggplot objects.
#' @inheritParams gemini_colors
#' @import ggplot2
#' @export
scale_color_gemini <- function(palette = 1) {
  scale_color_manual(values = gemini_colors(palette))
}
