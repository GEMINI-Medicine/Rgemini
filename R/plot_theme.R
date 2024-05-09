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
    ...) {
  res <- (
    theme_foundation(
      base_size = base_size,
      base_family = base_family
    ) +
      theme(
        plot.title = element_text(
          face = "bold",
          size = rel(1),
          hjust = 0.5,
          vjust = rel(5),
          margin = margin(0, 0, 0, 0),
        ),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold", size = rel(1)),
        axis.title.y = element_text(angle = 90, vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(size = rel(0.8), colour = "grey30"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks = element_line(),
        panel.grid.major = if (any(grepl("maj", show_grid, ignore.case = TRUE))) {
          element_line(color = "grey85", linetype = 2)
        } else {
          element_blank()
        },
        panel.grid.minor = if (any(grepl("min", show_grid, ignore.case = TRUE))) {
          element_line(color = "grey85", linetype = 2)
        } else {
          element_blank()
        },
        legend.position = "right",
        legend.justification = "center",
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(.05, "npc"), # length of legend entry line

        legend.spacing = unit(0, "mm"),
        legend.title = element_text(face = "bold", hjust = 0, size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.text.align = 0,
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "npc"),
        plot.subtitle = element_text(size = rel(0.9)),

        ## top strip for facet wrap plots
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.text = element_text(face = "bold", size = rel(0.75)),
        aspect.ratio = 1,
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
#' Name or index of color palette. Run `plot_color_palettes` to see options. By
#' default, the first palette ("GEMINI Rainbow") is returned.
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
gemini_colors <- function(palette = "GEMINI Rainbow") {
  palettes <- list(
    "GEMINI Rainbow" = c(
      "#022061", # GEMINI navy
      "#02B0F1", # GEMINI cyan
      "#4CAF50",
      "#F3A924",
      "#A6361C",
      "#7B68EE",
      "#9b9b9b"
    ),

    # "GeMQIN" = c(
    #   "#00b2e3", # blue_mountain
    #   "#c1b28f", # wasaga_beach
    #   "#49a7a2", # blue_coast
    #   "#92278f", # prince_edward_fields
    #   "#e2e3e4", # grey
    #   "#808080" # gray
    # ),

    "Shadowed Spectrum" = c(
      "#1A1C20",
      "#5a78a9",
      "#7b9e6e",
      "#BD443B",
      "#EDAE49",
      "#826088"
    ),
    "Mellow Medley" = c(
      "#022061", # GEMINI navy
      "#86b9b0",
      "#c266a7",
      "#6D6D6D",
      "#197BBD"
    ),
    "Lavender Lagoon" = c( # Rustic Ripple
      "#28797B",
      "#947EB0",
      "#61A2DA",
      "#DDAE7E",
      "#913136",
      "#022061" # GEMINI navy
    ),
    "Bubblegum Burst" = c(
      "#022061", # GEMINI navy
      "#2f5bb1",
      "#00b1f3", # GEMINI cyan
      "#1d686e",
      "#3bbeaa",
      "#a8dfd4",
      "#92278F",
      "#c29aeb",
      "#f4ccff"
    ),
    "JAMA (from ggsci)" = c(
      "#374E55FF",
      "#DF8F44FF",
      "#00A1D5FF",
      "#B24745FF",
      "#79AF97FF",
      "#6A6599FF",
      "#80796BFF"
    ),
    "Lancet (from ggsci)" = c(
      "#00468BFF",
      "#ED0000FF",
      "#42B540FF",
      "#0099B4FF",
      "#925E9FFF",
      "#FDAF91FF",
      "#AD002AFF",
      "#ADB6B6FF",
      "#1B1919FF"
    ),
    "Viridis (from viridis)*" = c(
      "#fde725",
      "#b5de2b",
      "#6ece58",
      "#35b779",
      "#1f9e89",
      "#26828e",
      "#31688e",
      "#3e4989",
      "#482878",
      "#440154"
    ),
    "Magma (from viridis)*" = c(
      "#fcfdbf",
      "#feca8d",
      "#fd9668",
      "#f1605d",
      "#cd4071",
      "#9e2f7f",
      "#721f81",
      "#440f76",
      "#180f3d",
      "#000004"
    ),
    "Twilight Gradient" = c(
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
  palettes <- as.list(gemini_colors(plot_palettes))

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
      ggtitle(paste0("  ", idx, ") ", pal_name)) +
      theme_void(base_size = 10) +
      theme(plot.margin = margin(l = .05, r = .05, b = .1, unit = "npc"))

    return(sub_fig)
  }

  ## create subfigure for each palette
  sub_figs <- lapply(
    seq_along(palettes),
    function(idx, name = names(palettes)) {
      plot_pal(palettes[[idx]], name[idx], idx)
    }
  )

  fig <- suppressWarnings(
    ggarrange(
      plotlist = sub_figs, nrow = ceiling(length(palettes) / 2), ncol = 2
    )
  )

  ## add annotation for colorblind friendly palettes
  fig <- annotate_figure(
    fig,
    bottom = text_grob(
      "*Colorblind accessible",
      hjust = 0, x = 0.05, face = "italic", size = 12
    )
  )

  return(fig)
}
