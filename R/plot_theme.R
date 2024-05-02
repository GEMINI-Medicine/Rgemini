#' @title
#' GEMINI plot theme
#'
#' @description
#' Common theme that can be applied to any ggplot object
#'
#' @param base_size (`numeric`)\cr
#' @param base_family (`numeric`)\cr
#' @param show_grid (`logical`)\cr
#' @param aspect_ratio (`numeric`)\cr
#' Aspect ratio of plot
#' @param ... \cr
#' Additional arguments passed to `theme()`.
#' @import ggplot2
#' @import ggthemes
#'
#' @return
#' ggplot with specified theme
#'
#' @export
#' 
plot_theme <- function(
    base_size = 12,
    base_family = "",
    show_grid = FALSE,
    aspect_ratio = 1,
    ...
) {

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
        panel.grid.major = if (show_grid) element_line(color = "grey85", linetype = 2) else element_blank(),
        panel.grid.minor = element_blank(),


        legend.position = "right",
        legend.justification = "center",

        #legend.direction = "horizontal",
        #legend.box = "vertical",

        legend.key = element_rect(colour = NA),
        legend.key.size = unit(.05, "npc"),  # length of legend entry line

        legend.spacing = unit(0, "mm"),
        legend.title = element_text(face = "bold", hjust = 0, size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.text.align = 0,

        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "npc"),
        plot.subtitle = element_text(size = rel(0.9)),

        ## top strip for facet wrap plots
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.text = element_text(face = "bold", size = rel(0.75)),

        aspect.ratio = aspect_ratio,

        ...
      ))

  return(res)
}



#' @title
#' GEMINI colors
#'
#' @description
#' Defines various color palettes for plotting purposes.
#'
#' @param palette (`character` or `numeric`)\cr
#' Name or index of color palette. Run `plot_color_palettes` to see options. By
#' default, the first palette ("GEMINI Rainbow") is returned. 
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
      "#6D6D6D" ,
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

    "Navy Gradient" = c(
      "#022061", # GEMINI navy
      "#022A6B",
      "#023576",
      "#023F80",
      "#02498A",
      "#025394",
      "#025E9F",
      "#0268A9",
      "#0272B3",
      "#027DBE",
      "#0287C8",
      "#0291D2",
      "#029BDC",
      "#02A6E7",
      "#02B0F1" # GEMINI cyan
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
    # (should usually only used when exploring all palettes)
    cols <- palettes
  } else {
    # return user-specified palette
    if (is.numeric(palette)) {
      if (palette > length(palettes)) {
        stop(paste0("There are currently only ", length(palettes), " color palettes available.
                    Please specify a valid input for argument 'palette'."))
      } else {
        cols <- palettes[[palette]]
      }
    } else if (is.character(palette)) {
      # search for entries matching provided input name
      pal_name <- grepl(palette, names(palettes), ignore.case = TRUE)
      if (sum(pal_name) == 0) {
        stop(paste("No color palette called '", palette, "' was found.
                  Please run `plot_color_palettes()` to explore available palettes."))
      } else {
        cols <- palettes[[pal_name]]
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
#' @seealso `vignette("plotting_functions", package = "Rgemini")`
#'
#' @export
#' 
plot_color_palettes <- function(plot_palettes = "all") {

  palettes <- as.list(gemini_colors(plot_palettes))
  
  ## plotting function
  plot_pal <- function(palette, pal_name) {
    
    ## plot palette
    sub_fig <- ggplot(data.frame(x = as.factor(seq(length(palette), 1))), aes(x = 1, y = 1, fill = x)) +
      geom_col(position = position_stack(reverse = TRUE), show.legend = FALSE) +
      scale_fill_manual(values=palette) + coord_flip() +
      ggtitle(paste0("  ", pal_name)) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.margin = margin(l = .2, r = .2, unit = "npc"))


    return(sub_fig)
  }

  ## create subfigure for each palette 
  sub_figs <- lapply(names(palettes), function(name) plot_pal(palettes[[name]], name))
  fig <- suppressWarnings(ggarrange(plotlist = sub_figs, ncol = 1))

  return(fig)

}
