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
#' Additional arguments passed to `ggplot2::facet_wrap()` (if a `facet_group` is
#' specified).
#' @import ggplot2
#' @import grid
#' @import ggthemes
#'
#' @references
#' https://emanuelaf.github.io/own-ggplot-theme.html
#' https://ggplot2.tidyverse.org/reference/theme.html
#' https://rpubs.com/mclaire19/ggplot2-custom-themes
#' https://themockup.blog/posts/2020-12-26-creating-and-using-custom-ggplot2-themes/
#' https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
#' http://www.cookbook-r.com/Graphs/Fonts/
#' https://github.com/wch/extrafont
#' https://github.com/koundy/ggplot_theme_Publication
#'
#' @return
#' ggplot with specified theme
#'
#' @export
plot_theme <- function(
    base_size = 14,
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
#' Name or index of color palette. Run `plot_color_palettes` to see options.
#'
#' @import ggplot2 lemon ggpubr
#'
gemini_colors <- function(palette = "rainbow") {


  if (palette == "rainbow1") {
    cols <- c(
      "#022061", # GEMINI navy
      "#02B0F1", # GEMINI cyan
      "#4CAF50",
      "#F3A924",
      "#A6361C",
      "#7B68EE", #"#92278F" "#6252c6" "#7B68EE""
      "#9b9b9b"
    )
  }

  if (palette == "rainbow2") {
    cols <- c(
      "#0F1115",
      "#5a78a9",
      "#7b9e6e",
      "#BD443B",
      "#EDAE49",
      "#8E527A"
    )
  }


  if (palette == "bubblegum") {
    cols <- c(
      "#022061", # GEMINI navy
      "#2f5bb1",
      "#00b1f3", # GEMINI cyan
      "#1d686e",
      "#3bbeaa",
      "#a8dfd4",
      "#92278F",
      "#c29aeb",
      "#f4ccff"
    )
  }


  if (palette == "basic2") {
    cols <- c(
      "#022061", # GEMINI navy
      "#86b9b0",
      "#c266a7",
      "#909090",
      "#0077B6", #"#61A2DA"
      "#913136", #"#BD443B"
      "#18696B" #"#49a7a2"
    )
  }


  if (palette == "shades1") {
    cols <-  c(
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
    )
  }


  if (palette == "shades2") {
    cols <-  c(
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
  }



  return(cols)
}


#' @title
#' Plot GEMINI color palettes
#'
#' @description
#' Plots all available GEMINI color palettes.
#'
#' @param palettes (`character` or `numeric`)\cr
#' Which palettes to plot. By default, plots all palettes.
#'
#' @import ggplot2 ggpubr
#'
#'
# Function to plot multiple color palettes
plot_color_palettes <- function(palettes = NULL) {

  if (is.null(palettes)) {
    palettes <- c("rainbow1",
                  "rainbow2",
                  "bubblegum",
                  "basic2",
                  "shades1",
                  "shades2"
    )
  }

  ## plotting function
  plot_pal <- function(palette) {

    ## generate palette
    pal <- gemini_colors(palette)

    ## plot palette
    sub_fig <- ggplot(data.frame(x = as.factor(seq(length(pal), 1))), aes(x = 1, y = 1, fill = x)) +
      geom_col(position = position_stack(reverse = TRUE), show.legend = FALSE) +
      scale_fill_manual(values=gemini_colors(palette)) + coord_flip() +
      ggtitle(paste0("  ", palette)) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.margin = margin(l = .2, r = .2, unit = "npc"))


    return(sub_fig)
  }

  ## create figure for each variable
  sub_figs <- lapply(palettes, plot_pal)
  fig <- suppressWarnings(ggarrange(plotlist = sub_figs, ncol = 1))#, labels = palettes, hjust = -0.5, vjust = 3))

  return(fig)

}
