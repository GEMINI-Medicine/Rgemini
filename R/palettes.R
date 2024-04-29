library(ggplot2)
library(lemon)
library(ggpubr)

#' @title
#' GEMINI colors
#'
#' @description
#' Defines various color palettes for plotting purposes.
#' Color palettes can be explored using `scales::show_col(gemini_colors())`
#'
#' @param palette (`character`)\cr
#' Name of color palette. Has to be one of the following:
#' -
#' -
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

plot_color_palettes()

