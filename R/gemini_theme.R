#' GEMINI plotting colors
plot_colors <- c(
  rgb(0, 26, 92, maxColorValue = 255),
  rgb(47, 129, 185, maxColorValue = 255),
  rgb(173, 216, 230, maxColorValue = 255)
)


# https://emanuelaf.github.io/own-ggplot-theme.html
# https://ggplot2.tidyverse.org/reference/theme.html
# https://rpubs.com/mclaire19/ggplot2-custom-themes
# https://themockup.blog/posts/2020-12-26-creating-and-using-custom-ggplot2-themes/
# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
# http://www.cookbook-r.com/Graphs/Fonts/
# https://github.com/wch/extrafont
# https://github.com/koundy/ggplot_theme_Publication

#' GEMINI plot theme
#'
#' @param base_size
#' @param base_family
#' @param base_colour
#' @param bg_colour
#'
#' @import ggplot2
#' @import grid
#' @import ggthemes
#'
#' @return
#' @export
gemini_theme <- function(
    base_size = 14,
    base_family = "",
    show_grid = FALSE,
    aspect_ratio = 1
    # base_colour='steelblue',
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
          vjust = 2,
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
        legend.title = element_text(face = "bold.italic", hjust = 0, size = rel(1)),
        legend.text = element_text(size = rel(1)),
        legend.text.align = 0,

        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "npc"),

        ## top strip for facet wrap plots
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.text = element_text(face = "bold", size = rel(1)),

        aspect.ratio = aspect_ratio,
      ))

  return(res)
}
