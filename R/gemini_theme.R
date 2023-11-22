
# https://emanuelaf.github.io/own-ggplot-theme.html
# https://ggplot2.tidyverse.org/reference/theme.html
# https://rpubs.com/mclaire19/ggplot2-custom-themes
# https://themockup.blog/posts/2020-12-26-creating-and-using-custom-ggplot2-themes/
# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
# http://www.cookbook-r.com/Graphs/Fonts/
# https://github.com/wch/extrafont
# https://github.com/koundy/ggplot_theme_Publication

#' Gemini base theme
#'
#' @param base.size 
#' @param base.family 
#' @param base.color 
#' @param bg.color 
#' 
#' @import ggplot2
#' @import grid
#' @import ggthemes
#'
#' @return
#' @export
gemini_theme <- function(
    base.size=14, 
    base.family="sans", 
    base.colour='steelblue', 
    bg.colour='lightgray'
    ) {

  library(grid)
  library(ggthemes)
  res <- (
    theme_foundation(base_size=base.size, base_family=base.family)
    + theme(plot.title = element_text(
                  face = "bold",
                  size = rel(1.2), 
                  hjust = 0.5, 
                  margin = margin(0,0,20,0)
                  ),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour=bg.colour),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.box = "vetical",
            legend.key.size= unit(0.5, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour=bg.colour,fill=bg.colour),
            strip.text = element_text(face="bold")
    ))

  return(res)
}

