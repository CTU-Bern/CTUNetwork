#' Customized ggplot2 theme applied to homogenize the plots in Shiny App
#'
#' @param titleSize Optional: ggplot2 plot.title size argument
#'
#' @return list containing ggplot2 theme settings
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @export
#'
#' @examples
#' CustomTheme = themeShiny(titleSize = 12)
#'
themeShiny = function(titleSize = 14) {

  # font <- "Georgia"   #assign font family up front

  ggplot2::theme_minimal() %+replace%    #replace elements we want to change

    ggplot2::theme(

      #text elements
      plot.title = ggplot2::element_text(             #title
        # family = font,            #set font family
        size = titleSize,         #set font size
        face = 'bold',            #bold typeface
        hjust = 0.5,              #center align
        vjust = 2),               #raise slightly

      plot.subtitle = ggplot2::element_text(          #subtitle
        # family = font,          #font family
        size = 14),               #font size

      plot.caption = ggplot2::element_text(           #caption
        # family = font,          #font family
        size = 9,                 #font size
        hjust = 1),               #right align

      axis.title = ggplot2::element_text(             #axis titles
        # family = font,          #font family
        size = 10),               #font size

      axis.text = ggplot2::element_text(              #axis text
        # family = font,          #axis family
        size = 9),                #font size

      axis.text.x = ggplot2::element_text(              #x axis text
        angle = 90,               #text angle
        vjust = 0.5,              #vertical adjustment
        hjust= 1 )                #horizontal adjustment

      # axis.text.x = element_text(            #margin for axis text
      #   margin=margin(5, b = 10))
    )
}
