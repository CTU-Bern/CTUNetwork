#' Emulate ggplot2 color palette
#' Taken from : https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#'
#' @param n integer indicating the number of equally spaced hues around the color wheel, i.e. ggplot2-like
#'
#' @return vector
#' @importFrom grDevices hcl
#' @export
#'
#' @examples
#' ggColorHue(3)
#' "#F8766D" "#00BA38" "#619CFF"
#'
ggColorHue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
