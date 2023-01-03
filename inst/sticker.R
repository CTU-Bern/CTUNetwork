library(ggplot2)
# library(emojifont)
# library(scales)
# library(hexSticker)
library(magick)
library(cowplot)
# library(fontawesome) # fa5
# library(icons) # https://github.com/mitchelloharawild/icons
#
# load.fontawesome()
#
# # BackgroundIcon <- fa(name = "fas fa-circle-nodes", fill = "#00798c")
# shiny::icon("circle-nodes")

# Image URL
imgurl <- paste0(getwd(),"/figures/circle-nodes.png")

# Create the plot
icon <- ggplot() +
  draw_image(imgurl, x = c(0.5), y = c(0.5),  valign = 0, halign = 0) +
  xlim(0,2) +
  ylim(0,2) +
  theme_void() + theme(rect = element_rect(fill = "transparent"))

# Create the sticker
s <- sticker(icon, package="",
             s_x=1, s_y=1, s_width=2, s_height=2,
             filename="man/figures/logo.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "CTUNetwork",
             u_size = 11,
             u_x = 1,
             u_y = 0.12
)
s
