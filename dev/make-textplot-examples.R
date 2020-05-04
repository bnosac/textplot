library(magick)
x <- list.files("dev", pattern = ".png", full.names = TRUE)
x <- image_read(x)
x
animation <- image_animate(x, fps = 0.5)
image_write(animation, "vignettes/textplot-examples.gif", format = "gif")
