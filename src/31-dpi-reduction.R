library(zip)
library(magick)
setwd("/home/jwijffels/magick/imgs")
unzip("text_en_foto.zip", exdir = getwd())
setwd("/home/jwijffels/magick/imgs/text_en_foto")
x <- list.files(pattern = ".jpg$")
for(i in seq_along(x)){
  cat(sprintf("%s/%s: %s ", i, length(x), x[i]), sep = "\n")
  from <- x[i]
  to <- sprintf("converted-%s", from)
  info <- image_info(image_read(from))
  system(sprintf("convert -resample 70 %s %s", from, to))
  #convert -resample 70 RABrugge_TBO119_693_088.jpg output.jpg
  img <- image_read(to)
  img <- image_resize(img, sprintf("%sx%s", info$width, info$height))
  image_write(img, path = from, quality = 100)
  file.remove(to)
}
zip("img-dpi70.zip", files = x)
