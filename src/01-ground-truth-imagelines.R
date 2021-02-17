library(magick)
library(image.textlinedetector)
library(image.binarization)
library(opencv)
library(readxl)
library(data.table)

####################################################################################################
## Get image lines from pero-ocr
##
####################################################################################################
library(xml2)
library(data.table)
read_pero_lines <- function(x, file = gsub(basename(x), pattern = ".xml$", replacement = "")){
  content <- read_xml(x)
  content <- xml_children(xml_children(xml_children(content)))
  content <- as_list(content)[-1]
  content <- lapply(content, FUN=function(x){
    coords <- strsplit(unlist(strsplit(unlist(attr(x$Coords, "points")), " ")), ",")
    coords <- data.frame(x = sapply(coords, head, 1), y = sapply(coords, tail, 1))
    baseline <- strsplit(unlist(strsplit(unlist(attr(x$Baseline, "points")), " ")), ",")
    baseline <- data.frame(x = sapply(baseline, head, 1), y = sapply(baseline, tail, 1))
    data.table::rbindlist(list(coords = coords, baseline = baseline), idcol = "type")
  })
  content <- data.table::rbindlist(content, idcol = "id")
  content <- data.table::setDF(content)
  content$file <- rep(file, nrow(content))
  content
}
f <- list.files("data/getuigenissen-line-locations/getuigenissen", pattern = ".xml$", full.names = TRUE)
x <- lapply(f, read_pero_lines)
x <- data.table::rbindlist(x, fill = TRUE)
x$file_path_sans_ext <- sapply(x$file, FUN=tools::file_path_sans_ext)
x$x <- as.integer(x$x)
x$y <- as.integer(x$y)
saveRDS(x, file = "data/getuigenissen-line-locations.rds")

## Combine image lines: if to the right and more than 33% in the same area, consider it as the same
lineloc <- readRDS(file = "data/getuigenissen-line-locations.rds")
lineloc <- setDT(lineloc)
lineloc <- lineloc[, list(x_left = min(x), x_right = max(x), y_bottom = max(y), y_top = min(y)), by = list(file, file_path_sans_ext, id, type)]
lineloc$width  <- lineloc$x_right - lineloc$x_left
lineloc$height <- lineloc$y_bottom - lineloc$y_top
lineloc <- lineloc[order(lineloc$file, lineloc$type, lineloc$x_left, lineloc$y_top, decreasing = FALSE), ]
lineloc <- subset(lineloc, type == "coords")


combine_id <- function(.SD){
  pct_overlap <- function(x_min, x_max, min_, max_){
    stopifnot(length(x_min) == length(x_max))
    stopifnot(length(x_min) == 1L)
    if(length(min_) == 0){
      return(NA)
    }
    overlap <- mapply(x_min = x_min, x_max = x_max, min_ = min_, max_ = max_, FUN=function(x_min, x_max, min_, max_){
      max_ <- min(x_max, max_)
      min_ <- max(x_min, min_)
      100*(max_ - min_) / (x_max - x_min)
    })
    overlap
  }
  overlap <- lapply(seq_len(nrow(.SD)), FUN=function(i){
    voorgaand <- seq_len(i-1)
    overlap <- pct_overlap(x_min = .SD$y_top[i], x_max = .SD$y_bottom[i], min_ = .SD$y_top[voorgaand], max_ = .SD$y_bottom[voorgaand])
    if(length(overlap) == 1 && is.na(overlap)){
      list(min = NA, max = NA, which.min = NA, which.max = NA)
    }else{
      list(min = min(overlap, na.rm=TRUE), max = max(overlap, na.rm=TRUE), which.min = which.min(overlap), which.max = which.max(overlap))
    }
  })
  id <- .SD$id
  for(i in seq_len(nrow(.SD))){
    intersection <- overlap[[i]]
    if(!is.na(intersection$max) && intersection$max > 30){
      id[i] <- id[intersection$which.max]
    }
  }
  id
}

f <- "data/scans/29e1a6b2-4a49-4b25-8591-a89a0425ce56.jpeg"
f <- list.files("data/scans", full.names = TRUE)
f <- sample(f, 1)
img <- image_read(f)
img
image_info(img)
## meer dan de helft van de hoogte zit in andere hoogte - combineer die adhv id
areas <- subset(lineloc, file == basename(f) & type %in% "coords")
areas <- areas[, newid := combine_id(.SD), by = list(file, file_path_sans_ext)]
areas <- areas[, list(x_left = min(x_left), x_right = max(x_right), y_bottom = max(y_bottom), y_top = min(y_top)), 
               by = list(file, file_path_sans_ext, id = newid)]
areas$width  <- areas$x_right - areas$x_left
areas$height <- areas$y_bottom - areas$y_top
areas <- areas[order(areas$file, areas$y_top, decreasing = FALSE), ]



areas_img <- lapply(seq_len(nrow(areas)), FUN=function(i){
  location <- areas[i, ]
  image_crop(img, geometry = geometry_area(width = location$width, height = location$height, 
                                           x_off = location$x_left, y_off = location$y_top))
  
})
areas_img <- do.call(c, areas_img)
image_append(areas_img, stack = TRUE)

# image_crop(img, geometry = geometry_area(width = 612, height = 50, x_off = 460, y_off = 225))
# 
# 
# image_crop(img, geometry = geometry_area(width = 1140-760, height = 410-361, x_off = 760, y_off = 361))
# image_crop(img, geometry = geometry_area(width = 1140-760, height = 410-361, x_off = 760, y_off = 361))
# geometry_area(width = NULL, height = NULL, x_off = 0, y_off = 0)
# 


########################################################################################
## Extract image lines using image.textlinedetector
##
########################################################################################
f <- list.files("data/scans", full.names = TRUE)
i <- 1
for(i in 1:100){
  print(sprintf("%s: %s %s", Sys.time(), i, f[[i]]))
  img <- image_read(f[[i]])
  img
  img_binary <- image_binarization(img, type = "su", opts = list(window = 75L, minN = 75L))
  areas <- image_textlines_astar(img_binary, morph = TRUE, step = 2, mfactor = 10, trace = TRUE)
  print(areas$overview)
}
i <- 27

plt <- image_draw(img)
lapply(areas$baseline, FUN=function(path){
  lines(x = path$x, y = path$y, col = "red", lwd = 5)  
})
dev.off()

width  <- image_info(img)$width
height <- image_info(img)$height
x      <- image_data(img, channels = "gray")
img    <- image.textlinedetector:::cvmat_bw(x, width = width, height = height)

areas <- image_textlines_astar(img, morph = FALSE, step = 2, mfactor = 5)
areas$overview
areas$textlines[[1]]
areas$textlines[[10]]
img <- image_binarization(img, type = "su", opts = list(window = 75L, minN = 75L))
areas$overview
image.textlinedetector::
  image.textlinedetector::image_textlines_flor

image.textlinedetector:::textlinedetector_binarization(
  image.textlinedetector:::cvmat_bgr(image_data(img, channels = "bgr"), 
                                     width = image_info(img)$width, 
                                     height = image_info(img)$height), light = TRUE, type = 0)
#img <- image.textlinedetector:::cvmat_bgr(image_data(img, channels = "bgr"), width = image_info(img)$width, height = image_info(img)$height)
#image.textlinedetector:::textlinedetector_(img)

ocv_write(imglines$textlines[[1]], )
