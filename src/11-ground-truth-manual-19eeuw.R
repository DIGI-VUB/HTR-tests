library(magick)
library(xml2)
library(data.table)
library(tools)
read_alto_xml <- function(path){
  x    <- read_xml(path, as_html = FALSE)
  info <- xml_children(x)
  info <- info[xml_name(info) %in% "Layout"]
  info <- xml_children(info)
  info <- lapply(info, FUN = function(x){
    content <- as_list(x)
    content <- content[["PrintSpace"]]
    content <- content[grepl(names(content), pattern = "TextBlock")]
    content <- lapply(content, FUN=function(content){
      content <- unlist(content, recursive = FALSE, use.names = TRUE)
      content <- content[grepl(names(content), pattern = "TextLine")]
      content <- lapply(content, attributes)
      content <- data.table::rbindlist(content)  
      content
    })
    content <- data.table::rbindlist(content)  
    content
  })
  info <- rbindlist(info)
  info
}
crop_textlines <- function(f, db, color = "royalblue"){
  img <- image_read(f)
  txtlines  <- db
  areas_img <- lapply(seq_len(nrow(txtlines)), FUN=function(i){
    location <- txtlines[i, ]
    areas <- geometry_area(width = location$width, height = location$height, 
                           x_off = location$x_left, y_off = location$y_top)
    image_crop(img, geometry = areas)
  })
  #image_append(do.call(c, lapply(areas_img,image_border, "white", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "#000080", "10x10")), stack = TRUE)
  #image_append(do.call(c, lapply(areas_img,image_border, "royalblue", "10x10")), stack = TRUE)
  list(areas = areas_img, 
       overview = image_append(do.call(c, lapply(areas_img, image_border, color, "10x10")), stack = TRUE))
}
image_rbind <- function(x){
  if(inherits(x, "magick-image")){
    image_append(x, stack = TRUE)
  }else{
    image_append(image_read(x), stack = TRUE)
  }
}
remove_comments <- function(x){
  regmatches(x = x, m = gregexpr(pattern = "\\[.*?\\]", x)) <- ""
  gsub(x, pattern = "<|>", replacement = "")
}

##################################################################################
## Read Alto XML files
##
files <- data.frame(path = list.files("data/transkribus-lines/Corpus_19de_eeuw/alto/", full.names = TRUE),
                    stringsAsFactors = FALSE)
files$image <- sprintf("%s.jpg", file_path_sans_ext(basename(files$path)))
files$path_image <- file.path("data/transkribus-lines/Corpus_19de_eeuw", files$image)

textlines <- lapply(files$path, read_alto_xml)
textlines <- setNames(textlines, files$path_image)
textlines <- data.table::rbindlist(textlines, idcol = "path_image")
textlines$file   <- basename(textlines$path_image)
textlines$width  <- as.integer(textlines$WIDTH)
textlines$height <- as.integer(textlines$HEIGHT)
textlines$x_left <- as.integer(textlines$HPOS)
textlines$y_top  <- as.integer(textlines$VPOS)

##################################################################################
## See 1 set of text lines
##
f <- sample(unique(textlines$path_image))[1]
db <- subset(textlines, file %in% basename(f))
img <- crop_textlines(f, db = db)
img$areas
img$overview
image_rbind(do.call(c, img$areas))

##################################################################################
## Build training set
##
dir.create(file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs"), recursive = TRUE)
dir.create(file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "overview"), recursive = TRUE)
dir.create(file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "textlines"), recursive = TRUE)
transcriptions <- list()

paths <- unique(textlines$path_image)
for(i in seq_along(paths)){
  cat(sprintf("%s: %s/%s", Sys.time(), i, length(paths)), sep = "\n")
  f  <- paths[i]
  subject_id <- file_path_sans_ext(basename(f))
  db <- subset(textlines, file %in% basename(f))
  
  matches <- crop_textlines(f, db = db)
  matches$text <- db$CONTENT
  combined <- matches$areas
  combined <- mapply(combined, matches$text, FUN=function(img, txt){
    image_annotate(img, txt, size = 10, color = "red", boxcolor = "pink")
  })
  combined <- image_rbind(do.call(c, combined))
  
  path <- file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "overview", sprintf("%s.png", subject_id))
  image_write(matches$overview, path = path, format = "png")
  
  matches$text <- remove_comments(matches$text)  
  matches$text <- gsub("[[:space:]]+", " ", matches$text)
  matches$text <- strsplit(matches$text, "")
  matches$text <- lapply(matches$text, FUN=function(x) gsub(" ", "@", x))
  traindata    <- data.frame(subject_id = subject_id, 
                             doc_id = paste(subject_id, sprintf("%02d", seq_along(matches$text)), sep = "-"),
                             text = sapply(matches$text, FUN=function(x) paste(x, collapse = " ")),
                             stringsAsFactors = FALSE)
  transcriptions[[i]] <- traindata
  for(j in seq_len(nrow(traindata))){
    path <- traindata$doc_id[j]
    path <- file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "textlines", sprintf("%s.jpg", path))
    image_write(matches$areas[[j]], path = path, format = "jpeg", quality = 100)
  }
  gc()
}
save(transcriptions, file = "data/transkribus-lines/Corpus_19de_eeuw/transcriptions.RData")

allowed_chars <- c("-", ",", ";", ":", "!", "?", "/", ".", "'", "\"", "(", ")", 
                   "@", "*", "&", "#", "+", "0", "1", "2", "3", "4", "5", "6", "7", 
                   "8", "9", "a", "A", "b", "B", "c", "C", "d", "D", "e", "E", "f", 
                   "F", "g", "G", "h", "H", "i", "I", "j", "J", "k", "K", "l", "L", 
                   "m", "M", "n", "N", "o", "O", "p", "P", "q", "Q", "r", "R", "s", 
                   "S", "t", "T", "u", "U", "v", "V", "w", "W", "x", "X", "y", "Y", 
                   "z", "Z")
modeldata <- rbindlist(transcriptions)
modeldata$text <- iconv(modeldata$text, from = "UTF-8", to = "ASCII//TRANSLIT")
modeldata$text <- strsplit(modeldata$text, " ")
modeldata$text <- lapply(modeldata$text, FUN=function(x) x[x %in% allowed_chars])
modeldata$text <- sapply(modeldata$text, FUN=function(x) paste(x, collapse = " "))
set.seed(123456789)
setdiff(names(table(unlist(strsplit(modeldata$text, " ")))), allowed_chars)
modeldata$traintest <- runif(nrow(modeldata))
modeldata$dataset <- ifelse(modeldata$traintest > 0.95, "test", ifelse(modeldata$traintest > 0.75, "validation", "train"))
x <- subset(modeldata, dataset == "train")
writeLines(sprintf("%s %s", x$doc_id, x$text), con = file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "tr.txt"))
x <- subset(modeldata, dataset == "validation")
writeLines(sprintf("%s %s", x$doc_id, x$text), con = file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "va.txt"))
x <- subset(modeldata, dataset == "test")
writeLines(sprintf("%s %s", x$doc_id, x$text), con = file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "te.txt"))
x <- sapply(strsplit(readLines( file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "te.txt")), " "), head, 1)
writeLines(x[1:3], file.path("data", "transkribus-lines", "Corpus_19de_eeuw", "imgs", "test.txt"))
save(transcriptions, file = "data/transkribus-lines/Corpus_19de_eeuw/transcriptions.RData")

## Also make images with fixed height: 128
x <- list.files("data/transkribus-lines/Corpus_19de_eeuw/imgs/textlines", full.names = TRUE)
x <- mapply(img = x, path = file.path("data/transkribus-lines/Corpus_19de_eeuw/imgs/textlines_h128", basename(x)), FUN=function(img, path){
  img <- image_read(img)
  img <- image_resize(img, "x128")
  image_write(img, path = path, format = "jpg", quality = 100)
})
image_read(x[1])

## Also make images with fixed height: 128 and using su binarization
library(image.binarization)
x <- list.files("data/transkribus-lines/Corpus_19de_eeuw/imgs/textlines", full.names = TRUE)
x <- mapply(img = x, path = file.path("data/transkribus-lines/Corpus_19de_eeuw/imgs/textlines_su_h128", basename(x)), FUN=function(img, path){
  img <- image_read(img)
  img <- image_resize(img, "x128")
  img <- image_binarization(img, type = "su")
  image_write(img, path = path, format = "jpg", quality = 100)
})
image_read(x[1])
