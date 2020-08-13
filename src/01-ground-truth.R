library(data.table)
library(xml2)
library(magick)
library(image.textlinedetector)
library(image.binarization)
library(opencv)

####################################################################################################
## Get image lines from pero-ocr
##
####################################################################################################
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

####################################################################################################
## Combine image lines: if to the right and more than 33% in the same area, consider it as the same line 
##
####################################################################################################
textline_id <- function(.SD, pct_overlap_at_least = 100/3){
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
  ## meer dan 30% van de hoogte zit in andere hoogte - combineer die adhv id
  id <- .SD$id
  for(i in seq_len(nrow(.SD))){
    intersection <- overlap[[i]]
    if(!is.na(intersection$max) && intersection$max > pct_overlap_at_least){
      id[i] <- id[intersection$which.max]
    }
  }
  id
}

lineloc <- readRDS(file = "data/getuigenissen-line-locations.rds")
lineloc <- subset(lineloc, type == "coords")
lineloc <- setDT(lineloc)
lineloc <- lineloc[, list(x_left = min(x), x_right = max(x), y_bottom = max(y), y_top = min(y)), by = list(file, file_path_sans_ext, id, type)]
lineloc$width  <- lineloc$x_right - lineloc$x_left
lineloc$height <- lineloc$y_bottom - lineloc$y_top
lineloc <- lineloc[order(lineloc$file, lineloc$type, lineloc$x_left, lineloc$y_top, decreasing = FALSE), ]

f <- "data/scans/29e1a6b2-4a49-4b25-8591-a89a0425ce56.jpeg"
f <- list.files("data/scans", full.names = TRUE)
f <- sample(f, 1)
img <- image_read(f)
img
image_info(img)
textlines <- subset(lineloc, type %in% "coords")
#textlines <- subset(lineloc, file == basename(f))
textlines <- textlines[, newid := textline_id(.SD, pct_overlap_at_least = 50), by = list(file, file_path_sans_ext)]
textlines <- textlines[, list(x_left = min(x_left), x_right = max(x_right), y_bottom = max(y_bottom), y_top = min(y_top)), 
                       by = list(file, file_path_sans_ext, id = match(newid, unique(newid)))]
textlines$width  <- textlines$x_right - textlines$x_left
textlines$height <- textlines$y_bottom - textlines$y_top
textlines <- textlines[order(textlines$file, textlines$y_top, decreasing = FALSE), ]
write.csv(textlines, file ="data/getuigenissen-textlines.csv", na = "", row.names = FALSE)
saveRDS(textlines, file = "data/getuigenissen-textlines.rds")

## Show an example
textlines <- readRDS(file = "data/getuigenissen-textlines.rds")
f <- "data/scans/29e1a6b2-4a49-4b25-8591-a89a0425ce56.jpeg"
f <- list.files("data/scans", full.names = TRUE)
f <- sample(f, 1)
crop_textlines <- function(f, db, color = "royalblue"){
  img <- image_read(f)
  txtlines  <- db[db$file %in% basename(f), ]
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
img <- crop_textlines(f, db = textlines)
img$areas
img$overview



########################################################################################
## Get the gold texts not included in the repository and combine these with the image chunks
## 1. map image to text files
## 2. map textlines to text chunks
##
## text is for all images together :(, not even per line
## TRY TO AVOID THE ADAGIUM GARBAGE IN - GARBAGE OUT so be as strict as possible
########################################################################################
image_rbind <- function(x){
  if(inherits(x, "magick-image")){
    image_append(x, stack = TRUE)
  }else{
    image_append(image_read(x), stack = TRUE)
  }
}

gold               <- readRDS(file = "data/gold-texts.rds")
gold$images_amount <- sapply(gold$image_urls, length)
gold$image_path    <- lapply(gold$images_zooniverse, FUN=function(x) file.path("data/scans", x))
gold <- subset(gold, !is.na(gold_text))
gold <- subset(gold, eeuw %in% c("18de eeuw", "19de eeuw"))
gold <- subset(gold, eeuw %in% c("18de eeuw"))
gold <- subset(gold, taal == "NL")

textlines <- readRDS(file = "data/getuigenissen-textlines.rds")
textlines <- subset(textlines, file %in% unlist(gold$images_zooniverse))
## beelden met textlines die amper 200 pixels breed zijn, beschouwen we niet als lijnen
## try to avoid double lines (everything after the wrong double line is discared)
MASS::truehist(textlines$width, col = "lightblue")
MASS::truehist(textlines$height, col = "lightblue")
textlines <- subset(textlines, width > 200)
textlines <- as.data.table(textlines)
textlines <- textlines[, problematic := any(height > 150), by = list(file)]
textlines <- subset(textlines, !problematic)
textlines <- textlines[, height_comparison := height / median(height), by = list(file)]
MASS::truehist(textlines$height_comparison, col = "lightblue")
textlines <- textlines[order(textlines$file, textlines$y_top, decreasing = FALSE), ] 
textlines <- textlines[, problematic := cumsum(height_comparison > 1.3) > 0, by = list(file)]
textlines <- subset(textlines, !problematic)
textlines <- setDF(textlines)


gold <- gold[sapply(gold$images_zooniverse, FUN=function(x) all(x %in% textlines$file)), ]
nrow(gold)

## Logic: take only text which has at least 20 characters per line, assuming that order is the same ...
align_textline_text <- function(gold, id, textlines){
  getuigenis <- subset(gold, subject_id == id)
  # show image(s)
  img <- image_rbind(unlist(getuigenis$image_path))
  
  # get on text of the image
  #cat(getuigenis$gold_text)
  txt <- unlist(strsplit(getuigenis$gold_text, split = "\n"))
  #txt <- txt[nchar(txt) > quantile(nchar(txt), 0.05)]
  txt <- txt[nchar(txt) > 20]
  
  txtlines <- subset(textlines, file %in% unlist(getuigenis$images_zooniverse))
  txtlines <- txtlines[order(txtlines$width, decreasing = TRUE), ]
  txtlines <- head(txtlines, n = length(txt))
  txtlines <- txtlines[order(txtlines$file, txtlines$y_top, decreasing = FALSE), ] ## extremely important
  txt <- head(txt, nrow(txtlines))
  #txtlines <- subset(txtlines, width > quantile(txtlines$width, 0.05))
  
  # img <- crop_textlines(getuigenis$image_path[[1]][1], txtlines)
  # length(img$areas)
  # img <- crop_textlines(getuigenis$image_path[[1]][2], txtlines)
  # length(img$areas)
  # img$overview
  
  areas <- unlist(getuigenis$image_path)
  areas <- areas[cumsum(!basename(areas) %in% unique(txtlines$file)) == 0]
  if(nrow(txtlines) == 0){
    return(list(raw = img, failed = TRUE))
  }
  
  areas <- lapply(areas, FUN=function(path){
    crop_textlines(path, txtlines)$areas
  })
  areas <- unlist(areas, recursive = FALSE)
  
  combined <- mapply(areas, txt, FUN=function(img, txt){
    image_annotate(img, txt, size = 10, color = "red", boxcolor = "pink")
  })
  combined <- image_rbind(do.call(c, combined))
  list(raw = img, text = txt, areas = areas, overview = combined, failed = FALSE)
}
remove_comments <- function(x){
  regmatches(x = x, m = gregexpr(pattern = "\\[.*?\\]", x)) <- ""
  gsub(x, pattern = "<|>", replacement = "")
}

transcriptions <- list()

#for(i in seq_along(gold$subject_id)){
for(i in 223:length(gold$subject_id)){
  cat(sprintf("%s: %s/%s", Sys.time(), i, nrow(gold)), sep = "\n")
  subject_id <- sample(gold$subject_id, 1)
  subject_id <- gold$subject_id[i]
  matches <- align_textline_text(gold, id = subject_id, textlines = textlines)
  if(!matches$failed){
    path <- file.path("data", "imgs", "overview", sprintf("%s.png", subject_id))
    image_write(matches$overview, path = path, format = "png")
    
    #matches$overview
    #matches$areas
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
      path <- file.path("data", "imgs", "textlines", sprintf("%s.jpg", path))
      image_write(matches$areas[[j]], path = path, format = "jpeg", quality = 100)
    }
  }
  gc()
}

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
modeldata$dataset <- ifelse(modeldata$traintest > 0.95, "test", 
                            ifelse(modeldata$traintest > 0.75, "validation", "train"))
x <- subset(modeldata, dataset == "train")
writeLines(sprintf("%s %s", x$doc_id, x$text), con = file.path("data", "imgs", "tr.txt"))
x <- subset(modeldata, dataset == "validation")
writeLines(sprintf("%s %s", x$doc_id, x$text), con = file.path("data", "imgs", "va.txt"))
x <- subset(modeldata, dataset == "test")
writeLines(sprintf("%s %s", x$doc_id, x$text), con = file.path("data", "imgs", "te.txt"))
save(transcriptions, file = "data/transcriptions.RData")

## Also make images with fixed height: 128
x <- list.files("data/imgs/textlines", full.names = TRUE)
x <- mapply(img = x, path = file.path("data/imgs/textlines_h128", basename(x)), FUN=function(img, path){
  img <- image_read(img)
  img <- image_resize(img, "x128")
  image_write(img, path = path, format = "jpg", quality = 100)
})
image_read(x[1])


ids <- readLines("issues.txt")
ids <- gsub(ids, pattern = "(.+')(.+)('])", replacement = "\\2")
ids <- subset(modeldata, doc_id %in% ids)
cat(sprintf("%s %s", ids$doc_id, ids$text), sep = "\n")
ids <- ids$doc_id
img <- image_read((file.path("data/imgs/textlines", paste0(ids, ".jpg"))))
image_info(img)
image_append(img, TRUE) %>% image_write(path = "loss-warnings.png")
# ## check
# load("transcriptions.RData")
# ids <- c('32505198-73', '32505199-31', '32500853-08', '32501271-05', '32505219-03', '32505217-04', '32505165-26', '32500973-14', '32500665-39', '32505171-19')
# subset(modeldata, doc_id %in% ids)
# img <- image_read((file.path("data/imgs/textlines_h128", paste0(ids, ".jpg"))))
# 
# ########################################################################################
# ## Extract image lines using image.textlinedetector
# ## TODO
# ##
# ########################################################################################
# f <- list.files("data/scans", full.names = TRUE)
# 
# i <- 27
# img <- image_read(f[[i]])
# img
# img <- image_binarization(img, type = "su", opts = list(window = 75L, minN = 75L))
# 
# width  <- image_info(img)$width
# height <- image_info(img)$height
# x      <- image_data(img, channels = "gray")
# img    <- image.textlinedetector:::cvmat_bw(x, width = width, height = height)
# 
# areas <- image_textlines_astar(img, morph = FALSE, step = 2, mfactor = 5)
# areas$overview
# areas$textlines[[1]]
# areas$textlines[[10]]
# img <- image_binarization(img, type = "su", opts = list(window = 75L, minN = 75L))
# areas$overview
# 
# image.textlinedetector:::textlinedetector_binarization(
#   image.textlinedetector:::cvmat_bgr(image_data(img, channels = "bgr"), 
#                                      width = image_info(img)$width, 
#                                      height = image_info(img)$height), light = TRUE, type = 0)
# #img <- image.textlinedetector:::cvmat_bgr(image_data(img, channels = "bgr"), width = image_info(img)$width, height = image_info(img)$height)
# #image.textlinedetector:::textlinedetector_(img)
# 
# ocv_write(imglines$textlines[[1]], )
