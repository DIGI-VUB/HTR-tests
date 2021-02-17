library(magick)
library(data.table)
library(tools)
settings <- list()
settings$path_tesseract <- "data/tesseract"
settings$path_textlines <- "data/transkribus-lines/HTR test corpus/imgs/textlines_su_h128/"

read_txt <- function(x){
  x <- readLines(x)
  x <- strsplit(x, " ")
  x <- data.frame(doc_id = sapply(x, FUN = function(x) x[1]),
                  text = sapply(x, FUN = function(x){
                    x <- x[-1] 
                    x <- gsub(x, pattern = "@", replacement = " ")
                    x <- paste(x, collapse = "")
                    x <- trimws(x)
                    x
                  }),
                  stringsAsFactors = FALSE)
  x
}
texts <- list()
texts$te <- read_txt("data/transkribus-lines/HTR test corpus/imgs/te.txt")
texts$tr <- read_txt("data/transkribus-lines/HTR test corpus/imgs/tr.txt")
texts$va <- read_txt("data/transkribus-lines/HTR test corpus/imgs/va.txt")
texts <- rbindlist(texts, idcol = "type")
texts <- subset(texts, trimws(text) > 0)

images <- data.frame(path = list.files(settings$path_textlines, full.names = TRUE))
images$file <- basename(images$path)
images$basename <- sapply(images$file, file_path_sans_ext)
images <- subset(images, basename %in% texts$doc_id)
texts  <- subset(texts, doc_id %in% images$basename)

for(i in seq_len(nrow(images))){
  #i <- 1
  f <- images[i, ]
  img <- image_read(f$path)
  img <- image_convert(img, "tiff")
  image_write(img, path = file.path(settings$path_tesseract, sprintf("%s.tif", f$basename)))
  txt <- subset(texts, doc_id %in% f$basename)
  txt <- txt$text
  writeLines(txt, con = file.path(settings$path_tesseract, sprintf("%s.gt.txt", f$basename)))
}

#unzip -j tesseract.zip tesseract/* -d data/getuigenissen-ground-truth
#nohup make training MODEL_NAME=getuigenissen &