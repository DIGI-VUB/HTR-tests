library(magick)
urls <- readLines("data/image_urls.txt")

##
## Get all images from zooniverse
##
#for(i in seq_along(urls)){
for(i in 5991:8109){
    Sys.sleep(2)
    f <- urls[i]
    cat(sprintf("%s: %s/%s, downloading %s", Sys.time(), i, length(urls), f), sep = "\n")
    x <- image_read(f)
    image_write(x, path = file.path("data/scans", basename(f)), quality = 100)
}

while(length(missing <- file.path(dirname(urls)[1], setdiff(basename(urls), list.files("data/scans")))) > 0){
    try({
        f <- sample(missing, size = 1)
        cat(sprintf("%s: todo %s, downloading %s", Sys.time(), length(missing), f), sep = "\n")
        x <- image_read(f)
        image_write(x, path = file.path("data/scans", basename(f)), quality = 100)
        gc()
    })   
}


