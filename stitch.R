############################################################
# 
# author: Ludwig Geistlinger
# date: 2022-07-28 10:25:49
# 
# descr: script for stitching image tiles into a mosaic
#
# usage: Rscript stitch.R <img.dir> <prefix> <fov> <corners>
#
#       img.dir: image directory, contains tiles to stitch
#                images are expected to be tiff
#       prefix: prefix of image files, eg "dapi_"
#       fov: file in csv format associating tile ID with
#            field-of-view coordinates
#       corners: files in csv format associating tile ID with
#                corner coordinates
# 
############################################################

# setup
library(EBImage)

# stitch mosaic
stitch <- function(img.files, fov, corners, frame = 1, block.size = 9)
{
    img.list <- lapply(img.files, EBImage::readImage, all = frame)
    img.list <- .crop(img.list, corners, block.size)
    ind <- .arrange(fov, block.size)
    image <- EBImage::combine(img.list[ind])
    timg <- EBImage::tile(image, nx = block.size, lwd = 0)
    timg <- EBImage::flip(timg)
    return(timg)
}

# arrange tiles based on field-of-view coordinates
.arrange <- function(fov, block.size = 9)
{ 
    y <- fov$fovPosY[seq_len(block.size)]
    ind <- lapply(y, function(i) which(fov$fovPosY == i))
    ind <- unlist(ind)
    return(ind)
}

# crop tiles as they intentionally overlap to a certain extent
.crop <- function(img.list, corners, block.size = 9)
{
    # get actual tile width/height
    tile.width <- corners$xEnd - corners$xStart 
    tile.width <- tile.width[1] 
    tile.height <- corners$yEnd - corners$yStart 
    tile.height <- tile.height[1]

    # raster width
    rwidth <- nrow(img.list[[1]])

    # get overlap 
    int.width <- abs(corners$xStart[1]) - abs(corners$xStart[1 + block.size])
    d <- (tile.width - int.width) / 2
    d <- d / tile.width
    d <- round(d * rwidth)
    
    # small manual adjustment
    # TODO: how can this be remedied?
    d <- d - 10
    
    # crop & return
    grid <- seq(d, rwidth - d) 
    img.list <- lapply(img.list, function(i) i[grid,grid])
    return(img.list) 
}

# get command line arguments
cmd.args <- commandArgs(trailingOnly = TRUE)

img.dir <- cmd.args[1]
prefix <- cmd.args[2]
fov <- cmd.args[3]
corners <- cmd.args[4]

# read image files
prefix <- paste0("^", prefix)
img.files <- list.files(img.dir, full.names = TRUE, pattern = prefix)

# get image IDs
spl <- strsplit(img.files, "_")
suffix <- vapply(spl, `[`, character(1), x = length(spl[[1]]))
suffix <- sub(".tiff$", "", suffix)
ids <- as.integer(suffix)

# how many frames per image?
i <- EBImage::readImage(img.files[1])
nr.frames <- EBImage::numberOfFrames(i) 

# read coordinates
corners <- read.csv(corners)
fov <- read.csv(fov)

# get block size
block.size <- unique(lengths(rle(fov$fovPosX)))
stopifnot(length(block.size) == 1)

# align image files with field-of-view and corners
stopifnot(all(ids %in% corners[,1]))
stopifnot(all(ids %in% fov[,1]))
ind <- match(ids, fov$fovID)
fov <- fov[ind,]
ind <- match(ids, corners$fovIDs)
corners <- corners[ind,]

# stitch one frame at a time
mosaic.stack <- lapply(seq_len(nr.frames), stitch, 
                         img.files = img.files, 
                         fov = fov,
                         corners = corners,
                         block.size = block.size)
# combine into a stack
mosaic.stack <- EBImage::combine(mosaic.stack)

# write mosaic stack
EBImage::writeImage(mosaic.stack, "mosaic.tiff", type = "tiff")
