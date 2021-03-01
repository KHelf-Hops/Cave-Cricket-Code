rm(list = ls())
pkgs <- c("exifr", "exiftoolr", "janitor", "tidyverse", "here")
lapply(pkgs, library, character.only = TRUE, quietly = TRUE)

setwd(getwd()) # Sets working directory to where this script is, which is at the same level as Images folder

# Grab files from all Mod folders, which are located several levels down from Images
x <- list.files("./Images/", pattern = ".JPG$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE) # Not necessary to specify the Images subdirectory, but I did anyway. 'full.names = TRUE' returns the full path name; 'recursive = TRUE' says to list matching files from all subdirectories 
file_ <- x[grepl('^((?!.*Intersect).)', x, ignore.case = TRUE, perl=TRUE)] # Exclude if has "Intersect" or "intersect" in file name
exif_df <- exif_read(photo_files)
exif_data <- select(exif_df,
                         SourceFile, FNumber, ISO,
                         MaxApertureValue, FocalLength)
