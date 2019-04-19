# # load save_flextable()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("save_flextable.R")
# setwd(current_wd)



library(tidyverse)
library(officer)
library(flextable)
library(rvg)
library(rmarkdown)
library(webshot)
library(magick)



# create save_flextable function
save_flextable <- function(flextable, filename_wo_ext, format = "png", zoom = 5) {
        
        # save regular table to png requires using webshot (per author of officer package)
        # https://stackoverflow.com/questions/50225669/how-to-save-flextable-as-png-in-r
        # note i use slightly differnt code than the stackoverflow answer, 
        # knitting rmd direct from r script intead of creating rmd
        # trying pdf()...dev.off() gets error that table is not a vector graphic with x,y coordinates
        
        # save r script to tempfile for rendering as an rmarkdown doc
        r_script_name <- tempfile(fileext = ".R")
        cat(deparse(substitute(flextable)), file = r_script_name)

        
        # create temporary html file for output of rendering
        html_name <- tempfile(fileext = ".html")
        
        # render r script as if it were rmarkdown
        # http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/
        render(input = r_script_name, output_format = "html_document", 
               output_file = html_name)
        
        # get a png from the html file with webshot
        # note that png and pdf will have higher resolution the higher the zoom
        # 1 vs 5 is very noticeable; 10 vs 20 is less so until you enlarge pdf to see fine details
        webshot(html_name, zoom = zoom, delay = .5, file = str_c(filename_wo_ext, ".png"), selector = "table")
        
        # handle if format = "pdf"
        if(format == "pdf") {
                
                # read png into magick
                table_png_image <- image_read(str_c(filename_wo_ext, ".png"))

                # output png as pdf
                image_write(table_png_image, path = str_c(filename_wo_ext, ".", format), format = "pdf")
        }
}


########################


# test
# flextable <- regulartable(head(mtcars))
# flextable
# filename_wo_ext <- "mtcars_ft"
# format <- "pdf"
# zoom <- 20
# 
# save_flextable(flextable = flextable, filename_wo_ext = filename_wo_ext, format = format, zoom = zoom)
