library(tidyverse)
library(officer)
library(flextable)
library(rvg)
library(rmarkdown)
library(webshot)
library(magick)

# https://davidgohel.github.io/flextable/articles/overview.html
# https://davidgohel.github.io/flextable/reference/index.html

# setwd
setwd("C:/Users/Stephen/Desktop/R/flextable")

# quick example - note saving with save_as_docx is best option
# saving as pdf or image etc results in data quality issues when copied to word and attempt pdf conversion
# quality can look good in Word, but when attempt to convert to pdf it gets grainy
# also go in Word to File -> Options -> Advanced -> 
# scroll down to Image Size/Quality, check don't compress, select highest quality dpi
# then hit File -> Print -> Adobe PDF (don't use Save As PDF option)
# in Print dialog window, hit Printer Properties and select highest quality
data <- starwars %>% select(name, species, homeworld, height, mass) %>% slice(1:5)
ft <- data %>% flextable()
ft
ft %>% save_as_docx(path = "test_starwars_ft.docx")


# flextable has lots of customization options, here is an example
# note that as the pacakge updated, some of this original code broke, but just visit the site docs, it's great
data %>% flextable() %>% 
        add_header_row(values = "This is the title", colwidths = data %>% ncol()) %>%
        align(i = 1, j = "name", align = "center", part = "header") %>%
        style(i = 1, j = c("name", "species", "homeworld", "height", "mass"),
              part = "header", pr_t = fp_text(font.size = 15, font.family = "Century")) %>%
        style(i = 2, j = c("name", "species", "homeworld", "height", "mass"),
              part = "header", pr_c = fp_cell(background.color = "#0066ff"),
              pr_t = fp_text(color = "#ffffff", bold = TRUE, font.family = "Century")) %>%
        style(i = seq(from = 1, to = nrow(data), by = 2), 
              part = "body", pr_c = fp_cell(background.color = "#f2f2f2")) %>%
        style(i = 1:nrow(data), j = c("name", "species", "homeworld", "height", "mass"),
              part = "body", 
              fp_text(font.family = "Century")) %>% 
        add_footer_row(values = "this is footnote 1", colwidths = ncol(data)) %>%
        add_footer_row(values = "this is footnote 2", colwidths = ncol(data)) %>%
        compose(i = 1, part = "footer", 
                value = as_paragraph(as_chunk("a ", props = fp_text(font.size = 7, 
                                                                    vertical.align = "superscript")),
                                     "this is footnote 1")) %>%
        compose(i = 2, part = "footer", 
                value = as_paragraph(as_chunk("b ", props = fp_text(font.size = 7, 
                                                                    vertical.align = "superscript")),
                                     "this is footnote 2")) %>%
        style(part = "footer", pr_t = fp_text(font.family = "Century")) %>%
        compose(j = "species", part = "header", 
                value = as_paragraph("species", as_chunk("a", props = fp_text(color = "#ffffff", font.size = 7, 
                                                                              vertical.align = "superscript")))) %>%
        compose(i = ~ height > 160, j = "mass", part = "body", 
                value = as_paragraph("new_value: ", 
                                     as_chunk(mass), 
                                     as_chunk("b", props = fp_text(color = "#ff3300", 
                                                                   font.size = 7, vertical.align = "superscript")))) %>%
        compose(i = ~ species == "Human", j = "species",
                value = as_paragraph(as_image( src = file.path( R.home("doc"), "html", "logo.jpg" ), 
                                               width = .20, height = .15))) %>%
        compose(j = "height", part = "body",
                value = as_paragraph(minibar(value = height, max = max(height)))) %>%
        autofit() %>% width(j = "mass", width = 2)



######################################################################
######################################################################
######################################################################


# example
mtcars_table <- mtcars %>% head() %>% regulartable(col_keys = c("am", "carb", "gear", "mpg", "drat")) 
mtcars_table

# can apply themes
mtcars_table %>% theme_booktabs() # default
mtcars_table %>% theme_vanilla()
mtcars_table %>% theme_zebra()
mtcars_table %>% theme_tron()
mtcars_table %>% theme_tron_legacy()
mtcars_table %>% theme_box()


#################################################################


# layout

# merge
# note that value of cell[min(i), min(j)] is used to populate the new merged cell
ft <- flextable( head( iris ),
                 col_keys = c("Species", "Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width") )
ft <- add_footer(ft, Species = "This is a footnote" )
ft <- merge_at(ft, j = 1:5, part = "footer")
ft <- merge_at(ft, i = 1:2, j = 1:2, part = "body")
ft

# merge_v will merge adjacent duplicated cells for each column of the selection
select_columns <- c("Species", "Petal.Length", "Petal.Width")
myft <- regulartable(iris[46:55,], col_keys = select_columns)
myft <- merge_v(myft, ~ Species + Petal.Width )
myft

# merge_h() will merge adjacent duplicated cells for each row of the selection
select_columns <- c("Species", "Petal.Length", "Petal.Width")
myft <- regulartable(head(mtcars, n = 10 ) )
myft <- merge_h(myft)
# and add borders
myft <- border(myft, border = fp_border(), part = "all") 
myft

# merge_at() will merge cells for a given continuous selection of rows and cells
# note that value of cell[min(i), min(j)] is used to populate the new merged cell
select_columns <- c("Species", "Petal.Length", "Petal.Width")
myft <- regulartable(head(mtcars, n = 6 ) )
myft <- merge_at( myft, i = 1:3, j = 1:3)
myft <- border(myft, border = fp_border(), part = "all")
myft

# can reverse merges
merge_none(myft)

# col_keys argument controls order of variables (like select function); by default it uses original ordering
iris %>% head() %>% regulartable()
iris %>% head() %>% regulartable(col_keys = c("Species", "Sepal.Width"))


########################################################################


# showcasing various formats: 
# note empty column "col_1" included for example of empty_blanks below
myft <- regulartable(
        data = iris[c(1:3, 51:53, 101:104),], 
        col_keys = c("Species", "col_1", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width") )
myft

# adding theme
myft <- theme_vanilla(myft)
myft

# autofit column width and row height
# myft <- autofit(myft)
myft

# show gap in table gridlines to reflect the empty column "col_1"
myft <- empty_blanks(myft)
myft

# change variable names (headers)
# note that though the displayed variable changes, future header functions still refer to them by original var_names
myft <- myft %>% set_header_labels(Sepal.Length = "Sepal", Sepal.Width = "Sepal", 
                                   Petal.Length = "Petal", Petal.Width = "Petal", Species = "Species")
myft

# add a new row of headers (top = FALSE will place it below lowest header)
myft <- myft %>% add_header(Species = "Species", Sepal.Length = "length", Sepal.Width = "width",
                            Petal.Length = "length", Petal.Width = "width", top = FALSE)
myft

# add a new row of headers (top = TRUE will place it on top)
myft <- myft %>% add_header(Species = "Species", Sepal.Length = "inches", Sepal.Width = "inches",
                            Petal.Length = "inches", Petal.Width = "inches", top = TRUE)
myft

# add footer
myft <- myft %>% add_footer(Species = "* This is a note")
myft

# color footer
myft <- myft %>% color(color = "orange", part = "footer")
myft

# merge identical cells of headers horizontally
myft <- myft %>% merge_h(part = "header")
myft

# merge identical cells of headers vertically
myft <- myft %>% merge_v(part = "header")
myft

# merge identical cells of footer
myft <- myft %>% merge_at(i = 1, j = 1:5, part = "footer") %>% theme_booktabs()
myft


###################################################################


# inspect dimensions with dim function
ft_base <- iris %>% head() %>% regulartable()
ft_base
dim(ft_base)

# use dim_pretty for optimized height/width
# dim_pretty(ft_base)

# autofit changes height/width, but not exactly to dim_pretty
ft_base_pretty <- ft_base %>% autofit()
ft_base_pretty
dim(ft_base_pretty)

# can adjust autofit height/width padding
ft_base %>% autofit(add_w = .5, add_h = .5)

# adjust height/width manually
ft_base
ft_base %>% width(j = ~ Species, width = 2)
ft_base %>% height_all(height = .4)
ft_base %>% height(i = 3, height = 1)


##############################################################################


# formatting
myft <- regulartable(head(iris))
myft

# bold
myft <- myft %>% bold(part = "header") 
myft

# font size
myft <- myft %>% fontsize(part = "header", size = 12) 
myft

# font color
myft <- myft %>% color(color = "#0000ff")
myft

# italics
myft <- myft %>% italic(i = ~ Sepal.Length > 5, 
               j = ~ Sepal.Length + Sepal.Width, italic = TRUE)
myft

# change background color
# dark gray as background color for header
myft <-  myft %>% bg(bg = "#a6a6a6", part = "header")
myft

# light gray as background color for body
myft <-  myft %>% bg(bg = "#e6e6e6", part = "body")
myft

# text alignment
myft <- myft %>% align(align = "center", part = "all")
myft

# add padding
myft <- myft %>% padding(padding = 3, part = "all") # can also specify padding.top/bottom/left/right
myft

# font name
myft <- myft %>% font(j = "Species", fontname = "Times")
myft

# font size for column
myft <- fontsize(myft, j = "Species", size = 20)
myft


################


# rotation - doesn't seem to work at least in r graphic device.  (maybe will show in pptx or docx??)
ft <- iris %>% head() %>% regulartable()
ft

ft <- ft %>% rotate(rotation = "tbrl", align = "top", part = "header")
ft

ft <- theme_vanilla(ft)
ft
ft <- autofit(ft)

# as autofit do not handle rotation, you will have
# to change manually header cells'height.
ft <- height(ft, height = 1, part = "header")
ft


################


# borders

# adjust each border manually for high customization

# remove all defined borders
myft <- myft %>% border_remove()
myft

# define customized replacement borders of class fp_border
big_b <- fp_border(color="gray70", width = 2)
std_b <- fp_border(color="white")
str(big_b)

myft <- vline(myft, border = std_b, part = "all")
myft

myft <- vline_left( myft, border = big_b, part = "all" )
myft

myft <- vline_right( myft, border = big_b, part = "all" )
myft

myft <- hline( myft, border = std_b )
myft <- hline_bottom( myft, border = big_b )
myft <- hline_top( myft, border = big_b, part = "all" )
myft


###############


# adjust boders more simply with generic border_outer/inner_h/inner_v functions
std_b2 <- fp_border(color="white", style = "dashed")

# remove all defined borders
myft <- border_remove( myft )
myft

# adjust borders with simpler border_outer/inner_h/inner_v functions
myft <- border_outer( myft, border = big_b, part = "all" )
myft 

myft <- border_inner_h( myft, border = std_b, part = "all" )
myft

myft <- border_inner_v( myft, border = std_b2, part = "all" )
myft

################

#  conditional formatting
myft <- myft %>% color(i = ~ Sepal.Length < 5 & Petal.Length > 1.3, 
              j = ~ Petal.Width + Species, color="red")
myft

myft <- bg(myft, j = 1, bg = "#D3C994", part = "header")
myft

myft <- italic(myft, i = ~ Sepal.Length > 5)
myft

myft <- bold(myft, i = 4, j = "Sepal.Length")
myft

# can also apply conditionals using TRUE/FALSE vectors
rows_w_formatting <- iris %>% head() %>% mutate(rows_w_formatting = Sepal.Length < 5 & Petal.Length > 1.3) %>% 
        pull(rows_w_formatting)
rows_w_formatting

cols_w_formatting <- iris %>% head() %>% select(Species, Sepal.Length, Petal.Length) %>% names()
cols_w_formatting 

myft <- myft %>% color(i = rows_w_formatting, j = cols_w_formatting, color = "green")
myft


##############################################################


# styles
def_cell <- fp_cell(border = fp_border(color="#00C9C9"))
def_par <- fp_par(text.align = "center")
def_text <- fp_text(color="#999999", italic = TRUE)
def_text_header <- update(color="black", def_text, bold = TRUE)

ft <- regulartable(head(mtcars, n = 10 ))
ft <- style( ft, pr_c = def_cell, pr_p = def_par, pr_t = def_text, part = "all")  
ft

ft <- style( ft, pr_t = def_text_header, part = "header")  
ft


##########


# note that fp_cell, fp_border, and fp_par create flextable properties (fp) for cells, borders, and paragraphs 
# then you can apply these properties using style() by passing to pr_c (cell), pr_p (paragraph), and pr_t (text)
# note fp_border seems to be applied using pr_c, since it applies at cell-level

def_cell <- fp_cell(border = fp_border(color="#00FFFF"))

def_par <- fp_par(text.align = "center")

ft <- flextable(mtcars)

ft <- style( ft, pr_c = def_cell, pr_p = def_par, part = "all")
ft <- style(ft, ~ drat > 3.5, ~ vs + am + gear + carb,
            pr_t = fp_text(color="red", italic = TRUE) )

ft


####################################################


# set_formatter function (only used with regulartable, use display function in flextables)
ft <- regulartable(head(mtcars, n = 10 ), 
                   col_keys = c("gear", "mpg", "qsec"))
ft <- set_formatter(ft, 
                    mpg = function(x) scales::percent(x, big.mark = ","),
                    gear = function(x) sprintf("%.0f gears", x)
)
ft <- theme_booktabs(ft)
ft <- autofit(ft)
ft


##########################################################


# compose allows you to build customized formatting functions that can relay on other variables in row 
# note display() seems to have been deprecated in docs (though still works) in favor of compose()
# note that purrr compose() will mask flextable compose() if purrr/tidyverse is loaded after flextable
# so you must load purr/tidyverse first, and then load flextable

myft <- flextable( head(mtcars),
                   col_keys = c("am", "separator", "gear", "mpg", "drat", "qsec" ))
myft <- bold(myft, part = "header")
myft <- border(myft, border = fp_border(width = 0),
               border.top = fp_border(), border.bottom = fp_border(),
               part = "all")
myft <- align(myft, align = "right", part = "all" )
myft <- border(myft, j = ~ separator, border = fp_border(width=0), part = "all")
myft <- width(myft, j = ~ separator, width = .1)
myft

myft <- compose(
        myft, j = "mpg",
        value = as_paragraph(
                "mpg value is ",
                as_chunk(sprintf("%.01f", mpg), props = fp_text(color = "red", bold = TRUE) ) )
)
myft
myft %>% autofit()


##########


myft <- compose(
        myft, j = "mpg",
        value = as_paragraph(
                "mpg value is ",
                as_chunk(sprintf("%.01f", mpg), props = fp_text(color = "red", bold = TRUE) ),
                " with ",
                as_chunk(sprintf("# %.0f", carb), props = fp_text(color = "gray", italic = TRUE) )
        )
)

myft <- autofit(myft)
myft


########


myft <- compose(
        myft, j = "mpg", part = "header",
        value = as_paragraph(
                "Miles/(US) gallon ",
                as_chunk("* with num of carb.", props = fp_text(color = "gray", vertical.align = "superscript") )
        )
)

myft <- autofit(myft)
myft


############


img.file <- file.path( R.home("doc"), "html", "logo.jpg" )

myft <- compose( myft, i = ~ qsec > 18, j = "qsec",
                 value = as_paragraph(as_image( src = img.file, width = .20, height = .15))
)
myft <- autofit(myft)
myft


############


# can also add a minbar
myft <- flextable( head(iris, n = 10 ))

myft <- compose( myft, j = 1,
                 value = as_paragraph(
                         minibar(value = Sepal.Length, max = max(Sepal.Length))
                 ),
                 part = "body")

autofit(myft)


######################################################3


# # display function to format flextables
# # allows you to build customized formatting functions that can relay on other variables in row
# myft <- flextable( head(mtcars),
#                    col_keys = c("am", "separator", "gear", "mpg", "drat", "qsec" ))
# myft <- bold(myft, part = "header")
# myft <- border(myft, border = fp_border( width = 0),
#                border.top = fp_border(), border.bottom = fp_border(),
#                part = "all")
# myft <- align(myft, align = "right", part = "all" )
# myft <- border(myft, j = ~ separator, border = fp_border(width=0), part = "all")
# myft <- width(myft, j = ~ separator, width = .1)
# myft
# 
# # simple example of using display
# myft <- display( myft, col_key = "mpg", pattern = "{{mpg}}",
#                  formatters = list(mpg ~ sprintf("%.01f", mpg) ),
#                  fprops = list(mpg = fp_text(color = "red", italic = TRUE) )
# )
# myft
# 
# # complex example
# myft <- display( myft, i = ~ drat > 3.6,
#                  col_key = "mpg", pattern = "{{mpg}} with {{carb}}",
#                  formatters = list(mpg ~ sprintf("%.01f", mpg),
#                                    carb ~ sprintf("# %.0f carb.", carb) ),
#                  fprops = list(mpg = fp_text(color = "#CC55CC", bold = TRUE) )
# )
# # myft <- autofit(myft)
# myft
# 
# # more complex example
# myft <- display( myft, col_key = "mpg",
#                  part = "header",
#                  pattern = "Miles/(US) gallon {{my_message}}",
#                  formatters = list(
#                          my_message ~ sprintf("* with num of carb.")
#                  ),
#                  fprops = list(
#                          my_message = fp_text(color = "gray", vertical.align = "superscript")
#                  )
# )
# # myft <- autofit(myft)
# myft
# myft %>% width(j = 4, width = 3) %>% align(j = 4, align = "center")
# 
# 
# #############################################################
# 
# 
# # images
# # display can accept image insertion
# img.file <- file.path( R.home("doc"), "html", "logo.jpg" )
# 
# myft <- flextable( head(mtcars),
#                    col_keys = c("am", "separator", "gear", "mpg", "drat", "qsec" ))
# myft
# 
# myft <- display( myft, i = ~ qsec > 18, col_key = "qsec",
#                  pattern = "blah blah {{r_logo}} {{qsec}}",
#                  formatters = list(
#                          r_logo ~ as_image(qsec, src = img.file, width = .20, height = .15),
#                          qsec ~ sprintf("qsec: %.1f", qsec) ),
#                  fprops = list(qsec = fp_text(color = "orange", vertical.align = "superscript"))
# )
# myft <- autofit(myft)
# myft



#############################################################################
##############################################################################
###############################################################################


# add flextable to powerpoint

# create table
ft <- mtcars %>% head() %>% regulartable() %>% theme_booktabs() %>% autofit()
ft

# create powerpoint with regulartable
# note the table can be easily copied from ppt to excel
ppt <- read_pptx() %>% add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with_flextable(value = ft, type = "body") 
pptx_summary(ppt) %>% head()

# save powerpoint
print(ppt, target = "regulartable_example.pptx")



############


# create powerpoint with flextable
# note the table can be easily copied from ppt to excel
myft 

ppt <- read_pptx() %>% add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with_flextable(value = myft, type = "body") 
pptx_summary(ppt) %>% head()

# save powerpoint - note that the r logo image is not saved to ppt for some reason??
print(ppt, target = "flextable_example.pptx")

class(myft)


##############


# add regulartable to word
# note the table can be easily copied from word to excel
doc <- read_docx() %>% body_add_flextable(value = ft)
docx_summary(doc) %>% head()

print(doc, target = "regulartable_example.docx")


##################


# add flextable to word - note that r logo image does save to word doc (unlike ppt??)
# note the table can be easily copied from word to excel
doc <- read_docx() %>% body_add_flextable(value = myft)
docx_summary(doc) %>% head()

print(doc, target = "flextable_example.docx")


###########################################################################################


# save regular table to png requires using webshot (per author of officer package)
# https://stackoverflow.com/questions/50225669/how-to-save-flextable-as-png-in-r
# note i use slightly differnt code than the stackoverflow answer, 
# knitting rmd direct from r script intead of creating rmd
# trying pdf()...dev.off() gets error that table is not a vector graphic with x,y coordinates

# save r script to tempfile for rendering as an rmarkdown doc
r_script_name <- tempfile(fileext = ".R")
cat("myft", file = r_script_name)

# create temporary html file for output of rendering
html_name <- tempfile(fileext = ".html")

# render r script as if it were rmarkdown
# http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/
render(input = r_script_name, output_format = "html_document", 
       output_file = html_name)

# get a png from the html file with webshot
# note that png and pdf will have higher resolution the higher the zoom
# 1 vs 5 is very noticeable; 10 vs 20 is less so until you enlarge pdf to see fine details
webshot(html_name, zoom = 20, delay = .5, file = "regulartable_zoom.png", 
        selector = "table")

# read png into magick
table_png_image <- image_read("regulartable_zoom.png")
table_png_image

# output png as pdf
image_write(table_png_image, path = "regulartable_zoom.pdf", format = "pdf")


###########################################################################################


# add ggplot to excel
gg <- ggplot(mtcars, aes(x = mpg , y = wt, colour = qsec)) + geom_point() + theme_minimal()
gg

doc <- read_xlsx()
doc <- xl_add_vg(doc, sheet = "Feuil1", code = print(gg), 
                 width = 6, height = 6, left = 1, top = 2 )
print(doc, target = "gg_example.xlsx")


###########################################################################################


# rvg package allows you put editable ggplots into powerpoint (not word)
gg <- ggplot(mtcars, aes(x = mpg , y = wt, colour = qsec)) + geom_point() + theme_minimal()
gg

# save editable plot to powerpoint with ph_with_vg function
read_pptx() %>% 
        add_slide(layout = "Title and Content", master = "Office Theme") %>% 
        ph_with_vg(code = print(gg), type = "body") %>% 
        print(target = "editable_ggplot.pptx")

