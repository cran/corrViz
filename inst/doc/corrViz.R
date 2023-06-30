## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
suppressPackageStartupMessages(library(corrViz))

## -----------------------------------------------------------------------------
library(corrViz)

## ---- corrMat-----------------------------------------------------------------
cm <- cor(mtcars)

## ---- heatmap, out.width='100%', out.height='100%'----------------------------
corrHeatmap(mat = cm,
            display = 'all',
            reorder = TRUE,
            pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100))


## ---- heatmap2, out.width='100%', out.height='100%'---------------------------
corrHeatmap(mat = cm,
            display = 'lower',
            reorder = FALSE)


## ---- network, out.width='100%', out.height='100%'----------------------------
corrNetwork(mat = cm,
            threshold = 0,
            layout = "layout_nicely",
            width = "100%",
            height = "400px",
            physics = FALSE)


## ---- network2, out.width='100%', out.height='100%'---------------------------
corrNetwork(mat = cm,
            threshold = 0.8,
            physics = TRUE)

## ---- solar2, fig.align='center', out.width='75%', out.height='75%'-----------
animSolar(mat = cm,
          sun = 'mpg',
          export = FALSE,
          num_frames = 100,
          path = NULL,
          gif_name = "solar_system.gif",
          fps = 60)

## ---- bar1, out.width='100%', out.height='100%'-------------------------------
corrBarplot(mat = cm,
            interactive = TRUE,
            pal = colorRampPalette(c("cornflowerblue", 'white', 'tomato'))(100))

## ---- bubble, out.width='100%', out.height='100%'-----------------------------
corrBubble(mat = cm,
           display = 'all',
           pal = colorRampPalette(c("cornflowerblue", "white", "tomato"))(100))

## ---- bubble1, out.width='100%', out.height='100%'----------------------------
corrBubble(mat = cm,
           display = 'upper')

## ---- sp, fig.align='center', out.width='100%', out.height='100%', warning=FALSE----

corrPairs(data = mtcars[1:5],
         interactive = TRUE,
         col_by = "cyl")


## ---- sankey, fig.align='center', out.width='100%', out.height='100%'---------
corrSankey(mat = cm,
           threshold = 0.6,
           colour = FALSE)


## ---- sankey1, fig.align='center', out.width='100%', out.height='100%'--------
corrSankey(mat = cm,
           threshold = 0.8,
           colour = TRUE)


## ---- shiny, eval=FALSE-------------------------------------------------------
#  corrShiny(data = mtcars,
#            x_var = "wt",
#            y_var = "mpg",
#            color_var = "cyl",
#            size_var = "hp")
#  

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/corrViz/main/badge/shiny.png")

## ---- square, fig.align='center',out.width='100%', out.height='100%'----------
corrGrid(mat = cm[1:7,1:7],
         type = 'square')


## ---- circlegrid, fig.align='center',out.width='100%', out.height='100%'------
corrGrid(mat = cm[1:7,1:7],
         type = 'circle')


## ---- text, fig.align='center', out.width='100%', out.height='100%'-----------
corrGrid(mat = cm[1:7,1:7],
         type = 'text')


## ---- pie, fig.align='center', out.width='100%', out.height='100%', warning=FALSE----
corrGrid(mat = cm[1:7,1:7],
         type = 'pie')


## ---- solar, fig.align='center', out.width='65%', out.height='65%'------------
corrSolar(mat = cm,
          sun = 'mpg')


## ---- chord, fig.align='center', out.width='60%', out.height='60%'------------
corrChord(mat = cm,
          circle = FALSE, 
          threshold = 0.8)

## ---- chord2, fig.align='center', out.width='60%', out.height='60%'-----------
corrChord(mat = cm, 
          circle = TRUE, 
          threshold = 0.8)

## ---- circle, fig.align='center', out.width='60%', out.height='60%'-----------
corrCircle(mat = cm, 
           threshold = 0,
           ticks = TRUE)

## ---- circle2, fig.align='center', out.width='60%', out.height='60%'----------
corrCircle(mat = cm, 
           ticks = FALSE, 
           threshold = 0.8)

## ---- m2df--------------------------------------------------------------------
corr_matrix <- cor(mtcars[1:4])
long_data_frame <- matrix2long(mat = corr_matrix)
long_data_frame

