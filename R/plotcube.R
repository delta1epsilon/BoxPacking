library(rgl)
library(dplyr)

#' A custom function to initialize RGL device
#' 
#' @param new.device - A logical value. If TRUE, creates a new device
#' @param width the width of the device
RGLInit <- function(new.device = FALSE, width = 500) { 
    if (new.device | rgl.cur() == 0) {  # rgl.cur(): returns active device ID
        rgl.open()
        par3d(windowRect = 50 + c(0, 0, width, width))
        rgl.bg(color = "white")
    }
    rgl.viewpoint(theta = 40, phi = 20)
}

#' Plot 3D cube 
#' 
#' @param origin - A vector
#' @param length - A numeric
#' @param height - A numeric
#' @param width  - A numeric
#' @param is_container - logical, plots the box in origin and color is black 
#' @param plot_origin  - logical, whether to plot point at the origin
#' @examples 
#' 
#' RGLInit(new.device = T)  # create new device with specific settings
#' # plot a container
#' PlotCube(width = 2, 
#'          length = 4, 
#'          height = 2, 
#'          is_container = TRUE
#'          )
#' # plot a box 
#' PlotCube(origin = c(0, 0, 0), 
#'          width = 1, 
#'          length = 1, 
#'          height = 1
#'          )
PlotCube <- function (origin,
                      length,
                      height,
                      width,
                      is_container = FALSE,
                      plot_origin = TRUE, ...) {
    
    if (width <= 0) {
        stop('Spicify argument: width')
    } else if (length <= 0) {
        stop('Spicify argument: length')
    } else if (height <= 0) {
        stop('Spicify argument: height')
    }
    
    if (is_container) {
        origin <- c(0, 0, 0)
    }
    
    vertex1 <- origin
    vertex2 <- origin + c(0, height, width)
    vertex3 <- origin + c(length, height, 0)
    vertex4 <- origin + c(length, 0, width)
    
    # create data frame with coordinates of lines
    # to be joined to form a cube
    lines <- data.frame(vertex1, origin + c(0, height, 0))
    lines <- lines %>% cbind(data.frame(vertex1, origin + c(length, 0, 0)))
    lines <- lines %>% cbind(data.frame(vertex1, origin + c(0, 0, width)))
    
    lines <- lines %>% cbind(data.frame(vertex2, origin + c(0, 0, width)))
    lines <- lines %>% cbind(data.frame(vertex2, origin + c(0, height, 0)))
    lines <- lines %>% cbind(data.frame(vertex2, origin + c(length, height, width)))
    
    lines <- lines %>% cbind(data.frame(vertex3, origin + c(0, height, 0)))
    lines <- lines %>% cbind(data.frame(vertex3, origin + c(length, 0, 0)))
    lines <- lines %>% cbind(data.frame(vertex3, origin + c(length, height, width)))
    
    lines <- lines %>% cbind(data.frame(vertex4, origin + c(0, 0, width)))
    lines <- lines %>% cbind(data.frame(vertex4, origin + c(length, 0, 0)))
    lines <- lines %>% cbind(data.frame(vertex4, origin + c(length, height, width)))
    
    lines <- t(lines)
    
    if (is_container) {
        cube_color <- 'black'
    } else {
        # randomly select color for cube
        colors <- c('blue', 'red', 'green', 'orange')
        cube_color <- sample(colors, 1)
    }
    
    # plot cube
    segments3d(lines, line_antialias = TRUE, color = cube_color, ...)
    if (plot_origin) {
       points3d(x = 0, y = 0, z = 0, color = 'red', size = 7)
    }
}
