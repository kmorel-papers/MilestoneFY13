
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
 
# Need at least 8 different points and shapes
library("grDevices")

# Common colors, shapes, and linetypes
pvspy_colors <- c(
	"goldenrod2", 
	"forestgreen", 
	"indianred1",
	"dodgerblue", 
	"darkorchid2", 
	"darkslategray3",
        "burlywood",
        "brown")

#pvspy_colors=rainbow(8)
#pvspy_colors=heat.colors(8)

# This will show you the colorwheel
# pie(rep(1, 8), col=pvpsy_colors)

pvspy_shapes <- c(21, 22, 23, 24, 25)
pvspy_lines <- 1:8      # linetype

pvspy_line_size <- 1
pvspy_point_size <- 3
pvspy_error_width <- 0.1

pvspy_plot_width <- 5
pvspy_plot_height <- 4

