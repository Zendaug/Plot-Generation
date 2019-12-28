# An R macro for creating a two-way moderation graph, which is automatically saved as an SVG file
# Version 1.0.1: Fixed the file notation to indicate: Dependent Variable ON Independent Variable X Moderator

# Data ----
result.table <- list(
  y.name = "Knowledge Sharing", # The name of the dependent variable
  y.intercept = 4.032,          # The intercept of the dependent variable
  predictors = data.frame(name = c("AGE", "GENDER", "TIMEWORK", "LGO2", "High Performance HRM", "HXL", "SCH2", "SCH3", "SCH4", "SCH5", "SCH6", "TEAMSIZE", "HR Strength", "HxS"),
                          mean = c(48.556, 0.636, 5.977, 0, 0, 0.044, 0.359, 0.109, 0.125, 0.109, 0.125, 10.75, 0, 0),
                          var = c(105.173, 0.232, 3.428, 0.475, 0.274, 0.135, 0.23, 0.097, 0.109, 0.097, 0.109, 33.562, 0.199, 0),
                          beta = c(-0.01, 0.025, 0.009, 0.156, 0.133, -0.19, 0.127, -0.275, -0.17, 0.075, 0.227, 0.006, 0.006, 0.133),
                          x.off = c(0, 0, 0, 0, 3.324, 0, 0, 0, 0, 0, 0, 0, 2.63, 0)),
  x = 5,  # The variable on the X-axis
  m = 13, # The variable that is the moderator
  xm = 14 # The variable that is the interaction term
)

# Settings----
image_width = 6  # The width of the image (in inches)
height_ratio = (1 + sqrt(5))/2 # Set the ratio to phi by default
image_height = image_width / height_ratio # The height of the image (in inches)
file_type = "svg" # The file type. It is recommended to use a vector format (e.g., "svg" or "emf")

ylim_lower <- NA #The lower limit of the Y-axis (leave as NA for it to be automatically set)
ylim_upper <- NA #The upper limit of the Y-axis (leave as NA for it to be automatically set)
xlim_lower <- NA #The lower limit of the X-axis (leave as NA for it to be automatically set)
xlim_upper <- NA #The upper limit of the Y-axis (leave as NA for it to be automatically set)

# Determine plot file name ----
file_name <- paste0(result.table$y.name,
                    " on ",
                   result.table$predictors$name[result.table$x],
                   " X ",
                   result.table$predictors$name[result.table$m],
                   ".",
                   file_type)# The name of the file to be created

# Load Packages----
load.package <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

load.package("ggplot2")
load.package("lemon")

# Include themes----
theme.plain <- theme(axis.line.x = element_line(colour = "black"), # Add in x axis
                     axis.line.y = element_line(colour = "black"),       # Add in y axis
                     text = element_text(family="sans"),                 # Set the font (options are: "mono", "serif", "sans"
                     panel.grid.major = element_blank(),                 # Delete major grid
                     panel.grid.minor = element_blank(),                 # Delete minor grid
                     panel.border = element_blank(),                     # Delete border
                     panel.background = element_blank(),                 # Make background blank
                     legend.key = element_blank(),
                     plot.margin = margin(5, 5, 5, 5, "pt"),
                     strip.background = element_blank(),                 # Make the facet heading blank
                     strip.placement = "outside",                        # For faceting, it will put the facet label outside of the Y-axis 
                     panel.spacing = unit(1, "lines"))                   # Move facets further apart

twoway <- function(dataset) {
  y_name <- c()
  cond_name <- c()
  x_val <- c()
  y_val <- c()
  fac_list <- c()
  x = dataset$x
  m = dataset$m
  xm = dataset$xm
  
    for (m_cnt in 1:2) {
      if (m_cnt == 1) {
        m_status = " +1SD"
        m_adj = dataset$predictors$mean[m] + sqrt(dataset$predictors$var[m])
      }
      else {
        m_status = " -1SD"
        m_adj = dataset$predictors$mean[m] - sqrt(dataset$predictors$var[m])       
      }
    
      x.begin <- dataset$predictors$mean[x] - sqrt(dataset$predictors$var[x])
      x.end <- dataset$predictors$mean[x] + sqrt(dataset$predictors$var[x])
      
      # Determine whether each variable is an IV / one of the moderators (TRUE), or one of the other variables (FALSE)
      is_main <- c()
      for (d in 1:dim(dataset$predictors)[1]) {
        is_main[d] <- FALSE
        if (d == x || d == m || d == xm) {is_main[d] <- TRUE}
      }
      
      # Create predicted values of Y
      y.pred1 <- dataset$y.intercept +
        dataset$predictors$beta[x] * x.begin +
        dataset$predictors$beta[m] * m_adj +
        dataset$predictors$beta[xm] * x.begin * m_adj

      y.pred2 <- dataset$y.intercept +
        dataset$predictors$beta[x] * x.end +
        dataset$predictors$beta[m] * m_adj +
        dataset$predictors$beta[xm] * x.end * m_adj
      
      for (d in 1:dim(dataset$predictors)[1]) { #(d in 1:length(is_main)) {
        if (is_main[d] == FALSE) {
          y.pred1 <- y.pred1 + dataset$predictors$beta[d] * dataset$predictors$mean[d]
          y.pred2 <- y.pred2 + dataset$predictors$beta[d] * dataset$predictors$mean[d]
        }
      }
      
      y_name <- c(y_name, rep(dataset$y.name, 2))
      cond_name <- c(cond_name, rep(paste0(dataset$predictors$name[m], m_status),2))
      x_val <- c(x_val, x.begin + dataset$predictors$x.off[x], x.end + dataset$predictors$x.off[x])
      y_val <- c(y_val, y.pred1, y.pred2)
    }
  
  return(list(
    xlab = dataset$predictors$name[x],
    ylab = unique(y_name)[1],
    line.coor = data.frame(
      y_name = factor(y_name, levels = unique(y_name)),
      cond_name = factor(cond_name, levels = unique(cond_name)),
      x = x_val, 
      y = y_val)))
}

# Run GGPlot----
twoway2_graph <- function(graph.data) {
  plot1 <- ggplot(data = graph.data$line.coor, aes(x = x, y = y, group = cond_name)) + 
    geom_line(aes(linetype = cond_name)) +
    geom_point(aes(shape = cond_name), size = 2.5) +
    #geom_point(data = point.data, aes(HRCT, KSH3, group = NULL)) + 
    scale_shape_manual(values=c(15,16,17,18)) +
    #ggtitle('Effects of X') +
    labs(linetype = "Condition", shape = "Condition") +
    xlab(graph.data$xlab) +
    ylab(graph.data$ylab) +
    #ylab(graph.info$ylab) + 
    theme.plain
  if (!is.na(ylim_lower) | !is.na(ylim_upper)) {plot1 = plot1 + ylim(ylim_lower, ylim_upper)}
  if (!is.na(xlim_lower) | !is.na(xlim_upper)) {plot1 = plot1 + xlim(xlim_lower, xlim_upper)}
  print(plot1)
}

# Generate the data
graph.data1 <- twoway(result.table)

# Generate the images and display them
image = twoway2_graph(graph.data1)

# Save the image
ggsave(filename = file_name, 
       plot = image, 
       units = "in", 
       width = image_width, 
       height = image_height)
