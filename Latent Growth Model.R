# An R macro for creating a plot for depicting the results of a latent growth model. Shows the effect of a predictor on the latent intercept / slope.
# Upcoming version: Plot data points at each wave with jitter. Separate into low/high on each variable.
# Version 1.0.0.

# Data ----
result.table <- list(
  y.name = "Knowledge Sharing", # The name of the dependent variable
  y.intercept = 4.032,          # The mean/intercept of the latent variable Intercept
  y.slope = 0.4,                # The mean/intercept of the latent variable Slope
  y.slope.loadings = c(0,1.4,2),  # The loadings for the slope factor
  waves = c(0,1,4),
  predictors = data.frame(name = c("AGE", "GENDER", "TIMEWORK", "LGO2", "High Performance HRM", "HXL", "SCH2", "SCH3", "SCH4", "SCH5", "SCH6", "TEAMSIZE", "HR Strength", "HxS"),
                          mean = c(48.556, 0.636, 5.977, 0, 0, 0.044, 0.359, 0.109, 0.125, 0.109, 0.125, 10.75, 0, 0),
                          var = c(105.173, 0.232, 3.428, 0.475, 0.274, 0.135, 0.23, 0.097, 0.109, 0.097, 0.109, 33.562, 0.199, 0),
                          beta.int = c(-0.01, 0.025, 0.009, 0.156, 0.133, -0.19, 0.127, -0.275, -0.17, 0.075, 0.227, 0.006, 0.006, 0.133),
                          beta.slp = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          x.off = c(0, 0, 0, 0, 3.324, 0, 0, 0, 0, 0, 0, 0, 2.63, 0)),
  x = 5,   # The variable influencing the latent intercept and slope
  raw.data =  data.frame(x = rnorm(200,3.5, 0.25),      # The raw data containing the points. Set to NA if you want to drop them.
                         y1 = rnorm(200,3.5, 0.25),
                         y2 = rnorm(200,3.5, 0.25),
                         y3 = rnorm(200,3.5, 0.25))
)
#result.table$wave.labels = waves # You can manually change the label to something like "Wave 1", "Wave 2" or "Wave 3"
result.table$wave.labels <- c("Wave 1\n(Jan 2020)", "Wave 2\n(Feb 2020)", "Wave 3\n(May 2020)")


# Settings----
image_width = 6  # The width of the image (in inches)
height_ratio = (1 + sqrt(5))/2 # Set the ratio to phi by default
image_height = image_width / height_ratio # The height of the image (in inches)
file_type = "svg" # The file type. It is recommended to use a vector format (e.g., "svg" or "emf")

ylim_lower <- NA #The lower limit of the Y-axis (leave as NA for it to be automatically set)
ylim_upper <- NA #The upper limit of the Y-axis (leave as NA for it to be automatically set)
xlim_lower <- min(result.table$waves) - 0.5
xlim_upper <- max(result.table$waves) + 0.5

# Determine plot file name ----
file_name <- paste0("LGM - ",
                    result.table$y.name,
                    " on ",
                    result.table$predictors$name[result.table$x],
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
  waves <- length(dataset$y.slope.loadings)
  
  m_status <- c("+1SD", "-1SD")
  
  # Determine whether each variable is the IV
  is_main <- rep(FALSE, dim(dataset$predictors)[1])
  is_main[x] <- TRUE

  # Create predicted values for the intercept and slope
  int.pred1 <- dataset$y.intercept
  int.pred2 <- dataset$y.intercept
  slp.pred1 <- dataset$y.slope
  slp.pred2 <- dataset$y.slope

  for (d in 1:dim(dataset$predictors)[1]) {
    if (d == x) {
      int.pred1 <- int.pred1 + dataset$predictors$beta.int[d] * (dataset$predictors$mean[d]+sqrt(dataset$predictors$var[x]))
      int.pred2 <- int.pred2 + dataset$predictors$beta.int[d] * (dataset$predictors$mean[d]-sqrt(dataset$predictors$var[x]))
      slp.pred1 <- slp.pred1 + dataset$predictors$beta.slp[d] * (dataset$predictors$mean[d]+sqrt(dataset$predictors$var[x]))
      slp.pred2 <- slp.pred2 + dataset$predictors$beta.slp[d] * (dataset$predictors$mean[d]-sqrt(dataset$predictors$var[x]))
    }
    else {
      int.pred1 <- int.pred1 + dataset$predictors$beta.int[d] * dataset$predictors$mean[d]
      int.pred2 <- int.pred2 + dataset$predictors$beta.int[d] * dataset$predictors$mean[d]
      slp.pred1 <- slp.pred1 + dataset$predictors$beta.slp[d] * dataset$predictors$mean[d]
      slp.pred2 <- slp.pred2 + dataset$predictors$beta.slp[d] * dataset$predictors$mean[d]
    }
  }
  
  # Create predicted points, based on the intercept and slope
  x_val <- rep(dataset$waves, 2)
  for (d in 1:waves) {
    y_val[d] <- int.pred1 + slp.pred1*dataset$y.slope.loadings[d]
    y_val[d+waves] <- int.pred2 + slp.pred2*dataset$y.slope.loadings[d]
  }
  
  cond_name <- c(rep(m_status[1],waves), rep(m_status[2],waves))
  y_name <- rep(dataset$y.name, 2*waves)

  y.data <- c()
  x.data <- c()
  
  if("raw.data" %in% names(dataset)) {
    for(d in 2:dim(dataset$raw.data)[2]) {
      x.data <- c(x.data, rep(dataset$waves[d-1], dim(dataset$raw.data)[1]))
      y.data <- c(y.data, dataset$raw.data[,d])
    }
  }

  return(list(
    xlab = "Time",
    ylab = unique(y_name)[1],
    llab = dataset$predictors$name[x],
    waves = dataset$waves,
    wave.labels = dataset$wave.labels,
    line.coor = data.frame(
      y_name = factor(y_name, levels = unique(y_name)),
      cond_name = factor(cond_name, levels = unique(cond_name)),
      x = x_val, 
      y = y_val),
    points.data = data.frame(
      #x.group = dataset$raw.data[,1] >= median(dataset$raw.data[,1]),
      x.group = factor(ifelse(dataset$raw.data[,1] < median(dataset$raw.data[,1]), "-1SD", "+1SD"), levels = c("+1SD", "-1SD")),
      x.data = x.data,
      y.data = y.data)
  )
    )
}

# Run GGPlot----
twoway2_graph <- function(graph.data) {
  plot1 <- ggplot(data = graph.data$line.coor, aes(x = x, y = y, group = cond_name))
  
  if(length(graph.data$points.data$y.data) > 0) {
    plot1 = plot1 + geom_jitter(data = graph.data$points.data, aes(x = x.data, y = y.data, group = NULL, shape = x.group), height = 0, width = 0.15, color = "grey50", fill = "grey80") +
    scale_shape_manual(values=c(21,24,15,17))
  }
    
  plot1 = plot1 +
    geom_line(aes(linetype = cond_name)) +
    geom_point(aes(shape = cond_name), size = 2.5, fill="black") +
    labs(linetype = graph.data$llab, shape = graph.data$llab) +
    xlab(graph.data$xlab) +
    ylab(graph.data$ylab) +
    scale_x_continuous(breaks = graph.data$waves, labels = graph.data$wave.labels, limits = c(xlim_lower, xlim_upper)) +
    #ylab(graph.info$ylab) + 
    theme.plain
  if (!is.na(ylim_lower) | !is.na(ylim_upper)) {plot1 = plot1 + ylim(ylim_lower, ylim_upper)}
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


