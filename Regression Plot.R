# An R macro for creating a plot of a regression, which is automatically saved as an SVG file
# It shows the line of best fit as well as higher-order polynomials. It shows the confidence interval and data points.
# If x or y is a latent variable, use the mean of the intercepts as the x offset (x.off)

# Data ----
# This list contains the results from the regression model (probably obtained in another program)
dataset <- list(
  y.name = "Knowledge Sharing", # The name of the dependent variable
  y.intercept = 4.032,          # The intercept of the dependent variable
  y.resvar = 2,                  # The residual variance / mean squared error of Y
  predictors = data.frame(name = c("AGE", "GENDER", "TIMEWORK", "LGO2", "High Performance HRM", "HXL", "SCH2", "SCH3", "SCH4", "SCH5", "SCH6", "TEAMSIZE", "HR Strength", "HxS", "HxH"),
                          mean = c(48.556, 0.636, 5.977, 0, 0, 0.044, 0.359, 0.109, 0.125, 0.109, 0.125, 10.75, 0, 0, 0),
                          var = c(105.173, 0.232, 3.428, 0.475, 0.274, 0.135, 0.23, 0.097, 0.109, 0.097, 0.109, 33.562, 0.199, 0, 0),
                          beta = c(-0.01, 0.025, 0.009, 0.156, 0.133, -0.19, 0.127, -0.275, -0.17, 0.075, 0.227, 0.006, 0.006, 0.133, 0.1),
                          x.off = c(0, 0, 0, 0, 3.324, 0, 0, 0, 0, 0, 0, 0, 2.63, 0, 0)),
  x = c(5, 15),  # The X variables. Each entry represents a different order of polynomial. The first entry is X, the second is X^2, the third is X^3 and so on.
  n = 200    # The sample size
)

# This data frame contains the actual X and Y data points
point_data <- data.frame(
  x = c(-1.5, -1.75, -1, 1, 1.75, -2, 2),
  y = c(-0.7, -0.3, -0.4, -0.3, -0.5, -0.7, 0)
)

# Settings----
image_width = 6  # The width of the image (in inches)
height_ratio = (1 + sqrt(5))/2 # Set the ratio to phi by default
image_height = image_width / height_ratio # The height of the image (in inches)
file_type = "svg" # The file type. It is recommended to use a vector format (e.g., "svg" or "emf") or high-quality image (e.g., "png")

ylim_lower <- NA #The lower limit of the Y-axis (leave as NA for it to be automatically set)
ylim_upper <- NA #The upper limit of the Y-axis (leave as NA for it to be automatically set)
xlim_lower <- NA #The lower limit of the X-axis (leave as NA for it to be automatically set)
xlim_upper <- NA #The upper limit of the Y-axis (leave as NA for it to be automatically set)

x_lower <- min(point_data$x) # The lower limit of the line of best fit on the X axis (defaults to smallest X value)
x_upper <- max(point_data$x) # The upper limit of the line of best fit on the X axis (defaults to largest X value)
points <- 20 # The number of points on the plot line (more means better resolution)
cinterval <- 0.95 # The confidence interval

# Determine plot file name ----
file_name <- paste0(dataset$y.name,
                    " on ",
                    dataset$predictors$name[dataset$x[1]],
                    ".",
                    file_type)# The name of the file to be created

# Offset the points data (if required)
point_data$x <- point_data$x + dataset$predictors$x.off[dataset$x[1]]

# Load Packages----
load.package <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

load.package ("ggplot2")
load.package ("lemon")

# Functions----
# Confidence / Prediction interval
# Set "pred_interval" to FALSE for a confidence interval, TRUE for a prediction interval
confidence.interval <- function(dataset, x_lower = 1, x_upper = 5, points = 20, cinterval = .975, pred_interval = FALSE) {
  x <- c()
  ci <- c()
  b <- dataset$x[1]
  for (a in 0:(points-1)) {
    x[a+1] <- x_lower + a*(x_upper - x_lower)/(points-1)
    ci[a+1] <- qt(cinterval, dataset$n)*sqrt(dataset$y.resvar*(pred_interval + (1/dataset$n)+(x[a+1]-dataset$predictors$mean[b])^2/(dataset$n-2)*dataset$predictors$var[b]))
  }
  return(ci)
}

# Returns the line of prediction, plus confidence intervals
predict.data <- function(dataset, x_lower = 1, x_upper = 5, points = 20, cinterval = .975) {
  x <- c()
  y_pred <- c()
  for (a in 0:(points-1)) {
    x[a+1] <- x_lower + a*(x_upper - x_lower)/(points-1)
    y_pred[a+1] <- 0
    for (b in 1:length(dataset$predictors$name)) {
      for (c in 1:length(dataset$x)) {
        if (b == dataset$x[c]) {
          y_pred[a+1] <- y_pred[a+1] + dataset$predictors$beta[b]*x[a+1]^c
        }
        else {
          y_pred[a+1] <- y_pred[a+1] + dataset$predictors$beta[b]*dataset$predictors$mean[b]
        }
      }
    }
  }
  ci = confidence.interval(dataset, x_lower, x_upper, points, cinterval)
  return(data.frame(x = x + dataset$predictors$x.off[dataset$x[1]], y_pred = y_pred, y_lower = y_pred - ci, y_upper = y_pred + ci))
}

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


# Create the GGPlot----
regression_plot <- function(plot.data, point.data = NA) {
  plot1 <- ggplot()

  if (exists("point.data")) {
    plot1 = plot1 + geom_point(data = point.data, aes(x = x, y = y))
  }
    
  plot1 = plot1 + 
    geom_ribbon(data = plot.data, aes(x = x, ymin = y_lower, ymax = y_upper), fill = "grey95", alpha = 0.5) + 
    geom_smooth(data = plot.data, aes(x = x, y = y_lower), size = 0.5, color = "black", linetype = "dashed", se = FALSE, method = "loess", formula = "y ~ x") +
    geom_smooth(data = plot.data, aes(x = x, y = y_upper), size = 0.5, color = "black", linetype = "dashed", se = FALSE, method = "loess", formula = "y ~ x")
  
  plot1 = plot1 + 
    geom_smooth(data = plot.data, aes(x = x, y = y_pred), size = 0.75, color = "black", se = FALSE, method = "loess", formula = "y ~ x") +
    xlab(dataset$predictors$name[dataset$x[1]]) +
    ylab(dataset$y.name) +
    theme.plain

  if (!is.na(ylim_lower) | !is.na(ylim_upper)) {plot1 = plot1 + ylim(ylim_lower, ylim_upper)}
  if (!is.na(xlim_lower) | !is.na(xlim_upper)) {plot1 = plot1 + xlim(xlim_lower, xlim_upper)}
  print(plot1)
}    

# Generate the prediction line and the confidence/prediction intervals
dataset_predict = predict.data(dataset, x_lower, x_upper, points, 1-((1-cinterval)/2))

# Generate the plot
image <- regression_plot(dataset_predict, point_data)

# Save the image
ggsave(filename = file_name, 
       plot = image, 
       units = "in", 
       width = image_width, 
       height = image_height)
