# An R macro for creating a plot of a boxplot, which is automatically saved as an SVG file
# It shows the line of best fit as well as higher-order polynomials. It shows the confidence interval and data points.
# If x or y is a latent variable, use the mean of the intercepts as the x offset (x.off)

# Settings----
image_width = 6  # The width of the image (in inches)
height_ratio = (1 + sqrt(5))/2 # Set the ratio to phi by default
image_height = image_width / height_ratio # The height of the image (in inches)
file_name = "Boxplot.svg" # The file type. It is recommended to use a vector format (e.g., "svg" or "emf") or high-quality image (e.g., "png")

load.package <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

load.package("ggplot2")

dataset <- read.csv("boxplot.csv", header=TRUE)

boxp <- ggplot(dataset, aes(x = as.factor(gender), y = score, fill = as.factor(music))) +
  geom_boxplot(outlier.color = "NA") +
  geom_point(position=position_jitterdodge(jitter.width = 0.1)) + 
  labs(x = "Gender", y = "Score", fill = "Music Condition") +
  scale_fill_manual(labels = c("No Music", "Music"), values = c("grey85", "grey50")) + 
  scale_x_discrete(labels = c("Male", "Female")) +
  
  theme( # My APA style theme
    axis.line.x = element_line(colour = "black"), # Adding a black x-axis
    axis.title.x = element_text(size = 12, margin = margin(t = 10)), # Add in x-title, make it bold, with a margin of 10pt
    axis.text.x = element_text(size = 12),
    axis.line.y = element_line(colour = "black"),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)), # Add in y axis
    axis.text.y = element_text(size = 12),
    text = element_text(family="sans"),                 # Set the font (options are: "mono", "serif", "sans"
    panel.grid.major = element_blank(),                 # Delete major grid
    panel.grid.minor = element_blank(),                 # Delete minor grid
    panel.border = element_blank(),                     # Delete border
    panel.background = element_blank(), # Delete the background
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key = element_blank(), # Delete the background for the legend key
    legend.position = "top" #c(0.9, 0.8) # Or can be set to "top", "right", "left", "bottom"
  ) 

# Generate the plot
image <- boxp

# Save the image
ggsave(filename = file_name, 
       plot = image, 
       units = "in", 
       width = image_width, 
       height = image_height)