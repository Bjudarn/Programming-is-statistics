# A
B <- function(n) {
  sum <- 0
  for (k in 0:n) {
    for (j in 0:k) {
      sum <- sum + (1/(k+1)) * (-1)^j * (j+1)^n * choose(k, j)
    }
  }
  return(sum)
}

B(2)

B(10)

microbenchmark::microbenchmark(B(2), B(10))

# B
# Open necessary packages
library(gam)
library(mgcv)
# Create function that takes as inputs data, response variable, target variable and explanatory variables
Semifun <- function(data, response_variable, target_variable, explanatory_variable) {
  Y <- response_variable
  Z <- target_variable
  J <- length(explanatory_variable)
  X <- explanatory_variable
  X_matrix <- matrix(unlist(explanatory_variable), ncol = J)
  
  # Define formula
  formula1 <- Y ~ Z
  
  # Update formula
  for (i in 1:J) {
    variable_name <- paste("X_", i, sep = "")
    variable_value <- X_matrix[, i]
    assign(variable_name, variable_value)
    formula1 <- update(formula1, paste(" ~. + s(", variable_name,")"))
  }
  
  print(formula1)
  
  # gam beta estimate
  gam_gam_formula <- gam::gam(formula1)
  gam_beta_estimate <- coefficients(gam_gam_formula)[2]
  
  # mgcv beta estimate
  mgcv_gam_formula <- mgcv::gam(formula1)
  mgcv_gam_estimate <- coefficients(formula_1)[2]
  
  # Return the information that is asked for
  final_vector <- c(mgcv_gam_estimate, gam_beta_estimate, length(Y), (J + 1))
  names(final_vector) <- c("mgcv beta fit", "gam beta fit", "number of observations", "number of explanatory variables")
  return(final_vector)
}

# Install package SemiPar
install.packages("SemiPar")
library(SemiPar)

# Apply the ragweed data to test the function
data(ragweed)
Y <- ragweed$ragweed
Z <- ragweed$rain
X_variables <- list(ragweed$temperature, ragweed$wind.speed, ragweed$day.in.seas)
Semifun(ragweed, Y, Z, X_variables)

# C
# Load data
data("CO2")

# Calculate the mean
plot <- aggregate(. ~ Type + Treatment + conc, CO2[-1], mean)
plot$test <- interaction(plot$Type, plot$Treatment)

# Open ggplot2
library(ggplot2)

# Line plot
l <- ggplot(plot, aes(x = conc, y = uptake, group = factor(test)))
l1 <- l +
  labs(x = expression(paste("Ambient ", CO[2], " concentration (mL/L)")),
       y = expression(paste("Mean ", CO[2], " uptake rates (", mu, "mol/", m^2, " sec)")),
       title = expression(paste("Mean ", CO[2], " uptake curves for three plants"))) + 
  theme_light() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.9038, 0.14)) +
  geom_line(aes(color = test)) + 
  geom_point(aes(shape = test, color = test)) + 
  scale_shape_manual(values = c(1, 2, 3, 4)) + 
  scale_color_manual(values = c('springgreen4', 'slateblue3', 'brown2', 'purple4')) +
  scale_x_continuous(breaks = c(95, 175, 250, 350, 500, 675, 1000))

# Boxplot
b <- ggplot(CO2, aes(x = Type, y = uptake, fill = Type))
b1 <- b + 
  geom_boxplot(fatten = 4) + 
  theme_light() + 
  labs(x = NULL, 
       y = expression(paste(CO[2], " uptake rates (", mu, "mol/", m^2, " sec)")), 
       title = expression(paste("Boxplot for ", CO[2], " uptake in terms of plant origin"))) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Histogram
h <- ggplot(CO2, aes(x = uptake, after_stat(density)))
h1 <- h + 
  geom_histogram(bins = 9, 
                 color = "black",
                 fill = "white") +
  theme_light() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(x = NULL,
       y = "Density",
       title = expression(paste("Histogram of ", CO[2], " uptake rates"))) +
  geom_line(aes(y = ..density.., colour = "orange"), 
            stat = 'density', 
            linetype = "dashed",
            size = 3)

# Set layout
install.packages("ggpubr")
library(ggpubr)
ggarrange(l1, ggarrange(b1, h1, ncol = 2), nrow = 2)