# Install and load necessary packages if they are not already installed
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("brms", quietly = TRUE)) {
  install.packages("brms")
}
if (!requireNamespace("kableExtra", quietly = TRUE)) {
  install.packages("kableExtra")
}

# Load libraries
library(leaflet)
library(sf)
library(dplyr)
library(brms)
library(kableExtra)

# Spatial dataset

# Sample locations with names, latitudes, and longitudes
locations <- data.frame(
  id = 1:5,
  name = c("Location A", "Location B", "Location C", "Location D", "Location E"),
  lat = c(37.7749, 34.0522, 40.7128, 47.6062, 51.5074),
  lon = c(-122.4194, -118.2437, -74.0060, -122.3321, -0.1278)
)

# Convert data frame to sf object
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

# Plot data with Leaflet
leaflet(data = locations_sf) %>%
  addTiles() %>%
  addMarkers(~ st_coordinates(geometry)[,1],
             ~ st_coordinates(geometry)[,2],
             popup = ~ name)

# Calculations with the sf package

# Calculate distance matrix
distances <- st_distance(locations_sf)

# Convert distance matrix to data frame
distances_df <- as.data.frame(distances)

# Add row and column names for better readability
rownames(distances_df) <- locations$name
colnames(distances_df) <- locations$name

# Print distance matrix
kable(distances_df, format = "html") %>%
  kable_styling(full_width = FALSE)

# Statistical analysis

# Fit a model with brms.

# Simulate data
set.seed(123)
N <- 100  # Number of observations
x <- rnorm(N, mean = 5, sd = 2)
y <- 2 * x + rnorm(N, mean = 0, sd = 1.5)
data <- data.frame(x = x, y = y)

# Specify the model formula
formula <- bf(y ~ x)

# Fit the model using brm
fit <- brm(formula,
           data = data,
           family = gaussian(),
           chains = 4,
           iter = 2000,
           warmup = 1000,
           cores = 4)  # Adjust chains, iter, warmup, and cores as needed

# Visualize MCMC chains
plot(fit)

# Visualize the posterior predictive check
pp_check(fit, ndraws = 100)

# Summarize model results
summary(fit)
