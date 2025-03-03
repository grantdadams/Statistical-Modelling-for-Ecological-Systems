library(marmap)
library(raster)
library(stars)
library(dplyr)
library(stars)

## Based on
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0232822

## Simulate numbers ----
set.seed(487)

K = 1e6 # Carrying capacity
R = 0.2 # Growth Rate
N0 = 5e5
nyrs = 50
Rer <- rnorm(nyrs, 0, 0.3) # AR1 error

# - Schaefer model
N <- N0
Ryr <- R

for(yr in 2:nyrs){
  Ryr[yr] = R + 0.5 * Rer[yr-1] + Rer[yr]
  N[yr] <- (N[yr-1] + Ryr[yr-1] * N[yr-1] * (1 - N[yr-1]/K)) * exp(rnorm(1, 0, 0.05))
}


## Get bathymetry
# chile_bathy <- getNOAA.bathy(lon1 = -85, lon2 = -65,
#                              lat1 = -10, lat2 = -60, resolution = 10)
# chile_bathy <- as.raster(chile_bathy)
# write(chile_bathy, "Day 3/chile_bathy.Rdata")
load("chile_bathy.Rdata")
chile_bathy[chile_bathy > 0 ] <- NA

## Convert to grid
polygony <- st_make_grid(chile_bathy, square = T, what = "polygons",
                         cellsize = 1) %>%
  st_sf()

# Mean depth in grid
e <- raster::extract(chile_bathy,
                     polygony,
                     buffer = .5, #  specify a .5 degree radius
                     fun = mean, # extract the MEAN value from each plot
                     cellnumbers = TRUE,
                     sp = TRUE)

# Store values in grid
min_depth = -20
polygony <- polygony %>%
  dplyr::mutate(
    depth = e$layer, # Depth
    grid_id = 1:length(polygony$geometry), # Grid ID
    grid_id = ifelse(depth >= min_depth, NA, grid_id)
  )


## Get centroid ----
centroid <- polygony %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()

polygony <- polygony %>%
  dplyr::mutate(
    X = centroid$X, # Longitude
    X = ifelse(is.na(grid_id), NA, X),
    Y = centroid$Y, # Latitude
    Y = ifelse(is.na(grid_id), NA, Y)
  )



## Distribution as a function of depth ----
get_density = function(depth,
                       best_depth = -1000,
                       DepthSigma = 1.5){
  exp(-1 * ( log(depth/best_depth) )^2 / DepthSigma ^2)
}


## Plot local density ----
polygony <- polygony %>%
  dplyr::mutate(
    habitat_suitability = get_density(depth),
    habitat_suitability = ifelse(depth >= min_depth, NA, habitat_suitability))


## Get local abundance across years ----
HabitatSigma <- 0.1 # Random noise around habitat
local_abundance <- list()

# Loop around years
for(i in 1:nyrs){
  local_abundance[[i]] <- polygony %>%
    dplyr::mutate(
      habitat_suitability = habitat_suitability * exp(rnorm(length(habitat_suitability), 0, HabitatSigma)), # Add random noise
      RelAbund = N[i] * habitat_suitability/sum(habitat_suitability, na.rm = TRUE))

  names(local_abundance)[i] <- paste0("Year", i)
}

# - Plot
tmp <- local_abundance[[1]]
tmp$Year1 <- local_abundance[[1]]$RelAbund
tmp$Year2 <- local_abundance[[2]]$RelAbund


## Apply annual survey ----
n_hauls <- 100
q <- 0.1


# * Fixed stations ----
cells <- sample(local_abundance[[1]]$grid_id[which(!is.na(local_abundance[[1]]$grid_id))], n_hauls) # Where to sample

## Sample ----
haul_data <- data.frame(Years = NULL, Haul = NULL, CPUE = NULL)

for(i in 1:nyrs){
  for(j in 1:n_hauls){
    ind <- which(local_abundance[[i]]$grid_id == cells[j])
    haul_tmp <- data.frame(Year = i,
                           Haul = j,
                           Cell = j,
                           X = local_abundance[[i]]$X[ind],
                           Y = local_abundance[[i]]$Y[ind],
                           Depth = local_abundance[[i]]$depth[ind],
                           CPUE =
                             rbinom(n = 1,
                                    size = round(local_abundance[[i]]$RelAbund[ind]),
                                    prob = q ))
    haul_data <- rbind(haul_data, haul_tmp)

  }
}


# * Random stations ----

## Sample ----
haul_data_random <- data.frame(Years = NULL, Haul = NULL, CPUE = NULL)

for(i in 1:nyrs){
  # Where to sample (reselected each year)
  cells <- sample(local_abundance[[1]]$grid_id[which(!is.na(local_abundance[[1]]$grid_id))], n_hauls)

  for(j in 1:n_hauls){
    ind <- which(local_abundance[[i]]$grid_id == cells[j])
    haul_tmp <- data.frame(Year = i,
                           Haul = j,
                           Cell = j,
                           X = local_abundance[[i]]$X[ind],
                           Y = local_abundance[[i]]$Y[ind],
                           Depth = local_abundance[[i]]$depth[ind],
                           CPUE =
                             rbinom(n = 1,
                                    size = round(local_abundance[[i]]$RelAbund[ind]),
                                    prob = q ))
    haul_data_random <- rbind(haul_data_random, haul_tmp)

  }
}
