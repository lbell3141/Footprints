library(raster)

# Open files
files = list.files('C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/Spatial_Regression', full.names = TRUE)
twi.original = raster(files[1])
rap.original = stack(files[2])
# Reproject RAP to match TWI coordinate reference system/gridcell resolution using nearest neighbor pixel selection ("ngb")
rap.reprojected = projectRaster(from = rap.original, to = twi.original, method = 'ngb')

# Mask and trim TWI so that the total number of pixels matches RAP
twi = trim(mask(twi.original, rap.reprojected[[1]]))

# Trim reprojected RAP data (exludes all NA white space)
rap = trim(rap.reprojected)
plot(rap)

#Filter rasters by wind direction 
