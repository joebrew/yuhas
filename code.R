# Point to wherever you have your private data stored
private_data <- "/home/joebrew/Documents/private_data/yuhas/"

# Point to a public repository (where this code is saved)
public <- "/home/joebrew/Documents/yuhas/"

# Point to wherever you have your zip code 
zip_data <- if(Sys.info()["user"] == "joebrew"){
  zip_data <- "/media/joebrew/JB/zipcode/"
} else{
  zip_data <- "D:/graphics/zipcode/"
}

# LOAD LIBRARIES 
library(maptools)
library(rgeos)
library(rgdal)
library(classInt)
library(RColorBrewer)
# SET WD
setwd(zip_data)

# READ IN DC ZIP CODE DATA
# (the first time, it will subset the national file and write a smaller dc_area file)
# (thereafter, it'll read in the smaller one directly)

# first, establish folder just for dc_area (in zipcode folder)
if("dc_area" %in% dir()){
  # Set wd to new directory
  setwd("dc_area")
} else {
  # if necesary, make a new directory
  dir.create(file.path(zip_data, "dc_area"), showWarnings = FALSE)
  setwd("dc_area")
}

# next, if it doesn't already exist, write a new shapefile
if("dc_area_zips.shp" %in% dir()){
  z <- readOGR(".", "dc_area_zips")
} else {
  # READ IN ZIP CODE SHAPE FILE  
  z <- readOGR(paste0(zip_data, "national_zip"),layer="tl_2013_us_zcta510")
  
  # CREATE A MORE NICELY WRITTEN ZIP CODE COLUMN
  z$zip <- as.numeric(as.character(z$ZCTA5CE10))
  
  # SUBSET ZIP TO ONLY INCLUDE OUR REGION OF INTEREST: 
#   keep_zips <- c(seq(07000, 12799, 1), # 10001
#                  seq(17500, 19699, 1))
  keep_zips <- seq(10001, 12790, 1)
  z <- z[which(z$zip %in% keep_zips),]
  
  #z <- z[which(z$zip > 10001 & z$zip < 12790),]
  
  #DC / MD / VA
#   z <- z[which(z$zip > 18000 &
#                  z$zip < 27000),]
  
  # WRITE A SMALLER ZIP CODE FILE (for next time's use)
  writeOGR(z, ".", "dc_area_zips", driver = "ESRI Shapefile")
}

# READ IN RETENTION DATA
setwd(private_data)
arch_zip <- read.csv("RetentionByZipv0.csv")
arch_zip$retention <- arch_zip$retention*100

# REMOVE NA ROW(S)
arch_zip <- arch_zip[which(!is.na(arch_zip$zip)),]

#campers.MD
z$retention <- vector(length=nrow(z), mode="numeric")
for (i in unique(z$zip)){
  z$retention[which(z$zip == i)] <-
    sum(arch_zip$retention[which(arch_zip$zip == i)])
}

######################
# MAPPING STUFF
# WRITE FUNCTION FOR PLOTTING
ben_map <- function(variable = z$retention, 
                    color = "Blues", 
                    my_title = "Archdiocese Retention by Zip", 
                    label_zips = FALSE, 
                    label_nums = FALSE,
                    border = NA,
                    border2 = "black", #border if you don't do grey background
                    grey_background = TRUE,
                    zoom = 1,
                    x_adjust = 0,
                    y_adjust = 0, 
                    label_size = 0.2,
                    add_lat_lon = FALSE){
  
  # Define some parameters based on function input
  plotvar <- variable
  nclr <- 8
  plotclr <- c("white",  brewer.pal(nclr-1, color))
   class <- classIntervals(plotvar, nclr, style = "equal", dataPrecision=0) 
#   class <- classIntervals(plotvar, nclr, style = "fixed",
#                           fixedBreaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80))
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), " ")
  
 
  # ESTABLISH ZOOM LEVEL AND HORIZONTAL/ VERTICAL ADJUSTMENT
  x_limits <- as.numeric(bbox(z)[1,]) 
  y_limits <- as.numeric(bbox(z)[2,])
  
  x_range <- max(x_limits) - min(x_limits)
  y_range <- max(y_limits) - min(y_limits)
  
  x_limits <- c((mean(x_limits) - (x_range * zoom)),
                (mean(x_limits) + (x_range * zoom)))
  y_limits <- c((mean(y_limits) - (y_range * zoom)),
                (mean(y_limits) + (y_range * zoom)))
  
  x_limits <- x_limits + x_adjust
  y_limits <- y_limits + y_adjust
  
  # PLOT
  if(grey_background){
    # Plot just the backdrop (a bunch of grey counties)
    my_cols <- colorRampPalette(c("lightgrey", "darkgrey"))(nrow(z))
    plot(z, col = sample(my_cols, nrow(z)), border = border,
         xlim = x_limits,
         ylim = y_limits)
    # subset z to just include rows with data
    z2 <- z[which(z$retention > 0),]
    colcode2 <- colcode[which(z$retention > 0)]
    #plot(z)
    plot(z2, border=border, col=colcode2, add = TRUE)
  } else {
    plot(z,
         xlim = x_limits,
         ylim = y_limits)
    plot(z, col = colcode, border = border2, add = T)
  }

  # Add a box to the map
  box("plot")
  
  # Optionally label zip codes 
  if(label_zips){
    text(x=coordinates(z)[,1][which(variable > 0)],
         y=coordinates(z)[,2][which(variable > 0)],
         labels=z$zip[which(variable > 0)],
         cex=label_size,
         col=adjustcolor("black", alpha.f=0.3))
  }
  
  # Optionally label the values
  if(label_nums){
    text(x=coordinates(z)[,1][which(variable > 0)],
         y=coordinates(z)[,2][which(variable > 0)]-0.02,
         labels=round(variable)[which(variable > 0)],
         cex=label_size,
         col=adjustcolor("darkred", alpha.f=0.5))
  }
  
  # Add lat/lon lines
  if(add_lat_lon){
    abline(h = seq(-180, 180, 1),
           v = seq(-180, 180, 1),
           col = adjustcolor("black", alpha.f = 0.2))
  }

  
  # Add a legend
  legend("bottomleft", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=border,
         #bty = "n",
         bg="white")
  
  # Add a title
  title(main=my_title)  
}


# EXAMPLES OF USING THE FUNCTION

# default
ben_map()

# zoomed-in
ben_map(zoom = 0.2)

# zoomed-in, but shifted more to the right and downwards
ben_map(zoom = 0.3,
        x_adjust = -0.5,
        y_adjust = -0.7)

# zoomed-in, but shifted more to the right and downwards,
# and with labeled values and zipcodes
ben_map(zoom = 0.3,
        x_adjust = -0.5,
        y_adjust = -0.7,
        label_nums = TRUE,
        label_zips = TRUE)

# zoomed-in, but shifted more to the right and downwards,
# and with labeled values and zipcodes, and blank background
ben_map(zoom = 0.3,
        x_adjust = -0.5,
        y_adjust = -0.7,
        label_nums = TRUE,
        label_zips = TRUE,
        grey_background = FALSE)

# Save a pdf with the last 2 maps
pdf(paste0(public, "my_map.pdf"),
           height = 8.5,
           width = 14)
# zoomed-in, but shifted more to the right and downwards,
# and with labeled values and zipcodes
ben_map(zoom = 0.3,
        x_adjust = -0.5,
        y_adjust = -0.7,
        label_nums = TRUE,
        label_zips = TRUE)

# zoomed-in, but shifted more to the right and downwards,
# and with labeled values and zipcodes, and blank background
ben_map(zoom = 0.3,
        x_adjust = -0.5,
        y_adjust = -0.7,
        label_nums = TRUE,
        label_zips = TRUE,
        grey_background = FALSE)
dev.off()


#####
# ZIP CODE CENTROIDS
#####

# Plot our shapefile
plot(z, col = "grey", border = NA)

# Add the centroid points
points(coordinates(z))


# Make size and color vectors
# which are functions of retention

# Color will be a spectrum of 100 colors: red = bad retention, blue = high retention
color_vector <- adjustcolor(colorRampPalette(c("red", "yellow", "blue"))(ceiling(max(z$retention))), alpha.f = 0.5)
colors <- color_vector[ceiling(z$retention)]

# Size will be spectrum of 100 sizes = big = low retention, small = high retention
size_vector <- (ceiling(max(z$retention)):1)/40
sizes <- size_vector[ceiling(z$retention)]

# Replot our shapefile
plot(z, col = "grey", border = NA)

# Add points, using our sizes and colors
points(coordinates(z),
       cex = sizes,
       col = colors,
       pch = 16)

# Add legend
legend_seq <- c(10, 25, 50, 75, 90)
legend("topright",
       pt.cex = legend_seq/40,
       legend = legend_seq,
       title = "Retention rate",
       col = color_vector[legend_seq],
       pch = 16)



####
# GET POINTS OF NEW SCHOOLS
####
# this is the normal map from before,  works perfectly
ben_map(zoom = 0.08,
        x_adjust = -0.5,
        y_adjust = -1.4)


#* here I read in a new file with zip and a location flag

charter_zip <- read.csv("Charter Campus Addresses.csv")

# REMOVE NA ROW(S)
charter_zip <- charter_zip[which(!is.na(charter_zip$Zip)),]
charter_zip <- charter_zip[which((charter_zip$Location>0)),]

# here I am on rocky ground, as I am trying to reuse the approach you used to map retention to z

z$location <- vector(length=nrow(z), mode="numeric")
for (i in unique(z$zip)){
  z$location[which(z$zip == i)] <-
    sum(charter_zip$Location[which(charter_zip$Zip == i)])
}


#* when I plot this, it looks good EXCEPT I seem to be putting a dot every place a zip code exists in z, not just the subset of those with charter_zip$Location > 0

points(coordinates(z),
       cex = .5,
       col = "red",
       pch = 16)