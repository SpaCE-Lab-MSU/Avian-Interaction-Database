library(jsonlite)
library(auk)
library(dplyr)
library(magick)

avi <- fromJSON("./website/avicommons_full.json")
clem <- read.csv("./website/clemtax_2025.csv")


# ---- Retrieving info from Avicommons using scientific name ----

get_avicommons_image <- function(sci_name, size = 480) {

  #Try auk::ebird_species()
  ebird_code <- tryCatch(
    ebird_species(sci_name, type = "code"),
    error = function(e) NA
  )

  #If that fails, try Clements CSV
  if (is.na(ebird_code) || length(ebird_code) == 0) {

    #Match case-insensitive
    clem_row <- clem %>%
      filter(tolower(scientific.name) == tolower(sci_name))

    if (nrow(clem_row) == 0) {
      stop("Species not found in eBird or Clements.")
    }

    ebird_code <- clem_row$species_code[1]
  }

  #Look for that species in Avicommons
  row <- avi %>% filter(code == ebird_code)

  if (nrow(row) == 0) {
    stop("Species code found, but no Avicommons image available.")
  }

  #Build the URL
  url <- paste0(
    "https://static.avicommons.org/",
    row$code, "-", row$key, "-", size, ".jpg"
  )

  list(
    ebird_code       = row$code,
    common_name      = row$name,
    scientific_name  = sci_name,
    url              = url,
    photographer     = row$by,
    license          = row$license
  )
}


# ---- Plotting in R ----

plot_avicommons_image <- function(img_info) {

  #Extract fields from the list
  url   <- img_info$url
  sci   <- img_info$scientific_name
  com   <- img_info$common_name
  code  <- img_info$ebird_code
  by    <- img_info$photographer
  lic   <- img_info$license

  #Download / read image
  img <- image_read(url)

  #Turn off margins and axes
  op <- par(mar = c(4, 0, 0, 0)) #Bottom margin for text
  on.exit(par(op))

  plot.new()
  #Fill entire plot
  rasterImage(as.raster(img), 0, 0.2, 1, 1)

  ##Label text under the image

  #Common name
  text(
    x = 0.5, y = 0.16,
    labels = bquote(bold(.(com))),
    cex = 1.3
  )

  #Scientific name
  text(
    x = 0.5, y = 0.11,
    labels = bquote(italic(.(sci))),
    cex = 1.1
  )

  #Attribution
  text(
    x = 0.5, y = 0.05,
    labels = paste0("Photo: ", by, " | ", toupper(lic)),
    cex = 0.8
  )
}

# ---- TESTING ----
test_info <- get_avicommons_image("Eumyias albicaudatus", size = 900)
plot_avicommons_image(test_info)
