library(avilistr)
library(rgbif)
library(taxize)

data(avilist_2025) # Complete dataset (26 fields)
data(avilist_2025_short) # Essential fields (~12 fields)
data(avilist_metadata) # Field descriptions


names <- c(df_clean$taxa1_scientific, df_clean$taxa2_scientific) |> unique()
harmon <- data.frame(amn_name = names)
temp <- lapply(harmon$amn_name, rgbif::name_backbone)
n_gbif <- bind_rows(temp)

temp2 <- rgbif::name_backbone_checklist(harmon$amn_name)

temp2 <- lapply(harmon$amn_name, taxize::get_gbifid, class = "Aves")

harmon$higher_rank <- grepl(" sp.", harmon$amn_name, fixed = T)


source("./R/auxiliary_scripts/aux_scrape_avibase.R")

# Download US+CA species checklist
# cac = "https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=avilist&synlang=&region=NA1&version=text&lifelist=&highlight=0"
cac = "C:/Users/kelly/Downloads/Avibase bird checklist - North America (US+CA).html"
her = "C:/Users/kelly/Downloads/Avibase bird checklist - Heredia.html"
nea = "C:/Users/kelly/Downloads/Avibase bird checklist - Nearctic.html"
sam = "C:/Users/kelly/Downloads/Avibase bird checklist - South America.html"
cam = "C:/Users/kelly/Downloads/Avibase bird checklist - Central America.html"

cac <- read_avibase(region_code = "North America (US + CA)", url = cac) |>
  rename(status_canada_ak_conus = status) |>
  select(-order, -region) |>
  mutate(canada_ak_conus = TRUE)
her <- read_avibase(region_code = "Heredia", url = her) |>
  rename(status_heredia = status) |>
  select(-order, -region) |>
  mutate(heredia = TRUE)
nea <- read_avibase(region_code = "Nearctic", url = nea) |>
  rename(status_nearctic = status) |>
  select(-order, -region) |>
  mutate(nearctic = TRUE)
sam <- read_avibase(region_code = "South America", url = sam) |>
  rename(status_south_america = status) |>
  select(-order, -region) |>
  mutate(south_america = TRUE)
cam <- read_avibase(region_code = "Central America", url = cam) |>
  rename(status_central_america = status) |>
  select(-order, -region) |>
  mutate(central_america = TRUE)

all(cac$common_name %in% avilist_2025$English_name_AviList)
all(her$common_name %in% avilist_2025$English_name_AviList)
all(nea$common_name %in% avilist_2025$English_name_AviList)
all(sam$common_name %in% avilist_2025$English_name_AviList)
all(cam$common_name %in% avilist_2025$English_name_AviList)

all(cac$scientific_name %in% avilist_2025$Scientific_name)
all(her$scientific_name %in% avilist_2025$Scientific_name)
all(nea$scientific_name %in% avilist_2025$Scientific_name)
all(sam$scientific_name %in% avilist_2025$Scientific_name)
all(cam$scientific_name %in% avilist_2025$Scientific_name)

data(avilist_2025)

left_join_avilist <- function(avilist, df) {
  left_join(
    avilist,
    df,
    join_by(
      English_name_AviList == common_name,
      Scientific_name == scientific_name,
      Family == family
    )
  )
}

avilist_with_regions <- left_join_avilist(avilist_2025, nea) |>
  left_join_avilist(cac) |>
  left_join_avilist(cam) |>
  left_join_avilist(her) |>
  left_join_avilist(sam)

avilist_with_regions$nearctic[is.na(avilist_with_regions$nearctic)] <- FALSE
avilist_with_regions$canada_ak_conus[is.na(
  avilist_with_regions$canada_ak_conus
)] <- FALSE
avilist_with_regions$central_america[is.na(
  avilist_with_regions$central_america
)] <- FALSE
avilist_with_regions$heredia[is.na(avilist_with_regions$heredia)] <- FALSE
avilist_with_regions$south_america[is.na(
  avilist_with_regions$south_america
)] <- FALSE

sum(avilist_with_regions$nearctic, na.rm = TRUE) == length(nea$common_name)
sum(avilist_with_regions$canada_ak_conus, na.rm = TRUE) ==
  length(cac$common_name)
sum(avilist_with_regions$central_america, na.rm = TRUE) ==
  length(cam$common_name)
sum(avilist_with_regions$heredia, na.rm = TRUE) == length(her$common_name)
sum(avilist_with_regions$south_america, na.rm = TRUE) == length(sam$common_name)
