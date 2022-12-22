# Fetch the most updated dataset of aquaculture production
fao_aquaculture <- function() {
  current_year <- as.numeric(str_split(string = Sys.Date(), pattern = "-")[[1]][1])
  last_release <- paste0("https://www.fao.org/fishery/static/Data/Aquaculture_", current_year, ".1.1.zip")
  check_urlzip <- HEAD(last_release)

  if (check_urlzip$status_code == 200) {
    download.file(url = last_release, destfile = file.path(tempdir(), "fao_aquaculture.zip"))
  } else {
    last_release <- paste0("https://www.fao.org/fishery/static/Data/Aquaculture_", current_year - 1, ".1.1.zip")
    download.file(url = last_release, destfile = file.path(tempdir(), "fao_aquaculture.zip"))
  }


  quant_data <- read_delim(unzip(file.path(tempdir(), "fao_aquaculture.zip"), "Aquaculture_Quantity.csv", exdir = tempdir(), overwrite = TRUE)) %>%
    select(COUNTRY.UN_CODE, SPECIES.ALPHA_3_CODE, PERIOD, VALUE) %>%
    rename("CTCODE" = COUNTRY.UN_CODE, "SPCODE" = SPECIES.ALPHA_3_CODE, "YEAR" = PERIOD, "QUANT" = VALUE)

  country_data <- read_delim(unzip(file.path(tempdir(), "fao_aquaculture.zip"), "CL_FI_COUNTRY_GROUPS.csv", exdir = tempdir(), overwrite = TRUE)) %>%
    select(UN_Code, Name_En) %>%
    rename("CTCODE" = UN_Code, "CTNAME" = Name_En)

  organisms_data <- read_delim(unzip(file.path(tempdir(), "fao_aquaculture.zip"), "CL_FI_SPECIES_GROUPS.csv", exdir = tempdir(), overwrite = TRUE)) %>%
    select(`3A_Code`, Name_En, Scientific_Name, Major_Group_En, ISSCAAP_Group_En) %>%
    rename("SPCODE" = `3A_Code`, "SPCNAME" = Name_En, "SPSNAME" = Scientific_Name , "SPGROUP" = Major_Group_En, "SPSTAT" = ISSCAAP_Group_En)

  aquaculte_production <- inner_join(quant_data, country_data) %>%
    inner_join(organisms_data) %>%
    select(CTNAME, SPCNAME, SPSNAME, SPGROUP, SPSTAT, YEAR, QUANT)

  return(aquaculte_production)
}
