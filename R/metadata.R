# Brick-based substrates and designed seedmixtures
# Prepare metadata ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.



### Packages ###
library(here)
library(tidyverse)
library(EML)
library(emld)
#remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

### Start ###
rm(list = ls())
setwd(here())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Collect metadata ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Methods and units #####################################################################################

#methods_file <- here("data/text/methods.odt")
#methods <- set_methods(methods_file)

#EMLassemblyline::view_unit_dictionary() # List of standard units, which should be used in metadata file


### 2 Raw data #####################################################################################

### a data_raw_species  -------------------------------------------------------------------------------------------
attributes <- read_csv(here("data/raw/data_raw_species_metadata.csv"))
physical_raw_species <- set_physical("data_raw_species.csv")

### b data_raw_traits  -------------------------------------------------------------------------------------------
attributes <- read_csv(here("data/raw/data_raw_traits_metadata.csv")) %>%
  select(-type, -factor)
physical_raw_traits <- set_physical("data_raw_traits.csv")

### c data_raw_sites  -------------------------------------------------------------------------------------------
attributes <- read_csv(here("data/raw/data_raw_sites_metadata.csv")) %>%
  select(-type, -factor)
physical_raw_sites <- set_physical("data_raw_sites.csv")

### 3 Processed data #####################################################################################


### 4 Put data table together #####################################################################################

dataTable <- list(
  list(
    entityName = "data_raw_species.csv",
    #entityDescription = "raw species abundances",
    #physical = physical_raw_species,
    #attributeList = attributeList_raw_species
  ),
  list(
    entityName = "data_raw_traits.csv",
    #entityDescription = "raw plant trait list",
    #physical = physical_raw_traits,
    #attributeList = attributeList_raw_traits
  ),
  list(
    entityName = "data_raw_sites.csv",
    #entityDescription = "environmental raw data of the sites",
    #physical = physical_raw_sites,
    #attributeList = attributeList_raw_sites
  ))


### 5 Contact #####################################################################################

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany")

creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus", 
    surName = "Bauer"
  ),
  positionName = "PhD student",
  organizationName = "Technical University of Munich",
  address = address,
  electronicMailAddress = "markusbauer@mailbox.org",
  phone = "0049-152-56391781",
  id = "https://orcid.org/0000-0001-5372-4174"
)

associatedParty <- list(
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Martin", 
      surName = "Krause"
    ),
    role = "Researcher",
    electronicMailAddress = "mek187@hotmail.de"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Valentin", 
      surName = "Heizinger"
    ),
    role = "Researcher",
    electronicMailAddress = "valentin.heizinger@leipfinger-bader.de"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Johannes", 
      surName = "Kollmann"
    ),
    role = "Professor",
    organizationName = "Technical University of Munich",
    address = address,
    electronicMailAddress = "johannes.kollmann@tum.de",
    phone = "0049-8161-714144",
    id = "https://orcid.org/0000-0002-4990-3636"
  )
)

contact <- 
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    onlineUrl = "DOI address to the database"
  )


### 6 Temporal and spatial coverage #####################################################################################

geographicDescription <- "Greenhouse in Duernast near Freising"

coverage <- set_coverage(
  
  begin = "2018-11-01", end = "2020-07-31",
  
  sci_names = list(list(
    Kingdom = "Plantae",
    Division = "Tracheophyta",
    Subdivision = "Spermatophytina"
  )),
  
  geographicDescription = geographicDescription,
  west = 11.69114 , east = 11.69114,
  north = 48.40577, south = 48.40577,
  altitudeMin = 481, altitudeMaximum = 481,
  altitudeUnits = "meter"
)


### 7 Description #####################################################################################

pubDate = "2022"

title = "Data of 'Using crushed waste bricks for urban greening: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures' by Bauer, Kruse, Heizinger and Kollmann"

abstract <- "This dataset contains data from greenhouse experiments testing the effects of brick quantity and quality, acid pre-treatment, soil type and soil moisture on the biomass of designed seed mixtures. The substrates consisted of different brick ratios (5% vs. 30%), brick types (clean production waste vs. demolition material), and brick treatments (acid vs. control) and were tested with three trait-based mixtures and one non-regional standard mixture. The trait-based mixtures were developed based on native species pool and the functional plant traits specific leaf area, seed mass and grass-to-legume ratio."

keywordSet <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list("droughts",
                   "grasslands",
                   "restoration",
                   "substrates")
  ),
  list(
    keywordThesaurus = "own vocabulary",
    keyword = list("functional traits",
                   "recycled aggregates",
                   "seed mixtures")
  )
)

intellectualRights <- "CC-BY-4.0: https://creativecommons.org/licenses/by/4.0/deed.en"



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B finalize EML ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataset <- list(
  title = title,
  pubDate = pubDate,
  creator = creator,
  associatedParty = associatedParty,
  intellectualRights = intellectualRights,
  abstract = abstract,
  keywordSet = keywordSet,
  coverage = coverage,
  contact = contact#,
  #methods = methods,
  #dataTable = dataTable,
  #additonalMetadata = list(metadata = list(unitList = unitList))
)

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
)

write_eml(eml, here("METADATA.xml"))
eml_validate(here("METADATA.xml"))

emldown::render_eml(here("METADATA.xml"), open = T, outfile = here("METADATA.html"), publish_mode = F)

