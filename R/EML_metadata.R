# Prepare Metadata ####
# Markus Bauer


### Packages ###
library(EML)

### Start ###
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Create infos about persons ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus", 
    surName = "Bauer"),
  electronicMailAddress = "markusbauer@mailbox.org"
)

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany")

contact <- 
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    phone = "0049-152-56391781"
    )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create site and experiment infos ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abstract <- "Coming up"

keywordSet <- list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "substrates",
      "restoration"
    )
)

geographicDescription <- "Greenhouse in Dürnast near Freising"

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



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C finalize EML ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataset <- list(
    title = "Coming up",
    creator = creator,
    pubDate = "2021",
    language = "English",
    intellectualRights = "CC BY 4.0",
    abstract = abstract,
    keywordSet = keywordSet,
    coverage = coverage,
    contact = contact
    )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
  )

setwd("Z:/Documents/0_Uni/2021_ZIM_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration")
write_eml(eml, "METADATA.xml")
eml_validate("METADATA.xml")
