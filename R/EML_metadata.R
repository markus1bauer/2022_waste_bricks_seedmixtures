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

write_eml(eml, here("METADATA.xml"))
eml_validate(here("METADATA.xml"))
