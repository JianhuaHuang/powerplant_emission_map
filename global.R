library(leaflet)
library(ggplot2)
library(maps)
library(shiny)

# data: all powerplants (pows)
# add goMap links to the data. This code should be only run once, and then save
# the results as *rda, which can be loaded much faster. 
# setwd('C:/Users/jianhua/Dropbox/test/shiny/powerplants')
# pows <- read.csv('C:/Users/temp/Dropbox/test/shiny/powerplants/eGRID2012V1_0_year09_DATA.csv')
# Go <- apply(pows, 1, function(x) paste0(tags$button("goMap", onclick = paste0("return (loc = ", x["ORISPL"], ")"))))
# pows$Go <- Go
# save(pows, file='C:/Users/temp/Dropbox/test/shiny/powerplants/eGRID2012V1_0_year09_DATA.rda')

load('eGRID2012V1_0_year09_DATA.rda')

# match the fuel category with displayed name and point color in the map
categ <- data.frame(
  PLFUELCT = c("COAL", "OFSL", "OIL", "GAS", ""),
  FUELPOPUP = c('Coal', 'Oil/Gas/Other fossil fuel', 'Oil/Gas/Other fossil fuel', 'Oil/Gas/Other fossil fuel', 'Renewable'),
  COLORS = c('#d7191c', '#fdae61', '#fdae61', '#fdae61', '#1f78b4')
)
power.categ <- categ[match(pows$PLFUELCT, categ$PLFUELCT), 2:3] 

# select the columns we want to display
# AN means annual emission, RTA means annual emission rate (lb/MWh) or (lb/GWh)
# all HG values are NA, so exclude the emission of HG
phy.info <- c('PNAME', 'ORISPL', 'PSTATABB', 'CNTYNAME', 'LAT', 'LON', 'PLPRMFL', 'PLNGENAN')
em <- c('PLNOXAN', 'PLSO2AN', 'PLCO2AN', 'PLCH4AN', 'PLN2OAN')
emr <- c('PLNOXRTA', 'PLSO2RTA', 'PLCO2RTA', 'PLCH4RTA', 'PLN2ORTA')

pows <- pows[,c(phy.info, em, emr, 'Go')]

# the state coordinates, which will be jumped to once the state is selected
# var.states is the choice of selectInput
state.coord <- read.csv('state_latlon.csv', stringsAsFactors = F)
state.abbr <- read.csv('states.abbr.csv', stringsAsFactors = F)
states <- merge(state.abbr, state.coord, by.x = 'Abbreviation', by.y = 'state')

var.states <- as.character(states$Abbreviation)
names(var.states) <- states$State

var.em <- em
names(var.em) <- c('NOX', 'SO2', 'CO2', 'CH4', 'N2O')
