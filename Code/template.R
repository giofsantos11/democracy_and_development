rm(list = ls())

# Before doing anything, make sure you have a directory for the folder location that contains
# the p5v2018.csv file

# Step 1: Assign folder location containing p5v2018.csv file
dataloc <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Democracy and Development/Datasets/Raw Data"

# Step 2: Assign folder location where outputs will be stored
outputloc <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Democracy and Development/Datasets/Processed Data"

# Step 3: Create column for assigned regions
## Change to your regions: options are "Africa", "Europe and North America", "Middle East and Central Asia",
## "Latin America", "Oceania" and "East and South Asia"
assignedregions <- c("Africa", "Oceania")

#--------------------------------------------------------------------------------------------
# No need to do anything beyond this point
#--------------------------------------------------------------------------------------------
# Library

## Install the packages
if(!require("dplyr")) install.packages("dplyr")
if(!require("ggplot2")) install.packages("ggplot2") 
if(!require("patchwork")) install.packages("patchwork") 
if(!require("hrbrthemes")) install.packages("hrbrthemes") 
if(!require("pwt10")) install.packages("pwt10") 
if(!require("countrycode")) install.packages("countrycode")

# Keep only Sub-Saharan Africa and Oceania
library(dplyr) #for dataset manipulation
library(ggplot2) #for creating graphs
library(patchwork) #for graphs with 2 y axes
library(hrbrthemes) #similar to the above
library(pwt10) #for the Penn World Tables -- load "pwt10.01"
library(countrycode) # for the country codes
data("pwt10.01") #be sure to left click "pwt10.01" in the values section in R
polity <- read.csv(paste0(dataloc, "/p5v2018.csv"))

#creating a subset of the datasets to build the main dataset
df_penn_world <- pwt10.01%>% dplyr::select(country, isocode, year, rgdpna, rgdpe, rgdpo, pop)
df_polity <- polity%>%dplyr::select(country, year, polity, polity2, p5, democ, autoc)

# Create a new variable 'iso' containing ISO codes for each country
df_polity$isocode <- countrycode(sourcevar = df_polity$country,
                                 origin = "country.name",
                                 destination = "iso3c")

# For documentation purposes, here are the countries without ISO codes
# Baden, Bavaria, Czechoslovakia, Germany East, Kosovo, Modena, Orange Free State, Parma, Prussia, Sardinia, # Saxony, Serbia and Montenegro, South Vietnam, Tuscany, Two Sicilies, United Province CA, Wuerttemburg, Yemen # North, Yemen South, Yugoslavia

#merging the following datasets
df_merged <- merge(df_penn_world, df_polity, by=c("isocode", "year"))
#not sure about this -- see the polity handbook for what -11, -66, and -88 mean
df_merged <- df_merged %>% filter(polity>-11)
#Add regions for each country in the dataset
df_merged$region <- countrycode(sourcevar = df_merged$country.x,
                                origin = "country.name",
                                destination = "region")
df_merged$isocode <- as.character(df_merged$isocode)
str(df_merged$isocode)

#adding a basic per capita gdp metric
df_merged$per_cap_gdp <- (df_merged$rgdpna*1000000)/(df_merged$pop*1000000)

# Capture countries per D&D classification
east_south_asia <- c("BGD", "BTN", "KHM", "CHN", "IND", "IDN", "JPN", "KOR", "LAO", "MYS", "NPL", "PAK", "PHL", "SGP", "LKA", "TWN", "THA", "VNM")

oceania <- c("AUS", "FJI", "NZL", "PNG", "SLB")

# Note the potential misclassification -- EGY is Middle East and Central Asia in the DD charts
middleeast_ctrasia <- c("AFG", "ARM", "AZE", "BHR", "EGY", "GEO", "IRN", "IRQ", "ISR", "JOR", "KAZ", "KWT", "KGZ", "LBN", "MNG", "OMN", "QAT", "SAU", "SYR", "TJK", "TKM", "ARE", "UZB", "YEM")

europe_na <- c("ALB", "AUT", "BLR", "BEL", "BIH", "BGR", "CAN", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "MKD", "MDA", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "UKR", "GBR", "USA")

lac <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV", "GTM", "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "PAN", "PRY", "PER", "TTO", "URY", "VEN")

africa <- c("AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CAF", "TCD", "COM", "COD", "COG", "CIV", "DJI", "GNQ", "ERI", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", "SEN", "SLE", "SOM", "ZAF", "SDN", "SWZ", "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE")

# Create column region_dd
df_merged <- df_merged %>% 
  mutate(region_dd = case_when(
    isocode %in% east_south_asia ~ "East and South Asia",
    isocode %in% oceania ~ "Oceania",
    isocode %in% middleeast_ctrasia ~ "Middle East and Central Asia",
    isocode %in% europe_na ~ "Europe and North America",
    isocode %in% lac ~ "Latin America",
    isocode %in% africa ~ "Africa",
    TRUE ~ "Country not in DD charts"
  ))

# The following countries were not in the D&D charts
unique(df_merged$country.x[df_merged$region_dd == "Country not in DD charts"])

# replace region_dd values for each country
df_merged$region_dd <- ifelse(df_merged$isocode == "LUX", "Europe and North America",
                              ifelse(df_merged$isocode == "CPV", "Africa",
                                     ifelse(df_merged$isocode == "DZA", "Africa",
                                            ifelse(df_merged$isocode == "MMR", "East and South Asia",
                                                   ifelse(df_merged$isocode == "SUR", "Latin America",
                                                          df_merged$region_dd)))))

subdata <- df_merged[df_merged$region_dd %in% assignedregions, ]
isocode <- unique(subdata$country.x)
plot_list <- list()

#basic loop in ggplot2
for (i in isocode) {
  # Generate a graph for the current country and year
  pl <- ggplot(subdata %>% filter(country.x==i), aes(x = per_cap_gdp/1000, y = polity)) + ylim(-10,10) + geom_line() + 
    ggtitle(paste("Polity Score for", i, "given a value of real nominal per capita GDP"))
  print(pl) 
  #store the plot in the list with id as the name
  plot_list[[as.character(i)]] <- pl
}

pdf(paste0(outputloc, "/graphs_", paste(assignedregions, collapse = "_"), ".pdf"))
pdf.options(width = 9, height = 7)
for (i in 1:length(plot_list)){
  print(plot_list[[i]])
}
dev.off()