
library(readr)
library(stringr)
library(dplyr)
library(magrittr)

postcodes1 = read_csv("~/Dropbox/packages/oddments/inst/extdata/postcode-outcodes.csv") %T>% print
postcodes2 = read_csv("~/Dropbox/packages/oddments/inst/extdata/ukpostcodes.csv") %T>% print
postcodes3 = read_csv("~/Dropbox/packages/oddments/inst/extdata/postcodes.csv") %T>% print

postcodes1
postcodes2
postcodes3

postcodes3 %<>% mutate(prefix = str_sub(Postcode, 1, 2))

postcodes3 %>%
    filter(`In Use?` == "Yes") %>%
    select(prefix, Region) %>%
    distinct %>%
    count(prefix, Region) %>%
    filter(!is.na(Region)) %>%
    select(-n) %>%
    as.data.frame

postcodes3 %>%
    filter(!is.na(Region)) %>%
    group_by(prefix) %>%
    summarise(Region = paste(unique(Region), collapse = "/")) %>%
    as.data.frame


    select(prefix, Region) %>%
    distinct %>%
    count(prefix, Region) %>%
    filter(!is.na(Region)) %>%
    select(-n) %>%
    as.data.frame

postcodes3 %>%
    filter(`In Use?` == "Yes") %>%
    select(prefix, NationalPark) %>%
    distinct %>%
    count(prefix, NationalPark) %>%
    as.data.frame


    select(Region) %>%
    sapply(table, useNA = "ifany")

postcodes3 %>% sapply(n_distinct)

vars = c(
    "In Use?","County",
    "District","Ward",
    "DistrictCode","WardCode",
    "Country","CountyCode",
    "Constituency","Introduced",
    "Terminated","Parish",
    "NationalPark","Population",
    "Households","Built up area",
    "Built up sub-division","Lower layer super output area",
    "Rural/urban","Region"
)

postcodes3[vars] %>% sapply(n_distinct)

for(i in vars){
    message(i)
    postcodes3[[i]] %>% n_distinct %>% print
    # postcodes3[[i]] %>% table %>% print
}

