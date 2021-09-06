# convert world odonata list into dwca-checklist

library(dplyr)
library(readxl)
library(rgbif)
library(purrr)
library(zip)

save_dir = "C:/Users/ftw712/Desktop/world_odonata_list/"
source_xls = "world-odonata-list-20210812.xls"

# clean and organize source_xls
d = source_xls %>%
paste0(save_dir,.) %>%
read_xls() %>%
slice(6:n()) %>% # skip the front matter
select(3,4) %>% 
setNames(c("names","syn")) %>% 
filter_all(any_vars(complete.cases(.))) %>% # remove completely missing rows
mutate(names = gsub(" \\(still much confusion about composition of this family\\)","",names)) %>%  # remove little note about family
mutate(names = gsub("INCERTAE SEDIS GROUP 8","FAMILY",names)) %>%  # remove no placement label in family
mutate(is_family = grepl("^[A-Z]*$",names)) %>% # families are CAPITALIZED with no spaces or no placement
mutate(family = case_when(is_family ~ names)) %>% # use for side effect of giving NA 
mutate(family = stringr::str_to_title(family)) %>%
mutate(is_genus = # a genus is something that has two Capitalized words like "Dog John, 2021"
case_when(
grepl("^[A-Z][^ ]+ [A-Z][^ ]+",names) ~ TRUE, # is a GENUS
grepl("^[A-Z][^ ]+ (von|van|de) [A-Z][^ ]+",names) ~ TRUE, # handle dutch names
TRUE ~ FALSE # otherwise not a GENUS
)) %>%
mutate(genus = case_when(is_genus ~ names)) %>% # add the genus column
tidyr::fill(family) %>% # fill down
tidyr::fill(genus) %>% 
tidyr::fill(names) %>% 
filter(!is_family) %>% # I don't include families in the base names (maybe I should)
mutate(rn = row_number()) %>% # for sorting back to the original list order
mutate(taxonID = as.numeric(as.factor(names))) %>% # create the taxonIDs
arrange(taxonID) %>% 
mutate(acceptedNameUsageID = if_else(is.na(syn),NaN,taxonID)) %>% 
mutate(syn_index = cumsum(if_else(is.na(syn),0,1))) %>% # important for unique ids
mutate(max_id = max(taxonID)) %>% # for unique taxonIDs
mutate(taxonID = if_else(!is.na(syn),max_id+syn_index,taxonID)) %>% 
arrange(rn) %>%
mutate(scientificName = names) %>% 
mutate(scientificName = if_else(!is.na(syn),syn,scientificName)) %>% # replace name with syn
mutate(is_doubtful = grepl("\\(doubtful species\\)|\\(incertae sedis\\)",scientificName)) %>% # get doubtful names from comments
mutate(is_junior_syn = grepl(" junior ",scientificName)) %>% # junior syn handling
mutate(scientificName = gsub("\\?","",scientificName)) %>% # run some clean up of the names. could move some of this to notes
mutate(scientificName = gsub("\\[[^][]*]","",scientificName)) %>%
mutate(scientificName = gsub("^Syn ","",scientificName)) %>%
mutate(scientificName = gsub("^syn ","",scientificName)) %>%
mutate(scientificName = gsub("\\(SIC\\!\\) ","",scientificName)) %>%
mutate(scientificName = gsub("\\(Sic\\!\\) ","",scientificName)) %>%
mutate(scientificName = gsub("\\(doubtful species\\)|\\(incertae sedis\\)","",scientificName)) %>% # clean up 
mutate(scientificName = gsub("\\(lapsus\\)","",scientificName)) %>%
mutate(scientificName = gsub("\\(incorrect gender\\)","",scientificName)) %>%
mutate(scientificName = gsub("\\(part\\)","",scientificName)) %>%
mutate(scientificName = gsub("\\(or syn of L. pontica\\)","",scientificName)) %>%
mutate(id = taxonID) %>% # id the same as taxonID
glimpse()

# run names through GBIF name parser
parsed = d %>% 
pull(scientificName) %>% 
unique() %>% 
rgbif::parsenames() %>%
mutate(scientificName = scientificname) %>%
select(scientificName,authorship,year,specificepithet) 

# merge back together to create checklist 
checklist = merge(parsed,d,id="scientificName",all.y=TRUE) %>% 
arrange(rn) %>%
mutate(kingdom = "Animalia") %>%
mutate(class = "Insecta") %>%
mutate(order = "Odonata") %>%
mutate(subgenus = NA_character_) %>%
mutate(taxonRank = if_else(is_genus,"genus","species")) %>%
mutate(scientificNameAuthorship = if_else(!is.na(authorship),paste0(authorship,", ",year),NA_character_)) %>%
mutate(taxonomicStatus = case_when(
is.na(syn) ~ "accepted",
!is.na(syn) ~ "synonym",
is_doubtful ~ "doubtful",
is_junior_syn ~ "junior synonym"
)) %>%
mutate(taxonRemarks = NA_character_) %>%
mutate(modified = Sys.time()) %>%
mutate(language = "en") %>%
mutate(informationWithheld = NA_character_) %>% 
mutate(references = "https://www2.pugetsound.edu/academics/academic-resources/slater-museum/biodiversity-resources/dragonflies/world-odonata-list2/") %>% 
select(
rn,
syn,
id,
taxonID,
acceptedNameUsageID,
scientificName,
kingdom,
class,
order,
family,
genus,
subgenus,
specificEpithet = specificepithet,
taxonRank,
scientificNameAuthorship,
taxonomicStatus,
taxonRemarks,
modified,
language,
informationWithheld,
references
) %>%
mutate(scientificNameAuthorship = # fix missing authors from name parser
if_else(is.na(scientificNameAuthorship),
stringr::str_extract_all(scientificName, "\\([^()]+\\)",simplify = TRUE),
scientificNameAuthorship
)) %>%
arrange(rn) %>% # relies on proper order of rows
mutate(parentNameUsageID = case_when(taxonRank == "genus" ~ id)) %>% 
tidyr::fill(parentNameUsageID) %>% 
mutate(parentNameUsageID = if_else(taxonRank == "genus",NaN,parentNameUsageID)) %>% # genus does not have a parent usage id
mutate(family = if_else(family == "Family",NA_character_,family)) %>% # remove dummy family we put in earlier
mutate(genus = stringr::str_extract_all(genus,"^[^ ]+",simplify = TRUE)) %>% # remove authors from genus
select(-syn) %>%  
select(-rn) %>%
select(
id,
taxonID,
acceptedNameUsageID,
parentNameUsageID,
everything()
) %>%
glimpse()

# tests
if(!all(table(checklist$taxonID) == 1)) stop("error: non-unique taxon ids")
if(any(!c("eml.xml","meta.xml") %in% list.files(save_dir))) stop("your forgot to add the meta data files!") 
if(nrow(checklist) < 10000) stop("probably something went wrong with the source")
if(ncol(checklist) < 20) stop("check your number of columns")

# save checklist 
checklist %>% readr::write_tsv(paste0(save_dir,"taxon.txt"),na = "")

# zip the archive
# setwd(dir)
# zip::zip(paste0(dwca,".zip"),dwca)

