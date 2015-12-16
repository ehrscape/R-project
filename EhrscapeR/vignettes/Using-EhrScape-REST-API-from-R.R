## ---- eval=FALSE, tidy=TRUE----------------------------------------------
#  query_data <- get_query(baseURL, credentials = c(user_name, password), aql_query, full_path = FALSE)

## ---- tidy=FALSE---------------------------------------------------------
aql_query <- 
  "select
    t/data[at0002|History|]/events[at0003|Any event|]/data[at0001]/items[at0004|Temperature|]/value as Temperature,
    bp/data[at0001|history|]/events[at0006|any event|]/data[at0003]/items[at0004|Systolic|]/value as Systolic,
    bp/data[at0001|history|]/events[at0006|any event|]/data[at0003]/items[at0005|Diastolic|]/value as Diastolic
from EHR e
contains COMPOSITION c
contains (
    OBSERVATION t[openEHR-EHR-OBSERVATION.body_temperature.v1] and
    OBSERVATION bp[openEHR-EHR-OBSERVATION.blood_pressure.v1])
offset 0 limit 100"

baseURL <- "https://rest.ehrscape.com/rest/v1/"
user_name <- "guidemo"
password <- "gui?!demo123"

## ---- tidy=TRUE, echo=-5-------------------------------------------------
aql_query <- qdapRegex::rm_white(aql_query)
aql_query <- gsub("\r?\n|\r", " ", aql_query)
query_enc <- utils::URLencode(aql_query)
URL_address = paste(baseURL, "query?aql=", query_enc, sep = "")
str(URL_address, strict.width = "cut")

## ---- echo=-2------------------------------------------------------------
resp <- httr::GET(URL_address, httr::authenticate(user_name, password), httr::content_type_json())
str(resp, strict.width = "wrap")

## ---- echo=-2------------------------------------------------------------
json <- httr::content(resp)
str(json, strict.width = "wrap", list.len = 4)

## ---- echo=-2------------------------------------------------------------
json_results <- unlist(json[[4]])
str(json_results, strict.width = "wrap")

## ---- echo=-3------------------------------------------------------------
json_nav <- as.data.frame(json_results, stringsAsFactors = FALSE)
json_nav$path <- attr(json_results, "names")
str(json_nav, strict.width = "wrap")

## ---- echo=-c(2,5,7)-----------------------------------------------------
unique_path <- unique(json_nav$path)
str(unique_path)
runs <- rle(json_nav$path == unique_path[1])
limits <- cumsum(runs$lengths)
str(limits)
rec_num <- sum(runs$values)
str(rec_num)

## ---- echo=-3------------------------------------------------------------
json_nav$ind <- rep(NA, dim(json_nav)[1])
for(i in 1:rec_num) {
  json_nav$ind[limits[2*i - 1] : limits[2*i]] <- i
}
str(json_nav, strict.width = "wrap")

## ---- echo=FALSE---------------------------------------------------------
pander::pander(json_nav[c(1:4, 11:14, 21:24),])

## ---- echo=-4------------------------------------------------------------
json_nav$seq <- with(json_nav, stats::ave(json_results, ind, path, FUN = seq_along))
json_nav$seq <- as.numeric(json_nav$seq)
out <- reshape2::dcast(ind + seq ~ path, data = json_nav, value.var = "json_results")
str(out)

## ---- echo=FALSE---------------------------------------------------------
pander::panderOptions('table.split.table', 100)
pander::pander(head(out))

## ---- echo=-3, tidy=TRUE-------------------------------------------------
tmp <- plyr::ddply(json_nav, plyr::.(ind, path), transform, newind = paste(path, seq_along(path), sep = "."))
out2 <- reshape2::dcast(tmp, ind ~ newind, value.var = "json_results")
str(out2)

## ---- echo=FALSE---------------------------------------------------------
pander::panderOptions('table.split.table', 100)
pander::pander(head(out2))

## ---- eval=FALSE---------------------------------------------------------
#  query_data_csv <- get_query_csv(baseURL, credentials = c(user_name, password), aql_query)

## ---- tidy=TRUE, echo=-5-------------------------------------------------
aql_query <- qdapRegex::rm_white(aql_query)
aql_query <- gsub("\r?\n|\r", " ", aql_query)
query_enc <- utils::URLencode(aql_query)
URL_address = paste(baseURL, "query/csv?aql=", query_enc, sep = "")
str(URL_address, strict.width = "cut")

## ---- echo=-2------------------------------------------------------------
resp <- httr::GET(URL_address, httr::authenticate(user_name, password), httr::content_type_json())
str(resp, strict.width = "wrap")

## ---- echo=-c(2,3)-------------------------------------------------------
json <- httr::content(resp)
str(json, strict.width = "wrap", list.len = 4)
pander::pander(head(json))

## ---- eval=FALSE, tidy=TRUE----------------------------------------------
#  post_data <- post_composition(baseURL, credentials = c(user_name, password), templateId, ehrId, format, composition)

## ---- tidy=FALSE---------------------------------------------------------
composition <- 
'{
  "ctx/language": "en", 
  "ctx/territory": "US", 
  "ctx/composer_name": "Silvia Blake", 
  "ctx/time": "2015-12-03T14:55:51.868+01:00", 
  "ctx/id_namespace": "HOSPITAL-NS", 
  "ctx/id_scheme": "HOSPITAL-NS", 
  "ctx/health_care_facility|name": "Hospital", 
  "ctx/health_care_facility|id": "9091", 
  "vital_signs/body_temperature:0/any_event:0/temperature|magnitude": 37.94,
  "vital_signs/body_temperature:0/any_event:0/temperature|unit": "°C"
}'
baseURL <- "https://rest.ehrscape.com/rest/v1/"
user_name <- "****"
password <- "****"
templateId <- "Vital Signs"
ehrId <- "6e031066-14df-46db-8320-bce31199fcbd"
format <- "FLAT"

## ---- echo=-2, tidy=FALSE------------------------------------------------
URL_address <- httr::modify_url(paste(baseURL, "composition", sep = ""), 
                                query = list(templateId = templateId, 
                                             ehrId = ehrId, format = format))
str(URL_address, strict.width = "cut")

## ------------------------------------------------------------------------
if(typeof(composition) == "character"){
  composition <- qdapRegex::rm_white(composition)
  composition <- gsub("\r?\n|\r", " ", composition)
  df <- jsonlite::fromJSON(composition)
} else {
  df <- composition
}
df

## ---- echo=-c(2,3), tidy=TRUE--------------------------------------------
resp <- httr::POST(URL_address, httr::authenticate(user_name, password), body = df, encode = "json")
load(system.file("extdata", "resp.Rda", package="EhrscapeR"))
str(resp, strict.width = "wrap")

## ---- echo=-2------------------------------------------------------------
resp_content <- httr::content(resp, as = "parsed")
str(resp_content, strict.width = "wrap")

