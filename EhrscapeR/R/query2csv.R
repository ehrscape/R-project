#' Apply the GET query/csv service from EHRScape to import query results as table
#' 
#' This function applies the GET method on the selected AQL query to obtain data in CSV format from EhrScape platform. 
#' @param baseURL url addres of the REST service (character string).   
#' @param credentials character vector of the authentication pair of \code{username} and \code{password}.  
#' @param aql_query character string containing the AQL query.  
#' @return A data frame containing the JSON content of query results.  
#' @export
#' @examples 
#' baseURL <- "https://rest.ehrscape.com/rest/v1/"
#' aql_query <- "select 
#'    a_a/data[at0002|History|]/events[at0003|Any event|]/data[at0001]/items[at0004|Temperature|]/value 
#'    as Temperature, 
#'    a_b/data[at0001|history|]/events[at0006|any event|]/data[at0003]/items[at0004|Systolic|]/value 
#'    as Systolic, 
#'    a_b/data[at0001|history|]/events[at0006|any event|]/data[at0003]/items[at0005|Diastolic|]/value 
#'    as Diastolic from EHR e contains COMPOSITION 
#'    a contains (OBSERVATION a_a[openEHR-EHR-OBSERVATION.body_temperature.v1] 
#'    and OBSERVATION a_b[openEHR-EHR-OBSERVATION.blood_pressure.v1]) offset 0 limit 100"
#' query_data <- get_query_csv(baseURL, credentials = c("guidemo", "gui?!demo123"), aql_query)

get_query_csv <- function(baseURL, credentials = c(user_name, password), aql_query) {
  
  aql_query <- qdapRegex::rm_white(aql_query)
  aql_query <- gsub("\r?\n|\r", " ", aql_query)
  query_enc <- utils::URLencode(aql_query)
  URL_address = paste(baseURL, "query/csv?aql=", query_enc, sep = "")
  resp <- httr::GET(URL_address, httr::authenticate(credentials[1], credentials[2]), httr::content_type_json())
  
  json <- httr::content(resp)
  
  return(json)
}