#' Parse JSON resulting from AQL query to data frame
#' 
#' This function applies the GET method on the selected AQL query to obtain data from EhrScape platform. 
#' @param baseURL url addres of the REST service (character string). 
#' @param credentials character vector of the authentication pair of \code{username} and \code{password}.
#' @param aql_query character string containing the AQL query. 
#' @param full_path logical value indicating the degree of flattening. If TRUE each value (even repetitions on the same level) in the JSON is assigned a unique identifier (basically flattened address). If FALSE values are sorted in the outpt data frame according to JSON path, case number and unique sequence within the same case (if there are several repetitions on the same JSON level).
#' @return A data frame containing the parsed JSON result.  
#' @export
#' @examples 
#' baseURL <- "https://rest.ehrscape.com/rest/v1/"
#' aql_query <- "select 
#'    a_a/data[at0002|History|]/events[at0003|Any event|]/data[at0001]/items[at0004|Temperature|]/value 
#'    as Temperature, 
#'    a_b/data[at0001|history|]/events[at0006|any event|]/data[at0003]/items[at0004|Systolic|]/value 
#'    as Systolic, 
#'    a_b/data[at0001|history|]/events[at0006|any event|]/data[at0003]/items[at0005|Diastolic|]/value 
#'    as Diastolic 
#'    from EHR e contains COMPOSITION 
#'    a contains (OBSERVATION a_a[openEHR-EHR-OBSERVATION.body_temperature.v1] 
#'    and OBSERVATION a_b[openEHR-EHR-OBSERVATION.blood_pressure.v1]) offset 0 limit 100"
#' query_data <- get_query(baseURL, credentials = c("guidemo", "gui?!demo123"), aql_query, full_path = FALSE)

get_query <- function(baseURL, credentials = c(user_name, password), aql_query, full_path = FALSE) {
  
  aql_query <- qdapRegex::rm_white(aql_query)
  aql_query <- gsub("\r?\n|\r", " ", aql_query)
  query_enc <- utils::URLencode(aql_query)
  URL_address = paste(baseURL, "query?aql=", query_enc, sep = "")
  resp <- httr::GET(URL_address, httr::authenticate(credentials[1], credentials[2]), httr::content_type_json())
  
  json <- httr::content(resp)
  json_results <- unlist(json[[4]])
  
  #Data frame with values and paths
  json_nav <- as.data.frame(json_results, stringsAsFactors = FALSE)
  json_nav$path <- attr(json_results, "names")
  unique_path <- unique(json_nav$path)
  
  # if number of values is not equal for all cases
  runs <- rle(json_nav$path == unique_path[1])
  limits <- cumsum(runs$lengths)
  rec_num <- sum(runs$values)
  
  json_nav$ind <- rep(NA, dim(json_nav)[1])
  for(i in 1:rec_num) {
    json_nav$ind[limits[2*i - 1] : limits[2*i]] <- i
  }

  if(full_path) {
    tmp <- plyr::ddply(json_nav, plyr::.(ind, path), transform, newind = paste(path, seq_along(path), sep = "."))
    out <- reshape2::dcast(tmp, ind ~ newind, value.var = "json_results")
  } else {
    # out <- reshape2::dcast(json_nav, ind ~ path, value.var = "json_results", fun.aggregate = max)
    json_nav$seq <- with(json_nav, stats::ave(json_results, ind, path, FUN = seq_along))
    json_nav$seq <- as.numeric(json_nav$seq)
    out <- reshape2::dcast(ind + seq ~ path, data = json_nav, value.var = "json_results")
  }
  
  
  return(out)
}