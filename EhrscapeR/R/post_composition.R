#' POST data to EHRScape composition
#' 
#' This function applies the GET method on the selected AQL query to obtain data from EhrScape platform. 
#' @param baseURL url addres of the REST service (character string).  
#' @param credentials character vector of the authentication pair of \code{username} and \code{password}. 
#' @param templateId character string containing the identifier of one of the existing templates in the EhrScape platform.  
#' @param ehrId character string containing the identifier of an existing EHR record.  
#' @param format character string indicating whether the provided JSON composition is in structured ("STRUCTURED") or flattened ("FLAT") form.  
#' @param composition the OpenEhr composition in JSON format to be saved to EhrScape platform. It can be given as character string or as a structured list.  
#' @return A list of the full server response and the parsed content field containing the link to the created composition.  
#' @export
#' @examples 
#' baseURL <- "https://rest.ehrscape.com/rest/v1/"
#' templateId <- "Vital Signs"
#' ehrId <- "6e031066-14df-46db-8320-bce31199fcbd"
#' format <- "FLAT"
#' composition <- '{"ctx/language": "en", "ctx/territory": "US", 
#'    "ctx/composer_name": "Silvia Blake", 
#'    "ctx/time": "2015-12-03T14:55:51.868+01:00", 
#'    "ctx/id_namespace": "HOSPITAL-NS", "ctx/id_scheme": "HOSPITAL-NS", 
#'    "ctx/health_care_facility|name": "Hospital", 
#'    "ctx/health_care_facility|id": "9091", 
#'    "vital_signs/body_temperature:0/any_event:0/temperature|magnitude": 37.94, 
#'    "vital_signs/body_temperature:0/any_event:0/temperature|unit": "°C"}'
#' post_c <- post_composition(baseURL, credentials = c("guidemo", "gui?!demo123"), templateId, ehrId, format, composition)

post_composition <- function(baseURL, credentials, templateId, ehrId, format, composition){
  
  URL_address <- httr::modify_url(paste(baseURL, "composition", sep = ""), query = list(templateId = templateId, ehrId = ehrId, format = format))
  
  if(typeof(composition) == "character"){
    composition <- qdapRegex::rm_white(composition)
    composition <- gsub("\r?\n|\r", " ", composition)
    df <- jsonlite::fromJSON(composition)
  } else {
    df <- composition
  }
  
  resp <- httr::POST(URL_address, httr::authenticate(credentials[1], credentials[2]), body = df, encode = "json")
  
  resp_content <- httr::content(resp, as = "parsed")
  
  return(list(answer = resp, answer_content = resp_content))

}