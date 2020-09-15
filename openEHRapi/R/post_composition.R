#' POST data to openEHR composition
#'
#' This function applies the POST method on the selected structured JSON to create a new composition for the selected EHR ID on an openEHR server.
#' @param baseURL url addres of the REST service (character string).
#' @param credentials character vector of the authentication pair of \code{username} and \code{password}.
#' @param ehrId character string containing the identifier of an existing EHR record.
#' @param composition the OpenEhr composition in structured JSON format to be saved to EhrScape platform. It can be given as character string or as a structured list.
#' @return A list of the full server response and the parsed content field containing the link to the created composition.
#' @export
#' @examples
#' baseURL <- "https://rest.ehrscape.com/rest/openehr/v1/"
#' ehrId <- "3944045a-656b-44e8-ba0f-136cfd44c045"
#' composition <- '{
#'                      "_type": "COMPOSITION",
#'                      "name": {
#'                        "_type": "DV_TEXT",
#'                        "value": "Vital Signs"
#'                      },
#'                      "archetype_details": {
#'                        "_type": "ARCHETYPED",
#'                        "archetype_id": {
#'                          "_type": "ARCHETYPE_ID",
#'                          "value": "openEHR-EHR-COMPOSITION.encounter.v1"
#'                        },
#'                        "template_id": {
#'                          "_type": "TEMPLATE_ID",
#'                          "value": "Vital Signs"
#'                        },
#'                        "rm_version": "1.0.4"
#'                      },
#'                      "archetype_node_id": "openEHR-EHR-COMPOSITION.encounter.v1",
#'                      "language": {
#'                        "_type": "CODE_PHRASE",
#'                        "terminology_id": {
#'                          "_type": "TERMINOLOGY_ID",
#'                          "value": "ISO_639-1"
#'                        },
#'                        "code_string": "en"
#'                      },
#'                      "territory": {
#'                        "_type": "CODE_PHRASE",
#'                        "terminology_id": {
#'                          "_type": "TERMINOLOGY_ID",
#'                          "value": "ISO_3166-1"
#'                        },
#'                        "code_string": "US"
#'                      },
#'                      "category": {
#'                        "_type": "DV_CODED_TEXT",
#'                        "value": "event",
#'                        "defining_code": {
#'                          "_type": "CODE_PHRASE",
#'                          "terminology_id": {
#'                            "_type": "TERMINOLOGY_ID",
#'                            "value": "openehr"
#'                          },
#'                          "code_string": "433"
#'                        }
#'                      },
#'                      "composer": {
#'                        "_type": "PARTY_IDENTIFIED",
#'                        "name": "Silvia Blake"
#'                      },
#'                      "context": {
#'                        "_type": "EVENT_CONTEXT",
#'                        "start_time": {
#'                          "_type": "DV_DATE_TIME",
#'                          "value": "2020-04-03T11:48:35.001+02:00"
#'                        },
#'                        "setting": {
#'                          "_type": "DV_CODED_TEXT",
#'                          "value": "other care",
#'                          "defining_code": {
#'                            "_type": "CODE_PHRASE",
#'                            "terminology_id": {
#'                              "_type": "TERMINOLOGY_ID",
#'                              "value": "openehr"
#'                            },
#'                            "code_string": "238"
#'                          }
#'                        },
#'                        "health_care_facility": {
#'                          "_type": "PARTY_IDENTIFIED",
#'                          "external_ref": {
#'                            "_type": "PARTY_REF",
#'                            "id": {
#'                              "_type": "GENERIC_ID",
#'                              "value": "9091",
#'                              "scheme": "HOSPITAL-NS"
#'                            },
#'                            "namespace": "HOSPITAL-NS",
#'                            "type": "PARTY"
#'                          },
#'                          "name": "Hospital"
#'                        }
#'                      },
#'                      "content": [
#'                        {
#'                          "_type": "OBSERVATION",
#'                          "name": {
#'                            "_type": "DV_TEXT",
#'                            "value": "Body temperature"
#'                          },
#'                          "archetype_details": {
#'                            "_type": "ARCHETYPED",
#'                            "archetype_id": {
#'                              "_type": "ARCHETYPE_ID",
#'                              "value": "openEHR-EHR-OBSERVATION.body_temperature.v1"
#'                            },
#'                            "rm_version": "1.0.4"
#'                          },
#'                          "archetype_node_id": "openEHR-EHR-OBSERVATION.body_temperature.v1",
#'                          "language": {
#'                            "_type": "CODE_PHRASE",
#'                            "terminology_id": {
#'                              "_type": "TERMINOLOGY_ID",
#'                              "value": "ISO_639-1"
#'                            },
#'                            "code_string": "en"
#'                          },
#'                          "encoding": {
#'                            "_type": "CODE_PHRASE",
#'                            "terminology_id": {
#'                              "_type": "TERMINOLOGY_ID",
#'                              "value": "IANA_character-sets"
#'                            },
#'                            "code_string": "UTF-8"
#'                          },
#'                          "subject": {
#'                            "_type": "PARTY_SELF"
#'                          },
#'                          "data": {
#'                            "_type": "HISTORY",
#'                            "name": {
#'                              "_type": "DV_TEXT",
#'                              "value": "History"
#'                            },
#'                            "archetype_node_id": "at0002",
#'                            "origin": {
#'                              "_type": "DV_DATE_TIME",
#'                              "value": "2020-04-03T11:48:35.001+02:00"
#'                            },
#'                            "events": [
#'                              {
#'                                "_type": "POINT_EVENT",
#'                                "name": {
#'                                  "_type": "DV_TEXT",
#'                                  "value": "Any event"
#'                                },
#'                                "archetype_node_id": "at0003",
#'                                "time": {
#'                                  "_type": "DV_DATE_TIME",
#'                                  "value": "2020-04-03T11:48:35.001+02:00"
#'                                },
#'                                "data": {
#'                                  "_type": "ITEM_TREE",
#'                                  "name": {
#'                                    "_type": "DV_TEXT",
#'                                    "value": "Tree"
#'                                  },
#'                                "archetype_node_id": "at0001",
#'                                  "items": [
#'                                  {
#'                                    "_type": "ELEMENT",
#'                                    "name": {
#'                                      "_type": "DV_TEXT",
#'                                      "value": "Temperature"
#'                                    },
#'                                    "archetype_node_id": "at0004",
#'                                    "value": {
#'                                      "_type": "DV_QUANTITY",
#'                                      "magnitude": 36.20,
#'                                      "units": "°C",
#'                                      "precision": 1
#'                                    }
#'                                  }
#'                                  ]
#'                              }
#'                            }
#'                            ]
#'                        }
#'                      }
#'                      ]
#'                    }'
#' post_c <- post_composition(baseURL, credentials = c("****", "****"), ehrId, composition)

post_composition <- function(baseURL, credentials, ehrId, composition){

  if (endsWith(baseURL, "/")) {
    URL_address <- httr::modify_url(paste0(baseURL, "ehr/", ehrId, "/composition"))
  } else {
    URL_address <- httr::modify_url(paste0(baseURL, "/ehr/", ehrId, "/composition"))
  }

  if(typeof(composition) == "character"){
    composition <- qdapRegex::rm_white(composition)
    composition <- gsub("\r?\n|\r", " ", composition)
    df <- jsonlite::fromJSON(composition)
  } else {
    df <- composition
  }

  resp <- httr::POST(URL_address, httr::authenticate(credentials[1], credentials[2]), body = df, encode = "json")
  resp_content <- httr::content(resp, as = "parsed")

  if (httr::http_error(resp)) {
    stop(
      paste0("Could not execute. HTTP ", resp$status_code, ": ", resp_content)
    )
    return(list(answer = resp, answer_content = resp_content))
  } else {
    return(list(answer = resp, answer_content = resp_content, location = resp$headers$location))
  }

}
