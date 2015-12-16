# Walkthrough for the EhrscapeR package

-   Introduction
-   Data query
    -   Make HTTP request
    -   GET response
    -   Parse response
    -   Convert to data frame
-   Data query - csv flattening
-   Save data 
    -   Make HTTP request
    -   Prepare composition
    -   POST data
    -   Parse response


## Introduction

[EhrScape](https://www.ehrscape.com/index.html) is an open health data platform providing a rich service framework that allows you to store, query and retrieve electronic health records based on the [openEHR](http://www.openehr.org/) specification. The data is available as [JSON](https://en.wikipedia.org/wiki/JSON) via a [REST](https://en.wikipedia.org/wiki/Representational_state_transfer) web service architecture. 

The overall approach is generic â€“ we want to obtain healthcare data by calling a REST API, and then format the returned result set in R to ready the data for more sophisticated analysis.

RESTful applications use HTTP requests to read data (e.g., make queries) and post data (create and/or update). At its most basic, calling a REST API to obtain data involves making a HTTP GET request to a server. If the call succeeds, youâ€™ll have a document that contains the requested data. To store data using REST API, a HTTP POST request needs to be made to the server along with the newly created data. 

This tutorial describes the `EhrscapeR` package, showcasing the use of R for handling data from EhrScape platform using REST API:

* [GET /query](https://www.ehrscape.com/api-explorer.html?api=thinkehr&service=/query&operation=/query&method=get&inline=true): returns the results of the specified AQL query,
* [GET /query/csv](https://www.ehrscape.com/api-explorer.html?api=thinkehr&service=/query&operation=/query/csv&method=get&inline=true): returns the results of the specified AQL query as CSV, 
* [POST /composition](https://www.ehrscape.com/api-explorer.html?api=thinkehr&service=/composition&operation=/composition&method=post&inline=true): creates and commits a contribution consisting of multiple composition creations, updates or deletes.

Please check the [EhrScape API Explorer](https://www.ehrscape.com/api-explorer.html) for a complete list of REST API. 

The basis of this package are functions from the:

- **httr** package: functions for making HTTP requests
- **jsonlite** package: functions for parsing JSON formated data into data frames (R-suitable form)

An important part is parsing the obtained JSON data into R-usable form. The result set in JSON format can be complex in structure with several nested levels which requires flattening of the structure to the data frame.  

The package `EhrscapeR` includes functions: 

* `get_query`: for a given [AQL](http://www.openehr.org/releases/QUERY/latest/docs/AQL/AQL.html) query returns the result set in a data frame. 
* `get_query_csv`: for a given AQL query returns the CSV formated result set in a data frame. 
* `post_composition`: stores new data records (compositions) in EhrScape.

All the examples below can be checked on EhrScape API Explorer.


## Data query

Data is queried from EhrScape using the `get_query` function:


```r
query_data <- get_query(baseURL, credentials = c(user_name, password), aql_query, 
    full_path = FALSE)
```

Input parameters: 

  - `baseURL`: base url address of the REST service, 
  - `credentials`: authentication pair of username and password
  - `aql_query`: AQL query, 
  - `full_path`: logical value indicating the degree of flattening. If `FALSE`, each value (even repetitions on the same path) in the JSON is assigned a unique identifier. If `TRUE`, values are sorted in the outpt data frame according to JSON path and unique sequence within the same path (if there are several repetitions on the same JSON path).
  
The function connects to the server with provided credentials and AQL query. The response in JSON format is parsed to a data frame and returned as the result of the function. 

Example: Query body temperature, systolic and diastolic measurements for all patients


```r
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
```

Result of the above query is: 

![Result of example query](./figures/EhrscapeR-walkthrough_files/figure-markdown_github/query_result.png)


### Make HTTP request

The function begins by preparing the AQL query for construction of the HTTP request. First, the `rm_white` function of the **qdapRegex** package removes from the input `aql_query` all multiple white spaces, white spaces before commas, semicolons, colons or endmarks, white spaces before or after brackets, leading and trailing white spaces. Next, line break and carriage return symbols are replaced with white space by using the `gsub` function. The edited AQL query string is then URL encoded (`query_enc`) and used to construct the HTTP request `URL_address` 


```r
aql_query <- qdapRegex::rm_white(aql_query)
aql_query <- gsub("\r?\n|\r", " ", aql_query)
query_enc <- utils::URLencode(aql_query)
URL_address = paste(baseURL, "query?aql=", query_enc, sep = "")
```

```
##  chr "https://rest.ehrscape.com/rest/v1/query?aql=select%20t/data[at0002"..
```

### GET response

The request `URL_address` is executed using the `GET` function of the **httr** package. The server response is stored as `resp`. Since EhrScape requires authentication, user credentials have to be included to the `GET` function call. Additionally, the `content_type_json` function is used to set the expected content type returned from the server to JSON. 


```r
resp <- httr::GET(URL_address, httr::authenticate(user_name, password), httr::content_type_json())
```

```
## List of 10
## $ url : chr
##    "https://rest.ehrscape.com/rest/v1/query?aql=select%20t/data[at0002%7"..
##    __truncated__
## $ status_code: int 200
## $ headers :List of 7
## ..$ date : chr "Wed, 16 Dec 2015 16:12:19 GMT"
## ..$ server : chr "Jetty(9.2.11.v20150529)"
## ..$ content-language: chr "en-US"
## ..$ content-type : chr "application/json;charset=UTF-8"
## ..$ vary : chr "Accept-Encoding"
## ..$ content-encoding: chr "gzip"
## ..$ content-length : chr "1094"
## ..- attr(*, "class")= chr [1:2] "insensitive" "list"
## $ all_headers:List of 1
## ..$ :List of 3
## .. ..$ status : int 200
## .. ..$ version: chr "HTTP/1.1"
## .. ..$ headers:List of 7
## .. .. ..$ date : chr "Wed, 16 Dec 2015 16:12:19 GMT"
## .. .. ..$ server : chr "Jetty(9.2.11.v20150529)"
## .. .. ..$ content-language: chr "en-US"
## .. .. ..$ content-type : chr "application/json;charset=UTF-8"
## .. .. ..$ vary : chr "Accept-Encoding"
## .. .. ..$ content-encoding: chr "gzip"
## .. .. ..$ content-length : chr "1094"
## .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
## $ cookies :'data.frame': 0 obs. of 7 variables:
## ..$ domain : logi(0)
## ..$ flag : logi(0)
## ..$ path : logi(0)
## ..$ secure : logi(0)
## ..$ expiration:Classes 'POSIXct', 'POSIXt' num(0)
## ..$ name : logi(0)
## ..$ value : logi(0)
## $ content : raw [1:14549] 7b 22 6d 65 ...
## $ date : POSIXct[1:1], format: "2015-12-16 16:12:19"
## $ times : Named num [1:6] 0 0 0.047 1.404 1.482 ...
## ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect"
##    "pretransfer" ...
## $ request :List of 7
## ..$ method : chr "GET"
## ..$ url : chr
##    "https://rest.ehrscape.com/rest/v1/query?aql=select%20t/data[at0002%7"..
##    __truncated__
## ..$ headers : Named chr [1:2] "application/json, text/xml,
##    application/xml, */*" "application/json"
## .. ..- attr(*, "names")= chr [1:2] "Accept" "Content-Type"
## ..$ fields : NULL
## ..$ options :List of 5
## .. ..$ useragent : chr "libcurl/7.43.0 r-curl/0.9.4 httr/1.0.0"
## .. ..$ cainfo : chr "C:/R/Rlibs/httr/cacert.pem"
## .. ..$ httpauth : num 1
## .. ..$ userpwd : chr "guidemo:gui?!demo123"
## .. ..$ customrequest: chr "GET"
## ..$ auth_token: NULL
## ..$ output : list()
## .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
## ..- attr(*, "class")= chr "request"
## $ handle :Class 'curl_handle' <externalptr>
## - attr(*, "class")= chr "response"
```

### Parse response

Response `resp` includes data such as the HTTP request, status, headers, cookies, and also the answer to the request in JSON format. This is stored in the content field, which is extracted with the `content` function to a nested list `json`. 


```r
json <- httr::content(resp)
```

```
## List of 4
## $ meta :List of 1
## ..$ href: chr
##    "http://rest.ehrscape.com/rest/v1/query/?aql=select%20t/data%5Bat0002"..
##    __truncated__
## $ aql : chr "select t/data[at0002|History|]/events[at0003|Any
##    event|]/data[at0001]/items[at0004|Temperature|]/value as Temperature,
##    bp/data["| __truncated__
## $ executedAql: chr "select t/data[at0002|History|]/events[at0003|Any
##    event|]/data[at0001]/items[at0004|Temperature|]/value as Temperature,
##    bp/data["| __truncated__
## $ resultSet :List of 56
## ..$ :List of 3
## .. ..$ Temperature:List of 4
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 37.3
## .. .. ..$ units : chr "°C"
## .. .. ..$ precision: int 1
## .. ..$ Systolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 99
## .. .. ..$ units : chr "mm[Hg]"
## .. ..$ Diastolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 70
## .. .. ..$ units : chr "mm[Hg]"
## ..$ :List of 3
## .. ..$ Temperature:List of 4
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 38.4
## .. .. ..$ units : chr "°C"
## .. .. ..$ precision: int 1
## .. ..$ Systolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 107
## .. .. ..$ units : chr "mm[Hg]"
## .. ..$ Diastolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 58
## .. .. ..$ units : chr "mm[Hg]"
## ..$ :List of 3
## .. ..$ Temperature:List of 4
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 36.2
## .. .. ..$ units : chr "°C"
## .. .. ..$ precision: int 1
## .. ..$ Systolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 101
## .. .. ..$ units : chr "mm[Hg]"
## .. ..$ Diastolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 62
## .. .. ..$ units : chr "mm[Hg]"
## ..$ :List of 3
## .. ..$ Temperature:List of 4
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 36
## .. .. ..$ units : chr "°C"
## .. .. ..$ precision: int 1
## .. ..$ Systolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 122
## .. .. ..$ units : chr "mm[Hg]"
## .. ..$ Diastolic :List of 3
## .. .. ..$ @class : chr "DV_QUANTITY"
## .. .. ..$ magnitude: num 83
## .. .. ..$ units : chr "mm[Hg]"
## .. [list output truncated]
```

Sublists contain the used AQL query and URL address as well as the result, which is extracted to the `json_results` named vector.


```r
json_results <- unlist(json[[4]])
```

```
## Named chr [1:560] "DV_QUANTITY" "37.3" "°C" "1" ...
## - attr(*, "names")= chr [1:560] "Temperature.@class"
##    "Temperature.magnitude" "Temperature.units" "Temperature.precision" ...
```

### Convert to data frame

The `json_results` named vector is converted to two-column data frame where the names of the vector are in one column and the values are in the other


```r
json_nav <- as.data.frame(json_results, stringsAsFactors = FALSE)
json_nav$path <- attr(json_results, "names")
```

```
## 'data.frame':	560 obs. of  2 variables:
## $ json_results: chr "DV_QUANTITY" "37.3" "°C" "1" ...
## $ path : chr "Temperature.@class" "Temperature.magnitude"
##    "Temperature.units" "Temperature.precision" ...
```

The names of the unlisted JSON `json_results` represent paths of the values in the nested JSON. And in the next step the unique paths of the JSON content are determined (`unique_path`) and used to determine the number of different records `rec_num` and their index limits (`limits`) in the `json_results` data_frame. 


```r
unique_path <- unique(json_nav$path)
```

```
##  chr [1:10] "Temperature.@class" "Temperature.magnitude" ...
```

```r
runs <- rle(json_nav$path == unique_path[1])
limits <- cumsum(runs$lengths)
```

```
##  int [1:112] 1 10 11 20 21 30 31 40 41 50 ...
```

```r
rec_num <- sum(runs$values)
```

```
##  int 56
```

The number of different records `rec_num` and their start and end limits `limits` are then used to add a new column `ind` to the `json_nav` data frame to assign each line to the corresponding record number


```r
json_nav$ind <- rep(NA, dim(json_nav)[1])
for(i in 1:rec_num) {
  json_nav$ind[limits[2*i - 1] : limits[2*i]] <- i
}
```

```
## 'data.frame':	560 obs. of  3 variables:
## $ json_results: chr "DV_QUANTITY" "37.3" "°C" "1" ...
## $ path : chr "Temperature.@class" "Temperature.magnitude"
##    "Temperature.units" "Temperature.precision" ...
## $ ind : int 1 1 1 1 1 1 1 1 1 1 ...
```

Below temperature data for the first three patients from the `json_nav` is visualized (the first column indicates which rows are displayed).


---------------------------------------------------
|&nbsp; | json_results |       path          | ind | 
|-------|--------------|---------------------|-----|
| **1** | DV_QUANTITY  | Temperature.@class  |  1  |
| **2** |     37.3     |Temperature.magnitude|  1  |
| **3** |      °C      |  Temperature.units  |  1  |
| **4** |      1       |Temperature.precision|  1  |
|**11** | DV_QUANTITY  | Temperature.@class  |  2  |
|**12** |     38.4     |Temperature.magnitude|  2  |
|**13** |      °C      |  Temperature.units  |  2  |
|**14** |      1       |Temperature.precision|  2  |
|**21** | DV_QUANTITY  | Temperature.@class  |  3  |
|**22** |     36.2     |Temperature.magnitude|  3  |
|**23** |      °C      |  Temperature.units  |  3  |
|**24** |      1       |Temperature.precision|  3  |
---------------------------------------------------


In the final step, the output data frame `out` is prepared according to the value of the `full_path` parameter, which can be either `FALSE` or `TRUE`. 

#### JSON paths without numbering

If `full_path` is set to `FALSE`, the column names of the `out` data frame are equal to the values of `unique_path` vector. Additionally, the `ind` and `seq` columns are included. This way each column represents elements on the same path. The `ind` column is used to annotate each row with the corresponding record number. The `seq` column is used to annotate each row with the number of repetitions in the same JSON path (to distinguish between several different elements in the same JSON path). If each JSON path contains a single element, all values in the `seq` are equal to 1.

The `seq` column is created using the `with` function, which evaluates the `json_nav` data frame to the expression defined by the `ave` function. The `ave` function groups the values in `json_results` according to unique combinations of variables `ind` and `path`. The `out` data frame is cast (by using function `dcast`) from the molten data frame `json_nav` by sorting the values in `json_results` according to the match between `ind` and `seq` on one side and `path` on the other.


```r
json_nav$seq <- with(json_nav, stats::ave(json_results, ind, path, FUN = seq_along))
json_nav$seq <- as.numeric(json_nav$seq)
out <- reshape2::dcast(ind + seq ~ path, data = json_nav, value.var = "json_results")
```

```
## 'data.frame':	56 obs. of  12 variables:
##  $ ind                  : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ seq                  : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ Diastolic.@class     : chr  "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" ...
##  $ Diastolic.magnitude  : chr  "70" "58" "62" "83" ...
##  $ Diastolic.units      : chr  "mm[Hg]" "mm[Hg]" "mm[Hg]" "mm[Hg]" ...
##  $ Systolic.@class      : chr  "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" ...
##  $ Systolic.magnitude   : chr  "99" "107" "101" "122" ...
##  $ Systolic.units       : chr  "mm[Hg]" "mm[Hg]" "mm[Hg]" "mm[Hg]" ...
##  $ Temperature.@class   : chr  "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" ...
##  $ Temperature.magnitude: chr  "37.3" "38.4" "36.2" "36" ...
##  $ Temperature.precision: chr  "1" "1" "1" "1" ...
##  $ Temperature.units    : chr  "°C" "°C" "°C" "°C" ...
```

The first six rows of output data frame `out` presented as table


----------------------------------------------------------------------------------------
|ind | seq | Diastolic.@class | Diastolic.magnitude | Diastolic.units | Systolic.@class |
|----|-----|------------------|---------------------|-----------------|-----------------|
| 1  |  1  |   DV_QUANTITY    |         70          |     mm[Hg]      |   DV_QUANTITY   |
| 2  |  1  |   DV_QUANTITY    |         58          |     mm[Hg]      |   DV_QUANTITY   |
| 3  |  1  |   DV_QUANTITY    |         62          |     mm[Hg]      |   DV_QUANTITY   |
| 4  |  1  |   DV_QUANTITY    |         83          |     mm[Hg]      |   DV_QUANTITY   |
| 5  |  1  |   DV_QUANTITY    |         70          |     mm[Hg]      |   DV_QUANTITY   |
| 6  |  1  |   DV_QUANTITY    |         59          |     mm[Hg]      |   DV_QUANTITY   |
----------------------------------------------------------------------------------------

Table: Table continues below

 
----------------------------------------------------------------------------------
|Systolic.magnitude | Systolic.units | Temperature.@class | Temperature.magnitude |
|-------------------|----------------|--------------------|-----------------------|
|        99         |     mm[Hg]     |    DV_QUANTITY     |         37.3          |
|       107         |     mm[Hg]     |    DV_QUANTITY     |         38.4          |
|       101         |     mm[Hg]     |    DV_QUANTITY     |         36.2          |
|       122         |     mm[Hg]     |    DV_QUANTITY     |          36           |
|        88         |     mm[Hg]     |    DV_QUANTITY     |         37.7          |
|        96         |     mm[Hg]     |    DV_QUANTITY     |         36.7          |
----------------------------------------------------------------------------------

Table: Table continues below

 
-------------------------------------------
|Temperature.precision | Temperature.units |
|----------------------|-------------------|
|          1           |        °C         |
|          1           |        °C         |
|          1           |        °C         |
|          1           |        °C         |
|          1           |        °C         |
|          1           |        °C         |
-------------------------------------------


#### JSON paths with numbering 

If `full_path` is set to `TRUE`, the number of columns of the output data frame is equal to the number of unique paths of the JSON content increased for the number of paths that repeat due to repetitions on the same JSON level (plus one for the `ind` column). The names of the columns are equal to the values of `unique_path` with appendend integer value of the repetition, which is used to distinguish between several different elements with the same JSON path. 

For this a new temporary data frame `tmp` is created, which includes the columns of the `json_nav` along with the column `newind` created as unique combinations of the columns `ind` and `path`. This is achieved by using the `ddply` function, which splits the input data frame (`json_nav`) according to given variables (`ind` and `path`), applies the selected function (`transform` - transform to data frame) on the splits along with the additional column `newind` and combines the results into a data frame. The `out` data frame is cast (by using function `dcast`) from the molten data frame `tmp` by sorting the values in `json_results` according to the match between `ind` on one side and `newind` on the other.


```r
tmp <- plyr::ddply(json_nav, plyr::.(ind, path), transform, newind = paste(path, 
    seq_along(path), sep = "."))
out2 <- reshape2::dcast(tmp, ind ~ newind, value.var = "json_results")
```

```
## 'data.frame':	56 obs. of  11 variables:
##  $ ind                    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Diastolic.@class.1     : chr  "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" ...
##  $ Diastolic.magnitude.1  : chr  "70" "58" "62" "83" ...
##  $ Diastolic.units.1      : chr  "mm[Hg]" "mm[Hg]" "mm[Hg]" "mm[Hg]" ...
##  $ Systolic.@class.1      : chr  "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" ...
##  $ Systolic.magnitude.1   : chr  "99" "107" "101" "122" ...
##  $ Systolic.units.1       : chr  "mm[Hg]" "mm[Hg]" "mm[Hg]" "mm[Hg]" ...
##  $ Temperature.@class.1   : chr  "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" "DV_QUANTITY" ...
##  $ Temperature.magnitude.1: chr  "37.3" "38.4" "36.2" "36" ...
##  $ Temperature.precision.1: chr  "1" "1" "1" "1" ...
##  $ Temperature.units.1    : chr  "°C" "°C" "°C" "°C" ...
```

The first six rows of output data frame `out` presented as table


------------------------------------------------------------------------------------------
|ind | Diastolic.@class.1 | Diastolic.magnitude.1 | Diastolic.units.1 | Systolic.@class.1 |
|----|--------------------|-----------------------|-------------------|-------------------|
| 1  |    DV_QUANTITY     |          70           |      mm[Hg]       |    DV_QUANTITY    |
| 2  |    DV_QUANTITY     |          58           |      mm[Hg]       |    DV_QUANTITY    |
| 3  |    DV_QUANTITY     |          62           |      mm[Hg]       |    DV_QUANTITY    |
| 4  |    DV_QUANTITY     |          83           |      mm[Hg]       |    DV_QUANTITY    |
| 5  |    DV_QUANTITY     |          70           |      mm[Hg]       |    DV_QUANTITY    |
| 6  |    DV_QUANTITY     |          59           |      mm[Hg]       |    DV_QUANTITY    |
------------------------------------------------------------------------------------------

Table: Table continues below

 
------------------------------------------------------------------------------------------
|Systolic.magnitude.1 | Systolic.units.1 | Temperature.@class.1 | Temperature.magnitude.1 |
|---------------------|------------------|----------------------|-------------------------|
|         99          |      mm[Hg]      |     DV_QUANTITY      |          37.3           |
|        107          |      mm[Hg]      |     DV_QUANTITY      |          38.4           |
|        101          |      mm[Hg]      |     DV_QUANTITY      |          36.2           |
|        122          |      mm[Hg]      |     DV_QUANTITY      |           36            |
|         88          |      mm[Hg]      |     DV_QUANTITY      |          37.7           |
|         96          |      mm[Hg]      |     DV_QUANTITY      |          36.7           |
------------------------------------------------------------------------------------------

Table: Table continues below

 
-----------------------------------------------
|Temperature.precision.1 | Temperature.units.1 |
|------------------------|---------------------|
|           1            |         °C          |
|           1            |         °C          |
|           1            |         °C          |
|           1            |         °C          |
|           1            |         °C          |
|           1            |         °C          |
-----------------------------------------------

In this case the `out2` notation was used just to distinguish between results for different values of the `full_path` argument. 

## Data query - csv flattening

EhrScape provides data structure flattening capability using `GET /query/csv`, which returns the results of the specified AQL query as CSV. This allows to get a data frame directly instead of manipulating multi-level generic JSON data structure. In `EhrscapeR` this is implemented in the function `get_query_csv`:


```r
query_data_csv <- get_query_csv(baseURL, credentials = c(user_name, password), aql_query)
```

Input parameters: 

  - `baseURL`: base url address of the REST service, 
  - `credentials`: authentication pair of username and password
  - `aql_query`: AQL query. 

The function connects to the server with provided credentials and AQL query. The response in JSON format prepared as a CSV table is parsed to a data frame and returned as the result of the function. This will be presented in more detail on an example using the previously defined parameters `baseURL`, `user_name`, `password` and `aql_query`. 

The function begins by preparing the AQL query for construction of the HTTP request. The only difference compared to the `get_query` function is the URL address, which uses `"query/csv?aql="` instead of `"query?aql="`. 


```r
aql_query <- qdapRegex::rm_white(aql_query)
aql_query <- gsub("\r?\n|\r", " ", aql_query)
query_enc <- utils::URLencode(aql_query)
URL_address = paste(baseURL, "query/csv?aql=", query_enc, sep = "")
```

```
##  chr "https://rest.ehrscape.com/rest/v1/query/csv?aql=select%20t/data[at"..
```

The request `URL_address` is executed using the `GET` function of the **httr** package. The server response is stored as `resp`.


```r
resp <- httr::GET(URL_address, httr::authenticate(user_name, password), httr::content_type_json())
```

```
## List of 10
## $ url : chr
##    "https://rest.ehrscape.com/rest/v1/query/csv?aql=select%20t/data[at00"..
##    __truncated__
## $ status_code: int 200
## $ headers :List of 5
## ..$ date : chr "Wed, 16 Dec 2015 16:12:20 GMT"
## ..$ server : chr "Jetty(9.2.11.v20150529)"
## ..$ content-language : chr "en-US"
## ..$ content-type : chr "text/csv;charset=UTF-8"
## ..$ transfer-encoding: chr "chunked"
## ..- attr(*, "class")= chr [1:2] "insensitive" "list"
## $ all_headers:List of 1
## ..$ :List of 3
## .. ..$ status : int 200
## .. ..$ version: chr "HTTP/1.1"
## .. ..$ headers:List of 5
## .. .. ..$ date : chr "Wed, 16 Dec 2015 16:12:20 GMT"
## .. .. ..$ server : chr "Jetty(9.2.11.v20150529)"
## .. .. ..$ content-language : chr "en-US"
## .. .. ..$ content-type : chr "text/csv;charset=UTF-8"
## .. .. ..$ transfer-encoding: chr "chunked"
## .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
## $ cookies :'data.frame': 0 obs. of 7 variables:
## ..$ domain : logi(0)
## ..$ flag : logi(0)
## ..$ path : logi(0)
## ..$ secure : logi(0)
## ..$ expiration:Classes 'POSIXct', 'POSIXt' num(0)
## ..$ name : logi(0)
## ..$ value : logi(0)
## $ content : raw [1:2252] 22 54 65 6d ...
## $ date : POSIXct[1:1], format: "2015-12-16 16:12:20"
## $ times : Named num [1:6] 0 0 0 0 0.078 0.078
## ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect"
##    "pretransfer" ...
## $ request :List of 7
## ..$ method : chr "GET"
## ..$ url : chr
##    "https://rest.ehrscape.com/rest/v1/query/csv?aql=select%20t/data[at00"..
##    __truncated__
## ..$ headers : Named chr [1:2] "application/json, text/xml,
##    application/xml, */*" "application/json"
## .. ..- attr(*, "names")= chr [1:2] "Accept" "Content-Type"
## ..$ fields : NULL
## ..$ options :List of 5
## .. ..$ useragent : chr "libcurl/7.43.0 r-curl/0.9.4 httr/1.0.0"
## .. ..$ cainfo : chr "C:/R/Rlibs/httr/cacert.pem"
## .. ..$ httpauth : num 1
## .. ..$ userpwd : chr "guidemo:gui?!demo123"
## .. ..$ customrequest: chr "GET"
## ..$ auth_token: NULL
## ..$ output : list()
## .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
## ..- attr(*, "class")= chr "request"
## $ handle :Class 'curl_handle' <externalptr>
## - attr(*, "class")= chr "response"
```

Response `resp` includes the answer to the request in JSON format. This is extracted from the content field with the `content` function to a data frame `json`, which is returned as the output of the function. 


```r
json <- httr::content(resp)
```

```
## 'data.frame':	56 obs. of  3 variables:
## $ Temperature: chr "37.3 °C" "38.4 °C" "36.2 °C" "36.0 °C" ...
## $ Systolic : chr "99.0 mm[Hg]" "107.0 mm[Hg]" "101.0 mm[Hg]" "122.0
##    mm[Hg]" ...
## $ Diastolic : chr "70.0 mm[Hg]" "58.0 mm[Hg]" "62.0 mm[Hg]" "83.0 mm[Hg]"
##    ...
```


--------------------------------------
|Temperature |  Systolic  | Diastolic |
|------------|------------|-----------|
|  37.3 °C   |99.0 mm[Hg] |70.0 mm[Hg]|
|  38.4 °C   |107.0 mm[Hg]|58.0 mm[Hg]|
|  36.2 °C   |101.0 mm[Hg]|62.0 mm[Hg]|
|  36.0 °C   |122.0 mm[Hg]|83.0 mm[Hg]|
|  37.7 °C   |88.0 mm[Hg] |70.0 mm[Hg]|
|  36.7 °C   |96.0 mm[Hg] |59.0 mm[Hg]|
--------------------------------------


## Save data

Function `post_composition` enables saving data records (composition) to EhrScape. Compositions are created by using web templates. Web template can be regarded as an input schema, which declares all possible settings of the input data structures for building an openEHR composition. To build a composition sample use `GET /template/{templateId}/example`, which returns an example of data values for a web template. This is used as a prototype composition and further filled with actual data. 

The composition has to be prepared and provided to the function in JSON format. Additionally, the composition has to be complemented by an identifier of one of the existing templates in the EhrScape and an identifier of an actual EHR record. The composition is saved to the server using the REST method **POST**. The function is called as: 


```r
post_data <- post_composition(baseURL, credentials = c(user_name, password), 
    templateId, ehrId, format, composition)
```

Input parameters: 

  - `baseURL`: base url address of the REST service, 
  - `credentials`: authentication pair of username and password
  - `templateId`: identifier of one of the existing templates in the EhrScape platform, 
  - `ehrId`: identifier of an existing EHR record
  - `format`: indicator of whether the provided JSON composition is in structured (`"STRUCTURED"`) or flattened (`"FLAT"`) form
  - `composition`: the OpenEhr composition in JSON format to be saved to EhrScape platform. It can be given as character string or as a structured list. 

The function connects to the server with provided credentials. The response in JSON format is parsed to a list and returned as the result of the function. 

Example: Save a patient's body temperature measurement to his EHR using a flattened web template composition


```r
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
  "vital_signs/body_temperature:0/any_event:0/temperature|unit": "Â°C"
}'
baseURL <- "https://rest.ehrscape.com/rest/v1/"
user_name <- "****"
password <- "****"
templateId <- "Vital Signs"
ehrId <- "6e031066-14df-46db-8320-bce31199fcbd"
format <- "FLAT"
```

### Make HTTP request

The function begins by constructing the HTTP request from the `baseURL`, `templateId`, `ehrId` and `format` arguments. For this, the `modify_url` function of the **httr** package is used 


```r
URL_address <- httr::modify_url(paste(baseURL, "composition", sep = ""), 
                                query = list(templateId = templateId, 
                                             ehrId = ehrId, format = format))
```

```
##  chr "https://rest.ehrscape.com/rest/v1/composition?templateId=Vital%20S"..
```

### Prepare composition

Construction of HTTP request is followed by preparing the `composition`, which can either be given as character string or a structured list or data frame. In case it is given as a character string. All inapropriate white space, line break and carriage return symbols are cleaned using the same procedure as with the function `get_query` and the cleaned character string is converted to a structured list using the `fromJSON` function of the **jsonlite** package


```r
if(typeof(composition) == "character"){
  composition <- qdapRegex::rm_white(composition)
  composition <- gsub("\r?\n|\r", " ", composition)
  df <- jsonlite::fromJSON(composition)
} else {
  df <- composition
}
df
```

```
## $`ctx/language`
## [1] "en"
## 
## $`ctx/territory`
## [1] "US"
## 
## $`ctx/composer_name`
## [1] "Silvia Blake"
## 
## $`ctx/time`
## [1] "2015-12-03T14:55:51.868+01:00"
## 
## $`ctx/id_namespace`
## [1] "HOSPITAL-NS"
## 
## $`ctx/id_scheme`
## [1] "HOSPITAL-NS"
## 
## $`ctx/health_care_facility|name`
## [1] "Hospital"
## 
## $`ctx/health_care_facility|id`
## [1] "9091"
## 
## $`vital_signs/body_temperature:0/any_event:0/temperature|magnitude`
## [1] 37.94
## 
## $`vital_signs/body_temperature:0/any_event:0/temperature|unit`
## [1] "Â°C"
```

### POST data 

Once the HTTP request `URL_address` and composition data `df` are prepared, they can be used to save the composition to EhrScape. This is done by using the `POST` function of the **httr** package. The required arguments include the HTTP request (`URL_address`), configuration settings (authentication with `user_name` and `password`), body (`df` containing the composition) and encoding used (the `encode` argument)


```r
resp <- httr::POST(URL_address, httr::authenticate(user_name, password), body = df, 
    encode = "json")
```

```
## List of 10
## $ url : chr
##    "https://rest.ehrscape.com/rest/v1/composition?templateId=Vital%20Sig"..
## $ status_code: int 201
## $ headers :List of 7
## ..$ date : chr "Fri, 11 Dec 2015 10:24:18 GMT"
## ..$ server : chr "Jetty(9.2.11.v20150529)"
## ..$ content-language : chr "en-US"
## ..$ content-type : chr "application/json;charset=UTF-8"
## ..$ vary : chr "Accept-Encoding"
## ..$ content-encoding : chr "gzip"
## ..$ transfer-encoding: chr "chunked"
## ..- attr(*, "class")= chr [1:2] "insensitive" "list"
## $ all_headers:List of 1
## ..$ :List of 3
## .. ..$ status : int 201
## .. ..$ version: chr "HTTP/1.1"
## .. ..$ headers:List of 7
## .. .. ..$ date : chr "Fri, 11 Dec 2015 10:24:18 GMT"
## .. .. ..$ server : chr "Jetty(9.2.11.v20150529)"
## .. .. ..$ content-language : chr "en-US"
## .. .. ..$ content-type : chr "application/json;charset=UTF-8"
## .. .. ..$ vary : chr "Accept-Encoding"
## .. .. ..$ content-encoding : chr "gzip"
## .. .. ..$ transfer-encoding: chr "chunked"
## .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
## $ cookies :'data.frame': 0 obs. of 7 variables:
## ..$ domain : logi(0)
## ..$ flag : logi(0)
## ..$ path : logi(0)
## ..$ secure : logi(0)
## ..$ expiration:Classes 'POSIXct', 'POSIXt' num(0)
## ..$ name : logi(0)
## ..$ value : logi(0)
## $ content : raw [1:223] 7b 22 6d 65 ...
## $ date : POSIXct[1:1], format: "2015-12-11 10:24:18"
## $ times : Named num [1:6] 0 0 0.031 0.858 0.936 ...
## ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect"
##    "pretransfer" ...
## $ request :List of 7
## ..$ method : chr "POST"
## ..$ url : chr
##    "https://rest.ehrscape.com/rest/v1/composition?templateId=Vital%20Sig"..
## ..$ headers : Named chr [1:2] "application/json, text/xml,
##    application/xml, */*" "application/json"
## .. ..- attr(*, "names")= chr [1:2] "Accept" "Content-Type"
## ..$ fields : NULL
## ..$ options :List of 8
## .. ..$ useragent : chr "libcurl/7.43.0 r-curl/0.9.4 httr/1.0.0"
## .. ..$ cainfo : chr "C:/R/Rlibs/httr/cacert.pem"
## .. ..$ post : logi TRUE
## .. ..$ postfieldsize: int 404
## .. ..$ postfields : raw [1:404] 7b 22 63 74 ...
## .. ..$ httpauth : num 1
## .. ..$ userpwd : chr "****:****"
## .. ..$ customrequest: chr "POST"
## ..$ auth_token: NULL
## ..$ output : list()
## .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
## ..- attr(*, "class")= chr "request"
## $ handle :Class 'curl_handle' <externalptr>
## - attr(*, "class")= chr "response"
```

### Parse response 

The server response contains the details of the call along with the HTTP status. In case of successfully executed request, the response body contains a link to the newly created composition. This link is parsed from the content field of the response


```r
resp_content <- httr::content(resp, as = "parsed")
```

```
## List of 3
## $ meta :List of 1
## ..$ href: chr
##    "http://rest.ehrscape.com/rest/v1/composition/33e5b31b-5d5e-46ab-b251"..
## $ action : chr "CREATE"
## $ compositionUid: chr
##    "33e5b31b-5d5e-46ab-b251-93b473c3d7a6::marand.ehrscape.com::1"
```

The function returns a list of the full response `resp` and the parsed content field containing the link to the created composition `resp_content`.
