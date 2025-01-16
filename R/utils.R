#' Stores your API key in your keyring. 
#'
#' @param api_key Your personal linear API key. See README.md for information.  
#' 
#' @return nothing
#' @export
store_linear_api_key <- function(api_key){
  keyring::key_set_with_value("linear_api", password = api_key)
}

get_linear_api_key <- function(){
  keyring::key_get("linear_api")
}

make_linear_api_request <- function(request_string){
  httr2::request("https://api.linear.app/graphql") |>
    httr2::req_method("POST") |> 
    httr2::req_headers(
      Authorization = get_linear_api_key(),
      `Content-Type` = "application/json",
    ) |> 
    httr2::req_body_raw(request_string) |> 
    httr2::req_perform() |> 
    httr2::resp_body_string() |> 
    jsonlite::fromJSON(flatten = TRUE) 
}