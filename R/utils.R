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

make_linear_api_request <- function(request_string, api_url = "https://api.linear.app/graphql"){
  response <- httr2::request(api_url) |> 
    httr2::req_headers(
      "Authorization" = get_linear_api_key(),
      "Content-Type" = "application/json"
    ) |> 
    httr2::req_body_json(list(query = request_string)) |> 
    httr2::req_perform()

  response_body <- httr2::resp_body_json(response)
  return(dplyr::as_tibble(response_body))
}