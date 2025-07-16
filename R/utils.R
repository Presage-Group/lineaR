#' Stores your API key in your keyring.
#'
#' @param api_key Your personal linear API key. See README.md for information.
#'
#' @return nothing
#' @export
store_linear_api_key <- function(api_key){
  keyring::key_set_with_value("linear_api", password = api_key)
}

#' Get API key from env or keyring
#'
#' @return character
get_linear_api_key <- function(){
  env_key <- Sys.getenv("LINEAR_API_KEY", unset = NA)
  if (!is.na(env_key)) return(env_key)

  keyring::key_get("linear_api")
}

#' Make an API request
#'
#' @param request_string GraphQL API query.
#' @param api_url Defaults to the current linear api url, but can be changed.
#'
#' @return httr2 response
make_linear_api_request <- function(request_string, api_url = "https://api.linear.app/graphql"){
  response <- httr2::request(api_url) |>
    httr2::req_headers(
      "Authorization" = get_linear_api_key(),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(query = request_string)) |>
    httr2::req_perform()

  return(response)
}
