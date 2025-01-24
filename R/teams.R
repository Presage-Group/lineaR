#' Returns a tibble with the id and name of Teams in the organization
#'
#' @param api_url Defaults to the current linear api url, but can be changed 
#' 
#' @return tibble
#' @export
get_linear_teams <- function(api_url = "https://api.linear.app/graphql"){
  return(
    make_linear_api_request(
      '{ "query" : "{ teams { nodes { id name } } }" }',
      api_url
    )
  )
}