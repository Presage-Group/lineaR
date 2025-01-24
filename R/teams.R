#' Returns a tibble with the id and name of Teams in the organization
#'
#' @param api_url Defaults to the current linear api url, but can be changed 
#' 
#' @return tibble
#' @export
get_linear_teams <- function(api_url = "https://api.linear.app/graphql"){
  resp <- make_linear_api_request(
      "{ teams { nodes { id name } } }",
      api_url
    ) |> 
    httr2::resp_body_json()
    
  return(dplyr::as_tibble(resp$data$teams) |> tidyr::unnest_wider(nodes))
}