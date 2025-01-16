#' Returns a tibble with the id and name of Teams in the organization
#'
#' @return tibble
#' @importFrom methods new
#' @export
get_linear_teams <- function(){

  result <- make_linear_api_request('{ "query" : "{ teams { nodes { id name } } }" }')

  return(dplyr::as_tibble(result$data$teams$nodes))
}