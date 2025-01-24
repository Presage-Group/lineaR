#' Returns a tibble with your id, name, and email. Useful for checking if you have successfully authenticated with the linear API.
#'
#' @param api_url Defaults to the current linear api url, but can be changed.
#' 
#' @return tibble
#' @export
get_linear_current_user <- function(api_url){
  return(
    make_linear_api_request(
      '{ "query": "{ viewer { id name email } }" }',
      api_url
    )
  )
}