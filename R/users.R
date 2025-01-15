#' Returns a tibble with your id, name, and email. Useful for checking if you have successfully authenticated with the linear API.
#'
#' @return tibble
#' @importFrom methods new
#' @export
me <- function(){
  link <- 'https://api.linear.app/graphql'
  conn <- ghql::GraphqlClient$new(url = link, headers = list("Authorization" = get_linear_api_key()))

  qry <- ghql::Query$new()

  qry$query("me", 
  '{
    viewer {
      id
      name
      email
    }
  }')

  res <- conn$exec(qry$queries$me)

  result <- jsonlite::fromJSON(conn$exec(new$link), flatten = TRUE)
  return(dplyr::as_tibble(result$data$viewer))
}