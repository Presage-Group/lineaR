#' Returns a tibble with the issues for a given team_id
#'
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want. 
#' 
#' @return tibble
#' @importFrom methods new
#' @export
get_linear_issues <- function(team_id){
  link <- 'https://api.linear.app/graphql'
  conn <- ghql::GraphqlClient$new(url = link, headers = list("Authorization" = get_linear_api_key()))

  qry <- ghql::Query$new()

  qry$query("issues", 
  paste0('{
  team(id: "',  team_id, '") {
    id
    name

    issues {
      nodes {
        id
        title
        description
        assignee {
          id
          name
        }
        createdAt
        archivedAt
      }
    }
  }
  }'))

  res <- conn$exec(qry$queries$issues)

  result <- jsonlite::fromJSON(res, flatten = TRUE)
  return(dplyr::as_tibble(result$data$team$issues$nodes))
}

#' Creates a new issue within a team specified using the team's id. 
#'
#' @param title The issue title
#' @param description The issue description
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want. 
#' 
#' @return tibble
#' @importFrom methods new
#' @export
create_linear_issue <- function(title, description, team_id){
  graphql_query <- paste0('mutation {
    issueCreate(input: {
      title: "', title, '",
      description: "', description, '",
      teamId: "', team_id, '"
    }) {
      issue {
        id
        url
      }
    }
  }')

  body <- list(query = graphql_query)

  response <- httr2::request("https://api.linear.app/graphql") |> 
    httr2::req_headers(
      "Authorization" = get_linear_api_key(),
      "Content-Type" = "application/json"
    ) |> 
    httr2::req_body_json(body) |> 
    httr2::req_perform()

  response_body <- httr2::resp_body_json(response)
  return(dplyr::as_tibble(response_body))
}
