#' Returns a tibble with the issues for a given team_id
#'
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want.
#' @param api_url Defaults to the current linear api url, but can be changed
#'
#' @return tibble
#' @export
get_linear_issues <- function(team_id, api_url = "https://api.linear.app/graphql"){
  graphql_query <- glue::glue('{
  team(id: "[team_id]") {
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
  }', .open = "[", .close = "]")

  resp <- make_linear_api_request(graphql_query, api_url)

  return(dplyr::as_tibble(httr2::resp_body_json(resp)) |>
    tidyr::unnest_wider(data) |>
    tidyr::unnest_wider(issues) |>
    tidyr::unnest_longer(nodes) |>
    dplyr::select(nodes) |>
    tidyr::unnest_wider(nodes))
}

#' Creates a new issue within a team specified using the team's id.
#'
#' @param title The issue title
#' @param description The issue description
#' @param team_id The id for the team. Use get_linear_teams() to find the id of the team you want.
#' @param api_url Defaults to the current linear api url, but can be changed
#'
#' @return tibble
#' @export
create_linear_issue <- function(
  title="New issue via lineaR",
  description=" ",
  team_id,
  api_url = "https://api.linear.app/graphql"){

  graphql_query <- glue::glue('mutation {
    issueCreate(input: {
      title: "[title]",
      description: "[description]",
      teamId: "[team_id]"
    }) {
      issue {
        id
        url
      }
    }
  }', .open = "[", .close = "]")

  resp <- make_linear_api_request(graphql_query, api_url = api_url)

  return(httr2::resp_body_json(resp) |>
    dplyr::as_tibble() |>
    tidyr::unnest_wider(data) |>
    tidyr::unnest_wider(issue))
}
