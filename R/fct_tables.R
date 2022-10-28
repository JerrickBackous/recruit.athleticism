#' Create Athletic Comps Table
#'
#' @param input_df This is a dataframe to be used
#' @param input_player Character string of players
#'
#' @return Athletic Comps Table
#'
#' @importFrom rlang .data
#' @importFrom dplyr tbl filter group_by slice_max ungroup mutate select arrange desc collect
#' @importFrom gt gt tab_header tab_spanner tab_footnote tab_source_note
#' @importFrom glue glue
#' @importFrom cfbplotR gt_fmt_cfb_logo
#' @importFrom gtExtras gt_hulk_col_numeric gt_theme_538 gt_merge_stack
#'
#' @export
athleticism_table <- function(input_df, input_player) {

  comp_player <- input_df |>
    dplyr::filter(.data$plot_name %in% input_player)

  df <- input_df |>
    dplyr::rowwise() |>
    dplyr::mutate(Comp = ((1-(((max(ifelse(is.na(comp_player$Height), 0, comp_player$Height), ifelse(is.na(.data$Height), 0, .data$Height)) -
                                 min(ifelse(is.na(comp_player$Height), 0, comp_player$Height), ifelse(is.na(.data$Height), 0, .data$Height)))*2) +
                                ((max(ifelse(is.na(comp_player$Weight), 0, comp_player$Weight), ifelse(is.na(.data$Weight), 0, .data$Weight)) -
                                   min(ifelse(is.na(comp_player$Weight), 0, comp_player$Weight), ifelse(is.na(.data$Weight), 0, .data$Weight)))*1.5) +
                                (max(ifelse(is.na(comp_player$Speed), 0, comp_player$Speed), ifelse(is.na(.data$Speed), 0, .data$Speed)) -
                                   min(ifelse(is.na(comp_player$Speed), 0, comp_player$Speed), ifelse(is.na(.data$Speed), 0, .data$Speed))) +
                                (max(ifelse(is.na(comp_player$Ath), 0, comp_player$Ath), ifelse(is.na(.data$Ath), 0, .data$Ath)) -
                                   min(ifelse(is.na(comp_player$Ath), 0, comp_player$Ath), ifelse(is.na(.data$Ath), 0, .data$Ath))) +
                                ifelse(is.na(comp_player$Burst), 0, (max(ifelse(is.na(comp_player$Burst), 0, comp_player$Burst), ifelse(is.na(.data$Burst), 0, .data$Burst)) -
                                                                       min(ifelse(is.na(comp_player$Burst), 0, comp_player$Burst), ifelse(is.na(.data$Burst), 0, .data$Burst)))) +
                                ifelse(is.na(comp_player$Agility), 0, (max(ifelse(is.na(comp_player$Agility), 0, comp_player$Agility), ifelse(is.na(.data$Agility), 0, .data$Agility)) -
                                                                         min(ifelse(is.na(comp_player$Agility), 0, comp_player$Agility), ifelse(is.na(.data$Agility), 0, .data$Agility)))) +
                                ifelse(is.na(comp_player$Power), 0, (max(ifelse(is.na(comp_player$Power), 0, comp_player$Power), ifelse(is.na(.data$Power), 0, .data$Power)) -
                                                                       min(ifelse(is.na(comp_player$Power), 0, comp_player$Power), ifelse(is.na(.data$Power), 0, .data$Power)))))) +
                            ifelse(comp_player$Position == .data$Position, 0.25, -1.0) -
                            (0.6) -
                            ifelse(is.na(.data$Size), 1, 0) -
                            ifelse(is.na(.data$Speed), 1, 0) +
                            ifelse(comp_player$player == .data$player,0.75, 0) +
                            ifelse(!is.na(comp_player$`ATH%`) & !is.na(.data$Ath), 0.3, 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(Comp = (.data$Comp - min(.data$Comp)) / (max(.data$Comp) - min(.data$Comp)),
                  Ath = .data$`ATH%`) |>
    dplyr::select(.data$player, .data$college, .data$`Draft Year`, .data$Position,
                  Similarity = .data$Comp, .data$display_height, .data$display_weight,
                  .data$Ath, .data$Speed, .data$Burst, .data$Agility, .data$Power) |>
    dplyr::slice_max(.data$Similarity, n = 9) |>
    dplyr::filter(.data$Similarity != max(.data$Similarity)) |>
    dplyr::mutate(dplyr::across(where(is.double), round, 2),
                  merge_size = paste0(.data$display_height, " | ", .data$display_weight)) |>
    dplyr::select(-.data$display_height, -.data$display_weight) |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md(glue::glue("**{comp_player$player} Athletic Comps**"))
    ) |>
    gt::tab_spanner(
      label = "Player Info",
      columns = c(.data$player, .data$merge_size, .data$college, .data$`Draft Year`, .data$Position)
    ) |>
    gt::tab_spanner(
      label = "Metrics",
      columns = c(
        .data$Similarity,
        .data$Ath,
        .data$Speed,
        .data$Burst,
        .data$Agility,
        .data$Power
      )
    ) |>
    cfbplotR::gt_fmt_cfb_logo(columns = "college") |>
    gt::cols_label(Position = "Pos",
                   college = "") |>
    gtExtras::gt_merge_stack(col1= "player", col2 = "merge_size", palette = c("#6c0000", "dark grey")) |>
    gt::tab_source_note(
      source_note = gt::md("**Table:** @JerrickBackous / @campus2canton")
    ) |>
    gt::tab_source_note(
      source_note = gt::md("**Data:** @bigWRguy")
    ) |>
    gtExtras::gt_hulk_col_numeric(.data$Similarity, trim = TRUE) |>
    gtExtras::gt_theme_538() |>
    gt::tab_options(heading.align = "center",
                    heading.title.font.size = 24,
                    table.font.color = "#6c0000")

  return(df)
}


#' Table of player metrics
#'
#' @param input_df data frame
#' @param input_season selected seasons
#'
#' @return player metric table
#' @importFrom rlang .data
#' @importFrom dplyr filter select mutate across
#' @export
leaderboard_table <- function(input_df, input_season) {

  df <- input_df |>
    dplyr::filter(.data$`Draft Year` %in% input_season) |>
    dplyr::mutate(Ath = .data$`ATH%`) |>
    dplyr::select(Player = .data$player, College = .data$college, .data$Position,
                  Class = .data$`Draft Year`, Height = .data$display_height,
                  Weight = .data$display_weight, .data$Ath, .data$Speed,
                  .data$Burst, .data$Agility, .data$Power) |>
    dplyr::mutate(dplyr::across(where(is.double), round, 3))

  return(df)
}
