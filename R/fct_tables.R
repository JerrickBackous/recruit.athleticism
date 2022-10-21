#' Create RYOE Table
#'
#' @param input_df This is a dataframe to be used (career or year)
#' @param input_player Character string of players
#'
#' @return RYOE Table
#'
#' @importFrom rlang .data
#' @importFrom dplyr tbl filter group_by slice_max ungroup mutate select arrange desc collect
#' @importFrom gt gt tab_header tab_spanner tab_footnote tab_source_note
#' @importFrom glue glue
#' @importFrom cfbplotR gt_fmt_cfb_logo
#' @importFrom gtExtras gt_hulk_col_numeric gt_theme_538
#'
#' @export
athleticism_table <- function(input_df, input_player) {

  comp_player <- input_df |>
    dplyr::filter(.data$plot_name %in% input_player)

  df <- input_df |>
    dplyr::rowwise() |>
    dplyr::mutate(Comp = ((1-((max(ifelse(is.na(comp_player$Height), 0, comp_player$Height), ifelse(is.na(.data$Height), 0, .data$Height)) -
                                 min(ifelse(is.na(comp_player$Height), 0, comp_player$Height), ifelse(is.na(.data$Height), 0, .data$Height))) +
                                (max(ifelse(is.na(comp_player$Weight), 0, comp_player$Weight), ifelse(is.na(.data$Weight), 0, .data$Weight)) -
                                   min(ifelse(is.na(comp_player$Weight), 0, comp_player$Weight), ifelse(is.na(.data$Weight), 0, .data$Weight))) +
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
                            ifelse(comp_player$Position == .data$Position, 0.25, -0.5) -
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
      # subtitle = glue("RBs in season | {years_out_of_high_school} {year_wording} out of high school | Sorted by YPTP")
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
    gtExtras::gt_merge_stack(col1= "player", col2 = "merge_size") |>
    # gt_merge_stack(col1= yptp, col2 = yptp_perc) |>
    # gt_merge_stack(col1= Backfield.Dominator.Rating, col2 = bdr_perc) |>
    # gt_merge_stack(col1= dominator_rtg, col2 = dr_perc) |>
    # gt_merge_stack(col1= reception_ms, col2 = rec_share_perc) |>
    # gt_merge_stack(col1= rush_yards_over_expected_per_attempt, col2 = ryoe_perc) |>
    # cols_label(
    #   player = "Name",
    #   logo = "Team",
    #   team = "",
    #   year_played = "Year",
    #   height_feet = "Ht",
    #   weight = "Wt",
    #   ppr_points = "PPR Pts",
    #   reception_ms = "Rec Share (%)",
    #   dominator_rtg = "DR (%)",
    #   Backfield.Dominator.Rating = "BDR (%)",
    #   rush_yards_over_expected_per_attempt = "RYOE/Att",
    #   yptp = "YPTP"
    # ) |>
    # # fmt_symbol_first(column = w_dom_oe, symbol = "%", decimals = 1) |>
    # gt::tab_footnote(
    #   footnote = "Yards per Carry",
    #   locations = gt::cells_column_labels(
    #     columns = .data$YPC
    #   )
    # ) |>
    # gt::tab_footnote(
    #   footnote = "Expected Rushing Yards per Attempt",
    #   locations = gt::cells_column_labels(
    #     columns = .data$`xRY/Att`
    #   )
    # ) |>
    # gt::tab_footnote(
    #   footnote = "Rushing Yards Over Expected per Attempt",
    #   locations = gt::cells_column_labels(
    #     columns = .data$`RYOE/Att`
    #   )
    # ) |>
    # tab_source_note(
    #   source_note = md("***Note:*** Percentile, in grey below value, is based on historical P5 values of drafted players from 2014-2020")
    # ) |>
    gt::tab_source_note(
      source_note = gt::md("**Table:** @JerrickBackous / @campus2canton")
    ) |>
    gt::tab_source_note(
      source_note = gt::md("**Data:** @bigWRguy")
    ) |>
    # data_color(columns = "yptp",
    #            colors = pal
    # ) |>
    # gt_hulk_col_numeric(ppr_points, trim = TRUE) |>
    # gt_hulk_col_numeric(reception_ms, trim = TRUE) |>
    # gt_hulk_col_numeric(dominator_rtg, trim = TRUE) |>
    # gt_hulk_col_numeric(Backfield.Dominator.Rating, trim = TRUE) |>
    # gt_hulk_col_numeric(rush_yards_over_expected_per_attempt, trim = TRUE) |>
    gtExtras::gt_hulk_col_numeric(.data$Similarity, trim = TRUE) |>
    gtExtras::gt_theme_538() |>
    gt::tab_options(heading.align = "center",
                    heading.title.font.size = 24,
                    table.font.color = "#6c0000") #|>
  # tab_options(footnotes.font.size = 12)

  return(df)
}
