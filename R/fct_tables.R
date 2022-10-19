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
  df <- pool |>
    dplyr::tbl(glue::glue("pbp_ryoe_graph_{input_df}")) |>
    dplyr::filter(.data$plot_name %in% input_player) |>
    dplyr::group_by(.data$plot_name) |>
    dplyr::slice_max(.data$rush_number, n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Rushes = .data$rush_number,
      YPC = round(.data$rush_yards / .data$rush_number, 2),
      `xRY/Att` = round(.data$cumulative_xry / .data$rush_number, 2),
      `RYOE/Att` = round(.data$cumulative_ryoe / .data$rush_number, 2)
    ) |>
    dplyr::select(
      Player = .data$plot_name,
      .data$team,
      .data$Rushes,
      .data$YPC,
      .data$`xRY/Att`,
      .data$`RYOE/Att`
    ) |>
    dplyr::arrange(dplyr::desc(.data$`RYOE/Att`)) |>
    dplyr::collect() |>
    gt::gt() |>
    gt::tab_header(
      title = gt::md(glue::glue("**Rushing Yards Over Expected - {input_data_type}**"))
      # subtitle = glue("RBs in season | {years_out_of_high_school} {year_wording} out of high school | Sorted by YPTP")
    ) |>
    gt::tab_spanner(
      label = "Player Info",
      columns = c(.data$Player, .data$team)
    ) |>
    gt::tab_spanner(
      label = "Metrics",
      columns = c(
        .data$Rushes,
        .data$YPC,
        .data$`xRY/Att`,
        .data$`RYOE/Att`
      )
    ) |>
    cfbplotR::gt_fmt_cfb_logo(columns = "team") |>
    # gt_merge_stack(col1= player, col2 = rating) |>
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
    gt::tab_footnote(
      footnote = "Yards per Carry",
      locations = gt::cells_column_labels(
        columns = .data$YPC
      )
    ) |>
    gt::tab_footnote(
      footnote = "Expected Rushing Yards per Attempt",
      locations = gt::cells_column_labels(
        columns = .data$`xRY/Att`
      )
    ) |>
    gt::tab_footnote(
      footnote = "Rushing Yards Over Expected per Attempt",
      locations = gt::cells_column_labels(
        columns = .data$`RYOE/Att`
      )
    ) |>
    # tab_source_note(
    #   source_note = md("***Note:*** Percentile, in grey below value, is based on historical P5 values of drafted players from 2014-2020")
    # ) |>
    gt::tab_source_note(
      source_note = gt::md("**Table:** @JerrickBackous / @campus2canton")
    ) |>
    gt::tab_source_note(
      source_note = gt::md("**Data:** @CFB_Data with @cfbfastR")
    ) |>
    # data_color(columns = "yptp",
    #            colors = pal
    # ) |>
    # gt_hulk_col_numeric(ppr_points, trim = TRUE) |>
    # gt_hulk_col_numeric(reception_ms, trim = TRUE) |>
    # gt_hulk_col_numeric(dominator_rtg, trim = TRUE) |>
    # gt_hulk_col_numeric(Backfield.Dominator.Rating, trim = TRUE) |>
    # gt_hulk_col_numeric(rush_yards_over_expected_per_attempt, trim = TRUE) |>
    gtExtras::gt_hulk_col_numeric(.data$`RYOE/Att`, trim = TRUE) |>
    gtExtras::gt_theme_538() |>
    gt::tab_options(table.font.color = "#6c0000") #|>
  # tab_options(footnotes.font.size = 12)

  return(df)
}
