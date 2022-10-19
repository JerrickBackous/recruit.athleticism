#' Create RYOE Plot
#'
#' @param input_df This is the data
#' @param input_player Character string of players
#'
#' @return plot
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom dplyr tbl filter select group_by slice_max collect
#' @importFrom magick image_read
#' @importFrom cowplot ggdraw draw_plot draw_image
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggiraph geom_line_interactive
#' @importFrom scales alpha
#'
#' @export
output_athleticism_plot <- function(input_df, input_player) {

  input_df <- input_df |>
    dplyr::filter(plot_name %in% input_player) |>
    dplyr::select(player, college, Height, Weight...12, Ath, Speed, Burst, Agility, Power, plot_name) |>
    tidyr::pivot_longer(c(Ath, Speed, Burst, Agility, Power),names_to = "Metrics", values_to = "Value")

  p1 <- ggplot2::ggplot(
    input_df,
    ggplot2::aes(
      x = .data$Value,
      y = .data$Metrics,
      fill = .data$Metrics
      )
    ) +
    ggplot2::geom_col(
      alpha = .5
    ) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#6c0000"),
      plot.title = ggplot2::element_text(size = 18, face = "bold", margin = ggplot2::margin(10, 0, 20, 0)),
      plot.subtitle = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 14, color = "#6c0000"),
      axis.title = ggplot2::element_text(size = 14),
      plot.caption = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      x = "Value",
      y = "",
      caption = paste0(
        "Figure: @JerrickBackous | @campus2canton\n Data: @CFB_Data with @cfbfastR"
      )
    ) +
    ggplot2::scale_x_continuous() +
    ggplot2::guides(color = "none", fill = "none") +
    ggplot2::ggtitle(
      glue::glue("{input_player}")
    )

  c2c <- magick::image_read(
    system.file("app/www/c2c_logo.png", package = "collegeryoeapp")
  )

  p2 <- cowplot::ggdraw() +
    cowplot::draw_plot(p1) +
    cowplot::draw_image(
      c2c,
      x = 1.01,
      y = .99,
      hjust = 1,
      vjust = 1,
      width = 0.10,
      height = 0.10
    )

  return(p2)
}

#' Create Downloadable Plot
#'
#' @param input_df This is the dataframe
#' @param input_player Character string of players
#'
#' @return downloadable plot
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom dplyr filter group_by top_n
#' @importFrom magick image_read
#' @importFrom cowplot ggdraw draw_plot draw_image
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggiraph geom_line_interactive
#' @importFrom scales alpha
#'
#' @export
download_athleticism_plot <- function(input_df, input_player) {

  input_df <- input_df |>
    dplyr::filter(plot_name %in% input_player) |>
    dplyr::select(player, college, Height, Weight...12, Ath, Speed, Burst, Agility, Power, plot_name) |>
    tidyr::pivot_longer(c(Ath, Speed, Burst, Agility, Power),names_to = "Metrics", values_to = "Value")


  p1 <- ggplot2::ggplot(
    input_df,
    ggplot2::aes(
      x = .data$Value,
      y = .data$Metrics,
      fill = .data$Metrics
    )
  ) +
    ggplot2::geom_col(
      alpha = .5
    ) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#6c0000"),
      plot.title = ggplot2::element_text(size = 32, face = "bold", margin = ggplot2::margin(10, 0, 20, 0)),
      plot.subtitle = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 16, color = "#6c0000"),
      axis.title = ggplot2::element_text(size = 18),
      plot.caption = ggplot2::element_text(size = 14),
      plot.background = ggplot2::element_rect(fill = "white"),
    ) +
    ggplot2::labs(
      x = "Values",
      y = "",
      caption = paste0(
        "Figure: @JerrickBackous | @campus2canton\n Data: @CFB_Data with @cfbfastR"
      )
    ) +
    ggplot2::scale_x_continuous() +
    ggplot2::guides(color = "none", fill = "none") +
    ggplot2::ggtitle(
      glue::glue("{input_player}")
    )

  c2c <- magick::image_read(
    system.file("app/www/c2c_logo.png", package = "collegeryoeapp")
  )

  p2 <- cowplot::ggdraw() +
    cowplot::draw_plot(p1) +
    cowplot::draw_image(
      c2c,
      x = 1.01,
      y = .995,
      hjust = 1,
      vjust = 1,
      width = 0.08,
      height = 0.08
    )

  return(p2)
}
