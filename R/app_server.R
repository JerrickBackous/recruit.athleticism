#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr tbl group_by summarize distinct arrange desc collect
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom sever sever
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom ggiraph renderGirafe girafe opts_selection opts_hover_inv
#' @importFrom gt render_gt
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom ggplot2 ggsave
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  sever::sever()

  athleticism_data <- reactive({
    c2cbucket <- aws.s3::get_bucket(bucket = "campus2canton", region = "")

    # athleticism_data <-
      aws.s3::s3readRDS(object = "app_data/recruit_athleticism.rds", bucket = c2cbucket, region = "")
  })

  observe({
    updateSelectizeInput(session,
                         "player_plot",
                         choices = athleticism_data()$plot_name,
                         selected = NULL,
                         server = TRUE
    )
  })

  seasons <- reactive({
    athleticism_data() |>
      dplyr::select(.data$`Draft Year`) |>
      dplyr::arrange(dplyr::desc(.data$`Draft Year`)) |>
      dplyr::distinct(.data$`Draft Year`)
  })

  observe({
    shinyWidgets::updatePickerInput(session,
                         "season_table",
                         choices = seasons(),
                         selected = max(seasons(), na.rm = TRUE):(max(seasons(), na.rm = TRUE) - 30)
    )
  })

  output$athleticism_plot_output <- ggiraph::renderGirafe({
      req(input$player_plot)
      ggiraph::girafe(
        ggobj = output_athleticism_plot(
          athleticism_data(),
          input$player_plot
        ),
        width_svg = 9,
        height_svg = 7,
        options = list(
          ggiraph::opts_selection(type = "single", only_shiny = FALSE),
          ggiraph::opts_hover_inv(css = "opacity:0.5")
        )
      )
    })

  output$download_athleticism_plot <- downloadHandler(
    filename = function() {
      paste0("athleticism_plot", ".png")
    },
    # content is a function with argument file.
    #   content writes the plot to the device
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = download_athleticism_plot(
                        athleticism_data(),
                        input$player_plot
                      ),
                      width = 16,
                      height = 9
      ) # for GGPLOT
    }
  )

  output$athleticism_table <- gt::render_gt({
      req(input$player_plot)
      athleticism_table <- athleticism_table(
        athleticism_data(),
        input$player_plot
      )
  })

  output$leaderboard_table <- reactable::renderReactable(
    reactable::reactable(
      {
        req(input$season_table)

        leaderboard_table(
          athleticism_data(),
          input$season_table
        )
      },
      filterable = TRUE,
      defaultColDef = reactable::colDef(sortNALast = TRUE),
      showPageSizeOptions = TRUE,
      defaultPageSize = 50
    )
  )

  # Stop the app timing out
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
}
