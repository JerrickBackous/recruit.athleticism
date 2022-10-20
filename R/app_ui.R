#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_google
#' @importFrom waiter use_waiter autoWaiter spin_hexdots transparent
#' @importFrom sever useSever
#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom ggiraph girafeOutput
#' @importFrom gt gt_output
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(
        bg = "#FFF",
        fg = "#6c0000",
        primary = "#6c0000",
        secondary = "#6c0000",
        base_font = bslib::font_google("Roboto")
      ),
      tags$head(
        tags$style(
          type = "text/css",
          ".well {
               background-color: #FFF;
               }",
          ".btn-light {
               color: #984D4D;
               background-color: #FFF;
               border-color: #C49999;
               }",
          "text {
               font-family: Roboto
               }"
        )
      ),
      waiter::use_waiter(),
      waiter::autoWaiter(
        html = waiter::spin_hexdots(),
        color = waiter::transparent(0.3)
      ),
      sever::useSever(),
      # p(style = "text-align: right;", paste0("Updated: ", update_date())),
      # First tab content
      fluidRow(
        column(
          width = 4,
          selectizeInput("player_plot",
                         label = "Select Player(s)",
                         choices = NULL,
                         multiple = FALSE
          )
        ),
        column(
          width = 4,
          br(),
          downloadButton(
            outputId = "download_athleticism_plot",
            label = "Download Plot"
          ),
          align = "right",
          br(),
          p("*Use this for a high quality plot")
        )
      ),
      fluidRow(
        column(
          width = 6,
          maximizable = TRUE,
          ggiraph::girafeOutput("athleticism_plot_output")
        ),
        column(
          width = 6,
          maximizable = TRUE,
          gt::gt_output(outputId = "athleticism_table")
        ))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "recruit_athleticism"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
