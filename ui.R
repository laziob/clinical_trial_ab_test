#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

ui <- fluidPage(
    titlePanel('Clinical A/B Test Simulator'),
    sidebarLayout(
        sidebarPanel(
            width=2,
            numericInput(
                'p_a',
                'Baseline Probability',
                min = 0.00001,
                max = 0.99999,
                value = 0.015
            ),
            numericInput(
                'min_lift',
                'Minimum Desirable Effect (Lift)',
                min = 0.00001,
                max = 1,
                value = 0.1
            ),
            numericInput(
                'alpha',
                'alpha (Type 1 Error)',
                min = 0.00001,
                max = 0.99999,
                value = 0.05
            ),
            numericInput(
                'beta',
                'beta (Type 2 Error)',
                min = 0.00001,
                max = 0.99999,
                value = 0.1
            ),
            radioButtons("hay_efecto",
                         "Hay Efecto Real?",
                         choices = list("Si" = TRUE,
                                        "No" = FALSE)
                         ),
            fluidRow(
                column(width=4,
                       actionButton(
                           'button',
                           'Calculate'
                           )
                       )
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Ejemplo",
            fluidRow(
                column(width = 8,
                       plotlyOutput("agile_plot")),
                column(width = 4,
                       tableOutput('table1'),
                       )),
            fluidRow(
                column(width = 12,
                       plotlyOutput("fixed_plot"))
            )
                )
            )
        )
    )
)
