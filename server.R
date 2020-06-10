#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source('utilities.R')


# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$button, {
        req(
            input$p_a,
            input$min_lift,
            input$alpha,
            input$beta,
            input$hay_efecto,
        )
        if(
            input$p_a >= 0 &&
            input$min_lift > 0 &&
            input$alpha >= 0 &&
            input$beta > 0
        ) {
            p_a <- isolate({input$p_a})
            min_lift <- isolate({input$min_lift})
            alpha <- isolate({input$alpha})
            beta <- isolate({input$beta})
            hay_efecto <- isolate({input$hay_efecto})
            res <- isolate({
                agile_sim_plot(p_a = p_a,
                               min_lift = min_lift,
                               alpha = alpha,
                               beta = beta,
                               hay_efecto = hay_efecto)
            })
            fixed_res <- isolate({
                fixed_sim_plot(p_a = p_a,
                               min_lift = min_lift,
                               alpha = alpha,
                               beta = beta,
                               hay_efecto = hay_efecto)
                
            })
            
            }
        
        output$table1 <- renderTable({res$data})
        output$agile_plot <- renderPlotly({res})
        output$fixed_plot <- renderPlotly({fixed_res})
    })
}