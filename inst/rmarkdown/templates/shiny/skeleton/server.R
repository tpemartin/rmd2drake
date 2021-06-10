# makecondition -----------

library(shiny)
library(ggplot2)



# server --------

server <- function(input, output){
output$distPlot <- shiny::renderPlot({
    ggplot(data = faithful) + geom_histogram(aes(x = eruptions), 
        bins = as.numeric(input$bins))
})
}
