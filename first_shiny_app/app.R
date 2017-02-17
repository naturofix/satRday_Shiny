#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(southafricastats)
library(ggplot2)
library(dplyr)
library(DT)
str(mortality_zaf)
head(mortality_zaf)
province_names = unique(mortality_zaf$province)
province_names
?selectInput

mortality = mortality_zaf %>% filter(indicator != 'All causes' & indicator != 'Other natural causes')
nc = mortality_zaf %>% filter(indicator == 'Other natural causes')
## code above this point is no reactive


# code within the app
ui <- fluidPage(
   
   # Application title
   titlePanel("South Africa"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30)
        selectInput(inputId = 'province',
                    label = 'Choose a Province',
                    choices = unique(mortality$province),
                    selected = 'Gauteng',
                    multiple = TRUE
                    ),
        checkboxInput(inputId = 'showtable',
                      label = 'Show table?',
                      value = FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("LinePlot"),
         dataTableOutput("mortalityTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$LinePlot <- renderPlot({
     mortality %>%
       filter(province %in% input$province) %>%
       ggplot(aes(year,deaths,colour=indicator)) + 
       facet_wrap(~province,scales = 'free') +
       geom_line(alpha = 0.8, size = 1.5) +
       theme_minimal(base_size = 18)
     # nc %>%
     #   filter(province == input$province) %>%
     #   ggplot(aes(year,deaths,colour=indicator)) + 
     #   geom_line(alpha = 0.8, size = 1.5) +
     #   theme_minimal(base_size = 18)
     
     
   })
   output$mortalityTable <- renderDataTable({
     if(input$showtable){
     datatable(mortality %>%
       filter(province %in% input$province))}
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

