source("helpers.R")

ui <- fluidPage(
  titlePanel("Brewery Information for Virignians, North Carolinians, and Visitors :)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a state", choices = c("North Carolina", "Virginia"))
    ), # close sidebar panel
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "About",
          strong(h2("The Purpose of the Site:")),
          p("This site is here for you to be able to gather information on breweries in the area.
            Between the states of North Carolina and Virginia, there is plenty of growth with
            population and business opportunity. Acknowledging this, we designed this website with
            you in mind: the visitors and newcomers."),
          p("As a very recent newcomer from Virginia to North Carolina, I remember how it felt to
            not know about the area as well as not know how to get started in fixing that. Here is
            your solution!"),
          h2("Data Information and its Source:"),
          p("This data has been collected from the "),
          h2("Purpose of Each Tab:"),
          p("ourpose"),
          h2("i need a pic here"),
        ),
        tabPanel(
          "Data Download",
          h3("Putting something here for now")
        ),
        tabPanel(
          "Data Exploration",
          plotOutput("brew_plot")
        )
      )
    )# close main panel
  ), # close sidebar layout

)

server <- function(input, output, session) {
  data_input <- reactive({
    get_breweries(input$state, input$city, input$type)
  })
  
  output$brew_plot <- renderPlot({
    plot_breweries(data_input())
  })
}

shinyApp(ui, server)

