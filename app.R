library(bslib)
source("helpers.R")

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Noto Sans") # Specify your custom font here
  ),
  
  titlePanel("Brewery Information for Virignians, North Carolinians, and Visitors :)"),
  
  tags$br(),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.main_tabs == 'About'",
        h5("Purpose/Guide of Each Tab:"),
        
        tags$br(),
        
        strong("About Tab:"),
        p("Discusses the overall purpose of the app as well as the data source information."),
        
        tags$br(),
        
        strong("Data Download Tab:"),
        p("Querying the API for the data. Essentially, you will be choosing the data in
          which you want to look more into using this app."),
        
        tags$br(),
        
        strong("Data Exploration Tab:"),
        p("The place in which you will look at the data you selected in the data download tab.
          You can look at numerical summaries as well as build interactive plots to visualize
          and understand the chosen data.")
        
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Data Download'",
        selectInput("state", "Choose a state", choices = c("North Carolina", "Virginia"))
      ), # close conditional panel
    ), # close sidebar panel
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "About",
          
          tags$br(),
          
          strong(h2("Purpose of the App:")),
          p("This app is here for you to be able to gather information on breweries in the area.
            Between the states of North Carolina and Virginia, there is plenty of growth with
            population and business opportunity. Acknowledging this, this app was designed with
            you in mind: the visitors and newcomers to these two lovely states!"),
          
          p("As a very recent newcomer from Virginia to North Carolina, I remember how it felt to
            not only know nothing about the are, but also not know how to get started in fixing that.
            Here is (hopefully) part of your solution!"),
          
          h2("Data Information and its Source:"),
          p("This data has been collected from the "),
          
          column(width = 12, align = "center",
                 img(src = "beer_buddies.png", height = "400px")
          )
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
  bs_themer()
  data_input <- reactive({
    get_breweries(input$state, input$city, input$type)
  })
  
  output$brew_plot <- renderPlot({
    plot_breweries(data_input())
  })
}

shinyApp(ui, server)

