library(bslib)
source("helpers.R")

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Noto Sans") # Specify your custom font here
  ),
  
  tags$br(),
  
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
        selectInput("choose_state", "Choose a state", choices = c("Both/No Selection",
                                                                  "North Carolina", "Virginia")),
        helpText("Note: you will not be eligible to select a city if you do not select a state."),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        uiOutput("city_ui"),
        uiOutput("helper_text"),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        uiOutput("type_ui"),
        
        # Column selector
        uiOutput("column_ui"),
        
        # Row filter (number of rows to keep)
        numericInput("n_rows", "Number of rows to show:", value = 10, min = 1),
        
        downloadButton("download_data", "Download CSV")
        
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
          
          hr(),
          
          h2("Data Information and its Source:"),
          
          p("This data has been collected from a website linked", 
            tags$a(href = "https://www.openbrewerydb.org/documentation#list-breweries",
                   "here.", target = "_blank"), 
            "This website allowed us to access this brewery data, and it has a lot of different
            options for looking at brewery data across the world. In this app, we have narrowed
            our analysis down to a few cities in North Carolina and Virginia. We did allow for
            all types of breweries to be brought down from the site, though!"
          ),
          
          column(width = 12, align = "center",
                 img(src = "beer_buddies.png", height = "400px")
          )
        ),
        tabPanel(
          "Data Download",
          
          tags$br(),
          
          tableOutput("filtered_table")
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

  # 1. Get filtered brewery data from API
  filtered_brewery_data <- reactive({
    get_breweries(
      state = if (input$state_input != "Both/No Selection") input$state_input else NULL,
      city  = if (!is.null(input$city_input) && input$city_input != "") input$city_input else NULL
    )
  })
  
  # Building the select input with the appropriate cities inside given the selected state
  output$city_ui <- renderUI({
    req(input$choose_state != "Both/No Selection")
    
    if (input$choose_state == "North Carolina"){

      checkboxGroupInput("choose_city", "Select all desired NC cities:",
                  choices = city_options_nc,
                  selected = city_options_nc)
    } else if (input$choose_state == "Virginia"){
      
      checkboxGroupInput("choose_city", "Select all desired VA cities:",
                  choices = city_options_va,
                  selected = city_options_va)
    }
    
  })
  
  output$helper_text <- renderUI({
    req(input$choose_state != "Both/No Selection")
    
    helpText("Note: this app will treat all cities being deselected the same way as all
             cities being selected (i.e. making no selection is equivalent to choosing
             all cities.)")
  })
  
  output$type_ui <- renderUI({
    
    # if state and city are both NULL
    type_choices <- get_breweries()$brewery_type |>
      unique() |>
      sort()
    
    
    if (input$choose_state == "North Carolina" || input$choose_state == "Virginia"){
      
      if (is.null(input$choose_city) || setequal(input$choose_city, city_options_nc) ||
          setequal(input$choose_city,city_options_va)){ # if none or all in NC or VA
        
        type_choices <- get_breweries(state = input$choose_state)$brewery_type |>
          unique() |>
          sort()
        
      } else if (input$choose_state == "Virginia" && !is.null(input$choose_city)){
        if (length(input$choose_city) == 1){
          
          type_choices <- get_breweries(state = input$choose_state,
                                        city = input$choose_city)$brewery_type |>
            unique() |>
            sort()
        } else if (length(input$choose_city) == 2){
          
          all_data <- map(input$choose_city, function(city) {
            get_breweries(state = input$choose_state, city = city)
          }) |> bind_rows()
          
          type_choices <- all_data$brewery_type |>
            unique() |>
            sort()
        }
      }
        

    
    }

    
    checkboxGroupInput("choose_type", "Choose brewery type(s):",
                       choices = type_choices,
                       selected = type_choices)
    
  })
  
  output$brew_plot <- renderPlot({
    plot_breweries(data_input())
  })
}

shinyApp(ui, server)

