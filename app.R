library(bslib)
library(tidyverse)
library(rlang)
source("helpers.R")

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    base_font = font_google("Noto Sans")
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
        
        # dynamic options for city based on state selection with helper text to pop up if needed
        uiOutput("city_ui"),
        uiOutput("helper_text"),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        # dynamic brewery type options
        uiOutput("type_ui"),
        
        # specifying the desired columns for table and .csv
        uiOutput("column_ui"),
        
        # specifying the desired rows to keep for table and .csv
        uiOutput("n_rows_ui"),
        
        downloadButton("download_data", "Download CSV")
        
      ), # close conditional panel
      
      conditionalPanel(
        condition = "input.main_tabs == 'Data Exploration'",
        
        # Always visible
        selectInput("plot_type", "Choose a plot type:",
                    choices = c("None", "Bar Plot", "Scatter Plot", "Boxplot")),
        
        # Show other inputs only if plot_type is chosen
        conditionalPanel(
          condition = "input.plot_type != 'None'",
          selectInput("x_var", "X-axis variable:", choices = NULL),
          
          # Y variable input not used if barplot is chosen, so not displated in this case
          conditionalPanel(
            condition = "input.plot_type != 'Bar Plot'",
            selectInput("y_var", "Y-axis variable:", choices = NULL)
          ),
          
          selectInput("color_var", "Color by (optional):", choices = c(None = "None")),
          selectInput("summary_var1", "First Variable (Rows of Contingency Table):", choices = NULL),
          selectInput("summary_var2", "Second Variable (Columns of Contingency Table):", choices = NULL),
          
          helpText("Note: I did the project one part at a time, so I did not realize that not having a single
                   numeric variable would pose a problem until it was too late. I made a contingency table
                   instead of a summary option, since count is really the only choice. Hope that is okay.")

        )
      ) # close conditional panel
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
          
          # center and put the image on app
          column(width = 12, align = "center",
                 img(src = "beer_buddies.png", height = "400px")
          )
        ),
        tabPanel(
          "Data Download",
          
          tags$br(),
          
          # data table
          tableOutput("filtered_table")
        ),
        tabPanel(
          "Data Exploration",
          
          tags$br(),
          
          plotOutput("brew_plot"),
              
          tags$hr(),
          
          h4("Contingency Table:"),
          tableOutput("contingency")
            )
          )
        )
      ) # close main panel
    ) # close sidebar layout


server <- function(input, output, session) {
  
  # reactive data source to let user query the API
  data_input <- reactive({
    req(input$choose_state)
    
    # for simplicity
    types <- input$choose_type
    cities <- input$choose_city
    
    # if no state is explicitly stated, show all breweries in both states of all types
    if (input$choose_state == "Both/No Selection") {
      all_data <- get_breweries(state = "Both/No Selection")
      
      if (!is.null(input$choose_type) && length(input$choose_type) > 0) {
        all_data <- filter(all_data, brewery_type %in% input$choose_type)
      }
      
      return(all_data)
    }
    
    # If no city is selected, treat it as "select all" for that state
    if (is.null(cities) || length(cities) == 0) {
      if (input$choose_state == "North Carolina") {
        cities <- city_options_nc
      } else if (input$choose_state == "Virginia") {
        cities <- city_options_va
      }
    }
    
    # Get data for each city and bind them
    all_data <- map_df(cities, function(city) {
      get_breweries(state = input$choose_state, city = city)
    })
    
    # Filter by brewery type if selected
    if (!is.null(types) && length(types) > 0 && "brewery_type" %in% names(all_data)) {
      all_data <- filter(all_data, brewery_type %in% types)
    }
    
    all_data
  })
  
  
  # putting the appropriate cities ONLY into the checkbox group input
  output$city_ui <- renderUI({
    req(input$choose_state != "Both/No Selection") # if no state selected, no city can be selected
    
    # checkbox being built for each state selection
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
  
  # text explaining that not selecting any state is the same as selecting all available
  output$helper_text <- renderUI({
    req(input$choose_state != "Both/No Selection")
    
    helpText("Note: this app will treat all cities being deselected the same way as all
             cities being selected (i.e. making no selection is equivalent to not having
             a city preference.)")
  })
  
  # dynamically change the options available to select for type based on state and city
  output$type_ui <- renderUI({
    
    # if "Both/No Selection" is chosen for state, all brewery types are shown for NC and VA
    if (input$choose_state == "Both/No Selection"){
      type_choices <- get_breweries(state = "Both/No Selection")$brewery_type |>
        unique() |>
        sort()
      
      # or if one state is chosen...
    } else if (input$choose_state == "North Carolina" || input$choose_state == "Virginia"){
      
      # and all (or none) of the cities are selected for that state...
      if (is.null(input$choose_city) || setequal(input$choose_city, city_options_nc) ||
          setequal(input$choose_city,city_options_va)){ # if none or all in NC or VA
        
        # the type options are those available for the selected state
        type_choices <- get_breweries(state = input$choose_state)$brewery_type |>
          unique() |>
          sort()
        
        # if the state is Virginia (3 options here) and at least one city is selected...
      } else if (input$choose_state == "Virginia" && !is.null(input$choose_city)){
        
        # if one city is selected...
        if (length(input$choose_city) == 1){
          
          # the type options are those in that one city
          type_choices <- get_breweries(state = input$choose_state,
                                        city = input$choose_city)$brewery_type |>
            unique() |>
            sort()
          
          # if 2 Virginia cities are selected...
        } else if (length(input$choose_city) == 2){
          
          # we go one by one through the cities and get separate dfs with all breweries for wach city
          all_data <- map(input$choose_city, function(city) {
            get_breweries(state = input$choose_state, city = city)
          }) |> bind_rows() # then put them together to have all the breweries in those cities together
          
          # then the type options are the sorted, unique brewery types from this data frame
          type_choices <- all_data$brewery_type |>
            unique() |>
            sort()
        }
      }
        

    
    }

    # finally, the checkbox is made with the appropriate type options in it
    checkboxGroupInput("choose_type", "Choose brewery type(s):",
                       choices = type_choices,
                       selected = type_choices)
    
  })
  
  # the data table is made with the selected amount of rows and columns
  output$filtered_table <- renderTable({
    req(input$selected_columns)
    req(input$n_rows)
    head(data_input()[, input$selected_columns, drop = FALSE], input$n_rows)
  })
  
  # the columns checkbox for the data table
  output$column_ui <- renderUI({
    req(data_input())
    
    checkboxGroupInput(
      "selected_columns",
      "Select columns to display:",
      choices = names(data_input()),
      selected = names(data_input())
    )
  })
  
  # dynamically change the rows able to be shown
  output$n_rows_ui <- renderUI({
    req(data_input())
    
    max_rows <- nrow(data_input())
    
    numericInput("n_rows", "Number of rows to show:",
                 value = max_rows,
                 min = 1,
                 max = max_rows)
  })
  
  # the ability to download the selected data as a .csv file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("brewery_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$selected_columns)
      req(input$n_rows)
      filtered <- head(data_input()[, input$selected_columns, drop = FALSE], input$n_rows)
      write.csv(filtered, file, row.names = FALSE)
    }
  )
  
  observe({
    data <- data_input()
    
    updateSelectInput(session, "x_var", choices = names(data))
    updateSelectInput(session, "y_var", choices = names(data))
    updateSelectInput(session, "color_var", choices = c(None = "None", names(data())))
    updateSelectInput(session, "sum_var", choices = c("Count", "Five Number Summary"))
    
  })
  
  # plot for tab 3
  output$brew_plot <- renderPlot({
    req(input$plot_type, input$x_var)
    data <- data_input()
    
    # aes mapping
    aes_mapping <- aes_string(x = input$x_var)
    
    if (input$plot_type != "Bar Plot") {
      req(input$y_var)
      aes_mapping <- aes_string(x = input$x_var, y = input$y_var)
    }
    
    # if "color by" is selected, add a color
    if (input$color_var != "None") {
      aes_mapping$colour <- sym(input$color_var)
    }
    
    # base plot
    g <- ggplot(data, mapping = aes_mapping)
    
    # Add geom based on plot type
    if (input$plot_type == "Bar Plot") {
      g <- g + geom_bar()
    } else if (input$plot_type == "Scatter Plot") {
      g <- g + geom_point()
    } else if (input$plot_type == "Boxplot") {
      g <- g + geom_boxplot()
    }
    
    g + theme_minimal()
  })
  
  
  
  observe({
    data <- data_input()
    var_names <- names(data)
    
    updateSelectInput(session, "x_var", choices = var_names)
    updateSelectInput(session, "y_var", choices = var_names)
    updateSelectInput(session, "color_var", choices = c(None = "None", var_names))
    updateSelectInput(session, "summary_var1", choices = var_names)
    updateSelectInput(session, "summary_var2", choices = var_names)
  })
  
  
  output$contingency <- renderTable({
    data <- data_input()
    req(input$summary_var1, input$summary_var2)
    
    # contingency table
    table_out <- data %>%
      count(.data[[input$summary_var1]], .data[[input$summary_var2]]) %>%
      pivot_wider(names_from = input$summary_var2, 
                  values_from = n, 
                  values_fill = 0)
    
    table_out
  })
  
  
}

# run the app
shinyApp(ui, server)

