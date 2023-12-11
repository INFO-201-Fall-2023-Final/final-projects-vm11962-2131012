library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)


# Function to create a bullet list
spot_list <- function(char, ordered = FALSE) {
  separate <- c("<li>", "</li>")
  html_wrap <- if (ordered) c("<ol>", "</ol>") else c("<ul>", "</ul>")
  bulletpoints <- paste0(separate[1], char, separate[2], collapse = "")
  web_list <- paste0(html_wrap[1], bulletpoints, html_wrap[2])
  
  return(web_list)
}

# UI
ui <- fluidPage(
  titlePanel("Outdoor Sport Incidents"),
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Libre+Baskerville&display=swap');
      body { 
        font-family: 'Libre Baskerville', sans-serif;
        background-color: #474747;
        color: white;
      }

      h2, h3, h4,{
        color: white;
      }
      ;"))),
  titlePanel(" "),
  tabsetPanel(
    tabPanel("Introduction",
             h3("Introduction to Climbing and Outdoor Accidents"),
             br(),
             
             tags$div(
               img(src = "https://media.npr.org/assets/img/2015/03/18/afghan_climbers_024_slide-e7448d41cc8c680261ffecfdb738db38c12ad15a.jpg",
                   width = "400px", style = "float: right; margin: 10px; border: 19px solid white;"),
               style = "background-color: white; border-radius: 100px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.2);"
             ),
             
             h4("Importance"),
             
             p("We decided to focus our attention towards accidents 
             and deaths that are not often spoken about when the topic 
             of hiking or climbing comes up. Through merging our two data sets we 
             found various common accidents and causes of deaths that range 
             from avalanche accidents to hypothermia, many leading to death. 
             Using the data collected, we will demonstrate and compare these 
             various causes in hopes to spread awareness of these unspoken events."),
             br(),
             
             h4("History"),
             
             p("Hiking and climbing as well as other outdoor sports are highly 
             common in the PNW region and have been since the early 1300’s. The 
             sport of mountaineering began in France but laster spread through 
             Europe and Sweden. Advances in converting to using ropes and equipment 
             took inspiration from Ice climbing in which the equipment was now used 
             for extra safety purposes."),
             br(),
             
             h4("Spotlight"),
             htmlOutput("bullet_list"),
    ),
    tabPanel("Accident Patterns",
             h3("Top Causes of Death per Year"),
             
             sliderInput("Year", "Select a Year:", min = 1978, max = 2021, value = 1978, sep = ""),
             plotOutput("plot")
    ),
    tabPanel("Compare years",
             h3("Compare Deaths Between Two Years"),
             selectInput("year1", "Select Year 1:", choices = unique(new_df$Year), selected = min(unique(new_df$Year))),
             selectInput("year2", "Select Year 2:", choices = unique(new_df$Year), selected = max(unique(new_df$Year))),
             plotOutput("compare_plot")
    ),
    tabPanel("Nationalities of People who died",
             h3("Select a Year"),
             selectInput("selected_year_pie", "Select Year:", choices = unique(new_df$Year), selected = min(unique(new_df$Year)),
                         multiple = FALSE),
             plotOutput("nationality_pie_chart")
    ),
  )
)

# Server
server <- function(input, output) {
  output$bullet_list <- renderText({
    spot_list(c("What were the causes for most accidents or causes of death?",
                "Did the accidents have any common factors within these individuals?",
                "What was the most common injury overall?",
                "Geographically, were there any hotspots for injuries or deaths?"))
  })
  
  output$plot <- renderPlot({
    selected_year <- input$Year
    filtered_data <- subset(new_df, Year == selected_year)
    
    data_order <- filtered_data[order(-filtered_data$Num_Deaths_In_Yr), ]
    same_rows <- data_order[!duplicated(data_order$Cause.of.death), ]
    
    top_deaths <- head(same_rows, 5)
    top_deaths <- top_deaths[!(top_deaths$Cause.of.death == " " | is.na(top_deaths$Cause.of.death)), ]
    
    barplot(
      height = top_deaths$Num_Deaths_In_Yr,
      names.arg = top_deaths$Cause.of.death,
      main = paste("Top Causes of Deaths in", selected_year),
      xlab = "Accident Type",
      ylab = "Number of Deaths",
      col = "skyblue",
      border = "black",
      ylim = c(0, max(top_deaths$Num_Deaths_In_Yr) + 10),
      cex.names = 0.8, # Adjust the size of axis labels
      las = 2, # Rotate axis labels for better visibility
      legend.text = top_deaths$Cause.of.death,
      args.legend = list(title = "Cause of Death", x = "topright", bty = "n")
    )
  })
  
  output$compare_plot <- renderPlot({
    selected_year1 <- input$year1
    selected_year2 <- input$year2
    filtered_data <- subset(new_df, Year %in% c(selected_year1, selected_year2))
    ggplot(filtered_data, aes(x = Num_Deaths_In_Yr, y = Cause.of.death, fill = factor(Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparison of Deaths Between Two Years",
           x = "Number of Deaths",
           y = "Cause of Death") +
      theme_minimal()
  })
  
  output$nationality_pie_chart <- renderPlot({
    selected_year_pie <- input$selected_year_pie
    
    filtered_data <- subset(new_df, Year == selected_year_pie)
    nationality_counts <- table(filtered_data$Nationality)
    ordered_counts <- nationality_counts[order(-nationality_counts)]
    top_n <- 10
    if (length(ordered_counts) > top_n) {
      ordered_counts <- c(ordered_counts[1:top_n], Other = sum(ordered_counts[(top_n + 1):length(ordered_counts)]))
    }
    
    legend_labels <- paste(names(ordered_counts), " (", ordered_counts, " deaths)", sep = "")
    
    pie(ordered_counts, main = paste("Nationality Distribution in", selected_year_pie),
        col = rainbow(length(ordered_counts)), labels = legend_labels)
  })
}

shinyApp(ui = ui, server = server)
