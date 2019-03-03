library(shiny)
library(bigrquery)
library(ggplot2)
library(readr)

options(shiny.sanitize.errors = FALSE)
project <- "optical-weft-233118"

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/bigquery")
options(googleAuthR.webapp.client_id = "MY_id")
options(googleAuthR.webapp.client_secret = "MY_id")


axis_vars <- c(
  "Day" = "day",
  "Month" = "month",
  "Number of children were born" = "plurality",
  "Number of pounds gained by the mother" = "weight_gain_pounds",
  "The number of weeks of the pregnancy" = "gestation_weeks",
  "Mother's age" = "mother_age",
  "APGAR 1 min" = "apgar_1min",
  "APGAR 5 min" = "apgar_5min"
)

ui <- navbarPage("Radchenko Assessment",
                 tabPanel("Task #1",
                          fluidPage(
                            
                            titlePanel("График количества рождённых детей"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("var", "Choose a variable:", 
                                            choices=c(axis_vars[1],axis_vars[2])),
                                sliderInput("date1", "Select Year Range:",
                                            min = 1971, max = 2008, value = c(1971,2008), sep = "")
                              ),
                              
                              mainPanel(
                                plotOutput("distPlot1")
                              )
                            )
                          )
                 ),
                 tabPanel("Task #2",
                          fluidPage(
                            
                            titlePanel("Гистограмма количества рождённых детей за одни роды"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("date2", "Select Year Range:",
                                            min = 1969, max = 2008, value = c(1969,2008), sep = "")
                              ),
                              
                              mainPanel(
                                plotOutput("distPlot2")
                              )
                            )
                          )              
                 ),
                 tabPanel("Task #3",
                          fluidPage(
                            
                            titlePanel("Зависимость здоровья новорождённого от различных факторов"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("x_var", "Choose a x-variable:", 
                                            choices=c(axis_vars[4], axis_vars[3], axis_vars[5], axis_vars[6])),
                                selectInput("y_var", "Choose a y-variable:", 
                                            choices=c(axis_vars[7], axis_vars[8]))
                              ),
                              
                              mainPanel(
                                plotOutput("distPlot3")
                              )
                            )
                          )
                 )
)


# Define server logic
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({
    
    sql <- paste("SELECT ", input$var, " AS Variable, sum(plurality) AS Number_of_Children 
                 FROM natality 
                 WHERE plurality IS NOT NULL AND year BETWEEN ", input$date1[1]," AND ", input$date1[2]," 
                 AND ", input$var, "IS NOT NULL AND ", input$var, " !=99 
                 GROUP BY Variable 
                 ORDER BY Variable")
    result <- query_exec(sql, project = project, default_dataset = "bigquery-public-data:samples")
    ggplot(result, aes(x=result$Variable, y=result$Number_of_Children)) +  geom_point(colour = "steelblue", size = 3) + geom_line(color='steelblue') + labs(x=input$var, y="Number of Children") + 
      scale_x_continuous(breaks = 1:31) + theme_bw()
  })
  
  output$distPlot2 <- renderPlot({
    sql <- paste("SELECT plurality, sum(plurality) AS Sum_of_Children 
                 FROM natality
                 WHERE plurality IS NOT NULL
                 AND year BETWEEN ", input$date2[1]," AND ", input$date2[2]," 
                 GROUP BY plurality
                 ORDER BY plurality")
    result <- query_exec(sql, project = project, default_dataset = "bigquery-public-data:samples")
    ggplot(result, aes(x=result$plurality, y=result$Sum_of_Children)) + geom_bar(stat = "identity", fill = "#FF6666") + labs(x="Number of children were born", y="Number of Children") + 
      scale_x_continuous(breaks = 1:7) + theme_bw() + geom_text(aes(label=result$Sum_of_Children), vjust=-0.4, size=4)
  })
  
  output$distPlot3 <- renderPlot({
    sql <- paste("SELECT AVG(", input$y_var, ") AS Variable_y, ", input$x_var, " AS Variable_x 
                 FROM natality
                 WHERE ",input$x_var, "IS NOT NULL AND ",input$x_var, " !=99
                 AND", input$y_var , "IS NOT NULL AND ", input$y_var , " !=99
                 GROUP BY Variable_x")
    result <- query_exec(sql, project = project, default_dataset = "bigquery-public-data:samples", use_legacy_sql = FALSE)
    corr_eqn <- function(x,y, digits = 2) {
      corr_coef <- round(cor(x, y), digits = digits)
      paste("italic(r) == ", corr_coef)
    }
    ggplot(result, aes(x=result$Variable_x, y=result$Variable_y)) +  geom_point(colour = "steelblue", size = 3) + geom_smooth() + labs(x=input$x_var, y=input$y_var) + 
      geom_label(x=max(result$Variable_x),
                 y=max(result$Variable_y),
                 label = corr_eqn(result$Variable_x, result$Variable_y), parse = TRUE 
      ) + theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
