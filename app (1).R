#
library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinythemes)


source("dataWrangling.R") #loads in your analysis file


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"), 
    
    navbarPage(
      "Shiny App",
      tabPanel("Introduction",
        mainPanel(
          h2(style = "font-family: Courier New;", "Fan Attendance vs. Win Percentage?"),
          h4(style = "font-family: Times New Roman;", "“Cleveland, this is for YOU!” - LeBron James"),
          p(style = "font-family: Times New Roman;","In the 2022-2023 NBA season, we want to see if NBA teams won more games due to more fans in attendance. We are examining
                    a win percentage and fans in attendance data set from the 2022-2023 NBA season. This analysis is interesting as it shows if teams need fans in attendance to have better performance. This info
                    is especially important for sports team's owners. This season was right after Covid hit so the people that haven’t seen a game in the past 2 years due to Covid would go to see a game and that
                    would in turn either affect how the team plays on the road or not."),
          img(src = "two.png", height = 475, width = 875, style="display: block; margin-left: auto; margin-right: auto;"),
        ),
       ),
      tabPanel("Rank vs. Teams",
               h3(style = "font-family: Courier New;","Home Attendance on Win Percentage Rank"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "rank",
                               label = "Choose a rank to show a team and its win percentage",
                               choices = df$Rank
                   ),
                   htmlOutput(outputId = "team_name"),
                   htmlOutput(outputId = "win_percentage_overall"),
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "bar_team"),
                   p(style = "font-family: Times New Roman;", "This shows each individual NBA teams average viewership for the 2022-2023 season. You can hover over
                     the columns of the graph to see how many viewers average team has."),
                 ),
                 position = c("right"),
                 fluid = TRUE
               ),
      ),
      tabPanel("Win Loss Ratio on Attendance",
               h3(style = "font-family: Courier New;","Pie Chart On Home and Away Wins"),
               sidebarLayout (
                 sidebarPanel(
                   selectInput("selected_team",
                               "Select Team:",
                               choices = df$TEAM
                               ),
                   htmlOutput(outputId = "home_attendance_avg2"),
                   htmlOutput(outputId = "away_attendance_avg2"),
                 ),
                 mainPanel(
                   plotlyOutput(outputId = "pieChart_team"),
                   p(style = "font-family: Times New Roman;","This shows the win percentage is directly correlated or not depending on how many fans are in attendance."),
                 )
               ),
               position = c("right"),
               fluid = TRUE
               ),
      tabPanel("Win Percentage Home & Away",
         h3(style = "font-family: Courier New;", "Comparing Home Win Percentage and Average Home Attendance"),
         sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "team_name",
                           label = "Choose a team",
                           choices = df$TEAM),
               htmlOutput(outputId = "home_attendance_avg"),
               htmlOutput(outputId = "home_win_percentange"),
               br(),
             ),
             mainPanel(
               plotlyOutput(outputId = "scatter_home"),
             ),
             position = c("right"),
             fluid = TRUE
           ),
         h3(style = "font-family: Courier New;","Comparing Away Win Percentage and Average Home Attendance"),
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId = "team_name",
                         label = "Choose a team",
                         choices = df$TEAM),
             htmlOutput(outputId = "away_attendance_avg"),
             htmlOutput(outputId = "away_win_percentange"),
             br(),
           ),
           mainPanel(
             plotlyOutput(outputId = "scatter_away"),
           ),
           position = c("right"),
           fluid = TRUE
         ),
      ),
      tabPanel("Conclusion",
               mainPanel(
                 h3(style = "font-family: Courier New;", "Conclusion and Final Thoughts on NBA Fan Attendance VS Win Percentange"),
                 p(style = "font-family: Times New Roman;","In conclusion, we can say that an NBA team’s win percentage is not positively 
                   correlated with how many fans are in attendance. For example, when we were comparing the average fans in attendance with teams, 
                   we saw that the number one-seeded Milwaukee Bucks and number two-seeded Memphis Grizzlies had one of the lower average fans in attendance. 
                   These teams are outliers. Looking at the scatter plot we can see that the mid-tier teams show a positive correlation with the win percentage and fans in attendance. 
                   The higher winning teams and lower winning teams are outliers based on observations from the graphs. So do NBA teams do better with more fans in attendance? I’d say 
                   there are extremely talented teams like for example the Bucks and Grizzlies that just don’t have the fan bases like teams like the Bulls or Lakers. These however are external 
                   factors and aren’t statistical. But for mid-tier teams, you can see a moderately strong positive correlation between win percentage and fans in attendance as shown in the graphs. 
                   We have explored the intersection between these two variables and we have evidence that can be used on the contrary or to support the correlation."),
                 img(src = "four.jpeg", height = 475, width = 875, style="display: block; margin-left: auto; margin-right: auto;"),
               ),
      ),
    ),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$home_attendance_avg <- renderUI({
      avg_attendance = get_home_attendance_avg(df, input$team_name)
      return(avg_attendance)
  })
  
  output$home_attendance_avg2 <- renderUI({
    avg_attendance = get_home_attendance_avg(df, input$team_name)
    return(avg_attendance)
  })

  output$home_win_percentange <- renderUI({
    win_percent = get_home_win_percentange(df, input$team_name)
    return(win_percent)
  })
  
  output$away_attendance_avg <- renderUI({
    avg_attendance = get_away_attendance_avg(df, input$team_name)
    return(avg_attendance)
  })
  
  output$away_attendance_avg2 <- renderUI({
    avg_attendance = get_away_attendance_avg(df, input$team_name)
    return(avg_attendance)
  })
  
  output$away_win_percentange <- renderUI({
    win_percent = get_away_win_percentange(df, input$team_name)
    return(win_percent)
  })
  
  output$team_name <- renderUI ({
    teamName = get_team_name(df, input$rank)
    return(teamName)
  })
  
  output$win_percentage_overall <- renderUI ({
    win_percent_overall <- get_win_percent_overall(df, input$rank)
    return(win_percent_overall)
  })

  output$scatter_home <- renderPlotly({
    
    scatter_home <- ggplot(df, aes(x = Home_AVG, y = Home, text = TEAM)) + 
      geom_point(mapping = aes(color = TEAM)) +
      labs(title = "Win Percentage per Team at Home Games", x = "Home Average Fan Attendance", y = "Home Win Percentage")

    scatter_home <- ggplotly(scatter_home, tooltip = "text")
    return(scatter_home)
  })
  
  output$scatter_away <- renderPlotly({
    
    scatter_away <- ggplot(df, aes(x = Road_AVG, y = Away, text = TEAM)) + 
      geom_point(mapping = aes(color = TEAM)) +
      labs(title = "Win Percentage per Team at Away Games", x = "Away Average Fan Attendance", y = "Away Win Percentage")
    scatter_away <- ggplotly(scatter_away, tooltip = "text")
    
      return(scatter_away)
  })
  
  output$bar_team <- renderPlotly({
    
    bar_team <- ggplot(df, aes(x = TEAM, y = Overall_AVG, text = TEAM, fill = TEAM)) +
      geom_col() + 
      labs(title = "Teams AVG Fans in Attendance", x = "Teams", y = "Average Fans in Attendance")
    
    bar_team <- ggplotly(bar_team, tooltip = "text")
    return(bar_team)
  })
  
  output$pieChart_team <- renderPlotly ({
    
    selected_data <- df[df$TEAM == input$selected_team, ]
    
    pie_data <- data.frame(
      Category = c("Home Wins", "Away Wins"),
      Values = c(selected_data$Home, selected_data$Away)
    )
    
    p <- plot_ly(pie_data, labels = ~Category, values = ~Values, type = 'pie',
                 textinfo = 'label+percent', insidetextorientation = 'radial')
    p
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
