library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(igraph)
library(ggnetwork)
library(ggplot2)
library(dplyr)

# Import data
networks <- readRDS("networks.rds")
transitivity <- readRDS("transitivity.rds") |>
  mutate(
    Transitivity = as.numeric(Transitivity) 
  )
slr_data <- readRDS("slr_data.rds")

#UI
ui <- navbarPage(
  title = "Does Teamwork Really Make the Dream Work?",
  theme = shinytheme("flatly"),
  
  #TAB 1: network
  tabPanel(
    title = "Team Network",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "team",
          "Select a team:",
          choices = names(networks)
        )
      ),
      mainPanel(
        plotOutput("network_plot")
      )
    )
  ),
  
  #Tab 2: Scatterplot
  tabPanel(
    title = "Transitivity vs AvgWin% Scatterplot",
    plotOutput("slr_plot")
  ),
  
  #Tab 3: Table 
  tabPanel(
    "Transitivity Score Table",
    DTOutput("trans_table")
  ),
  
  #Tab 4: Information
  tabPanel(
    "Information",
    mainPanel(
      h1("Information about this app"),
      p("This Shiny app visualizes MLB roster networks from the 2015–2025 seasons 
        and explores whether roster interconnectedness measured with transitivity 
        is associated with team success. Transitivity is calculated as the 
        likelihiood that Player B and C are teammates since Player A is teammates 
        with Player B and Player C. Higher transivity scores indicate that the 
        overall roster is more connected. For the networks, each node represents 
        individual players and edges are any pairs of players who played together 
        on the same team for at least 3 seasons. All the data for this project 
        was collected from Baseball Reference (Baseball Reference). For more 
        detailed information, please check my final report."),
      h2("Team Name Key:"),
      p("NYY - New York Yankees"),
      p("BOS - Boston Red Sox"),
      p("TOR - Toronto Blue Jays"),
      p("TBR - Tampa Bay Rays"),
      p("CLE - Cleveland Guardians"),
      p("DET - Detroit Tigers"),
      p("KCR - Kansas City Royals"),
      p("MIN - Minnesota Twins"),
      p("CHW - Chicago White Sox"),
      p("SEA - Seattle Mariners"),
      p("HOU - Houston Astros"),
      p("TEX - Texas Rangers"),
      p("ATH - Athletics"),
      p("LAA - Los Angeles Angels"),
      p("PHI - Philadelphia Phillies"),
      p("NYM - New York Mets"),
      p("MIA - Miami Marlins"),
      p("WSN - Washington Nationals"),
      p("MIL - Milwaukee Brewers"),
      p("CHC - Chicago Cubs"),
      p("CIN - Cincinnati Reds"),
      p("STL - St Louis Cardinals"),
      p("PIT - Pittsburgh Pirates"),
      p("SDP - San Diego Padres"),
      p("SFG - San Francisco Giants"),
      p("ARI - Arizona Diamondbacks"),
      p("COL - Colorado Rockies"),
      p("LAD - Los Angeles Dodgers"),
      p("ATL - Atlanta Braves"),
      h3("Data Sources"),
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2025 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on October 26,\n",
        "https://www.baseball-reference.com/leagues/majors/2025-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2024 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on October 26,\n",
        "https://www.baseball-reference.com/leagues/majors/2024-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2023 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on October 26,\n",
        "https://www.baseball-reference.com/leagues/majors/2023-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2022 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on October 26,\n",
        "https://www.baseball-reference.com/leagues/majors/2022-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2021 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on October 26,\n",
        "https://www.baseball-reference.com/leagues/majors/2021-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2020 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2020-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2019 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2019-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2018 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2018-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2017 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2017-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2016 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2016-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2015 Major League Baseball Standard Pitching: Player Standard Pitching.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2015-standard-pitching.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2025 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on October 24,\n",
        "https://www.baseball-reference.com/leagues/majors/2025-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2024 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on October 24,\n",
        "https://www.baseball-reference.com/leagues/majors/2024-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2023 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on October 24,\n",
        "https://www.baseball-reference.com/leagues/majors/2023-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2022 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on October 24,\n",
        "https://www.baseball-reference.com/leagues/majors/2022-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 2)\n",
        "2021 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on October 24,\n",
        "https://www.baseball-reference.com/leagues/majors/2021-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2020 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2020-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2019 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2019-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2018 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2018-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2017 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2017-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2016 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2016-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, November 18)\n",
        "2015 Major League Baseball Standard Batting: Player Standard Batting.\n",
        "Sports Reference LLC. Retrieved on November 18,\n",
        "https://www.baseball-reference.com/leagues/majors/2015-standard-batting.shtml"
      )),
      
      p(paste(
        "- Baseball-Reference (Last updated 2025, December 10)\n",
        "Major League Baseball Detailed Standings (2015–2025).\n",
        "Sports Reference LLC. Retrieved on December 10,\n",
        "https://www.baseball-reference.com/leagues/MLB-standings.shtml"
      ))
      )
    )
  )


#Server
server <- function(input, output) {
  
  #Individual Team Networks
  output$network_plot <- renderPlot({
    g <- networks[[input$team]]
    net <- ggnetwork(g)
    
    ggplot(net, aes(x, y, xend = xend, yend = yend)) +
      geom_edges(color = "black") +
      geom_nodes(size = 3) +
      geom_nodelabel(aes(label = name), size = 3) +
      theme_blank() +
      ggtitle(paste(input$team, "Social Network 2015–2025"))
  })
  
  
  #Regression Plot
  output$slr_plot <- renderPlot({
    ggplot(slr_data, aes(x = Transitivity, y = AvgWinPct, label = Team)) +
      geom_point(size = 3) +
      geom_text(nudge_y = 0.003, size = 3) +
      labs(
        title = "Transitivity vs Average Winning Percentage (2015–2025)",
        x = "Transitivity Score",
        y = "Average Win %"
      ) +
      theme_minimal()
  })
  
  #table
  output$trans_table <- renderDT({
    transitivity |>
      mutate(
        Transitivity = round(Transitivity, 3) #round to 3 digits
      ) |>
      arrange(desc(Transitivity))
  })
}

#call to server
shinyApp(ui = ui, server = server)
