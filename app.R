#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(htmltools)
library(tidyverse)
library(scales)
library(spatstat)

load("data/pums.rds")

pums <- pums
max_income <- max(pums$Income)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),

  # Application title
  titlePanel("2016-2020 ACS PUMS Worker Earnings Distributions"),

  # Sidebar with demographic/occupational filters
  sidebarLayout(
    sidebarPanel(
      h2("Filter the data"),
      selectInput("state",
                  label = "State",
                  choices = c("All 50 States", sort(unique(pums$State)))),
      selectizeInput("occupation",
                     label = "Occupation",
                     choices = c("All Occupations", sort(unique(pums$Occupation)))),
      selectInput("sex",
                  label = "Sex",
                  choices = c("Either Sex", sort(unique(pums$Sex)))),
      selectInput("age",
                  label = "Age",
                  choices = c("All Ages", sort(unique(pums$Age)))),
      selectInput("raceeth",
                  label = "Race/Ethnicity",
                  choices = c("All Races/Ethnicities", sort(unique(pums$`Race/Ethnicity`)))),
      h2("Adjust the chart"),
      sliderInput("xmax",
                  label = "Maximum income to display",
                  min = 0,
                  max = max_income,
                  value = max_income,
                  step = 50000)
    ),

    # Show a plot of the generated distribution
    mainPanel(
     plotOutput("earningsPlot", height = "700px")
    )
  )
)

# Define server logic required to draw income distribution
server <- function(input, output) {

    output$earningsPlot <- renderPlot({
        df <- pums

        # Apply filters
        if (input$state != "All 50 States") {df <- filter(df, State == input$state)}
        if (input$occupation != "All Occupations") {df <- filter(df, Occupation == input$occupation)}
        if (input$sex != "Either Sex") {df <- filter(df, Sex == input$sex)}
        if (input$age != "All Ages") {df <- filter(df, Age == input$age)}
        if (input$raceeth != "All Races/Ethnicities") {df <- filter(df, `Race/Ethnicity` == input$raceeth)}

        df_med_inc <- round(weighted.median(df$Income, df$PWGTP))

        p <- ggplot(df) +
          geom_density(aes(x = Income, weight = PWGTP), data = pums, alpha = 0.15, fill = "black", col = NA) +
          geom_density(aes(x = Income, weight = PWGTP), alpha = 0.65, fill = "darkblue", col = NA) +
          geom_vline(xintercept = df_med_inc, col = "darkblue") +
          annotate(geom = "text", x = df_med_inc, y = Inf, hjust = -0.1, vjust = 2,
                   label = paste("Median:", dollar(df_med_inc)), col = "darkblue") +
          scale_x_continuous(limits = c(0, input$xmax), labels = scales::label_dollar()) +
          theme_minimal() +
          theme(axis.text.y = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()) +
          labs(title = "Annual Earnings of Full-Time Workers",
               subtitle = paste(input$state, input$occupation, input$sex, input$age, input$raceeth, sep = " | "),
               caption = paste(
                 "Only includes workers who reported working at least 35 hours per week",
                 "and at least 48 weeks per year (including paid time off).\n",
                 "Income includes wages, salary, commissions, bonuses and tips.",
                 "Other income sources (including from self-employment) are excluded.\n",
                 "The distribution is an estimate based on", comma(nrow(df)),
                 "surveyed individuals, representing", comma(sum(df$PWGTP)), "workers.")) +
          xlab("Annual earnings (2020 dollars)") +
          ylab("Proportion of full-time workers")

        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)
