library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(scales)
library(spatstat)

load("data/pums.rds")

pums <- pums
max_income <- max(pums$Income)

# Define UI for application that draws frequency distributions
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
                  min = 50000,
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

  df <- reactive({
    # Apply filters to PUMS data
    pums %>% filter(
      if (input$state != "All 50 States") {State == input$state} else {TRUE},
      if (input$occupation != "All Occupations") {Occupation == input$occupation} else {TRUE},
      if (input$sex != "Either Sex") {Sex == input$sex} else {TRUE},
      if (input$age != "All Ages") {Age == input$age} else {TRUE},
      if (input$raceeth != "All Races/Ethnicities") {`Race/Ethnicity` == input$raceeth} else {TRUE}
    )
  })

  output$earningsPlot <- renderPlot({
    # Log info about current plot
    chart_spec <- paste(input$state, input$occupation, input$sex, input$age, input$raceeth, sep = " | ")
    df_med_inc <- round(weighted.median(df()$Income, df()$PWGTP))
    cat(file=stderr(), "Filters:", chart_spec, "\n")
    cat(file=stderr(), "Max display:", input$xmax, "\n")
    cat(file=stderr(), "Obs:", nrow(df()), "\n")
    cat(file=stderr(), "Median:", df_med_inc, "\n")

    # Generate the plot
    p <- ggplot(df()) +
      geom_density(aes(x = Income, weight = PWGTP), data = pums, alpha = 0.15, fill = "black", col = NA) +
      geom_density(aes(x = Income, weight = PWGTP), alpha = 0.65, fill = "darkblue", col = NA) +
      geom_vline(xintercept = df_med_inc, col = "darkblue") +
      annotate(geom = "text", x = df_med_inc, y = Inf, hjust = -0.1, vjust = 2,
               label = paste("Median:", dollar(df_med_inc)), col = "darkblue") +
      scale_x_continuous(limits = c(0, input$xmax), labels = label_dollar()) +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(title = "Annual Earnings of Full-Time Workers",
           subtitle = chart_spec,
           caption = paste(
             "Only includes workers who reported working at least 35 hours per week",
             "and at least 48 weeks per year (including paid time off).\n",
             "Income includes wages, salary, commissions, bonuses and tips.",
             "Other income sources (including from self-employment) are excluded.\n",
             "The distribution is an estimate based on", comma(nrow(df())),
             "surveyed individuals, representing", comma(sum(df()$PWGTP)), "workers.")) +
      xlab("Annual earnings (2020 dollars)") +
      ylab("Proportion of full-time workers")

    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
