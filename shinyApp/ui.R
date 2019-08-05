library(ggvis)
library(shiny)

langs <- list("All"="all", "English"="en", "French"="fr", "Bahasa Malaysia"="my")
fluidPage(theme="bootstrap.min.css",
          # Application title
          titlePanel("Sentiment Analysis"),
          sidebarLayout(
            # Sidebar with a slider and selection inputs
            sidebarPanel(
              textInput("source1", "Search on Social Media:", value="@ktm_berhad"),
              conditionalPanel(
                condition = "input.show_source2 == true",
                textInput("source2", "Compare with:" , value="@MRTMalaysia")
              ),
              checkboxInput("show_source2", "Compare"),
              actionButton("plot_feel", "Plot Sentiments"),
              hr(),
              selectInput("lang",
                          "Language:", langs),
              tags$style("body{background-color: linen; color:blue}")
            ),
            
            mainPanel(
              verbatimTextOutput("twitter_view"),
              tabsetPanel(type = "tabs",
                          tabPanel("Sentiment Chart", ggvisOutput("plot1")),
                          tabPanel("Trends", plotOutput("trends")),
                          tabPanel("Sources", plotOutput("plot")))

            )
          )
)