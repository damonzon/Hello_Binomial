# Binomial Distribbution - Final - February 21, 2018
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)

# https://campus.datacamp.com/courses/foundations-of-probability-in-r/the-binomial-distribution?ex=2
# https://betterlesson.com/lesson/section/51735/exploration-of-probability
# https://en.wikipedia.org/wiki/Binomial_distribution
# http://www.stats.uwo.ca/faculty/braun/RTricks/basics/BasicRIV.pdf


# Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, 
# journal, lumen, paper, readable, sandstone, simplex, slate, 
# spacelab, superhero, united, yeti.


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Hello Binomial Distribution!"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            div(
                id = "erase_all",
                sliderInput(
                    inputId = "flips",
                    label = "Number of flips/tosses/trials:",
                    min = 1,
                    max = 100,
                    value = 1,
                    step = 1
                ),
                
                sliderInput(
                    inputId = "coins",
                    label = "Number of coins/n:",
                    min = 1,
                    max = 100,
                    value = 10,
                    step = 1
                ),
                
                sliderInput(
                    inputId = "pheads",
                    label = "Prob. of Heads/Success:",
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.001
                ),
              
                sliderInput(
                    inputId = "heads",
                    label = "Exact Number of Heads/Successes/X:",
                    min = 0,
                    max = 50,
                    value = 1,
                    step = 1
                    
                ),
                
                sliderInput(
                    inputId = "atleast_heads",
                    label = "Heads/Successes/X at Most:",
                    min = 0,
                    max = 50,
                    value = 1,
                    step = 1
                )
            ),
            
            br(),
            actionButton("reset_flips", "Reset All Sliders")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            verbatimTextOutput("Rcode"),
            verbatimTextOutput("binomial"),
            plotOutput(outputId = "Plot"),
            h4("Density of a binomial distribution for 1 trial"),
            verbatimTextOutput("Rcode2"),
            verbatimTextOutput("bdensity"),
            h4("Cumulative density of a binomial distribution for 1 trial"),
            verbatimTextOutput("Rcode3"),
            verbatimTextOutput("cumdensity"),
            verbatimTextOutput("oneminus_cumdensity"),
            
            tabPanel(
                "",
                tags$img(
                    " Heads = Success",
                    src = "CFA5_heads.jpg",
                    width = "100px",
                    height = "100px"
                ),
                tags$img(
                    " Tails = Failure",
                    src = "CFA5_tails.png",
                    width = "100px",
                    height = "100px"
                ),
                
                tags$img(
                    "",
                    src = "BlueOwl.png",
                    width = "130px",
                    height = "130px"
                )
            )
        )
    )
)

server <- function(input, output) {
    output$Rcode <- renderPrint({
        cat(
            paste(
                "R Code = rbinom(trials =",
                input$flips,
                ", n =",
                input$coins,
                ", p =",
                input$pheads,
                ")"
            )
        )
    })
    
    output$binomial <- renderPrint({
        binom <- rbinom(input$flips, input$coins, input$pheads)
        mbinom <- mean(binom)
        var_binom <- var(binom)
        std_binom <- sqrt(var_binom)
        
        cat(
            "Successes per trial = ",
            binom,
            "\nExpected Value = Mean = ",
            signif(mbinom,5),
            "Successes",
            "\nVariance = ",
            signif(var_binom, 5),
            "Standard Deviation = ",
            signif(std_binom, 5)
        )
    })
    
    output$Plot <- renderPlot({
        x <- rbinom(input$flips, input$coins, input$pheads)
        barplot(
            table(x),
            #breaks = input$bins,
            main = "Barplot of a Binomial Distribution",
            xlab = "Number of Successes",
            cex.main = 1.5,
            cex.axis = 1.3,
            cex.lab = 1.3,
            ylab = "Frequency",
            col = "lightblue",
            border = "black"
        )
        
    })
    
    output$Rcode2 <- renderPrint({
        cat(
            paste(
                "R Code = dbinom(exact successes = ",
                input$heads,
                ", n = ",
                input$coins,
                ", p = ",
                input$pheads,
                ")"
            )
        )
    })
    
    output$bdensity <- renderPrint({
        density_binom <- dbinom(input$heads, input$coins, input$pheads)
        cat(
            "Probability of exactly",
            input$heads,
            " successes for",
            1,
            "trial of n =",
            input$coins,
            "=",
            signif(density_binom, 5)
        )
        
    })
    
    output$Rcode3 <- renderPrint({
        cum <- pbinom(input$heads, input$coins, input$pheads)
        cat(
            paste(
                "R Code = pbinom(successes at most = ",
                input$atleast_heads,
                ", n = ",
                input$coins,
                ", p = ",
                input$pheads,
                ")"
            )
        )
    })
    
    output$cumdensity <- renderPrint({
        atleast_binom <-
            pbinom(input$atleast_heads, input$coins, input$pheads)
        cat(
            "Probability of",
            input$atleast_heads,
            " or fewer successes for",
            1,
            "trial of n =",
            input$coins,
            "=",
            signif(atleast_binom, 5)
        )
    })
    
    output$oneminus_cumdensity <- renderPrint({
        n_or_more_binom <-
            1 - pbinom(input$atleast_heads, input$coins, input$pheads)
        cat(
            "Probability of more than ",
            input$atleast_heads,
            "successes for ",
            1,
            "trial of n=",
            input$coins,
            "=",
            signif(n_or_more_binom, 5)
        )
    })
    
    observeEvent(input$reset_flips, {
        reset("erase_all")
    })
}

shinyApp(ui = ui, server = server)
