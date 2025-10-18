
# packages
library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(scales)
library(VGAM)

# load model and data
fit_mnl <- readRDS("fit_mnl.rds")
arrest  <- readRDS("arrest.rds")

arrest <- arrest %>%
  mutate(
    CRIME_TYPE  = factor(CRIME_TYPE),
    ARREST_BORO = factor(ARREST_BORO),
    AGE_GROUP   = factor(AGE_GROUP),
    PERP_SEX    = factor(PERP_SEX),
    PERP_RACE   = factor(PERP_RACE)
  )

# predict class probabilities with profile data
profile_probs <- function(boro, age, sex, race) {
  nd <- data.frame(
    ARREST_BORO = factor(boro, levels = levels(arrest$ARREST_BORO)),
    AGE_GROUP   = factor(age,  levels = levels(arrest$AGE_GROUP)),
    PERP_SEX    = factor(sex,  levels = levels(arrest$PERP_SEX)),
    PERP_RACE   = factor(race, levels = levels(arrest$PERP_RACE))
  )
  on.exit({ rm(nd); gc(verbose = FALSE) }, add = TRUE)
  
  p <- predict(fit_mnl, newdata = nd, type = "response")  # 1 x K matrix
  on.exit({ rm(p); gc(verbose = FALSE) }, add = TRUE)
  
  tibble(Class = colnames(p), Prob = as.numeric(p[1, ]))
}

# ---------------- UI ----------------
ui <- fluidPage(
  titlePanel("Crime Type — Profile A vs B (Multinomial VGAM)"),
  sidebarLayout(
    sidebarPanel(
      h5("Profile A"),
      selectInput("boroA","Borough A", levels(arrest$ARREST_BORO)),
      selectInput("ageA","Age A", levels(arrest$AGE_GROUP)),
      selectInput("sexA","Sex A", levels(arrest$PERP_SEX)),
      selectInput("raceA","Race A", levels(arrest$PERP_RACE)),
      hr(),
      h5("Profile B"),
      selectInput("boroB","Borough B", levels(arrest$ARREST_BORO)),
      selectInput("ageB","Age B", levels(arrest$AGE_GROUP)),
      selectInput("sexB","Sex B", levels(arrest$PERP_SEX)),
      selectInput("raceB","Race B", levels(arrest$PERP_RACE)),
      actionButton("compare","Compare"),
      br(), br(),
      verbatimTextOutput("cmp_status")
    ),
    mainPanel(
      plotOutput("cmp_plot", height = 360),
      tableOutput("cmp_table")
    )
  )
)

# --------------- SERVER --------------
server <- function(input, output, session) {
  
  output$cmp_status <- renderText({
    if (!inherits(fit_mnl, "vgam")) return("Model not loaded.")
    paste0("Model and data loaded. N=", nrow(arrest), ", Classes: ",
           paste(levels(arrest$CRIME_TYPE), collapse = ", "))
  })
  
  # compute both profile probabilities 
  cmp <- eventReactive(input$compare, {
    validate(
      need(inherits(fit_mnl, "vgam"), "fit_mnl is not a VGAM model"),
      need(is.data.frame(arrest), "arrest data not loaded")
    )
    
    A <- profile_probs(input$boroA, input$ageA, input$sexA, input$raceA) %>%
      mutate(Profile = "A")
    B <- profile_probs(input$boroB, input$ageB, input$sexB, input$raceB) %>%
      mutate(Profile = "B")
    
    out <- bind_rows(A, B)
    
    rm(A, B); gc(verbose = FALSE)
    out
  }, ignoreInit = TRUE)
  
  # plot
  output$cmp_plot <- renderPlot({
    req(cmp())
    df <- cmp() %>% mutate(Profile = factor(Profile, levels = c("A","B")))
    ggplot(df, aes(x = Class, y = Prob, fill = Profile)) +
      geom_col(position = position_dodge2(width = 0.7, preserve = "single", reverse = TRUE)) +
      coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "Probability", title = "Predicted class probabilities: Profile A vs B") +
      theme_minimal(base_size = 12)
  })
  
  # table — side-by-side probs
  output$cmp_table <- renderTable({
    req(cmp())
    pivot_wider(cmp(), names_from = Profile, values_from = Prob) %>%
      mutate(A = percent(A, accuracy = 0.1),
             B = percent(B, accuracy = 0.1)) %>%
      arrange(desc(A))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # session-end GC
  session$onSessionEnded(function() {
    gc(verbose = FALSE)
  })
}

shinyApp(ui, server)
