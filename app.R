#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)

#load data from other file
load("bio_age_objects.RData")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Biological Age Estimator"),
    
    # Separate into 2 tabs
    
    # Starter code 
    tabsetPanel(
      # First Tab
      tabPanel("Biological Age Calculator",
               sidebarLayout(
                 sidebarPanel(
                    h4("Input Your Personal Info Below: "),
                 
                   # sliders for each quantitative variable
                   sliderInput("pulse", 
                               "Pulse (bpm):",
                               min = round(min(nhanes$Pulse), 1), 
                               max = round(max(nhanes$Pulse), 1),
                               value = round(median(nhanes$Pulse), 1)),
                   
                   sliderInput("sys", 
                               "Systolic BP (mmHg):",
                               min = round(min(nhanes$BPSysAve), 1), 
                               max = round(max(nhanes$BPSysAve), 1),
                               value = round(median(nhanes$BPSysAve), 1)),
                   
                   sliderInput("dia", 
                               "Diastolic BP (mmHg):",
                               min = round(min(nhanes$BPDiaAve), 1), 
                               max = round(max(nhanes$BPDiaAve), 1),
                               value = round(median(nhanes$BPDiaAve), 1)),
                   
                   sliderInput("bmi", 
                               "BMI (weight/height2 in kg/m2):",
                               min = round(min(nhanes$BMI), 1), 
                               max = round(max(nhanes$BMI), 1),
                               value = round(median(nhanes$BMI), 1)),
                   
                   sliderInput("totchol", 
                               "Total Cholesterol (mmol/L):",
                               min = round(min(nhanes$TotChol), 1),
                               max = round(max(nhanes$TotChol), 1),
                               value = round(median(nhanes$TotChol), 1)),
                   
                   sliderInput("sleephrs", 
                               "Usual # of Hours of Sleep (per Night):",
                               min = round(min(nhanes$SleepHrsNight), 1),
                               max = round(max(nhanes$SleepHrsNight), 1),
                               value = round(median(nhanes$SleepHrsNight), 1)),
                   
                   sliderInput("physactivedays", 
                               "Usual # of Days Physically Active (per Week):",
                               min = round(min(nhanes$PhysActiveDays), 1),
                               max = round(max(nhanes$PhysActiveDays), 1),
                               value = round(median(nhanes$PhysActiveDays), 1)),
                   
                   # selection input for categorical variables
                   selectInput("gender", "Gender:",
                               choices = levels(nhanes$Gender)),
            
                   selectInput("smoke100", "Smoker Status:",
                               choices = levels(nhanes$Smoke100n)),
                  
                   # numeric input for user age
                   numericInput("actualAge", "Your Actual Age:",
                                value = 30, min = 18, max = max(nhanes$Age))),
                 
                 mainPanel(
                   h3("Predicted Biological Age: "), 
                   h5("Your predicted biological age is an estimate of how old your body appears based on key health biomarkers you entered."),
                   
                   verbatimTextOutput("bioAgeOut"),
                   
                   h3("Difference Between Predicted Biological Age and Actual (input) Age: "),
                   h5("This value shows how many years older or younger your predicted biological age is compared to your actual age."),
                   p("A positive difference indicates you are aging faster than expected for your chronological (actual) age, 
                  while a negative difference suggests you may be aging more slowly or are biologically younger."),
                   
                   verbatimTextOutput("deltaAgeOut"),
                   
        
                   h3("How Each Biomarker Shapes Your Biological Age: "),
                   p("This plot breaks down how each of your individual biomarkers influences your predicted biological age. 
                Bars above zero indicate biomarkers that increase your biological age, while bars below zero represent biomarkers 
                that contribute to a younger biological age. The size of each bar reflects the strength of the effect, based on the 
                coefficients from our regression model."),
                   
                   plotOutput("bioContribPlot")
                  
                 )
               )),
      
      # Second Tab
      
      # TO - DO: maybe add an element to look at the poulation for ur age group
      tabPanel("Population Explorer",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("xvar", "Biomarker:",
                               choices = c("Pulse", "BPSysAve", "BPDiaAve",
                                           "BMI", "TotChol","SleepHrsNight",
                                           "PhysActiveDays", "BioAge", "DeltaAge"))
                 ),
                 
                 mainPanel(
                   h3("How You Compare to Individuals Your Age"),
                   
                   p("This section allows you to explore how your personal health metrics compare to those 
                    of other individuals within your age range (your age +/- 5 years). The scatterplot shows 
                    the relationship between the chosen biomarker and biological age in your age group, 
                    with your own point marked in red. The density plot displays the distribution of the 
                    selected biomarker for people your age. This plot helps you see whether you fall above, below, 
                    or near the typical range."),
                   
                   plotOutput("scatterPlot"),
                   hr(),
                   plotOutput("distPlot")
                 )
               )
      )
  ) 
)
       
    
      
      
    
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Need to standardize user data/input the same way as the model training data
  userData <- reactive({
      data.frame(
        Pulse_z = (input$pulse - mean(nhanes$Pulse)) / sd(nhanes$Pulse),
        BPSys_z = (input$sys - mean(nhanes$BPSysAve)) / sd(nhanes$BPSysAve),
        BPDia_z = (input$dia - mean(nhanes$BPDiaAve)) / sd(nhanes$BPDiaAve),
        BMI_z = (input$bmi - mean(nhanes$BMI)) / sd(nhanes$BMI),
        TotChol_z = (input$totchol - mean(nhanes$TotChol)) / sd(nhanes$TotChol),
        SleepHrsNight_z = (input$sleephrs - mean(nhanes$SleepHrsNight)) / sd(nhanes$SleepHrsNight),
        PhysActiveDays_z = (input$physactivedays - mean(nhanes$PhysActiveDays)) / sd(nhanes$PhysActiveDays),
        Gender = factor(input$gender, levels = levels(nhanes$Gender)),
        Smoke100n = factor(input$smoke100, levels = levels(nhanes$Smoke100n))
      )
  })
  
  userPoint <- reactive({
    # Get user's X value based on selected variable
    user_x <- switch(input$xvar,
                     "Pulse" = input$pulse,
                     "BPSysAve" = input$sys,
                     "BPDiaAve" = input$dia,
                     "BMI" = input$bmi,
                     "TotChol" = input$totchol,
                     "SleepHrsNight" = input$sleephrs,
                     "PhysActiveDays" = input$physactivedays,
                     "BioAge" = predict(model, newdata = userData()),
                     "DeltaAge" = predict(model, newdata = userData()) - input$actualAge
    )
    
    data.frame(
      xvar = user_x,
      BioAge = predict(model, newdata = userData()),
      Gender = input$gender
    )
  })
  
  
  # Age Range Data for Tab 2
  # Filter NHANES data to people near the user's actual age
  AGE_WINDOW <- 5   # or whatever range you want
  
  filteredData <- reactive({
    req(input$actualAge)   # ensures age is entered
    
    nhanes %>%
      filter(
        Age >= input$actualAge - AGE_WINDOW,
        Age <= input$actualAge + AGE_WINDOW
      )
  })
  
  # Predict Biological Age
  output$bioAgeOut <- renderText({
    pred <- predict(model, newdata = userData())
    paste("Estimated Biological Age:", round(pred, 1))
  })
  
  # Delta Age
  output$deltaAgeOut <- renderText({
    pred <- predict(model, newdata = userData())
    paste("Delta Age:", round(pred - input$actualAge, 1))
  })
  
  # Personalized Biomarker Contribution Plot 
  output$bioContribPlot <- renderPlot({
  
    
    coefs <- coef(model)
    
    mm <- model.matrix(
     ~ Pulse_z + BPSys_z + BPDia_z + BMI_z + TotChol_z +
        SleepHrsNight_z + PhysActiveDays_z + Gender + Smoke100n,
      data = userData()
    )
    
    mm <- mm[, -1, drop = FALSE]
    
    contributions <- mm[1, ] * coefs[-1]
    
    df <- data.frame(
      Biomarker = names(contributions),
      Contribution = as.numeric(contributions)
    )
    
    ggplot(df, aes(x = reorder(Biomarker, Contribution), y = Contribution,
                   fill = Contribution > 0)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "#d73027", "FALSE" = "#1a9850"),
                        labels = c("Increase Biological Age", "Decrease Biological Age"),
                        guide = FALSE) +
      coord_flip() +
      labs(
        title = "Personalized Biomarker Contributions",
        x = "",
        y = "Effect on Biological Age (years)"
      )
  })
  
  
  # Population Explorer scatterplot (Color fixed to Gender)
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes_string(x = input$xvar, y = "BioAge", color = "Gender")) +
      geom_point(alpha = 0.5) +
      geom_point(data = userPoint(), aes(x = xvar, y = BioAge),
                 color = "red", size = 5) +
      labs(title = "Your Position vs People Your Age")
  })
  
  
  #output$scatterPlot <- renderPlot({
    #ggplot(filteredData(), aes_string(
     # x = input$xvar, y = "BioAge", color = "Gender")) +
      # geom_point(alpha = 0.5)
 #  })
  
  
  #output$distPlot <- renderPlot({
   # ggplot(filteredData(), aes_string(
   #   x = input$xvar, fill = "Gender")) +
    #  geom_density(alpha = 0.4)
  # })
  
  # Density plot (Color fixed to Gender)
  output$distPlot <- renderPlot({
    ggplot(filteredData(), aes_string(x = input$xvar, fill = "Gender")) +
      geom_density(alpha = 0.4) +
      geom_vline(data = userPoint(), aes(xintercept = xvar),
                 color = "red", size = 1.3) +
      labs(title = "Your Value Compared to Others Your Age")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)