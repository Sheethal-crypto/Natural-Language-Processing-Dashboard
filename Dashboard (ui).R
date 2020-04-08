library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    dashboardPage(skin = "green",
        dashboardHeader(title = "Dashboard"),
        dashboardSidebar(
            
            sidebarMenu(
                menuItem("Overview", tabName = "Overview", icon = icon("th")),
                menuItem("Lifestyle", tabName = "Lifestyle", icon = icon("home")),
                menuItem("Survey_Characteristics", tabName = "Survey_Characteristics", icon = icon("dashboard")),
                menuItem("Greenhouse_Gases", tabName = "Greenhouse_Gases", icon = icon("leaf")),
                menuItem("Naive_Bayes", tabName = "Naive_Bayes", icon = icon("car"))
            )
        ),
        dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "Overview",
                        
                      titlePanel(h1(strong("What you thought you knew..."))),
                      
                      p("Survey aims to anwer the following questions:"),
                      strong(p("Does the age range influence the lifestyle of an individual?")),
                      strong(p("What is the distinct characteristics of our participants?")),
                      strong(p("Are vegetarians more environmentally conscious?")),
                      
                      p("Finally, a predictive model to say if the person is vegetarian or not")),
                
                tabItem(tabName = "Lifestyle",
                      
                      h2("Bigram: Lifestyle"),
                      p("Group 1 - less than 30: Active, Healty but Partying"),
                      p("Group 2 - between 30 & 40: Stay balance, Extremely healthy, Prefers Gluten-Free & GMO-Free"),
                      p("Group 3 - more 40: Relaxation activities"),
                      selectInput(inputId="Choice",label="Your option pls", choices = c("< 30", "30-40", ">40"),selected = NULL,
                                  multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
                      
                     mainPanel(
                         plotOutput("bigramnet"))),    
                
                tabItem(tabName = "Survey_Characteristics",

                        mainPanel(
                          h2("TF-IDF: Survey Characteristics"),
                          p("Our respondents are mainly young people aged from 25 till 30 striving to lead a healthy & balanced lifestyle while occasionally consuming both rice & also fried food"),
                          plotOutput("tfidf")                         
)
    
                ),
                
                tabItem(tabName = "Greenhouse_Gases",
                        
                        mainPanel(
                          h2("Sentiment Analysis: Greenhouse gases and Meals"),
                          p("Being vegetarian doesn't necessarily mean they are more aware of greenhouse gas emission")
                        ),
                        selectInput(inputId="Decision", label="Vegetarian?", choices = c("Yes","No"), selected = NULL, multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        mainPanel(
                            splitLayout(cellWidths = c("55%", "55%"), plotOutput("vegYorN"), plotOutput("QQ6")),
),
                ),
                tabItem(tabName = "Naive_Bayes",
                        mainPanel(
                            h2("Naive Bayes Model"),
                            p("Likelihood of being a vegetarian or non-vegetarian by model with 83.3% accuracy"),
                            verbatimTextOutput("nv"))
                        
                        )
                        
    
    )))))

