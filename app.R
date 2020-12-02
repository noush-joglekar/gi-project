library(shinydashboard)
library(shinyMobile)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinymanager)

credentials <- data.frame(
  user = c("Patient0"),
  password = c("innovation"),
  stringsAsFactors = FALSE
)


mData <- readRDS('Data/microbiome.rds')

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "bioTrac - Trust your gut",
                  tags$li(actionLink("s", label = "Schedule a visit", icon = icon("stethoscope")),
                          class = "dropdown"),
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "bioTrac Clinic",
                                 message = "Your newest results are here!"
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "log your daily calories",
                                 icon("hamburger"),
                                 status = "info"
                               )
                  ),
                  dropdownMenu(type = "tasks", icon = icon("user"))
                  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("Diet", tabName = "diet", icon = icon("chart-pie")),
      menuItem("Mirobiome", tabName = "microbiome", icon = icon("chart-bar")),
      menuItem("Cohort Analysis", tabName = "population", icon = icon("users")),
      menuItem("Mood tracker", tabName = "mood", icon = icon("smile-beam"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 3, status = "success",
                    h3("About us")
                ),
                
                box(width = 3, status = "success",
                    h3("Our Mission")
                ),
                
                
                box(width = 3, status = "success",
                    h3("Workflow"),
                  tags$image(src="workflow.png", width = 280, height = 420)
                )
              )
      ),
      
      tabItem(tabName = "diet",
              h2("Widgets tab content")
      ),
      
      tabItem(tabName = "microbiome",
              fluidRow(
                box(plotlyOutput("mplot", height = 250)),
                
                box(
                  title = "Choose patient #",
                  selectInput('patientID', 'Patient', unique(mData$UserName)),
                )
              )
      )
      
      
    )
  )
  
)

server <- function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$mplot <- renderPlotly({
    df <- mData %>% filter(UserName == input$patientID)
    
    colors_micro <- c("lightgrey", "#afd35a", "#ec9bfa", "#3c3e00", "#aaa4e1", "#f4f4f4", "#a717d9","#f4e909",
                      "#ffaa00", "#ff0000", "#69ef7b", "#d20000", "#ff5084", "#b04fbc", "#5bc07f",
                      "#e242a8", "#ffaae8", "#df97ac", "#ffede7", "#0024ff", "#ffa787", "#71619c",
                      "#668e57", "#3d7600", "#1f72d4", "#12982d", "#dd6c43", "#c25945", "#b6601e",
                      "#01ceff", "#02531d", "#d7dd4c", "#8f0000", "#4b03a9", "#A0447F")
    
    micro_plot <- ggplot(data = df, aes(x=StudyDayNo, y = newvalue, fill=Species)) +
      geom_area(stat = "identity") +
      #facet_grid(.~UserName, scales = "free") +
      scale_fill_manual(values = colors_micro) +
      scale_x_discrete(drop = FALSE) +
      theme_classic() + 
      theme(strip.text.x = element_text(angle = 0, size = 6, face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 9),
            axis.title = element_text(size = 9),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            strip.background = element_rect(color = "grey"), 
            legend.position = "none",
          #  legend.text = element_text(size = 7),
           # legend.title = element_blank(),
            panel.spacing.x=unit(0.05, "lines")) +
      guides(fill = guide_legend(reverse = TRUE,
                                 keywidth = 0.5,
                                 keyheight = 0.5,
                                 ncol = 1)) +
      ylab("Relative Abundance")
    
    ggplotly(micro_plot)
  
  })
  
}

ui <- secure_app(ui)
shinyApp(ui, server)