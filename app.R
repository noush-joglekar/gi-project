library(shinydashboard)
library(shinyMobile)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinymanager)
library(reshape2)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #009900;
}"'

mData <- readRDS('Data/microbiome.rds')
nData <- readRDS('Data/nutriData.rds')

mood_df <- data.frame("happy" = sample(5:10,29,replace = TRUE, 
                               prob = sample(seq(0.5,1,0.1),6,TRUE)),
                      "sad" = sample(1:4,29,replace = TRUE, 
                                       prob = sample(seq(0,0.4,0.1),4,TRUE)),
                      "tired" = sample(3:10,29,replace = TRUE, 
                                       prob = sample(seq(0,0.7,0.1),8,TRUE)),
                      "dizzy" = sample(1:4,29,replace = TRUE, 
                                       prob = sample(seq(0,0.25,0.1),4,TRUE)),
                      "meh" = sample(2:6,29,replace = TRUE, 
                                       prob = sample(seq(0,0.4,0.1),5,TRUE)))

credentials <- data.frame(
  user = unique(mData$UserName),
  password = paste("innovation",unique(mData$UserName),sep = "_"),
  stringsAsFactors = FALSE
)

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

messageData = data.frame("from"=c("bioTrac clinic"), 
                         "message"= c("Track your shipment"))
                         
taskData = data.frame("value"=c(100,80,35), 
                         "text"= c("Mood","Calories","Water intake"),
                      "color" = c("green","yellow","red"))

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "bioTrac - Trust your gut",
                  dropdownMenuOutput("messageMenu"),
                  dropdownMenuOutput("taskMenu")
                  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("Diet", tabName = "diet", icon = icon("chart-pie")),
      menuItem("Microbiome", tabName = "microbiome", icon = icon("chart-area")),
      menuItem("Clinical trial", tabName = "population", icon = icon("users")),
      menuItem("Mood tracker", tabName = "mood", icon = icon("smile-beam"))
    )
  ),
  dashboardBody(
    tags$style(js),
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    tabItems(
      # First tab content

      tabItem(tabName = "diet",
            fluidRow(
              box(h4("Scanner coming soon .."),
                  background = "olive"),
              dateRangeInput('dateRange',label = "Period to analyze : ",
                             format = "mm/dd/yyyy",
                             start = Sys.Date()-60, 
                             end=Sys.Date(),
                             startview = "year",
                             separator = " - "),
                box(plotlyOutput("nplot", height = 350)),
              )
      ),
      
      tabItem(tabName = "microbiome",
              fluidRow(
                box(h4("Newest results still being analyzed .."),
                    background = "olive"),
                dateRangeInput('dateRange',label = "Period to analyze : ",
                               format = "mm/dd/yyyy",
                               start = Sys.Date()-60, 
                               end=Sys.Date(),
                               startview = "year",
                               separator = " - "),
                box(plotlyOutput("mplot", height = 350)),
              )
      ),
      
      tabItem(tabName = "population",
              h3("Contact your clinician to see if you are eligible")
      ),
      
      tabItem(tabName = "mood",
              h3("How are you feeling today?"),
              fluidRow(
                box(
                  title = icon("smile-beam","fa-2x"),
                  sliderInput("s1", "Happy", 1, 10, 10)
                ),
                box(
                  title = icon("sad-cry","fa-2x"),
                  sliderInput("s2", "Sad", 1, 10, 10)
                ),
                box(
                  title = icon("tired","fa-2x"),
                  sliderInput("s3", "Tired", 1, 10, 10)
                ),
                box(
                  title = icon("dizzy","fa-2x"),
                  sliderInput("s4", "Dizzy", 1, 10, 10)
                ),
                box(
                  title = icon("meh","fa-2x"),
                  sliderInput("s5", "Meh", 1, 10, 10)
                ),
                box(h3("View results for past days"),
                    textInput('nDays', 'Number of days', value = 7),
                  plotOutput("moodPlot", height = 350))
                
              )
      ),
      
      tabItem(tabName = "dashboard",
                column(width = 4,
              infoBoxOutput("ib1",width = 4),
              infoBoxOutput("ib2",width = 4),
              infoBoxOutput("ib3",width = 4),
              infoBoxOutput("ib4",width = 4),
              infoBox("Heart rate", "Sync your wearable", 
                      icon = icon("sync"), color = "olive")
                ),
              box(h4("Experiencing a flare-up?"),
              actionButton("s","Schedule a visit",
                           icon = icon("stethoscope")),
                  background = "red", width = 3)
      )
              )
      
    )
)
  

server <- function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  Dates <- reactiveValues()
  observe({
    Dates$SelectedDates <- c(as.character(format(input$input_var_name[1],
                                                 format = "%m/%d/%Y")),
                             as.character(format(input$dateRange[2],format = "%m/%d/%Y")))
  })
  
  output$ib1 <- renderInfoBox({
    infoBox("Diet", 
            a("Scan a food item", onclick = "openTab('diet')", href="#"),
            icon = icon("barcode"),
            color = "olive")
  })
  
  output$ib2 <- renderInfoBox({
    infoBox("Microbiome",
            a("Check results",onclick = "openTab('microbiome')", href="#"),
            icon = icon("chart-area"), 
            color = "orange")
  })
  
  output$ib3 <- renderInfoBox({
    infoBox("Clinical trial",
            a("Participate",onclick = "openTab('population')", href="#"),
            icon = icon("users"), color = "olive")
  })
  
  output$ib4 <- renderInfoBox({
    infoBox("Mood",
            a("Enter your mood",onclick = "openTab('mood')", href="#"),
            icon = icon("smile-beam"), color = "orange")
  })
  
  
  output$moodPlot <- renderPlot({
    
    a = c(seq_len(input$s1),seq_len(input$s2),seq_len(input$s3),
          seq_len(input$s4),seq_len(input$s5))
    mood_df <- rbind(mood_df,a)
    mood_df <- mood_df[nrow(mood_df)-input$nDays:nrow(mood_df),]
    mdf <- melt(mood_df,id.vars = NULL)
    
    ggplot(mdf,aes(x=variable, y = value, fill = variable)) + 
      geom_violin() + theme_classic() + 
      scale_fill_brewer(palette = "Set2") + 
      theme(legend.position = "none",
            axis.text.x = element_text(size = 13))
    
  })
  
  
  
  output$messageMenu <- renderMenu({
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  output$taskMenu <- renderMenu({
    tsks <- apply(taskData, 1, function(row) {
      taskItem(text = row[["text"]], 
               value = row[["value"]],
               color = row[["color"]])
    })
    
    dropdownMenu(type = "tasks", .list = tsks)
  })
  
  output$mplot <- renderPlotly({
    df <- mData %>% filter(UserName == reactiveValuesToList(result_auth)$user)
    
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
           # legend.position = "bottom", legend.direction = "horizontal",
           # legend.text = element_text(size = 7),
           # legend.title = element_blank(),
           legend.position = "none",
            panel.spacing.x=unit(0.05, "lines")) +
      guides(fill = guide_legend(reverse = TRUE,
                                 keywidth = 0.5,
                                 keyheight = 0.5,
                                 ncol = 1)) +
      ylab("Relative Abundance")
    
    ggplotly(micro_plot)
  
  })
  
  
  output$nplot = renderPlotly({
    df2 = nData %>% filter(UserName == reactiveValuesToList(result_auth)$user)
      
    colors_nutr = sample(c("#bf0000", "#f29d3d", "#a3d9b1", "#bfd9ff", "#f780ff", 
                       "#ff0000", "#ffe1bf", "#00d957", "#0020f2", "#e60099", 
                       "#730f00", "#7f6600", "#336655", "#293aa6", "#a6538a", 
                       "#8c4f46", "#e5c339", "#00ffcc", "#333a66", "#40202d", 
                       "#f29979", "#fbffbf", "#00b3a7", "#8091ff", "#cc335c", 
                       "#594943", "#a3d936", "#003033", "#300059", "#400009", 
                       "#cc5200", "#354020", "#39c3e6", "#cbace6", "#d9a3aa",
                       "#7f5940", "#6a8040", "#006fa6", "#cc00ff", "#402910",
                       "#0a4d00", "#566573", "#83008c"))[1:41]
      
    nutr_plot <- ggplot(data = df2, aes(x = StudyDayNo, y = newvalue , fill = Nutrient)) +
      geom_area(stat = "identity") +
      facet_grid(.~UserName, scales = "free") +
      scale_fill_manual(values= colors_nutr) +
      scale_x_discrete(drop = FALSE) +
      theme_classic() +
      theme(strip.text.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 9),
            axis.title = element_text(size = 9),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            strip.background = element_blank(),
            #legend.position = "bottom", legend.direction = "horizontal",
            #legend.text = element_text(size = 7),
            #legend.title = element_blank(),
            legend.position = "none",
            panel.spacing.x=unit(0.05, "lines")) +
      guides(fill = guide_legend(reverse = TRUE,
                                 keywidth = 0.5,
                                 keyheight = 0.5,
                                 ncol = 5)) +
      ylab("sqrt(Relative Abundance)")
    
    ggplotly(nutr_plot)
  })
  
}

ui <- secure_app(ui)
shinyApp(ui, server)


## Testing purposes:
#preview_mobile(appPath = system.file("app.R", package = "shinyMobile"), device = "iphoneX")