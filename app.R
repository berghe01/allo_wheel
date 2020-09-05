
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(dplyr)
library(stringi)
library(DT)
library(janitor)
library(googlesheets4)
library(rvest)
library(purrr)
library(magrittr)
library(stringr)


#df <- read.csv("data.csv", header = TRUE, sep = ",")

df <- read_sheet("https://docs.google.com/spreadsheets/d/1vuZXmAeFlRAHJR6yp3s2mP0NpG2l-EsXmteVzjOLtOU/edit?usp=sharing")
df$values <- sapply(df$values, function(x) eval(parse(text = x)))
df$values <- as.numeric(df$values)
df <- data.frame(df)

ui <- fluidPage(
  
  useShinyjs(),
  
  h1(id = "mytitle", '"Allo"-wheel'),
  
  tags$style(HTML("#mytitle{
                  color:red;
                  font-size:70px;
                  font-family:Monoton;
                  font-style:bold}")
  ),
  
  
  tags$style(HTML("#adaptation {
                  color:black;
                  font-style:italic;
                  text-align: justify;}")),
  
  
  
  tags$style(type = "text/css",
             HTML("th { 
                    text-align: center;
                    color:red;
                    font-family:Helvetica Neue;
                    font-size:25px
                    }")
             
  ),
  
  tags$style(HTML("#explore1 { 
                    text-align: center;
                    color:red;
                    font-family:Helvetica Neue;
                    font-size:20px
                    }")
             
  ),
  
  tags$style(HTML("#risk_text { 
                    color:Red;
                    font-family:Helvetica Neue;
                    font-size:30px
                    }")
             
  ),
  
  
  tags$style(type = "text/css", 
             HTML(".hovertext text{
                  line-height:8px;
                  font-family:Helvetica Neue;
                  }")
  ),
  
  #  font-size:20px !important;
  
  tags$style(type = "text/css", 
             HTML(".hoverlayer {
                    opacity:0.90;
                  }")
  ),
  
  
  
  
  tags$style(type = "text/css",
             HTML("ul { text-align: center; }")
  ),
  
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Monoton');
                    @import url('//fonts.googleapis.com/css?family=Montserrat);
                    "))
  ),
  
  
  
  mainPanel(
    
    tabsetPanel(type = "tabs", 
                tabPanel("Instructions",
                         br(),
                         tags$p(id = "warning", 
                                HTML(paste0(
                                  'This app was created for medical professionals, as well as patients, 
                                     and serves as a resource to better visualize and understand the complexities of alloimmunization 
                                     in pregnancy. Alloimmunization refers to the presence of antibodies to red cell antigens, placing the fetus at risk for <b><i>Hemolytic Disease of the
                                     Fetus and Newborn</i></b>', 
                                  tags$a(href = "https://en.wikipedia.org/wiki/Hemolytic_disease_of_the_newborn", ' (HDFN)'), 
                                  '. You may "click/drill down" and "hover" over the boxes to learn more 
                                     about each antigen. The organization of the allo-wheel is adapted from a previously published',
                                  tags$a(href = "https://www.uptodate.com/contents/management-of-non-rhd-red-blood-cell-alloantibodies-during-pregnancy?search=Management%20of%20non-RhD%20red%20blood%20cell%20alloantibodies%20during%20pregnancy.&source=search_result&selectedTitle=1~150&usage_type=default&display_rank=1>", " table. "),
                                  'Antigen descriptions are scraped from Wikipedia with links to the full articles.
                                     <br>
                                     <i>Disclaimer: this is an illustrative tool, <b style = color:red>NOT</b> medical advice!</i>')
                                )
                         ),
                         fluidRow("",
                                  column(6, style = "height:80px;", 
                                         
                                         selectInput(width = NULL, inputId = "select_antigen", label = h5(id = "selectinput",  "Click on allo-wheels or search antigen family:"),
                                                     choices =  c("select antigen" = " ", setNames(nm = df$newids)),
                                                     selectize = TRUE,
                                                     selected = "mytext"
                                         )
                                  )
                                  
                         ),
                         
                         fluidRow("", 
                                  column(12, style = "height:240px;", 
                                         div(dataTableOutput("table"), style = "font-size:90%; height:240px; overflow-y: scroll;"),
                                         br(),
                                         br()
                                  )
                         ),
                         
                         fluidRow(
                           column(6,
                                  align = "center",
                                  h5(id = "explore1", "Explore by risk for HDFN"),
                                  # br(),
                                  # br(),
                                  div(plotlyOutput("mystarburst1"), align = "center"),
                                  br()
                           ), 
                           column(6,
                                  align = "center",
                                  h5(id = "explore1", "Explore by antigen family"),
                                  # br(),
                                  # br(),
                                  div(plotlyOutput("mystarburst2"), align = "center"),
                                  br()
                           )
                           
                         ), 
                         br(), 
                         fluidRow( 
                           box(height = "30px", width = "400px", 
                               htmlOutput("mytext"))
                           
                         ),
                         br(),
                         fluidRow(
                           h5(id = "adaptation", "Adapted from", 
                              tags$a(href = "https://www.uptodate.com/contents/management-of-non-rhd-red-blood-cell-alloantibodies-during-pregnancy?search=Management%20of%20non-RhD%20red%20blood%20cell%20alloantibodies%20during%20pregnancy.&source=search_result&selectedTitle=1~150&usage_type=default&display_rank=1>", 
                                     " 'Red blood cell antibodies associated with hemolytic disease of the fetus and newborn;'"),
                              " Moise K J. Jr. Management of non-RhD red blood cell alloantibodies during pregnancy. In: UpToDate, Post TW
                              Waltham, MA. (Accessed September 4, 2020)"), 
                           br(), 
                           br()
                         )
                         
                         
                ),
                
                tabPanel("About", 
                         tags$p(HTML(paste0(br(),
                                            "<b>Eric Bergh, M.D.</b> is an OB/GYN, Maternal-Fetal Medicine Specialist and Fetal Surgeon",
                                            " in Houston, TX with interests in both fetal diseases and computer/data science.",
                                            " This web app was designed using R, Plotly and the Shiny 
                                                web framework. Please do not hesitate to contact ",
                                            tags$a(href = "https://twitter.com/ericberghMD", "@ericberghMD "),
                                            "with any questions/suggestions. The code for this app can be found ",
                                            tags$a(href = "https://github.com/berghe01/smfm_tweets/blob/master/app.R", "here. "), 
                                            br(), 
                                            br(),
                                            'The "allo"-wheel app draws heavily upon the following table which may be found in UpToDate, 
                                            <i><a href = https://www.uptodate.com/contents/management-of-non-rhd-red-blood-cell-alloantibodies-during-pregnancy?search=Management%20of%20non-RhD%20red%20blood%20cell%20alloantibodies%20during%20pregnancy.&source=search_result&selectedTitle=1~150&usage_type=default&display_rank=1>
                                            (Red blood cell antibodies associated with hemolytic disease of the fetus and newborn)</a></i>;
                                            permission was requested prior to publication on the web.',
                                            br(), 
                                            br(),
                                            "<i>Special thanks to <b>Ken J. Moise Jr., M.D.</b> <i>(author of the referenced UpToDate article)</i> for inspiring an interest in HDFN/alloimmunization 
                                            and to <b>Anthony Johnson, D.O.</b> and <b>Ramesha Papanna, M.D.</b> for their mentorship in fetal medicine 
                                            and fetal surgery.</i>")))
                         
                )
    ) 
    
    
  )
)



server <- function(input, output, session) {
  
  
  antigenselector1 <- reactive ({
    
    df %>%
      filter(sunburst_wheel == 1, 
             newids == input$select_antigen) %>%
      select(selector) %>%
      as.character('['(1))
    
  })
  
  antigenselector2 <- reactive ({
    
    df %>%
      filter(sunburst_wheel == 1, 
             newids == input$select_antigen) %>%
      select(selector) %>%
      as.character('['(1))
    
  })
  
  output$mystarburst1 <- renderPlotly({
    
    plot_ly(df %>%
              filter(sunburst_wheel == 1),
            customdata = ~ids_lookup,
            ids = ~ids,
            labels = ~labels,
            parents = ~parents,
            values = ~values,
            level = antigenselector1(),
            branchvalues = 'total',
            type = 'sunburst',
            name = "",
            source = "subset", 
            hoverinfo = "text",
            hovertext = ~labels,
            maxdepth = 2,
            insidetextorientation = 'radial' 
    ) %>%
      config(displayModeBar = FALSE) %>%
      layout(hoverlabel = list(font=list(size=20)),
             #grid = list(columns =2, rows = 1),
             margin = list(l = 0, r = 0, b = 0, t = 0 ),
             sunburstcolorway = c(
               "#EA4492",
               "#FF9CDA",
               "#428CD4",
               "#004E9A"),
             extendsunburstcolors = TRUE
      )
  })
  
  
  output$mystarburst2 <- renderPlotly({ 
    
    plot_ly(df %>%
              filter(sunburst_wheel == 2), 
            customdata = ~ids_lookup,
            ids = ~ids, 
            labels = ~labels, 
            parents = ~parents, 
            values = ~values,
            level = antigenselector2(),
            #level = input$select_antigen,
            branchvalues = 'total', 
            type = 'sunburst', 
            name = "",
            source = "subset", 
            hoverinfo = "text",
            hovertext = ~labels,
            maxdepth = 2,
            insidetextorientation = 'radial'
    ) %>%
      config(displayModeBar = FALSE) %>%
      layout(hoverlabel = list(font=list(size=20)),
             margin = list(l = 0, r = 0, b = 0, t = 0) 
      )
  })
  
  
  
  hoverData <- reactive({
    
    return(currentEventData <- unlist(event_data(event = "plotly_click", source = "subset", priority = "event")))
    
  })
  
  
  
  newdf <- reactive({
    if(is.null(hoverData()[3])){
      df %>%
        filter(ids_lookup == "severe_1") %>%
        select(titles, link, node, element)
    }else{
      df %>% 
        filter(ids_lookup == hoverData()[3]) %>%
        select(titles, link, node, element, risk)
    }
  })
  
  
  
  output$mytext <- renderText({
    
    paste("<em style = 'font-size:15px;'>Risk for HDFN:", as.character(newdf()[1,5]), "</em>", sep = " ")

    
  })
  
  
  output$table <- DT::renderDataTable({
    
    
    if(!is.null(hoverData()[3]) & 
       (newdf()$titles != "severe HDFN" & newdf()$titles != "moderate HDFN" & newdf()$titles != "mild HDFN" & newdf()$titles != "no HDFN")) {
      
      a = newdf()[1,1]
      b = as.character(read_html(newdf()[1,2]) %>%
                         html_nodes(newdf()[1,4]) %>%
                         html_text())
      
      if(!is.na(as.numeric(newdf()[1,3]))){
        
        b = b[as.numeric(newdf()[1,3])]
        
      } else {
        
        c = as.integer(unlist(strsplit(as.character(newdf()[1,3]), ",")))
        c = paste("c(", paste(c[1], c[2], c[3], c[4], sep = ","), ")")
        c = gsub(",NA", "", c)
        b = b[eval(parse(text = c))]
        b = paste(b, collapse = "<br><br>")
        
        
      }
      
      tibble(a = a, 
             b = b) %>%
        mutate(b = str_replace(b, "<p>", '<p>"'), 
               b = str_replace(b, "</p>", '"</p>'), 
               b = paste("<i style = color:blue;>Description scraped from Wikipedia: </i><a href =", newdf()[1,2], "> <i><strong style = color:red;>link to full article</strong></i> </a>",br(), '<strong>"</strong>', b, '<strong>"</strong>', sep = "")) %>%
        t() %>%
        tibble() %>%
        row_to_names(row_number = 1) %>%
        select(1) %>%
        datatable(
          filter = "none",
          rownames = FALSE,
          options = list(dom = "t",
                         bSort = FALSE),
          escape = FALSE
        )
      
    } else if(!is.null(hoverData()[3]) & (newdf()$titles == "severe HDFN" | newdf()$titles == "moderate HDFN" | newdf()$titles == "mild HDFN" | newdf()$titles == "no HDFN")) {
      
      
      tibble(a = newdf()[1,1],
             b = newdf()[1,2]) %>%
        t() %>%
        tibble() %>%
        row_to_names(row_number = 1) %>%
        select(1) %>%
        datatable(
          filter = "none",
          rownames = FALSE,
          options = list(dom = "t",
                         bSort = FALSE),
          escape = FALSE
        )
      
    } else if (is.null(hoverData()[[3]])) {
      
      
      tibble(`Click an antigen` = "") %>%
        datatable(
          filter = "none",
          rownames = FALSE,
          options = list(dom = "t",
                         bSort = FALSE),
          escape = FALSE
        )
    }
    
    
    
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
