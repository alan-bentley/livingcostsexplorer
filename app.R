#
# This is a Shiny app for visually exploring the Household Living-costs Price Indexes (HLPIs)
#
# Author: Alan Bentley, August 2017

library(shiny)
library(shinyBS)
library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(scales)
library(zoo)
library(tidyr)

#1. Read HLPI weight data 
wgt <- read.csv(url("http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/HouseholdLivingCostsPriceIndexes/HOTPJun17qtr/hlpi_weights.csv"))

nicenames <- function(data) {
  data$hlpi_name <- gsub("\\\\u0101","\u0101", data$hlpi_name) #remove added escape
  
  #create some nicer names
  data$hlpi_name <- gsub("Expenditure quintile 1 \\(low\\)","Lowest expenditure group", data$hlpi_name)
  data$hlpi_name <- gsub("Expenditure quintile 3","Middle expenditure group", data$hlpi_name)
  data$hlpi_name <- gsub("Expenditure quintile 5 \\(high\\)","Highest expenditure group", data$hlpi_name)
  data$hlpi_name <- gsub("Income quintile 1 \\(low\\)","Lowest income group", data$hlpi_name)
  data$hlpi_name <- gsub("Income quintile 3","Middle income group", data$hlpi_name)
  data$hlpi_name <- gsub("Income quintile 5 \\(high\\)","Highest income group", data$hlpi_name)

  #control default order of groups
  data$hlpi_name_f <- factor(data$hlpi_name, levels=c("All households","Beneficiary","M\u0101ori","Superannuitant",
                                                    "Lowest expenditure group","Expenditure quintile 2",
                                                    "Middle expenditure group","Expenditure quintile 4",
                                                    "Highest expenditure group",
                                                    "Lowest income group","Income quintile 2",
                                                    "Middle income group","Income quintile 4",
                                                    "Highest income group"))
  return(data)
}

wgt <- nicenames(wgt)

#control default order of groups
wgt$nzhec1_short_f <- factor(wgt$nzhec1_short, levels=c("Food",
                                                          "Alcohol and tobacco",
                                                          "Clothing and footwear",
                                                          "Housing",
                                                          "Contents and services",
                                                          "Health",
                                                          "Transport",
                                                          "Communication",
                                                          "Recreation and culture",
                                                          "Education",
                                                          "Miscellaneous",
                                                          "Interest"))

wgt$Year <- as.factor(wgt$year)#Year as factor for plots

#2. Read the inflation data
index <- read.csv(url("http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/HouseholdLivingCostsPriceIndexes/HOTPJun17qtr/hlpi_index_jun17.csv"))

index <- nicenames(index)

index$year <- format(as.Date(as.yearqtr(index$quarter)),"%Y")
index$yearmon <- format(as.Date(as.yearqtr(index$quarter)),"%Y%b")

isubg <- subset(index, level=="subgroup")

#3. Read the group facts
facts <- read.csv(url("http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/HouseholdLivingCostsPriceIndexes/HOTPJun17qtr/hlpi_groupfacts.csv"))

facts <- nicenames(facts)

factslong <- gather(facts, stat_type, stat, -c(hlpi,hlpi_name,hlpi_name_f,year))

#4. Set theme options
theme_ab <- function(base_size = 14, base_family = "Source Sans Pro"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_text(colour = "#706f6e"),
          axis.text.x = element_text(size=12),
          title = element_text(colour = "#ec6607"),
          legend.text = element_text(colour = "#706f6e"),
          legend.title = element_text(colour = "#706f6e"),
          axis.title = element_text(colour = "#706f6e"),
          plot.title = element_text(colour = "#ec6607", size=16, face="bold"),
          strip.background = element_rect(fill="black"), 
          strip.text = element_text (color="white", size=12, margin = margin(0,.5,0,.5, "cm")),
          strip.text.y = element_text(angle=0),
          legend.position="bottom",
          legend.box="vertical")
}

#5. Add radio tooltip (for hover text)
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

#6. Define UI
ui <- fluidPage(
  # tags$head(includeScript("google-analytics.js")),
  tags$style(type="text/css", "
           body {
    font-family: Source sans pro;
    width:90%;
    margin-left:auto;
    margin-right:auto;
           }
          .irs-bar-edge {background: #ec6607; border: #ec6607}
          .irs-bar {background: #ec6607; border: #ec6607}  
          .irs-from { color: white; background: #ec6607}
          .irs-to {color: white; background: #ec6607}
          .irs-single {color: white; background: #ec6607}
#loadmessage {
            width: 80%;
            padding: 150px 0px 1000px 0px;
            text-align: center;
            font-size: 200%;
            color: #ec6607;
            background-color: white;
           }
  "),


  headerPanel("","Explore living-costs in New Zealand"),
  
   # Application title
  titlePanel(div(
    # img(height = 56, width = 151, src = "SNZlogo1.png"),
    "Explore living-costs in New Zealand", style = "color:#ec6607; font-size: 120%;", tags$hr(style="border-color:#ec6607;margin-left:auto;"))),
 
   # Sidebar
  column(3, offset=0,
      wellPanel(
        conditionalPanel("input.which != 'more'",
        tags$div(title="Select a household group to explore", selectInput("HLPI_name", "Choose a group:", 
                    choices = c("Superannuitant", "Beneficiary", "M\u0101ori","Lowest expenditure group",
                                "Middle expenditure group","Highest expenditure group",
                                "Lowest income group","Middle income group",
                                "Highest income group")))),
        conditionalPanel("input.which === 'expenditure'",
        radioButtons("x", NULL,
                     choiceNames = list(
                     icon("home"), icon("child"), icon("pie-chart")),
                     choiceValues = list(
                        "exp_pw", "eqv_exp_pw", "weight"
                     ),
                     inline=TRUE),
        radioTooltip(id = "x", choice = "exp_pw", title = "$ per household", placement = "right", trigger = "hover"),
        radioTooltip(id = "x", choice = "eqv_exp_pw", title = "$ per person", placement = "right", trigger = "hover"),
        radioTooltip(id = "x", choice = "weight", title = "proportion", placement = "right", trigger = "hover"),
        tags$div(title="Choose which year(s) to view", sliderInput("years", div(icon("calendar"),"Year(s):"),
                    min = 2008, max = 2014, step=3, value = c(2014,2014), sep="", ticks=TRUE, width=150))),
        conditionalPanel("input.which === 'inflation'",
                         radioButtons("freq", "Frequency",
                                      choiceNames = list(
                                        div(icon("angle-right"),"Ann"),div(icon("angle-double-right"),"Qrt"),
                                        div(icon("angle-double-up"),"Tot")),
                                      choiceValues = list(
                                        "annual", "quarter","cumulative"
                                      ),
                                      inline=TRUE),
                         radioTooltip(id = "freq", choice = "annual", title = "Annual % change", placement = "right", trigger = "hover"),
                         radioTooltip(id = "freq", choice = "quarter", title = "Quarterly % change", placement = "right", trigger = "hover"),
                         radioTooltip(id = "freq", choice = "cumulative", title = "Cumulative inflation", placement = "right", trigger = "hover"),
                         tags$div(title="Choose which dates to view", sliderInput("years2", div(icon("calendar"),"Year(s):"),
                                                  min = 2009, max = 2018, step=1, value = c(2009,2018), sep="", ticks=TRUE))),
        conditionalPanel("input.which === 'group facts'",
                         radioButtons("fact", NULL,
                                      choiceNames = list(div(icon("home"),"home ownership"),
                                                         div(icon("th-large"),"facts")),
                                      choiceValues = list("home","averages"),
                                      inline=FALSE),
                         conditionalPanel("input.fact === 'averages'",
                         radioButtons("by", NULL,
                                      choiceNames = list(div(icon("home"),"household"),div(icon("child"),"person")),
                                      choiceValues = list("household","person"),
                                      inline=TRUE)),
                         tags$div(title="Choose which year(s) to view", radioButtons("years3", div(icon("calendar"),"Year:"),
                                      choices = list("2008"=2008, "2011"=2011, "2014"=2014),
                                      selected = 2014, inline=TRUE))
                         ),
        conditionalPanel("input.which === 'expenditure'",
                         hr(style="border-color:#ec6607"),
                         tags$div(title="Choose which level of the New Zealand Household Expenditure Classification", 
                                  radioButtons("nzhec_lvl", div("Detail", icon("sitemap")),
                     choiceNames = list(
                       icon("camera"),icon("search")
                       # , icon("search-plus")
                       ),
                     choiceValues = list(
                       "group", "subgroup"
                       # ,"class"
                     ),
                     inline=TRUE),
                     radioTooltip(id = "nzhec_lvl", choice = "group", title = "group", placement = "right", trigger = "hover"),
                     radioTooltip(id = "nzhec_lvl", choice = "subgroup", title = "subgroup", placement = "right", trigger = "hover")
                     # , radioTooltip(id = "nzhec_lvl", choice = "class", title = "class", placement = "right", trigger = "hover")
                     )),
        conditionalPanel("input.which === 'inflation'",
                         hr(style="border-color:#ec6607"),
                         tags$div(title="Choose which level of the New Zealand Household Expenditure Classification", 
                                  radioButtons("nzhec_lvl2", div("Detail", icon("sitemap")),
                                               choiceNames = list(
                                                 icon("image"),icon("camera"),icon("search")
                                                 # , icon("search-plus")
                                                 ),
                                               choiceValues = list(
                                                "allgroups", "group", "subgroup"
                                                #,"class"
                                               ),
                                               inline=TRUE),
                                  radioTooltip(id = "nzhec_lvl2", choice = "allgroups", title = "all groups", placement = "right", trigger = "hover"),
                                  radioTooltip(id = "nzhec_lvl2", choice = "group", title = "group", placement = "right", trigger = "hover"),
                                  radioTooltip(id = "nzhec_lvl2", choice = "subgroup", title = "subgroup", placement = "right", trigger = "hover")
                                  # ,radioTooltip(id = "nzhec_lvl2", choice = "class", title = "class", placement = "right", trigger = "hover")
                         )),
        conditionalPanel("input.nzhec_lvl != 'group' & input.which === 'expenditure'",
                         selectInput("class_sub", "Group to explore", 
                                     c("Food",
                                       "Alcohol and tobacco",
                                       "Clothing and footwear",
                                       "Housing",
                                       "Contents and services",
                                       "Health",
                                       "Transport",
                                       "Communication",
                                       "Recreation and culture",
                                       "Education",
                                       "Miscellaneous",
                                       "Interest"
                                     )
                                      , selected="Housing"
                                     , multiple=TRUE, selectize=FALSE)),
        conditionalPanel("input.nzhec_lvl2 === 'group' & input.which === 'inflation'",
                         selectInput("iclass_sub", "Group to explore", 
                                     c("Food",
                                       "Alcohol and tobacco",
                                       "Clothing and footwear",
                                       "Housing",
                                       "Contents and services",
                                       "Health",
                                       "Transport",
                                       "Communication",
                                       "Recreation and culture",
                                       "Education",
                                       "Miscellaneous",
                                       "Interest"
                                     ), selected="Housing", multiple=TRUE, selectize=FALSE)),
        conditionalPanel("input.nzhec_lvl2 === 'subgroup' & input.which === 'inflation'",
                         selectInput("iclass_sub2", "Subgroup to explore", 
                                     unique(isubg$nzhec_short), selected="Fruit & veg", multiple=TRUE, selectize=FALSE)),
        conditionalPanel("input.which != 'more' ",
                         conditionalPanel("input.which != 'group facts'",
        radioButtons("split", "Comparison group:",
                     choiceNames = list(
                       icon("object-group"),icon("object-ungroup")),
                     choiceValues = list(
                       "no", "yes"
                     ),
                     inline=TRUE),
        radioTooltip(id = "split", choice = "no", title = "Overlap comparison", placement = "right", trigger = "hover"),
        radioTooltip(id = "split", choice = "yes", title = "Parallel comparison", placement = "right", trigger = "hover")
        ),
                        conditionalPanel("input.which === 'group facts'",
                                         h4(strong("Comparison group:"))),
        selectInput("HLPI_name1b", NULL, 
                    choices = c("[none]","All households","Superannuitant", "Beneficiary", "M\u0101ori","Lowest expenditure group",
                                "Middle expenditure group","Highest expenditure group",
                                "Lowest income group","Middle income group",
                                "Highest income group")),
        conditionalPanel("input.HLPI_name1b != '[none]'",
      selectInput("HLPI_name2", div(icon("plus-square"),"Add another group:"), 
                  choices = c("[none]","Superannuitant", "Beneficiary", "M\u0101ori","Lowest expenditure group",
                              "Middle expenditure group","Highest expenditure group",
                              "Lowest income group","Middle income group",
                              "Highest income group"))),
      conditionalPanel("input.HLPI_name2 != '[none]'",
                       selectInput("HLPI_name3", div(icon("plus"),"Add another group:"), 
                                   choices = c("[none]","Superannuitant", "Beneficiary", "M\u0101ori","Lowest expenditure group",
                                               "Middle expenditure group","Highest expenditure group",
                                               "Lowest income group","Middle income group",
                                               "Highest income group")))),
      conditionalPanel("input.which === 'expenditure'",
                       tags$div(title="Download all the expenditure data", actionButton(inputId='b1', label="Expenditure", 
                                    icon = icon("download"), style="color: #fff; background-color: #ec6607; border-color: #ec6607", 
                                    onclick ="window.open('http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/HouseholdLivingCostsPriceIndexes/HOTPJun17qtr/hlpi_weights.csv', '_blank')"))),
      conditionalPanel("input.which === 'inflation'",
                       tags$div(title="Download all the inflation data", actionButton(inputId='b2', label="Inflation", 
                                    icon = icon("download"), style="color: #fff; background-color: #ec6607; border-color: #ec6607",
                                    onclick ="window.open('http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/HouseholdLivingCostsPriceIndexes/HOTPJun17qtr/hlpi_index_jun17.csv', '_blank')"))),
      conditionalPanel("input.which === 'group facts'",
                       tags$div(title="Download all the group facts", actionButton(inputId='b3', label="Facts", 
                                    icon = icon("download"), style="color: #fff; background-color: #ec6607; border-color: #ec6607",
                                    onclick ="window.open('http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/HouseholdLivingCostsPriceIndexes/HOTPJun17qtr/hlpi_groupfacts.csv', '_blank')"))),
      conditionalPanel("input.which === 'more'",
                       div(h2("About the app", style = "color:#ec6607"),
                           h4(icon("tablet", "fa-2x"),
                              "Build: R // shiny // shinyBS // ggplot2 // plyr // dplyr // grid // gridExtra // scales
                              // zoo // tidyr",
                              style = "color:#706f6e"),
                           # br(),
                           h4(icon("user-o", "fa-2x"),
                              "Designed by", a(href="http://www.linkedin.co.nz/in/bentleyalan", "Alan Bentley", style = "color:#706f6e"),
                              style = "color:#706f6e"),
                           hr(style="border-color:#ec6607"),
                           h2("Tell us what you think", style = "color:#ec6607"),
                           h4(icon("thumbs-o-up", "fa-2x"), icon("thumbs-o-down", "fa-2x"),
                              "This app is very young - we'd love to get your feedback
                              to help make it better", a(div("Send us an email", icon("envelope"), style = "color:#ec6607"), href = "mailto:alan.bentley@stats.govt.nz 
                                                         &body=Hi Alan, This app is awesome/awful [delete as appropriate] because...  My suggestions for improvement are...
                                                         &subject=Living-costs explorer feedback"), a(href="https://www.surveygizmo.com/s3/3864364/Living-costs-explorer", div("Feedback form",icon("file-text")), style = "color:#ec6607"),
                              style = "color:#706f6e"),
                           hr(style="border-color:#ec6607"),
                           h2("Open data", style = "color:#ec6607"),
                           h4(icon("creative-commons", "fa-2x"),
                              "Content is licensed under",
                              a(href="https://creativecommons.org/licenses/by/4.0/", "Creative Commons Attribution 4.0 International."),
"You are free to copy, distribute, and adapt, as long as you attribute the work to Stats NZ.",
                              style = "color:#706f6e")
                           )),
      width = 3, offset = 1, style = "color:black; font-family: Source sans pro; 
       # background:white; 
      border-color:#ec6607;font-size: 140%;")),
      
      
      # Show a plot of the data selected
      
      mainPanel( fluidRow(column(6,radioButtons("which", NULL,
                             choiceNames = list(
                               icon("shopping-basket", "fa-2x"), icon("line-chart", "fa-2x"),icon("users", "fa-2x"), icon("cogs", "fa-2x")),
                             choiceValues = list(
                               "expenditure","inflation","group facts","more"), inline=TRUE), 
                             style = "color:#ec6607" 
                             ),
                         radioTooltip(id = "which", choice = "expenditure", title = "Expenditure patterns", placement = "right", trigger = "hover"),
                         radioTooltip(id = "which", choice = "inflation", title = "Inflation", placement = "right", trigger = "hover"),
                         radioTooltip(id = "which", choice = "group facts", title = "Group facts", placement = "right", trigger = "hover"),
                         radioTooltip(id = "which", choice = "more", title = "About", placement = "right", trigger = "hover"),
                         conditionalPanel("input.which === 'expenditure'",
                                tags$div(title="Download the chart",
                                          column(3, downloadButton('downloadPlot2', 'PDF'),
                                                    downloadButton('downloadPlot', 'PNG'),
                                                    downloadButton('downloadData', 'CSV')))),
                         conditionalPanel("input.which === 'inflation'",
                                          column(3,downloadButton('idownloadPlot2', 'PDF'),
                                                 downloadButton('idownloadPlot', 'PNG'),
                                                 downloadButton('idownloadData', 'CSV'))),
                         conditionalPanel("input.which === 'group facts'",
                                          column(3,downloadButton('fdownloadPlot2', 'PDF'),
                                                 downloadButton('fdownloadPlot', 'PNG'),
                                                 downloadButton('fdownloadData', 'CSV')))),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div(div("",icon("spinner", "fa-4x fa-spin")),id="loadmessage")),
                conditionalPanel("input.which === 'expenditure'",
                plotOutput("tPlot1")),
                conditionalPanel("input.which === 'inflation'",
                                 plotOutput("tPlot2")),
                conditionalPanel("input.which === 'group facts'",
                                 plotOutput("tPlot3")),
                conditionalPanel("input.which === 'more'",
                                 div(h2("About the data", style = "color:#ec6607"),
                                     h4("The data in this app is from our Household Living-costs Price Indexes"), 
                                     hr(style="border-color:#ec6607"),
                                     h4(icon("shopping-basket", "fa-2x"),
                                     "Expenditure patterns are based on information from the Household Economic Survey, 
                                     triangulated with other sources"),
                                     br(),
                                     h4(icon("line-chart", "fa-2x"),
                                     "Inflation series are the Household Living-costs Price Indexes"),
                                     br(),
                                     h4(icon("sitemap", "fa-2x"),
                                     "The series are available at three levels of the New Zealand Household Expenditure Classification:",
                                     icon("camera"), "group", icon("search"), "subgroup", icon("search-plus"), "class"),
                                     br(),
                                     h4(icon("users", "fa-2x"),
                                     "Demographic facts about the household-groups are sourced from the Household Economic Survey:",
                                     icon("calendar"),
                                     "year labels relate to the weight reference year that follows the survey. 
                                     For example, 2014 relates to 2012/13 Household Economic Survey"),
                                     br(),
                                     h4(icon("child", "fa-2x"),
                                        "Expenditure and facts per person are for a 'standardised 1-person household'. 
                                        This helps aids comparison across household groups on a like-for-like basis, by
                                        controlling for differences in the size and age make-up of households"),
                                     hr(style="border-color:#706f6e"),
                                 actionButton(inputId='b4', label="Latest release", style="color: #fff; background-color: #ec6607; border-color: #ec6607; font-size: 140%;",
                                              icon = icon("bar-chart"), 
                                              onclick ="window.open('http://www.stats.govt.nz/browse_for_stats/economic_indicators/prices_indexes/hsehld-living-costs-price-indexes-info-rlses.aspx', '_blank')"),
                                 actionButton(inputId='b4', label="Learn More", style="color: #fff; background-color: #ec6607; border-color: #ec6607; font-size: 140%;",
                                              icon = icon("book"), 
                                              onclick ="window.open('http://www.stats.govt.nz/browse_for_stats/economic_indicators/prices_indexes/hlpi-backgrd-paper-oct-16.aspx', '_blank')"))),
        width = 9) 
)

#7. Define server logic 
server <- function(input, output, session) {
  
  observe({
    if (input$HLPI_name1b == "[none]"){
    updateSelectInput(session, "HLPI_name2", choices = c("[none]"))
    }
    else {
      updateSelectInput(session, "HLPI_name2",
                  choices = c("[none]","Superannuitant", "Beneficiary", "M\u0101ori","Lowest expenditure group",
                              "Middle expenditure group","Highest expenditure group",
                              "Lowest income group","Middle income group",
                              "Highest income group"))
    }
  })
  
  observe({
    if (input$HLPI_name1b == "[none]"|input$HLPI_name2 == "[none]"){
      updateSelectInput(session, "HLPI_name3", choices = c("[none]"))
    }
    else {
      updateSelectInput(session, "HLPI_name3",
                        choices = c("[none]","Superannuitant", "Beneficiary", "M\u0101ori","Lowest expenditure group",
                                    "Middle expenditure group","Highest expenditure group",
                                    "Lowest income group","Middle income group",
                                    "Highest income group"))
    }
  })
  

  #7a. Expenditure plot
  output$tPlot1 <- renderPlot({
    
    output$text1 <- renderText({
      if (input$x == "eqv_exp_pw"){
      paste("$ per person")
      }
      else if (input$x == "exp_pw"){
        paste("$ per household")
      }
      else{
        paste("proportion")
      }
    })
    
    output$text2 <- renderText({
        paste(input$nzhec_lvl)
    })
    
    
    
    if (input$nzhec_lvl == "group"){
      wgt1 <- subset(wgt, level == input$nzhec_lvl) 
    }
    else{
      if (input$class_sub %in% c("Interest","Housing")){
        wgt1 <- subset(wgt, level == input$nzhec_lvl & nzhec1_short %in% c("Interest","Housing"))
      }
      else{
    wgt1 <- subset(wgt, level == input$nzhec_lvl & nzhec1_short==input$class_sub)
      }
    }
    
    wgt1 <- subset(wgt1, year >= input$years[1] & year <= input$years[2])
    
  wgt2 <- subset(wgt1, hlpi_name == input$HLPI_name|hlpi_name == input$HLPI_name2|hlpi_name == input$HLPI_name3)
  

  
  
  #subset the data (for comparison)
  
  wgt3 <- subset(wgt1, hlpi_name == input$HLPI_name1b)
  
  
  #Show all groups in background (rather than seperate facet)
  if (input$split == "no"){
    wgt3 <- select(wgt3, -hlpi_name_f)
  }
  else{
    wgt3 <- wgt3
  }
  
  
  #create data download
  
  wgt4 <- subset(wgt1, hlpi_name == input$HLPI_name|hlpi_name == input$HLPI_name1b|hlpi_name == input$HLPI_name2|hlpi_name == input$HLPI_name3)
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('HLPI_', 'Expenditure', '.csv', sep='') },
    content = function(file) {
      write.csv(wgt4, file)
    }
  )
  
  if (input$HLPI_name1b == "[none]"){
    pal1 <- c("#ec6607","#51ae32","#4a148c","#0378bd") 
  }
  else{
    pal1 <- c("#706f6e","#ec6607","#51ae32","#4a148c","#0378bd")
  }
  
   a1 <- ggplot(data=wgt2, aes(x=eqv_exp_pw,y=reorder(nzhec_short,weight),group=nzhec_short))+
     geom_point(data=wgt3, aes(colour=hlpi_name, shape=Year), alpha=0.6, size=5)+
    facet_grid(factor(nzhec1_short, levels= nzhec1_short[order(-weight)]) ~ hlpi_name_f,
               scales="free_y",
               space="free",
               labeller=label_wrap_gen(width=20))+
    geom_point(aes(colour=hlpi_name_f, shape=Year), size=5)+
    theme_ab()+
    scale_colour_manual(values=pal1,
                        name="")+
     scale_shape_manual(values=c("2008"=1,"2011"=10,"2014"=19),
                         name="Year")+
    labs(title="Typical weekly expenditure \n", subtitle="per person (equivalised 1-person household)", y="", 
         x="",
         caption="Source: Stats NZ")+
    scale_x_continuous(labels=dollar)
   
   a2 <- ggplot(data=wgt2, aes(x=exp_pw,y=reorder(nzhec_short,weight),group=nzhec_short))+
     geom_point(data=wgt3, aes(colour=hlpi_name, shape=Year), alpha=0.6, size=5)+
     facet_grid(factor(nzhec1_short, levels= nzhec1_short[order(-weight)]) ~ hlpi_name_f,
                scales="free_y",
                space="free",
                labeller=label_wrap_gen(width=20))+
     geom_point(aes(colour=hlpi_name_f, shape=Year), size=5)+
     theme_ab()+
     scale_colour_manual(values=pal1,
                         name="")+
     scale_shape_manual(values=c("2008"=1,"2011"=10,"2014"=19),
                        name="Year")+
     labs(title="Typical weekly expenditure \n", subtitle="per household", y="", 
          x="",
          caption="Source: Stats NZ")+
     scale_x_continuous(labels=dollar)
   
   a3 <- ggplot(data=wgt2, aes(x=weight/100,y=reorder(nzhec_short,weight),group=nzhec_short))+
     geom_point(data=wgt3, aes(colour=hlpi_name, shape=Year), alpha=0.6, size=5)+
     facet_grid(factor(nzhec1_short, levels= nzhec1_short[order(-weight)]) ~ hlpi_name_f,
                scales="free_y",
                space="free",
                labeller=label_wrap_gen(width=20))+
     geom_point(aes(colour=hlpi_name_f, shape=Year), size=5)+
     theme_ab()+
     scale_colour_manual(values=pal1,
                         name="")+
     scale_shape_manual(values=c("2008"=1,"2011"=10,"2014"=19),
                        name="Year")+
     labs(title="Typical weekly expenditure \n", subtitle="proportion", y="", 
          x="",
          caption="Source: Stats NZ")+
     scale_x_continuous(labels=percent)
  
   
   if (input$x == "eqv_exp_pw"){
     print(a1)
   }
   else if (input$x == "exp_pw"){
     print(a2)
   }
   else{
     print(a3)
   }
   
   output$downloadPlot <- downloadHandler(
     filename = function() { paste('HLPI_', 'Expenditure', '.png', sep='') },
     content = function(file) {
       png(file, width=800, height=600, res=70)
       if (input$x == "eqv_exp_pw"){
         print(a1) 
       }
       else if (input$x == "exp_pw"){
         print(a2) 
       }
       else{
         print(a3) 
       }
       dev.off()
     }
   )
   
   output$downloadPlot2 <- downloadHandler(
     filename = function() { paste('HLPI_', 'Expenditure', '.pdf', sep='') },
     content = function(file) {
       cairo_pdf(file, width=11.7, height=8.3)
       if (input$x == "eqv_exp_pw"){
         print(a1) 
       }
       else if (input$x == "exp_pw"){
         print(a2) 
       }
       else{
         print(a3) 
       }
       dev.off()
     }
   )
   }, height = 600, width=800)

  #7b. Inflation plot
  
  output$tPlot2 <- renderPlot({
    
     indexb <- subset (index,year >= input$years2[1] & year <= input$years2[2])

     if (input$freq == "cumulative"){
     indexb <- ddply(indexb, .(nzhec, hlpi), mutate, change.tot=((index/index[1])-1)*100)
     }

    if (input$nzhec_lvl2 == "allgroups"){
      index_top <- subset(indexb,level=="All groups" & !is.na(change.a))
    }
    else if (input$nzhec_lvl2 == "group"){
      if (input$iclass_sub %in% c("Interest","Housing")){
        index_top <- subset(indexb,level=="group" & !is.na(change.a)
                            & nzhec_short %in% c("Interest","Housing"))
      }
      else{
        index_top <- subset(indexb,level=="group" & !is.na(change.a)
                            & nzhec_short==input$iclass_sub)
      }

    }
     else if (input$nzhec_lvl2 == "subgroup"){
       index_top <- subset(indexb,level=="subgroup" & !is.na(change.a)
                           & nzhec_short==input$iclass_sub2
       )
     }
    else{
      index_top <- subset(indexb,level=="class" & !is.na(change.a) 
                          & nzhec_short==input$class_sub
                          )
    }
    
    index_top1 <- subset(index_top, hlpi_name == input$HLPI_name1b)
    
    index_top2 <- subset(index_top, hlpi_name == input$HLPI_name|hlpi_name == input$HLPI_name2|hlpi_name == input$HLPI_name3)
    
    output$idownloadData <- downloadHandler(
      filename = function() { paste('HLPI_', 'Inflation', '.csv', sep='') },
      content = function(file) {
        write.csv(index_top2, file)
      }
    )
    
    i1 <- ggplot(data = index_top2, aes(x=as.yearqtr(quarter), y=change.a/100, group=hlpi_name))+
      geom_line(data = index_top1, colour="#706f6e",aes(alpha=hlpi_name), linetype="longdash", size=1)+
      geom_line(aes(linetype=hlpi_name, colour=hlpi_name, group=hlpi_name), size=1)+
      facet_grid(nzhec_short~., scales="free_y", labeller=label_wrap_gen(width=15))+
      scale_linetype_discrete(name="")+
      scale_colour_manual(values=c("#ec6607","#51ae32","#4a148c","#0378bd"),
                          name="")+
      scale_alpha_manual(values=c(0.6,0.8),
                          name="")+
      labs(title="Typical experience of inflation",
           subtitle="Annual change",
           x="Date", y="",
           caption="Source: Stats NZ")+
      theme_ab()+
      scale_y_continuous(labels=percent)+
      scale_x_yearqtr(format = "%YQ%q")
    
    i2 <- ggplot(data = index_top2, aes(x=as.yearqtr(quarter), y=change.q/100, group=hlpi_name))+
      geom_line(data = index_top1, colour="#706f6e",aes(alpha=hlpi_name), linetype="longdash")+
      geom_line(aes(linetype=hlpi_name, colour=hlpi_name, group=hlpi_name))+
      facet_grid(nzhec_short~., scales="free_y", labeller=label_wrap_gen(width=15))+
      scale_linetype_discrete(name="")+
      scale_colour_manual(values=c("#ec6607","#51ae32","#4a148c","#0378bd"),
                          name="")+
      scale_alpha_manual(values=c(0.6,0.8),
                         name="")+
      labs(title="Typical experience of inflation",
           subtitle="Quarterly change",
           x="Date", y="",
           caption="Source: Stats NZ")+
      theme_ab()+
      scale_y_continuous(labels=percent)+
      scale_x_yearqtr(format = "%YQ%q")
    
    i3 <- ggplot(data = index_top2, aes(x=as.yearqtr(quarter), y=change.tot/100, group=hlpi_name))+
      geom_line(data = index_top1, colour="#706f6e",aes(alpha=hlpi_name), linetype="longdash")+
      geom_line(aes(linetype=hlpi_name, colour=hlpi_name, group=hlpi_name), size=1)+
      geom_area(aes(fill=hlpi_name, group=hlpi_name), alpha=.2, position = "identity")+
      facet_grid(nzhec_short~., scales="free_y", labeller=label_wrap_gen(width=15))+
      scale_linetype_discrete(name="")+
      scale_colour_manual(values=c("#ec6607","#51ae32","#4a148c","#0378bd"),
                          name="")+
      scale_fill_manual(values=c("#ec6607","#51ae32","#4a148c","#0378bd"),
                          name="")+
      scale_alpha_manual(values=c(0.6,0.8),
                         name="")+
      labs(title="Typical experience of inflation",
           subtitle="Cumulative change",
           x="Date", y="",
           caption="Source: Stats NZ")+
      theme_ab()+
      scale_y_continuous(labels=percent)+
      scale_x_yearqtr(format = "%YQ%q")
    
    if (input$freq == "annual"){
      if (input$split == "no"){
      print(i1) 
      }
      else{
        print(i1+facet_grid(nzhec_short~hlpi_name_f))
      }
    }
    else if (input$freq == "quarter"){
      if (input$split == "no"){
        print(i2) 
      }
      else{
        print(i2+facet_grid(nzhec_short~hlpi_name_f))
      }
    }
    else{
      if (input$split == "no"){
        print(i3) 
      }
      else{
        print(i3+facet_grid(nzhec_short~hlpi_name_f)+
                geom_area(aes(fill=hlpi_name, group=hlpi_name), alpha=.2, position = "identity")+
          theme(axis.text.x  = element_text(angle=90)))
      }
    }
    
    output$idownloadPlot <- downloadHandler(
      filename = function() { paste('HLPI_', 'Inflation', '.png', sep='') },
      content = function(file) {
        png(file, width=800, height=600, res=70)
        if (input$freq == "annual"){
          if (input$split == "no"){
            print(i1)
          }
          else{
            print(i1+facet_grid(nzhec_short~hlpi_name_f))
          }
        }
        else if (input$freq == "quarter"){
          if (input$split == "no"){
            print(i2)
          }
          else{
            print(i2+facet_grid(nzhec_short~hlpi_name_f))
          }
        }
        else{
          if (input$split == "no"){
            print(i3)
          }
          else{
            print(i3+facet_grid(nzhec_short~hlpi_name_f)+
                    geom_area(aes(fill=hlpi_name, group=hlpi_name), alpha=.2, position = "identity")+
                    theme(axis.text.x  = element_text(angle=90)))
          }
        }
        dev.off()
      }
    )
    
    output$idownloadPlot2 <- downloadHandler(
      filename = function() { paste('HLPI_', 'Inflation', '.pdf', sep='') },
      content = function(file) {
        cairo_pdf(file, width=11.7, height=8.3)
        if (input$freq == "annual"){
          if (input$split == "no"){
            print(i1)
          }
          else{
            print(i1+facet_grid(nzhec_short~hlpi_name_f))
          }
        }
        else if (input$freq == "quarter"){
          if (input$split == "no"){
            print(i2)
          }
          else{
            print(i2+facet_grid(nzhec_short~hlpi_name_f))
          }
        }
        else{
          if (input$split == "no"){
            print(i3)
          }
          else{
            print(i3+facet_grid(nzhec_short~hlpi_name_f)+
                    geom_area(aes(fill=hlpi_name, group=hlpi_name), alpha=.2, position = "identity")+
                    theme(axis.text.x  = element_text(angle=90)))
          }
        }
        dev.off()
      }
    )
    
  }, height = 500, width=800)
  
 
  #7c. Facts plot
  
  output$tPlot3 <- renderPlot({
    factslong2 <- subset(factslong, year == input$years3)
  facts2 <- subset(factslong2, hlpi_name == input$HLPI_name|hlpi_name == input$HLPI_name1b|hlpi_name == input$HLPI_name2|hlpi_name == input$HLPI_name3)
  age <- subset(facts2, stat_type=="age")
  size <- subset(facts2, stat_type=="size")
  own <- subset(facts2, stat_type=="own_prop"|stat_type=="own_wm_prop")
  inc <- subset(facts2, stat_type=="income")
  exp <- subset(facts2, stat_type=="expenditure")
  inc_p <- subset(facts2, stat_type=="eqv_income")
  exp_p <- subset(facts2, stat_type=="eqv_exp")
  prop <- subset(facts2, stat_type=="prop_hhs")
  
  
  f1 <- ggplot(data=own) +
    geom_bar (aes(x=reorder(hlpi_name,stat_type), y=stat, fill=hlpi_name_f),  stat="identity")+
    facet_wrap(~stat_type, labeller=labeller(stat_type=c(own_prop="Own",
                                                             own_wm_prop="Own with mortgage")))+
    theme_ab()+
    scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                       name="")+
    labs(title="Home ownership", subtitle=paste(input$years3,"\n"), caption="Source: Stats NZ")+
    scale_x_discrete(NULL, expand = c(0,0))+
    theme(axis.text.y = element_blank(), axis.text.x = element_blank())+
    scale_y_continuous(NULL, limits=c(0,100), expand = c(0,0))+
    geom_text(aes(x = hlpi_name, y = stat/2, label = paste0(round(stat,digits=0), "%")), size=5, colour="white")+
    theme(legend.position = "bottom")+
    coord_polar(theta= "y", direction=1)
  
  f1b <- ggplot(data=own) +
    geom_bar (aes(x=reorder(hlpi_name,stat_type), y=stat, fill=hlpi_name_f),  stat="identity")+
    facet_grid(stat_type~year, labeller=labeller(stat_type=c(own_prop="Own",
                                                             own_wm_prop="Own with mortgage")))+
    theme_ab()+
    scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                       name="")+
    labs(title="Home ownership \n", caption="Source: Stats NZ")+
    scale_x_discrete(NULL, expand = c(0,0))+
    theme(axis.text.y = element_blank(), axis.text.x = element_blank())+
    scale_y_continuous(NULL, limits=c(0,100), expand = c(0,0))+
    geom_text(data=subset(own,stat_type=="own_prop"), aes(x = hlpi_name, y = stat/2, label = paste0(round(stat,digits=0), "%")), size=5, colour="white")+
    geom_text(data=subset(own,stat_type=="own_wm_prop"), aes(x = hlpi_name, y = stat/2, label = paste0(round(stat,digits=0), "%")), size=3, colour="white")+
    theme(legend.position = "bottom")+
    coord_polar(theta= "y", direction=1)

  
    f2 <- ggplot(data=age) +
      geom_bar (aes(x=reorder(hlpi_name,-stat), y=round(stat), fill=hlpi_name_f), stat="identity")+
      theme_ab()+
      scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                         name="")+
      labs(title="Age \n", subtitle="Average", x="",y="")+
      theme(axis.text.x = element_blank())+
      geom_text(aes(x = hlpi_name, y = stat/2, label = paste0(stat," years"), size = 4), colour = "white")+
      coord_flip()+
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#706f6e"))
    
    f3 <- ggplot(data=size) +
      geom_bar (aes(x=reorder(hlpi_name,-stat), y=stat, fill=hlpi_name_f), stat="identity")+
      theme_ab()+
      scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                         name="")+
      labs(title="Size \n", subtitle="Average", x="",y="")+
      theme(axis.text.x = element_blank())+
      geom_text(aes(x = hlpi_name, y = stat/2, label = paste0(stat," people"), size = 4), colour = "white")+
      coord_flip()+
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#706f6e"))
    
    f4 <- ggplot(data=inc) +
      geom_bar (aes(x=reorder(hlpi_name,-stat), y=stat/1000, fill=hlpi_name_f), stat="identity")+
      theme_ab()+
      scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                         name="")+
      labs(title="Income \n", x="",y="", subtitle="Median per household", caption="")+
      theme(axis.text.x = element_blank())+
      geom_text(aes(x = hlpi_name, y = stat/2000, label = paste0("$"," ", format(round(stat,digits=-3), big.mark=",", scientific=FALSE)), size = 4), 
                colour = ifelse(max(inc$stat)>min(inc$stat)*3,"grey","white"))+
      coord_flip()+
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#706f6e"))
    
    f5 <- ggplot(data=exp) +
      geom_bar (aes(x=reorder(hlpi_name,-stat), y=stat/1000, fill=hlpi_name_f), stat="identity")+
      theme_ab()+
      scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                         name="")+
      labs(title="Expenditure \n", x="",y="", subtitle="Median per household", caption="Source: Stats NZ")+
      theme(axis.text.x = element_blank())+
      geom_text(aes(x = hlpi_name, y = stat/2000, label = paste0("$"," ", format(round(stat,digits=-3), big.mark=",", scientific=FALSE)), size = 4), 
                colour = ifelse(max(inc$stat)>min(inc$stat)*3,"grey","white"))+
      coord_flip()+
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#706f6e"))
    
    f4b <- ggplot(data=inc_p) +
      geom_bar (aes(x=reorder(hlpi_name,-stat), y=stat/1000, fill=hlpi_name_f), stat="identity")+
      theme_ab()+
      scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                         name="")+
      labs(title="Income \n", x="",y="", subtitle="Median per 'equivalised' person", caption="")+
      theme(axis.text.x = element_blank())+
      geom_text(aes(x = hlpi_name, y = stat/2000, label = paste0("$"," ", format(round(stat,digits=-3), big.mark=",", scientific=FALSE)), size = 4), 
                colour = ifelse(max(inc$stat)>min(inc$stat)*3,"grey","white"))+
      coord_flip()+
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#706f6e"))
    
    f5b <- ggplot(data=exp_p) +
      geom_bar (aes(x=reorder(hlpi_name,-stat), y=stat/1000, fill=hlpi_name_f), stat="identity")+
      theme_ab()+
      scale_fill_manual (values=c("#ec6607","#706f6e","#51ae32","#4a148c","#0378bd"),
                         name="")+
      labs(title="Expenditure \n", x="",y="", subtitle="Median per 'equivalised' person", caption="Source: Stats NZ")+
      theme(axis.text.x = element_blank())+
      geom_text(aes(x = hlpi_name, y = stat/2000, label = paste0("$"," ", format(round(stat,digits=-3), big.mark=",", scientific=FALSE)), size = 4), 
                colour = ifelse(max(inc$stat)>min(inc$stat)*3,"grey","white"))+
      coord_flip()+
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#706f6e"))
    
      
      if (input$fact == "home"){
        print(f1)
      }
      else if (input$fact == "averages" & input$by == "household"){
        grid.arrange(top=textGrob(
          label = paste("\nHousehold facts, ",input$years3),
          x = unit(0, "lines"), 
          y = unit(0, "lines"),
          hjust = -0.1, vjust = -0.5,
          gp = gpar(fontsize = 18, fontfamily="Source Sans Pro", col="#706f6e")),
          f2, f3, f4, f5, ncol=2)
      }
        else{
          grid.arrange(top=textGrob(
            label = paste("\nHousehold facts, ",input$years3),
            x = unit(0, "lines"), 
            y = unit(0, "lines"),
            hjust = -0.1, vjust = -0.5,
            gp = gpar(fontsize = 18, fontfamily="Source Sans Pro", col="#706f6e")),
            f2, f3, f4b, f5b, ncol=2) 
        }
      
    output$fdownloadData <- downloadHandler(
      filename = function() { paste('HLPI_', 'Facts', '.csv', sep='') },
      content = function(file) {
        write.csv(facts2, file)
      }
    )
    
    output$fdownloadPlot <- downloadHandler(
      filename = function() { paste('HLPI_', 'Facts', '.png', sep='') },
      content = function(file) {
        png(file, width=800, height=600, res=70)
        if (input$fact == "home"){
          print(f1)
        }
        else if (input$fact == "averages" & input$by == "household"){
          grid.arrange(top=textGrob(
            label = paste("\nHousehold facts, ",input$years3),
            x = unit(0, "lines"), 
            y = unit(0, "lines"),
            hjust = -0.1, vjust = -0.5,
            gp = gpar(fontsize = 18, fontfamily="Source Sans Pro", col="#706f6e")),
            f2, f3, f4, f5, ncol=2)
        }
        else{
          grid.arrange(top=textGrob(
            label = paste("\nHousehold facts, ",input$years3),
            x = unit(0, "lines"), 
            y = unit(0, "lines"),
            hjust = -0.1, vjust = -0.5,
            gp = gpar(fontsize = 18, fontfamily="Source Sans Pro", col="#706f6e")),
            f2, f3, f4b, f5b, ncol=2) 
        }
        dev.off()
      }
    )
    
    output$fdownloadPlot2 <- downloadHandler(
      filename = function() { paste('HLPI_', 'Facts', '.pdf', sep='') },
      content = function(file) {
        cairo_pdf(file, width=11.7, height=8.3)
        if (input$fact == "home"){
          print(f1)
        }
        else if (input$fact == "averages" & input$by == "household"){
          grid.arrange(top=textGrob(
            label = paste("\nHousehold facts, ",input$years3),
            x = unit(0, "lines"), 
            y = unit(0, "lines"),
            hjust = -0.1, vjust = -0.5,
            gp = gpar(fontsize = 18, fontfamily="Source Sans Pro", col="#706f6e")),
            f2, f3, f4, f5, ncol=2)
        }
        else{
          grid.arrange(top=textGrob(
            label = paste("\nHousehold facts, ",input$years3),
            x = unit(0, "lines"), 
            y = unit(0, "lines"),
            hjust = -0.1, vjust = -0.5,
            gp = gpar(fontsize = 18, fontfamily="Source Sans Pro", col="#706f6e")),
            f2, f3, f4b, f5b, ncol=2) 
        }
        dev.off()
      }
    )
    
  }, height = 500, width=800)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

