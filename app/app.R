# General settings, data and libraries
library(shiny)
library(zoo)
library(dplyr)
library(stringr)
load('./data/g.RData')
#g.sub <- read.csv('./data/g.csv')

colnames(g.sub) <- gsub("\\.", " ",colnames(g.sub))

countries = c("Argentina","Australia","Austria","Belarus", "Belgium","Brazil", "Bulgaria","Canada", "Chile", "Colombia", 
              "Croatia","Czechia","Denmark","Ecuador", "Egypt", "Estonia",
              "Finland","France","Germany","Greece","Hong Kong","Hungary","India", "Indonesia", "Ireland","Israel", "Italy", "Japan",
              "Latvia","Lebanon", "Lithuania","Luxembourg","Malta","Malaysia", "Mexico", "Netherlands","New Zealand", "Nigeria","North Macedonia", "Norway",
              "Pakistan", "Poland","Portugal",
              "Romania","Russia","Saudi Arabia", "Serbia", "Singapore", "Slovakia","Slovenia", "South Africa", "South Korea", 
              "Spain","Sweden", "Switzerland","Taiwan", "Thailand", "Turkey", "Ukraine","United Kingdom", "United States", "Venezuela", "Vietnam")
              
vars = c ('Places of work','Public transport hubs','Residential places', 'Retail and recreation','Grocery shops and markets','Parks and outdoor areas')

date.min = min(g.sub$date)
date.max = max(g.sub$date)

date.seq.day = c(seq(as.Date(date.min), as.Date(date.max),1))
date.seq.week = c(seq((as.Date('2020-02-17')), as.Date(date.max),7))
date.seq.month = c(as.Date('2020-02-15'), as.Date('2020-03-01'), as.Date('2020-04-01'), as.Date('2020-05-01'), as.Date('2020-06-01'), 
                   as.Date('2020-07-01'), as.Date('2020-08-01'), as.Date('2020-09-01'), as.Date('2020-10-01'), as.Date('2020-11-01'), 
                   as.Date('2020-12-01'), as.Date('2021-01-01'), as.Date('2021-01-15'))

labels.seq.day = paste0(as.numeric(as.character(substr(date.seq.day, 9, 10))), '\n', month.abb[as.numeric(as.character(substr(date.seq.day, 6, 7)))])
labels.seq.week = paste0(as.numeric(as.character(substr(date.seq.week, 9, 10))), '\n', month.abb[as.numeric(as.character(substr(date.seq.week, 6, 7)))])
labels.seq.month = paste0(as.numeric(as.character(substr(date.seq.month, 9, 10))), '\n', month.abb[as.numeric(as.character(substr(date.seq.month, 6, 7)))])

background.color = rgb(248, 244, 255, max=255) 
dark.color = rgb(24, 24, 38, max=255) 
red.1 = rgb(230, 0, 0, max=255, alpha=255) 
red.2 = rgb(228, 63, 41, max=255, alpha=255) 
red.3 = rgb(227, 135, 108, max=255, alpha=255) 
#blue.1 = rgb(24,48,100, max=255, alpha=255) 
blue.1 = rgb(0,48,135, max=255, alpha=255)  
blue.2 = rgb(10,80,161, max=255, alpha=255) 
blue.3 = rgb(30,135,180, max=255, alpha=255) 
#green.1 = rgb(0, 82, 33, max=255, alpha=255)
green.1 = rgb(0, 173, 67, max=255, alpha=255) 
green.2 = rgb(42, 111, 55, max=255, alpha=205)
green.3 = rgb(105, 155, 103, max=255, alpha=155)

s = 3

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel(windowTitle = 'COVID-19 Mobility',
        title = fluidRow(
            column(10, strong("The Impact of COVID-19 on Mobility,"), (" based on"), a(href='https://www.google.com/covid19/mobility/', "Google Community Mobility Reports")), 
            column(2, div(img(height = 0.8*100, width = 0.8*155, src = "logo.png", class = "pull-right")))
        )
    ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            helpText(h3("To update the plot, select countries, time period, type of mobility and focus on policy measure to visualize:")),

            hr(),
            
           selectInput(inputId = "country1",
                        label = h4("Select Country 1:"),
                        choices = countries, selected = 'Sweden'),
           
           selectInput(inputId = "country2",
                       label = h4("Select Country 2:"),
                       choices = countries, selected = 'United Kingdom'),
           
           selectInput(inputId = "country3",
                       label = h4("Select Country 3:"),
                       choices = c("", countries), selected = "France"),
           
           hr(),
            
           dateRangeInput("c_dates", h4("Select time period:"), start = date.min, end = date.max, min = date.min, max = date.max, separator = 'until'),
            
           hr(),
           
           radioButtons("places", h4("Select type of location:"),
                        c("Places of work" = "Places of work",
                          "Public transport hubs" = "Public transport hubs",
                          "Residential places" = "Residential places",
                          "Retail and recreation" = "Retail and recreation",
                          "Grocery shops and markets" = "Grocery shops and markets",
                          'Parks and outdoor areas' = 'Parks and outdoor areas')),
           
          hr(),
           
          #h4("Show policy changes:"),
          
          radioButtons("policies", h4("Select focus of policy measure:"),
                       c("Workplace closing" = "workplace_d",
                         "Public transport closing" = "transport_d",
                         "Stay at home orders" = "stayhome_d",
                         "School closing" = "schools_d",
                         "Restrictions on gatherings" = "gathers_d",
                         'Restrictions on events' = 'events_d',
                         'Masks requirement' = 'masks_d')),
          
          tags$div(checkboxInput("policyup", "More restrictive [squares]     ", value = TRUE),  style="display:inline-block"),
          tags$div(checkboxInput("policydown", "Less restrictive [rhombuses]", value = TRUE) ,  style="display:inline-block"), 
          
          hr(),
           
            sliderInput("smooth", h4("Choose number of days for rolling mean:"),
                        min = 1, max = 7,
                        value = 7, step = 1),
           helpText(em("The rolling mean smooths the data by averaging daily observations.")),
            hr(),
            
            # Button
            downloadButton("save", "Save the plot"),
            
            br(),
  
        ),
        
        mainPanel(
            plotOutput("plot", width = "100%", height = '700px'),
            hr(),
            htmlOutput("notes")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    x.min <- reactive({min(input$c_dates[1])})
    x.max <- reactive({max(input$c_dates[2])})
    
    x.min.r <- reactive({min(input$c_dates[1]) - round(0.5 * (input$smooth-0.9),0)})
    x.max.r <- reactive({max(input$c_dates[2]) - round(0.5 * (input$smooth-0.9),0)})
    
    policy_level <- reactive({  str_remove(input$policies, '_d') })
    
    series.1 <-  reactive({  rollmean(unlist(g.sub[g.sub$country_region==input$country1 & g.sub$date >= x.min() & g.sub$date <= x.max(),input$places]), input$smooth, fill=NA)  })
    series.1p <-  reactive({  g.sub[g.sub$country_region==input$country1 & g.sub$date >= x.min() & g.sub$date <= x.max(), input$policies]  })
    series.1p.level <-  reactive({  g.sub[g.sub$country_region==input$country1 & g.sub$date >= x.min() & g.sub$date <= x.max(), policy_level()]  })
    dates.1 <-  reactive({g.sub$date[g.sub$country_region==input$country1 & g.sub$date >= x.min() & g.sub$date <= x.max()]})
    series.1_1.na <- reactive ({ifelse(series.1p.level()==1, series.1(), NA)})
    dates.1_1.na <-  reactive ({ifelse(series.1p.level()==1, dates.1(), NA)})
    series.1_2.na <- reactive ({ifelse(series.1p.level()<1 & series.1p.level()>0, series.1(), NA)})
    dates.1_2.na <-  reactive ({ifelse(series.1p.level()<1 & series.1p.level()>0, dates.1(), NA)})
    series.1.y.max.na <-  reactive({last(na.omit(series.1()))})
    match.1 <-  reactive({max(which(series.1()==series.1.y.max.na()))})
    series.1.x.max.na <-  reactive({dates.1()[match.1()]})
    series.1.y.min.na <-  reactive({first(na.omit(series.1()))})
    match.1.min <-  reactive({min(which(series.1()==series.1.y.min.na()))})
    series.1.x.min.na <-  reactive({dates.1()[match.1.min()]})
    
    series.2 <-  reactive({  rollmean(unlist(g.sub[g.sub$country_region==input$country2 & g.sub$date >= x.min() & g.sub$date <= x.max(),input$places]), input$smooth, fill=NA)  })
    series.2p <-  reactive({  g.sub[g.sub$country_region==input$country2 & g.sub$date >= x.min() & g.sub$date <= x.max(), input$policies]  })
    series.2p.level <-  reactive({  g.sub[g.sub$country_region==input$country2 & g.sub$date >= x.min() & g.sub$date <= x.max(), policy_level()]  })
    dates.2 <-  reactive({g.sub$date[g.sub$country_region==input$country2 & g.sub$date >= x.min() & g.sub$date <= x.max()]})
    series.2_1.na <- reactive ({ifelse(series.2p.level()==1, series.2(), NA)})
    dates.2_1.na <-  reactive ({ifelse(series.2p.level()==1, dates.2(), NA)})
    series.2_2.na <- reactive ({ifelse(series.2p.level()<1 & series.2p.level()>0, series.2(), NA)})
    dates.2_2.na <-  reactive ({ifelse(series.2p.level()<1 & series.2p.level()>0, dates.2(), NA)})
    series.2.y.max.na <-  reactive({last(na.omit(series.2()))})
    match.2 <-  reactive({max(which(series.2()==series.2.y.max.na()))})
    series.2.x.max.na <-  reactive({dates.2()[match.2()]})
    
    series.3 <-  reactive({  rollmean(unlist(g.sub[g.sub$country_region==input$country3 & g.sub$date >= x.min() & g.sub$date <= x.max(),input$places]), input$smooth, fill=NA)  })
    series.3p <-  reactive({  g.sub[g.sub$country_region==input$country3 & g.sub$date >= x.min() & g.sub$date <= x.max(), input$policies]  })
    series.3p.level <-  reactive({  g.sub[g.sub$country_region==input$country3 & g.sub$date >= x.min() & g.sub$date <= x.max(), policy_level()]  })
    dates.3 <-  reactive({g.sub$date[g.sub$country_region==input$country3 & g.sub$date >= x.min() & g.sub$date <= x.max()]})
    series.3_1.na <- reactive ({ifelse(series.3p.level()==1, series.3(), NA)})
    dates.3_1.na <-  reactive ({ifelse(series.3p.level()==1, dates.3(), NA)})
    series.3_2.na <- reactive ({ifelse(series.3p.level()<1 & series.3p.level()>0, series.3(), NA)})
    dates.3_2.na <-  reactive ({ifelse(series.3p.level()<1 & series.3p.level()>0, dates.3(), NA)})
    series.3.y.max.na <-  reactive({last(na.omit(series.3()))})
    match.3 <-  reactive({max(which(series.3()==series.3.y.max.na()))})
    series.3.x.max.na <-  reactive({dates.3()[match.3()]})
    
    y.min <- reactive ({min (series.1(), series.2(), series.3(), na.rm=TRUE) })
    y.max <- reactive ({max (series.1(), series.2(), series.3(), na.rm=TRUE) })
    
    x.min2 <- reactive({series.1.x.min.na()})
    x.max2 <- reactive({series.1.x.max.na()})
    
    output$notes <- renderText({ 
        paste('<em>Notes:</em> The plot shows relative changes in mobility (visits and length of stay) for a specific category of places within a country compared to a baseline. 
        The baseline is computed as the median for the day of the week during the 5-week period between January 3 and February 6, 2020.  
        Hence, the plot shows how mobility has changed relative to the situation in the same country in the begining of the year. 
        For more information, see <a target="_blank" href="https://www.google.com/covid19/mobility/">Google Community Mobility Reports</a>.
        The squares indicate when policy restrictions were introduced or strengthened, and the rhombuses show when they were relaxed.
        For details on the policy coding, see the <a target="_blank" href="https://github.com/OxCGRT/covid-policy-tracker">Oxford COVID-19 Government Response Tracker</a>.
        This web app has been put together by <a target="_blank" href="https://twitter.com/DToshkov">Dimiter Toshkov</a> with <code>R</code>, <code>RStudio</code> and <code>Shiny</code>.')
    })
    
 
    plotInput <- function(){
        y.range = abs(y.min() - y.max())
        dates.range = as.numeric(abs(x.max.r() - x.min.r()))
        n.lines = ifelse (y.range > 100, 30, ifelse(y.range > 50, 10, 5))
        
        date.seq.day = date.seq.day[date.seq.day <= series.1.x.max.na() & date.seq.day >=series.1.x.min.na()]
        date.seq.week = date.seq.week[date.seq.week <= series.1.x.max.na() & date.seq.week >=series.1.x.min.na()]
        date.seq.month = date.seq.month[date.seq.month <= series.1.x.max.na() & date.seq.month >=series.1.x.min.na()]
        
        labels.seq.day = paste0(as.numeric(as.character(substr(date.seq.day, 9, 10))), '\n', month.abb[as.numeric(as.character(substr(date.seq.day, 6, 7)))])
        labels.seq.week = paste0(as.numeric(as.character(substr(date.seq.week, 9, 10))), '\n', month.abb[as.numeric(as.character(substr(date.seq.week, 6, 7)))])
        labels.seq.month = paste0(as.numeric(as.character(substr(date.seq.month, 9, 10))), '\n', month.abb[as.numeric(as.character(substr(date.seq.month, 6, 7)))])
        
        n.dates = if (dates.range > 7*12+5) {date.seq.month} else if (dates.range > 2*8+2) {date.seq.week} else {date.seq.day} 
        n.labels = if (dates.range > 7*12+5) {labels.seq.month} else if (dates.range > 2*8+2) {labels.seq.week} else {labels.seq.day} 
        
        
        par(mfrow=c(1,1), # number and distribution of plots
            oma=c(0.5,0,0,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
            mar=c(2,2,1,0), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
            bty='n', # no box
            cex = 1.25*s, # magnification of text and symbols
            xpd = FALSE, # clipping of plotting to the figure region
            ann = FALSE, # switch off titles,
            bg=background.color # background color
        )
        
        #plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c (x.min() + round(0.5 * (input$smooth-0.9),0), x.max() - round(0.5 * (input$smooth-0.9),0) + 0.007*dates.range), ylim = c(min(0, y.min()), max(0, y.max())))
        plot(NULL, yaxt = 'n', xaxt = 'n', xlim = c(x.min2(), x.max2() + 0.007*dates.range), ylim = c(min(0, y.min()), max(0, y.max())))
        
        title(paste0('Changes in mobility for: ', input$places), cex.main = 0.6, col.main=dark.color)
        
        title(ylab = "Change compared to country baseline in Jan 3-Feb 6", line = 1, cex.lab=0.4, font.lab=2)
  
        
        axis (1, 
              line = -1, # position
              tck = -0.01,
              lwd = 0,
              col = dark.color, 
              col.axis = dark.color, # colors of the actual labels
              cex.axis = 0.25,
              font=2, # font type (bold)
              at=n.dates,
              labels= n.labels,
              las=1 # orientation of the labels
        )
        
        axis (2, 
              line = -0.75, # position
              tck = -0.01,
              lwd = 0,
              col = 'white', # the actual axis (line) 
              col.axis = dark.color, # colors of the actual labels
              cex.axis = 0.25, 
              font=2, # font type (bold)
              at=seq(-600, 600, n.lines), # where to put labels  
              labels= paste0(seq(-600, 600, n.lines), "%"), # text of labels 
              las=1 # orientation of the labels
        )

        segments(x0 = rep(x.min.r(),length(seq(-600,600,n.lines))), x1 = rep(x.max.r(),length(seq(-600,600,n.lines))), y0 = seq(-600,600,n.lines), y1 = seq(-600,600,n.lines),col='white', lwd=0.5*s )
        segments(x0 = x.min.r(), x1 = x.max.r(), y0 = 0, y1 = 0,col='white', lwd=2*s )
        
        segments (x0 = as.numeric(n.dates), x1 = as.numeric(n.dates),y0 = rep (min(0, y.min()), length(as.numeric(n.dates))), y1 = rep (max(0, y.max()), length(as.numeric(n.dates))), col='white', lwd=0.5*s)
        segments (x0 = as.numeric(date.seq.day), x1 = as.numeric(date.seq.day),y0 = rep (min(0, y.min()), length(as.numeric(date.seq.day))), y1 = rep (max(0, y.max()), length(as.numeric(date.seq.day))), col='white', lwd=0.2*s)
        #abline(v = as.numeric(n.dates), col='white', lwd=0.5*s)
        #abline(v = as.numeric(date.seq.day), col='white', lwd=0.1*s)
        
        lines (dates.1(), series.1(), col=red.3, lwd=1.8*s)
        lines (dates.1_2.na(), series.1_2.na(), col=red.1, lwd=1.8*s, lty='dotdash')
        lines (dates.1_1.na(), series.1_1.na(), col=red.1, lwd=1.8*s)
        
        lines (dates.2(), series.2(), col=blue.3, lwd=1.8*s)
        lines (dates.2_2.na(), series.2_2.na(), col=blue.1, lwd=1.8*s, lty='dotdash')
        lines (dates.2_1.na(), series.2_1.na(), col=blue.1, lwd=1.8*s)
        
        lines (dates.3(), series.3(), col=green.3, lwd=1.8*s)
        lines (dates.3_2.na(), series.3_2.na(), col=green.1, lwd=1.8*s, lty='dotdash')
        lines (dates.3_1.na(), series.3_1.na(), col=green.1, lwd=1.8*s)
        
        points (x = series.1.x.max.na() , y = series.1.y.max.na(), col=red.1, bg=red.1, pch=19, cex = 0.6)
        points (x = series.2.x.max.na() , y = series.2.y.max.na(), col=blue.1, bg=blue.1, pch=19, cex = 0.6)
        points (x = series.3.x.max.na() , y = series.3.y.max.na(), col=green.1, bg=green.1, pch=19, cex = 0.6)
        
        if (input$policyup==TRUE){
          points (x = dates.1()[which (series.1p()>0)], y = series.1()[which (series.1p()>0)], col='white', bg=red.1, pch=22, cex = 0.8)
          points (x = dates.2()[which (series.2p()>0)], y = series.2()[which (series.2p()>0)], col='white', bg=blue.1, pch=22, cex = 0.8)
          points (x = dates.3()[which (series.3p()>0)], y = series.3()[which (series.3p()>0)], col='white', bg=green.1, pch=22, cex = 0.8)
          }
        
        if (input$policydown==TRUE){
          points (x = dates.1()[which (series.1p()<0)], y = series.1()[which (series.1p()<0)], col='white', bg=red.1, pch=23, cex = 0.8)
          points (x = dates.2()[which (series.2p()<0)], y = series.2()[which (series.2p()<0)], col='white', bg=blue.1, pch=23, cex = 0.8)
          points (x = dates.3()[which (series.3p()<0)], y = series.3()[which (series.3p()<0)], col='white', bg=green.1, pch=23, cex = 0.8)
        }

        text (g.sub$country_region_code[g.sub$country_region==input$country1][1], x = series.1.x.max.na()+0.007*dates.range , y = series.1.y.max.na(), col=red.1, cex = 0.5, adj=0)
        text (g.sub$country_region_code[g.sub$country_region==input$country2][1], x = series.2.x.max.na()+0.007*dates.range , y = series.2.y.max.na(), col=blue.1, cex = 0.5, adj=0)
        text (g.sub$country_region_code[g.sub$country_region==input$country3][1], x = series.3.x.max.na()+0.007*dates.range , y = series.3.y.max.na(), col=green.1, cex = 0.5, adj=0)
        
        # explanatory note
        mtext(text=expression("Solid lines show periods of max restrictions in dark shades and of no restrictions in light shades. Dotdash lines show periods of partial or non-compulsory restrictions."), 
              side=3, line=-1.1, outer=T, col=dark.color, cex= 0.4 * s, font=1, adj=0.5, padj=1)
        #Squares and rhombuses show when measures were strengthened or relaxed. at = 0.02, 
        
        # data and signature
        mtext(text=expression("Data: Google Community Mobility Reports, accessed 20 January 2021."), 
              side=1, line=-0.75, outer=T, at = 0.01, col=blue.1, cex= 0.4 * s, font=1, adj=0, padj=1)
        mtext(text=expression("https://dimiter.shinyapps.io/covid-19_mobility/"), side=1, line=-0.75, outer=T, at = 1 - 0.01,
              col=blue.1, cex= 0.4 * s, font=1, adj=1, padj=1)
        
    }

    
    output$plot <- renderPlot({
        print(plotInput())
    })
        
   
    output$save <- downloadHandler(
        file = "google_mobility_plot.png" , 
        content = function(file) {
            png(file = file, width=1979, height=1400, res=96)
            plotInput()
            dev.off()
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
