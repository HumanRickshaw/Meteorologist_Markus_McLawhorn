library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(ggridges)
library(htmlwidgets)
library(plotly)
library(readxl)
library(scales)
library(shiny)
library(tidyr)
library(viridis)



#Read xlsx.
dfdf <- read_excel("./DENSE FOG.xlsx")



#Create Year, Create Date, format Hovertext and URL for links.
dfdf <- dfdf %>%
    mutate(Year = as.numeric(substr(Date, 1, 4)),
           Month = factor(as.numeric(substr(Date, 6, 7)), levels = 12:1, labels = rev(month.name)),
           Date = as.Date(Date),
           oTime = strftime(Time, format = "%H:%M:%S", tz = "PST" ),
           Hour = as.integer(substr(oTime, 1, 2)),
           Reactions = rowSums(.[6:12]),
           Hovertext = paste(Text,
                             format(Date, "%A %B %d, %Y"),
                             format(Time, "%H:%M"),
                             sep = "<br>"),
           URL = paste("https://www.facebook.com/mark.mclawhorn/posts/",
                  Post,
                  sep = ""))




#Create for Reacts stacks.
likes <- dfdf[c(1, 18, 6:12)] %>%
    gather(Reaction, Count, Like:Angry) %>%
    mutate(Reaction = ordered(Reaction, levels = c("Like", "Love", "Care", "Wow", "Haha", "Sad", "Angry")))



#Viridis, Transpose, and Axes Info.
gg_helper <- function(gg_object, discrete_bool, legend_bool, transpose_bool) {
    
    #Viridis Color Scale.
    gg_object <- gg_object + scale_fill_viridis(discrete = discrete_bool)    
    
    #Transpose.
    if (transpose_bool) {
        gg_object <- gg_object + coord_flip()
        #Modify labels and text.
        gg_object <- gg_object + theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                                       plot.caption = element_text(size = 12),
                                       axis.text.x = element_text(size = 14),
                                       axis.title.x = element_text(size = 18, face = "bold"),
                                       axis.text.y = element_text(hjust = 1, size = 14, angle = 20),
                                       axis.title.y = element_text(hjust = 0.5, size = 18, face = "bold"),
                                       legend.text = element_text(size = 14),
                                       legend.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                                       legend.position = ifelse(legend_bool, "right", "none"))
    } else {
        #Modify labels and text.
        gg_object <- gg_object + theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                                       plot.caption = element_text(size = 12),
                                       axis.text.x = element_text(hjust = 1, size = 12, angle = 45),
                                       axis.title.x = element_text(size = 18, face = "bold"),
                                       axis.text.y = element_text(size = 14),
                                       axis.title.y = element_text(hjust = 0.5, size = 18, face = "bold"),
                                       legend.text = element_text(size = 14),
                                       legend.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                                       legend.position = ifelse(legend_bool, "bottom", "none"))
    }
    return(gg_object)
}



#Interactive helper.  Set X, Y, size, and color from user input drop down selections.
interactive <- function(data, i_x, i_y, i_s, i_c) {
    cnames <- colnames(data)
    x <- cnames[as.numeric(i_x)]
    y <- cnames[as.numeric(i_y)]
    s <- cnames[as.numeric(i_s)]
    c <- cnames[as.numeric(i_c)]
    g <- ggplot(data = data,
                aes(x = !!ensym(x),
                    y = !!ensym(y),
                    size = !!ensym(s),
                    color = !!ensym(c)))
    return(g)
}
    
    

# Define UI for app.
ui <- fluidPage(

    # Application title
    titlePanel(h1(tagList(a("Meteorologist Markus McLawhorn",
                            href = "https://media0.giphy.com/media/3orieZMmRdBlKk5nY4/giphy.gif"),
                          "presents...DENSE FOG"))),

    # Sidebar with output options.
    sidebarLayout(
        sidebarPanel(width = 3,
                     h5("This is a shoutout to bruh..."),
                     h5("keeping us apprised of what's really going on."),
                     h3(strong(span("Choose Main Display"))),
                     selectInput("display", "", c("Distributions" = 1,
                                                  "Interactive" = 2,
                                                  "Links" = 3,
                                                  "Maff" = 4,
                                                  "Raw Data" = 5),
                                 selected = 3),
                     
                     #Date is flexible for all choices.
                     sliderInput("date_in",
                                 "Date range :",
                                 as.Date(min(dfdf$Date)),
                                 as.Date(Sys.Date()),
                                 value = c(as.Date(min(dfdf$Date)), as.Date(Sys.Date()))),
                     
                     #Distributions
                     conditionalPanel(condition = "input.display == 1",
                                      selectInput("dist_var", "Variable :", c("Comment", "Date", "Hour", "Month", "Privacy", "Reaction", "Text", "Time", "Year")),
                                      conditionalPanel(condition = "input.dist_var == 'Comment' || input.dist_var == 'Reaction'",
                                                       selectInput("dist_x", "Sort By", c("Date of Post", "Time of Day"))),
                                      conditionalPanel(condition = "input.dist_var == 'Date' || input.dist_var == 'Time'",
                                                       sliderInput("num_bins",
                                                                   "Number of Bins",
                                                                   4, 24, value = 8))
                     ),
                     
                     #Interactive
                     conditionalPanel(condition = "input.display == 2",
                                      h5("Create your own Graph!"),
                                      selectInput("x_axis", "X-axis :", c("Comments" = 13,
                                                                          "Date" = 1,
                                                                          "Month" = 17,
                                                                          "Time" = 2,
                                                                          "Year" = 16)),
                                      selectInput("y_axis", "Y-axis :", c("Hour" = 19,
                                                                          "Privacy" = 3,
                                                                          "Text" = 4,
                                                                          "Time" = 2)),
                                      selectInput("size", "Size :", c("Comments" = 13,
                                                                      "Hour" = 19,
                                                                      "Reactions" = 20,
                                                                      "Year" = 16)),
                                      selectInput("color", "Color :", c("Comments" = 13,
                                                                        "Hour" = 19,
                                                                        "Month" = 17,
                                                                        "Privacy" = 3,
                                                                        "Reactions" = 20,
                                                                        "Year" = 16)),
                     ),
        ),

        # Show a plots and text based off of user selection.
        mainPanel(
            
            #Distributions.
            conditionalPanel(condition = "input.display == 1",
                             plotOutput("dist_plot")),
            #Interactive.
            conditionalPanel(condition = "input.display == 2",
                             strong(span(textOutput("interactive_title"), align = "center", style = "font-size: 30px")),
                             plotOutput("inter_plot")),
            #Links.
            conditionalPanel(condition = "input.display == 3",
                             br(),
                             strong(span(textOutput("links_title"), align = "center", style = "font-size: 30px")),
                             plotlyOutput("links_plot")),
            
            #Maff.
            conditionalPanel(condition = "input.display == 4",
                             strong(span(textOutput("maff_title"), align = "center", style = "font-size: 30px")),
                             br(),
                             strong("Location", style = "font-size: 24px"),
                             br(),
                             span(tagList("We assume Raleigh and the Triangle, but he is ",
                                          a("worldwide",
                                            href = "https://upload.wikimedia.org/wikipedia/commons/6/6b/Rotating_globe.gif"),
                                          "."),
                                  style = "font-size: 16px"),
                             br(),
                             span(tagList("On May 4th, 2019 he posted from ",
                                          a("Asheville, NC",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10106177579962149"),
                                          "."),
                                  style = "font-size: 16px"),
                             br(),
                             span(tagList("On November 29th, 2015 he posted from ",
                                          a("Mt. Afton, VA",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10102987182504229"),
                                          "."),
                                  style = "font-size: 16px"),
                             br(),
                             br(),
                             strong("Privacy", style = "font-size: 24px"),
                             br(),
                             span("Over 65% of the posts are public.  He don't care about your political affiliation.", style = "font-size: 16px"),
                             br(),
                             br(),
                             strong("Social Engagement", style = "font-size: 24px"),
                             br(),
                             span(tagList("FB introduced reactions on ",
                                          a("February 24, 2016",
                                            href = "https://www.theverge.com/2016/2/24/11094374/facebook-reactions-like-button"),
                                          "."),
                                  style = "font-size: 16px"),
                             br(),
                             span(tagList("Post on ",
                                          a("February 20, 2018",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10105159621389289"),
                                          "had 17 reactions and 10 comments."),
                                  style = "font-size: 16px"),
                             br(),
                             span(tagList("Post on ",
                                          a("December 27, 2019",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10106816721841699"),
                                          "had 11 reactions and 13 comments."),
                                  style = "font-size: 16px"),
                             br(),
                             br(),
                             strong("When?", style = "font-size: 24px"),
                             br(),
                             span("Over 70% of the posts were made between 06:00 - 10:00.  Helpin' us get started with our day.", style = "font-size: 16px"),
                             br(),
                             span("Over 70% of the posts were made between November and February.  Our winters would be colder. Much colder.", style = "font-size: 16px"),
                             br(),
                             span("Over 50% of the posts were made in the past four years.  A safety beacon in these dark times, if you will.", style = "font-size: 16px"),
                             br(),
                             br(),
                             strong("Miscellaneous", style = "font-size: 24px"),
                             br(),
                             span(tagList("There was a ",
                                          a("morning post",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10102951989850589"),
                                          " and ",
                                          a("evening post",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10102953216297779"),
                                          " on November 5th, 2015."),
                                  style = "font-size: 16px"),
                             br(),
                             span(tagList("There was an ",
                                          a("early morning post",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10103980129400689"),
                                          " and ",
                                          a("late morning post",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10103980793270289"),
                                          " on January 21st, 2017."),
                                  style = "font-size: 16px"),
                             br(),
                             br(),
                             span(tagList("Post on ",
                                          a("December 16th, 2019",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10106785080975249"),
                                          "is a memory of post on",
                                          a("December 16th, 2008",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/40080784899"),
                                          ".  As far as the ancient texts tell us, this is the original DENSE FOG."),
                                  style = "font-size: 16px"),
                             br(),
                             span(tagList("Post on ",
                                          a("January 21st, 2020",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10106883961477929"),
                                          "is a memory of post on",
                                          a("January 21st, 2017",
                                            href = "https://www.facebook.com/mark.mclawhorn/posts/10103980129400689"),
                                          "."),
                                  style = "font-size: 16px"),
                             br(),
                             br(),
                             span(tagList("ShinyApp Source Code in R can be found ",
                                          a("here",
                                            href = "https://github.com/HumanRickshaw/Meteorologist_Markus_McLawhorn/blob/master/app.R"),
                                          "."),
                                  style = "font-size: 16px")),
            
            #Raw Data.
            conditionalPanel(condition = "input.display == 5",
                             strong(span(textOutput("rd_title"), align = "center", style = "font-size: 30px")),
                             br(),
                             DTOutput("rd_table"),
                             br(),
                             span(tagList("ShinyApp Source Code in R can be found ",
                                          a("here",
                                            href = "https://github.com/HumanRickshaw/Meteorologist_Markus_McLawhorn/blob/master/app.R"),
                                          "."),
                                  style = "font-size: 16px")),
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Filter Date.
    df_update <- reactive({
        dfdf %>%
            filter(Date >= input$date_in[1],
                   Date <= input$date_in[2])
    })
    like <- reactive({
        likes %>%
            filter(Date >= input$date_in[1],
                   Date <= input$date_in[2])
    })

    #Distributions.
    output$dist_plot <- renderPlot({
        
        #Comments.
        if (input$dist_var == "Comment") {
            #Stacked Bar Chart.
            if (input$dist_x == "Date of Post") {
                g1 <- ggplot(data = df_update(), aes(x = as.character(Date), y = Comments, fill = Comments))
            } else {
                g1 <- ggplot(data = df_update(), aes(x = as.character(oTime), y = Comments, fill = Comments))
            }
            g1 <- g1 + labs(caption = "Comments are numbers from original posts.")
            g1 <- g1 + geom_bar(stat = "identity")
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_discrete("When He Tell Us?", expand = c(0,0)) +
                scale_y_continuous("How Many Comments?", expand = c(0,0), breaks = c(0, 3, 6, 9, 12))
            gg_helper(g1, FALSE, FALSE, FALSE)
         
        }
        
        #Date.
        else if (input$dist_var == "Date") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = Date, fill = factor(Date)))
            g1 <- g1 + geom_histogram(bins = input$num_bins)
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_date("When He Tell Us?", expand = c(0,0)) +
                scale_y_continuous("How Many Posts?", expand = c(0,0), breaks = c(0, 3, 6, 9, 12, 15, 18))
 
            gg_helper(g1, TRUE, FALSE, TRUE)
        }
        
        #Hour.
        else if (input$dist_var == "Hour") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = Hour, fill = factor(Hour)))
            g1 <- g1 + geom_bar()
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_continuous("What Hour?", expand = c(0,0), breaks = c(6, 9, 12, 15, 18, 21), labels = c("6:00", "9:00", "12:00", "15:00", "18:00", "21:00")) +
                scale_y_continuous("How Many Posts?", breaks = c(0, 2, 4, 6, 8, 10), expand = c(0,0))
            
            gg_helper(g1, TRUE, FALSE, TRUE)
        }
        
        #Month.
        else if (input$dist_var == "Month") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = Month, fill = Month))
            g1 <- g1 + geom_bar()
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_discrete("What Month?", expand = c(0,0), labels = rev(month.name), drop = FALSE) +
                scale_y_continuous("How Many Posts?", breaks = c(0, 2, 4, 6, 8, 10), expand = c(0,0))
            
            gg_helper(g1, TRUE, FALSE, TRUE)
        }
        
        #Privacy.
        else if (input$dist_var == "Privacy") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = reorder(Privacy, desc(Privacy)), fill = reorder(Privacy, desc(Privacy))))
            g1 <- g1 + geom_bar()
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_discrete("Who Can See Dat?", expand = c(0,0)) +
                scale_y_continuous("How Many Posts?", expand = c(0,0))

            gg_helper(g1, TRUE, FALSE, TRUE)
        }
        
        #Reactions.
        else if (input$dist_var == "Reaction") {
            #Stacked Bar Chart.
            if (input$dist_x == "Date of Post") {
                g1 <- ggplot(data = like(), aes(x = as.character(Date), y = Count, fill = Reaction))
            } else {
                g1 <- ggplot(data = like(), aes(x = as.character(oTime), y = Count, fill = Reaction))
            }
            g1 <- g1 + labs(caption = "Likes and reactions are numbers from original posts.")            
            g1 <- g1 + geom_bar(position = "stack", stat = "identity")
            #Axis Labels.
            g1 <- g1 + scale_x_discrete("When He Tell Us???", expand = c(0,0)) +
                scale_y_continuous("Count of Reactions", expand = c(0,0))
            gg_helper(g1, TRUE, TRUE, FALSE)

        }
        #Text.
        else if (input$dist_var == "Text") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = fct_rev(Text), fill = Text))
            g1 <- g1 + geom_bar()
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_discrete("What the Forecast???", expand = c(0,0)) +
                scale_y_continuous("How many times he did it!", breaks = c(0, 3, 6, 9, 12, 15), expand = c(0,0))
            #Viridis Color Scale.
            g1 <- g1 + scale_fill_viridis(discrete = TRUE)    
            #Transpose.
            g1 <- g1 + coord_flip()
            #Modify labels and text.
            g1 <- g1 + theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                             axis.text.x = element_text(size = 14),
                             axis.title.x = element_text(size = 18, face = "bold"),
                             axis.text.y = element_text(hjust = 1, size = 9, angle = 20),
                             axis.title.y = element_text(hjust = 0.5, size = 18, face = "bold"),
                             legend.text = element_text(size = 14),
                             legend.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                             legend.position = "none")
            g1
        }
        
        #Time.
        else if (input$dist_var == "Time") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = Time, fill = factor(Time)))
            g1 <- g1 + geom_histogram(bins = input$num_bins)
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_datetime("What Time of Day?", expand = c(0,0),
                                 breaks = date_breaks("3 hour"),
                                 labels = date_format("%H:%M")) +
                scale_y_continuous("How Many Posts?", expand = c(0,0),
                                   breaks = c(0, 4, 8, 12, 16, 20, 24))
            
            gg_helper(g1, TRUE, FALSE, TRUE)
        }

        #Year.
        else if (input$dist_var == "Year") {
            #Histogram
            g1 <- ggplot(data = df_update(), aes(x = Year, fill = factor(Year)))
            g1 <- g1 + geom_histogram(binwidth = 1)
            #Axis Labels.
            g1 <- g1 + ggtitle("Distribution of Updates") +
                scale_x_continuous("What Year?", expand = c(0,0),
                                   breaks = c(2008, 2011, 2014, 2017, 2020),
                                   labels = c(2008, 2011, 2014, 2017, 2020)) +
                scale_y_continuous("How Many Posts?", expand = c(0,0))
            
            gg_helper(g1, TRUE, FALSE, TRUE)
        }
        
    },
    #Bar Chart/Histogram size.
    width = 850,
    height = 550,
    res = 100
    )
    
    
    
    #Interactive.
    output$interactive_title <- renderText("Page Under Construction...")
    
    output$inter_plot <- renderPlot({

        g2 <- interactive(df_update(), input$x_axis, input$y_axis, input$size, input$color)
        g2 <- g2 + geom_point()
        
        #Viridis Color Scale.
        g2 <- g2 + scale_color_viridis()  
        
        
        #Modify labels and text.
        #g2 <- g2 + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        #                 axis.text.x = element_text(hjust = 1, size = 12, angle = 55),
        #                 axis.title.x = element_text(size = 14, face = "bold"),
        #                 axis.text.y = element_text(size = 12),
        #                 axis.title.y = element_text(hjust = 0.5, size = 14, face = "bold"))
        
        g2
    },
    #Bar Chart/Histogram size.
    width = 850,
    height = 550,
    res = 100
    )
    
    
    
    #Links.
    output$links_title <- renderText("Click Point for Archived Report")
    
    #JavaScript to allow points to open URLs.
    js <- "
        function(el, x) {
            el.on('plotly_click', function(d) {
                var point = d.points[0];
                var url = point.data.customdata[point.pointIndex];
                window.open(url);
                });
                }"
    
    #Title fonts.
    f1 <- list(size = 24, color = "black")
    #Label font.
    f2 <- list(size = 18, color = "grey")
    #Times
    tickvals = c("1899-12-31 06:00:00", "1899-12-31 09:00:00",
                 "1899-12-31 12:00:00", "1899-12-31 15:00:00",
                 "1899-12-31 18:00:00", "1899-12-31 21:00:00")
    ticktext = c("06:00", "09:00", "12:00",
                 "15:00", "18:00", "21:00")
    #Axes and Legend setup.
    xaxis <- list(title = "<b>What Time of Day?</b>",
                  titlefont = f1,
                  tickfont = f2,
                  tickvals = tickvals,
                  ticktext = ticktext,
                  y = 0.25)
    yaxis <- list(title = "<b>What Year?</b>",
                  titlefont = f1,
                  tickfont = f2)
    legend <- list(title = list(text = "<b>Who Can See It?</b>",
                                font = f1),
                   font = f2,
                   orientation = 'h',
                   xanchor = 'center',
                   x = 0.45,
                   y = -0.3)
    
    output$links_plot <- renderPlotly({
        p <- plot_ly(data = df_update(),
                     x = ~Time,
                     y = ~Date,
                     color = ~Privacy,
                     colors = viridis_pal(direction = -1, option = "D")(3),
                     alpha = 0.75,
                     marker = list(size = 20),
                     text = ~Hovertext,
                     hoverinfo = 'text',
                     customdata = ~URL,
                     type = "scatter",
                     mode = "markers",
                     width = 850,
                     height = 550) %>%
            layout(xaxis = xaxis,
                   yaxis = yaxis,
                   legend = legend) %>%
            onRender(js)
    })

    
    
    #Maff.
    output$maff_title <- renderText("Some numbers...")
        
    
        
    #Raw Data.
    output$rd_title <- renderText("Complete Data")
    output$rd_table <- renderDT(datatable(df_update()[c(1:13)] %>%
                                              mutate(Date = as.character(Date),
                                                     Time = substr(as.character(Time), 12, 19)),
                                          rownames = FALSE,
                                          options = list(autoWidth = TRUE)))
}



# Run the app. 
shinyApp(ui = ui, server = server)
