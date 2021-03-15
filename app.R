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



hour_list <- c("00:00", "03:00", "06:00", "09:00",
               "12:00", "15:00", "18:00", "21:00")



#Read xlsx.
#setwd("C:/Users/Rohan/Documents/Data Science/Completed/DENSE FOG")
dfdf <- read_excel("./DENSE FOG.xlsx")



#Create full FB post from unique post #.
url_helper <- function(post_num) {
    paste("https://www.facebook.com/mark.mclawhorn/posts/",
          post_num,
          sep = "")
}



#Create Year, Create Date, format Hovertext and URL for links.
dfdf <- dfdf %>%
    mutate(Year = as.numeric(substr(Date, 1, 4)),
           Month = factor(as.numeric(substr(Date, 6, 7)), levels = 12:1, labels = rev(month.name)),
           Date = as.Date(Date),
           oTime = strftime(Time, format = "%H:%M:%S", tz = "UTC" ),
           Hour = as.integer(substr(oTime, 1, 2)),
           Reactions = rowSums(.[6:12]),
           Hovertext = paste(Text,
                             format(Date, "%A %B %d, %Y"),
                             format(Time, "%H:%M"),
                             sep = "<br>"),
           URL = url_helper(Post),
           Text2 = ifelse(nchar(Text) > 50,
                          paste(substring(Text, 1, 50), "...", sep = ""),
                          Text))



#Create for Reacts stacks.
likes <- dfdf[c(1, 18, 6:12)] %>%
    gather(Reaction, Count, Like:Angry) %>%
    mutate(Reaction = ordered(Reaction, levels = c("Like", "Love", "Care", "Wow", "Haha", "Sad", "Angry")))



#Viridis, Transpose, and Axes Info.
gg_helper <- function(gg_object, discrete_bool, legend_bool, transpose_bool) {
    
    #Viridis Color Scale.
    gg_object <- gg_object + scale_fill_viridis(discrete = discrete_bool, direction = -1)    
    
    #Transpose.
    if (transpose_bool) {
        gg_object <- gg_object + coord_flip()
        #Modify labels and text.
        gg_object <- gg_object + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                                       plot.caption = element_text(size = 8),
                                       axis.text.x = element_text(size = 12),
                                       axis.title.x = element_text(size = 16, face = "bold"),
                                       axis.text.y = element_text(hjust = 1, size = 14, angle = 20),
                                       axis.title.y = element_text(hjust = 0.5, size = 16, face = "bold"),
                                       legend.text = element_text(size = 12),
                                       legend.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                       legend.position = ifelse(legend_bool, "right", "none"))
    } else {
        #Modify labels and text.
        gg_object <- gg_object + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                                       plot.caption = element_text(size = 8),
                                       axis.text.x = element_text(hjust = 1, size = 6, angle = 45),
                                       axis.title.x = element_text(size = 16, face = "bold"),
                                       axis.text.y = element_text(size = 12),
                                       axis.title.y = element_text(hjust = 0.5, size = 16, face = "bold"),
                                       legend.text = element_text(size = 12),
                                       legend.title = element_text(hjust = 0.5, size = 16, face = "bold"),
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



#Dataframe for Maff outputs.
#Takes in date when boolean is true,
#takes in post when boolean is false.
helper_df <- function(post_filter, boolean) {

    if (boolean) {
        temp <- dfdf %>%
            filter(Date == as.Date(post_filter,
                                   format = "%B %d, %Y")) 
    } else {
        temp <- dfdf %>%
            filter(Post == post_filter)
    }
    temp %>%
        select(Date, oTime, URL, Reactions, Comments, Reference, Post, Secretary, Text)
}



#Ready title for output.
title_out <- function(string) {
    strong(span(textOutput(string), style = "text-align: center; font-size: 36px;"))
}



#Ready heading for output.
heading_out <- function(string) {
    strong(string, style = "font-size: 24px")
}



#Ready text for output.
text_out <- function(string) {
    span(string, style = "font-size: 16px")
}



#General text with hyperlink.
text_link <- function(string, anchor, hyperlink, optional) {
    if (missing(optional)) {
        text_out(tagList(string, a(anchor, href = hyperlink)))
    } else {
        text_out(tagList(string, a(anchor, href = hyperlink), optional))
    } 
}



#Maff Helper for stats.
maff_helper <- function(df){
    
    df_posts <- dim(df)[1]
    privacy <- percent_conv(dim(df[df$Privacy == "Public",])[1] / df_posts)
    hour <- percent_conv(dim(df[df$Hour < 10,])[1] / df_posts)
    month <- percent_conv(dim(df[df$Month %in% c("January", "February", "October", "November", "December"),])[1] / df_posts)
    year <- percent_conv(dim(df[df$Year > 2016,])[1] / df_posts)
    output <- list(privacy, hour, month, year)
    
    output
}



#Converts decimal to string percent.
percent_conv <- function(number){
    return(paste(round(100*number), "%", sep = ""))
}



#From Date, grab the URL, Reactions, and Comments.
#Print a statement.
social_helper <- function(post_date) {
    
    temp <- helper_df(post_date, T)
    url <- temp[1, 3]
    reactions <- temp[1, 4]
    comments <- temp[1, 5]

    text_link("Post on ",
              post_date,
              url,
              paste("had", reactions, "reactions and", comments, "comments."))
}



#Create double statement given a date that has two DF posts.
double_helper <- function(post_date) {
    
    temp <- helper_df(post_date, T)
    url1 <- temp[1, 3]
    text1 <- temp[1, 9]    
    url2 <- temp[2, 3]
    text2 <- temp[2, 9] 
  
    tagList(text_link(paste("On", post_date, "there was "),
                      text1,
                      url1),
            text_link(" and ",
                      text2,
                      url2))
}



#Create a memory statement given a date that has a post with memory.
memory_helper <- function(post_date) {
    
    temp <- helper_df(post_date, T)
    repost_url <- temp[1, 3]
    memory <- helper_df(temp[[6]], F)

    if (post_date == "November 6, 2020"){
        memory_url = paul_url
    } else {
        memory_url = memory[1, 3]
    }
    
    tagList(text_link("Post on ",
                      post_date,
                      repost_url),
            text_link(" is a memory of post on ",
                      format(memory[[1]], "%B %d, %Y"),
                      memory_url))
}



#Create a memory statement given a date that secretary assisted.
#Deprecated.  See Links, Secretaries.
#secretary_helper <- function(post_date) {
#    
#    temp <- helper_df(post_date, T)
#    
#    if (post_date == "November 6, 2017"){
#        temp_url = paul_url
#    } else {
#        temp_url = temp[[3]]
#    }
#
#    text_link(paste(temp[[8]], "handled business on"),
#              post_date,
#              temp_url)
#}



#Paul McCauley posted on his wall and tagged Mark, instead of posting on Mark's wall.
#The URL format is thus inconsistent.
paul_url <- "https://www.facebook.com/paul.mccauley.56/posts/10104874956579849"



#Github URL.
source_code <- function() {
    text_link("ShinyApp Source Code in R can be found ",
              "here",
              "https://github.com/HumanRickshaw/Meteorologist_Markus_McLawhorn/blob/master/app.R")
}
    
    

# Define UI for app.
ui <- fluidPage(

    # Application title
    titlePanel(h1(tagList(a("Meteorologist Markus McLawhorn",
                            href = "https://media0.giphy.com/media/3orieZMmRdBlKk5nY4/giphy.gif"),
                          "presents...DENSE FOG"))),

    # Sidebar with output options.
    sidebarLayout(
        sidebarPanel(
            width = 3,
            h5("This is a shoutout to bruh..."),
            h5("keeping us apprised of what's really going on."),
            h3(strong(span("Choose Main Display"))),
            selectInput("display", "", c("Distributions" = 1,
                                         "Interactive" = 2,
                                         "Links, All" = 3,
                                         "Links, Secretaries" = 4,
                                         "Maff" = 5,
                                         "Raw Data" = 6),
                        selected = 4),
            
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
                                                                 "Reactions" = 20,
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
        
        
        
        #Show a plots and text based off of user selection.
        mainPanel(
            
            #Distributions.
            conditionalPanel(condition = "input.display == 1",
                             plotOutput("dist_plot")),
            #Interactive.
            conditionalPanel(condition = "input.display == 2",
                             title_out("interactive_title"),
                             plotOutput("inter_plot")),
            
            #Links, All.
            conditionalPanel(condition = "input.display == 3",
                             br(),
                             title_out("links_title"),
                             plotlyOutput("links_plot")),
            
            #Links, Secretaries.
            conditionalPanel(condition = "input.display == 4",
                             title_out("secretary_title"),
                             plotlyOutput("secretary_plot")),
            
            #Maff.
            conditionalPanel(condition = "input.display == 5",
                             title_out("maff_title"),
                             br(),
                             heading_out("Location"),
                             br(),
                             text_link("We assume Raleigh, but he is ",
                                       "worldwide",
                                       "https://upload.wikimedia.org/wikipedia/commons/6/6b/Rotating_globe.gif"),
                             br(),
                             text_link("On May 4th, 2019 he posted from ",
                                       "Asheville, NC",
                                       url_helper("10106177579962149")),
                             br(),
                             text_link("On January 21st, 2017 he ventured out to bougie-ass ",
                                       "Cary, NC",
                                       url_helper("10103980793270289")),
                             br(),
                             text_link("On November 29th, 2015, visiting his roots, he informed us from ",
                                       "Mt. Afton, VA",
                                       url_helper("10102987182504229")),
                             br(),
                             br(),
                             heading_out("Privacy"),
                             br(),
                             text_out(paste("About",
                                            maff_helper(dfdf)[1],
                                            "of the posts are public.  He don't care about your political affiliation.")),
                             br(),
                             br(),
                             heading_out("Social Engagement"),
                             br(),
                             text_link("FB introduced reactions on ",
                                       "February 24, 2016",
                                       "https://www.theverge.com/2016/2/24/11094374/facebook-reactions-like-button"),
                             br(),
                             social_helper("March 31, 2020"),
                             br(),
                             social_helper("April 25, 2017"),
                             br(),
                             social_helper("December 27, 2019"),
                             br(),
                             social_helper("November 5, 2020"),
                             br(),
                             br(),
                             heading_out("When?"),
                             br(),
                             text_out(paste("Since",
                                            paste(format(dfdf$Date[1], "%B %d, %Y"), ",", sep = ""),
                                            "Young Markus has made",
                                            nrow(dfdf),
                                            "Facebook Weather Reports.")),
                             br(),
                             text_out(paste("About",
                                            maff_helper(dfdf)[2],
                                            "of the posts were made between 06:00 - 10:00.  Helpin' us get started with our day.")),
                             br(),
                             text_out(paste("About",
                                            maff_helper(dfdf)[3],
                                            "of the posts were made between October and February.  Our autumns & winters would be colder. Much colder.")),
                             br(),
                             text_out(paste("About",
                                            maff_helper(dfdf)[4],
                                            "of the posts were made in the past four years.  A safety beacon in these dark times, if you will.")),
                             br(),
                             br(),
                             heading_out("Serious Weather Days"),
                             br(),
                             double_helper("April 16, 2011"),
                             br(),
                             double_helper("November 5, 2015"),
                             br(),
                             double_helper("January 21, 2017"),
                             br(),
                             double_helper("May 31, 2019"),
                             br(),
                             double_helper("February 15, 2021"),
                             br(),
                             br(),
                             heading_out("Memories"),
                             br(),
                             memory_helper("December 16, 2019"),
                             br(),
                             text_out("As far as the ancient texts tell us, this is the original 'Dense Fog'."),
                             br(),
                             memory_helper("January 17, 2020"),
                             br(),
                             memory_helper("January 21, 2020"),
                             br(),
                             memory_helper("November 5, 2020"),
                             br(),
                             memory_helper("November 6, 2020"),
                             br(),
                             memory_helper("December 5, 2020"),
                             br(),
                             memory_helper("December 6, 2020"),
                             br(),
                             memory_helper("December 14, 2020"),
                             br(),
                             memory_helper("January 3, 2021"),
                             br(),
                             memory_helper("January 17, 2021"),
                             br(),
                             memory_helper("February 16, 2021"),
                             br(),
                             br(),
                             br(),
                             source_code()),
            
            #Raw Data.
            conditionalPanel(condition = "input.display == 6",
                             title_out("rd_title"),
                             br(),
                             DTOutput("rd_table"),
                             br(),
                             source_code()),
        )
    )
)
source



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
  
  df_sec <- reactive({
    dfdf %>%
      filter(!is.na(Secretary)) %>%
      select(Date, Text, URL, Hovertext, Secretary)
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
        scale_y_continuous("How Many Comments?", expand = c(0,0))
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
        scale_y_continuous("How Many Posts?", expand = c(0,0))
      
      gg_helper(g1, TRUE, FALSE, TRUE)
    }
    
    #Hour.
    else if (input$dist_var == "Hour") {
      #Histogram
      g1 <- ggplot(data = df_update(), aes(x = Hour, fill = factor(Hour)))
      g1 <- g1 + geom_bar()
      #Axis Labels.
      g1 <- g1 + ggtitle("Distribution of Updates") +
        scale_x_continuous("What Hour?", breaks = c(0, 3, 6, 9, 12, 15, 18, 21),
                           labels = hour_list, expand = c(0,0)) +
        scale_y_continuous("How Many Posts?", expand = c(0,0))
      
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
        scale_y_continuous("How Many Posts?", expand = c(0,0))
      
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
      g1 <- ggplot(data = df_update(), aes(x = Text2, fill = Text2))
      g1 <- g1 + geom_bar()
      #Axis Labels.
      g1 <- g1 + ggtitle("Distribution of Updates") +
        scale_x_discrete("What the Forecast???", expand = c(0,0)) +
        scale_y_continuous("How many times he did it!", expand = c(0,0))
      #Viridis Color Scale.
      g1 <- g1 + scale_fill_viridis(discrete = TRUE)    
      #Modify labels and text.
      g1 <- g1 + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                       axis.text.x = element_text(hjust = 1, size = 5, angle = 60),
                       axis.title.x = element_text(size = 16, face = "bold"),
                       axis.text.y = element_text(hjust = 1, size = 12),
                       axis.title.y = element_text(hjust = 0.5, size = 16, face = "bold"),
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
        scale_y_continuous("How Many Posts?", expand = c(0,0))
      
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
  width = 1200,
  height = 750,
  res = 150
  )
  
  
  
  #Interactive.
  output$interactive_title <- renderText("Page Under Construction...")
  
  output$inter_plot <- renderPlot({
    
    g2 <- interactive(df_update(), input$x_axis, input$y_axis, input$size, input$color)
    g2 <- g2 + geom_point()
    
    #Viridis Color Scale.
    g2 <- g2 + scale_color_viridis()  
    
    g2
  },
  
  #Bar Chart/Histogram size.
  width = 850,
  height = 550,
  res = 100
  )
  
  
  
  #Links.
  output$links_title <- renderText("Hover & Click Points to Open Archived Reports")
  
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
  tickvals = c("1899-12-31 00:00:00", "1899-12-31 03:00:00",
               "1899-12-31 06:00:00", "1899-12-31 09:00:00",
               "1899-12-31 12:00:00", "1899-12-31 15:00:00",
               "1899-12-31 18:00:00", "1899-12-31 21:00:00")
  ticktext = hour_list
  #Axes and Legend setup.
  xaxis1 <- list(title = "<b>What Time of Day?</b>",
                 titlefont = f1,
                 tickfont = f2,
                 tickvals = tickvals,
                 ticktext = ticktext,
                 y = 0.25)
  yaxis1 <- list(title = "<b>When?</b>",
                 titlefont = f1,
                 tickfont = f2)
  legend1 <- list(title = list(text = "<b>Who Can See It?</b>",
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
      layout(xaxis = xaxis1,
             yaxis = yaxis1,
             legend = legend1) %>%
      onRender(js)
  })
  
  
  
  #Links.
  output$secretary_title <- renderText("Hover & Click Points to Open Secretary Reports")
  
  xaxis2 <- list(title = "<b>When?</b>",
                 titlefont = f1,
                 tickfont = f2,
                 y = 0.25)
  yaxis2 <- list(title = "<b>Employees</b>",
                 titlefont = f1,
                 tickfont = f2,
                 tickangle = -35,
                 autorange = "reversed")
  output$secretary_plot <- renderPlotly({
    p <- plot_ly(data = df_sec(),
                 x = ~Date,
                 y = ~Secretary,
                 color = ~Secretary,
                 colors = viridis_pal(direction = -1, option = "D")(7),
                 alpha = 0.75,
                 marker = list(size = 20),
                 text = ~Hovertext,
                 hoverinfo = 'text',
                 customdata = ~URL,
                 type = "scatter",
                 mode = "markers",
                 width = 850,
                 height = 550) %>%
      layout(xaxis = xaxis2,
             yaxis = yaxis2,
             showlegend = FALSE) %>%
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