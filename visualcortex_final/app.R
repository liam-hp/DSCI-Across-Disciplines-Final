#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(ggseg)
library(shinydashboard)
options(scipen = 100000)

#setwd("~/JTerm 2022 - DSCI/visualcortex_final")

evoked.df <- read_csv("spikeTimes_monkey1_evoked.csv")
evoked.df[evoked.df == 'NaN']= NA
evoked.df[evoked.df == 0]= NA

evoked_asOne <- data.frame(spikes = NULL, trial = NULL)

# grouping into 1 data set
for(i in seq(1,25,by=1)){
  spikes <- na.omit(data.frame(spikes = (evoked.df[,i]), trial = factor(i)))
  names(spikes)[1] <- 'spikes'
  evoked_asOne = rbind(evoked_asOne, spikes)
}

ui <- dashboardPage(skin = "yellow", title = "yabadabadoo",
  dashboardHeader(title = "Visual Cortex"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      menuItem("Neuro Background", tabName = "neuroscience_vocab", icon = icon("book")),
      
      menuItem("Visual Cortex", tabName = "visual_cortex", icon = icon("brain")),
      
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
      
      menuItem("Citations", tabName = "cites", icon = icon("newspaper"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", 
              fluidRow(column(12, align="center", h1("An Exploration of Neuron Spikes in the Visual Cortex", align = "center")),
              ),br(), 
              fluidRow(column(8, align="center", br(), br(), offset = 2, img(src="brain.png", width = "80%", height = "80%"))),br(),
              fluidRow(column(8, align = "center", offset = 2, h3("Overview", align = "center"))),
              br(),
              h4(textOutput("exp")),
              br(),
              fluidRow(column(12, align = "center", img(src="occipital_lobe-C.png", width = "29%", height = "29%"),
                              img(src="electrode_array-C.png", width = "20%", height = "20%"),
                              img(src="grating-C.png", width = "42%", height = "42%"))),
              br(),br(),br()
              
      ), 
      tabItem(tabName = "neuroscience_vocab", 
              fluidRow(column(12, align="center", h1("Neuroscience Background Info", align = "center"))), br(),
              h4(textOutput("neurV")), br(),
              fluidRow(column(12, align="center", img(src="Neuron diagram.jpeg", width = "60%", height = "60%"),
                              img(src="actionPotential.png", width = "25%", height = "25%"), br(), br()))
              
      ),
      tabItem(tabName = "visual_cortex",
              fluidRow(column(12, align="center", h1("The Visual Cortex", align = "center"))), br(),
              h4(textOutput("genView")), br(), 
              fluidRow(column(12, br(), align = "center", img(src="eye.jpg", width = "50%", height = "50%"), br(), br(), br())),
              h4(textOutput("path")), br(), 
              fluidRow(column(12, br(), align = "center", img(src="eye2.jpeg", width = "50%", height = "50%"), br(), br(), br()))
              #h4(textOutput("vCort")), br(), br(), br()
              ),
      tabItem(tabName = "graphs",
              fluidRow(column(12, align="center", h1("Graphical Analysis", align = "center"))), br(),
              uiOutput("graph_settings"),
              uiOutput("graphs"),
              fluidRow(column(12, br(), br(), align="center", h4(textOutput("graph_desc")), br())),
      ),
      tabItem(tabName = "cites",
              fluidRow(column(12, align="center", h1("Bibliography"))),
              fluidRow(column(12, align="center", br(), h3("Research Paper:"))),
              fluidRow(column(12, align="center", "Spatial and Temporal Scales of Neuronal Correlation in Primary Visual Cortex,")),
              fluidRow(column(12, align="center", "Matthew A. Smith, Adam Kohn, ")),
              fluidRow(column(12, align="center", "Journal of Neuroscience 26 November 2008, 28 (48) 12591-12603; DOI: 10.1523/JNEUROSCI.2929-08.2008")),
              fluidRow(column(12, align="center", br(), br(), h3("Image Sources:"))),
              fluidRow(column(12, align = "center", "What Lobe of the Brain Is Responsible for Vision? Socratic.")),
              fluidRow(column(12, align="center", "Introduction to Psychology. Lumen Learning")),
              fluidRow(column(12, align="center", "The Summation of Vertical Sine Wave Gratings. Research Gate")),
              fluidRow(column(12, align="center", "Kurzweil, Ray. Kurzweiltracking the Acceleration of Intelligence. Kurzweil IBM Scientists Emulate Neurons with Phasechange Technology Comments")),
              fluidRow(column(12, align="center", "Figure 1. Neural Network Biology - Researchgate.net")),
              fluidRow(column(12, align="center", "Human Eye. Body Adaptation")),
              fluidRow(column(12, align="center", "Anne Marie Helmenstine, Ph.D. Here's How the Human Eye Works. ThoughtCo")),
              fluidRow(column(12, align="center", br(), br(), h3("Other"))),
              fluidRow(column(12, align="center", "The Senses - Carter, Rita. The Human Brain Book. Dorling Kindersley Ltd"), br(), br())
      )
    )
  )
)

server <- function(input, output, session) {
  output$exp <- renderText({
    "This app allows you to explore spike train data collected from a monkey's visual cortex. Our data is from 'Spatial and Temporal Scales of Neuronal Correlation in Primary Visual Cortex,' a research paper written by
    Dr. Matthew A. Smith and Dr. Adam Kohn. In this paper, they were investigating the causes of spike activity correlation (sychrony) in neurons. 
    They specifically focused on the function and extent of correlated spontaneous and evoked activity. The authors paralyzed monkeys and implanted electrode arrays in the V1 area of the visual cortex. They collected spontaneous data 
    by measuring the spike activity in the monkey with no outside stimulus and also acquired evoked spike activity using visual stimuli
    in the form of sinusoidal gratings.
 "
  })
  
  output$neurV <- renderText({
    "A neuron is an excitable cell that sends and receives elecrochemical signals called action potentials.
    When neurons receive signals from other cells, ion channels are opened which change the membrane potential of the cell. 
    This leads to an increase in the voltage of the cell and when the cell reaches its spiking threshold, it releases an action potential from the axon terminals. 
    After the cell fires, the membrane potential decreases and there is a refractory period during which time no spikes can be generated.
  "
  })
  
  output$genView <- renderText({
    "The visual cortex is the area of the brain, located in the occipital lobe, that processes and receives visual information: light which is converted to electrical signals.
     "
  })
  output$path <- renderText({
    "Light initially enters the eye by passing through the cornea and continuing through the pupil. The iris, which is made up of pigmented fibers on the pupil, controls how much light is allowed in. 
    Once the light passes through the lens, which refracts the light so that it reaches the retina. 
    The third layer of the retina has photoreceptors (light sensitive cells) which transduce light energy into electrochemical signals. 
    When that happens, the cells fire pulses to the optical nerve. 
    There is an optic nerve for both eyes which join at the optic chiasm. 
    The left side of both nerves join and go to the left optic tract and the right side of both nerves join and go to the right optic tract. 
    Both tracts end at thalamus, but the signal continues on to the visual cortex through the optic radiation."
  })
  output$vCort <- renderText({
    "The visual cortex is broken up into eleven functional areas that process an aspect of vision. 
    Three basic aspects that it process are: colors, recognition, depth, and dimension. 
    Humans can distinguish millions of colors, but we only learned to distinguish them based on our language. 
    Images are compared to our memory to see if there is any significance or emotional reaction related to it. 
    Object recognition actually happens in the temporal lobe. 
    There is a specific brain region that specializes in identifying faces and thus differentiating between people. 
    Depth and dimension are processed by the brain by interpretting any differences in the view of each movement and shape to produce a 3D image."
  })
  
  output$graphs <- renderUI({
    mainPanel(
      tabsetPanel(id = "curr_tab",
                  tabPanel("Spike Times", plotOutput(outputId = "spiketimes")),
                  tabPanel("Firing Rate", plotOutput(outputId = "firingrate")),
                  tabPanel("Interspike Interval", plotOutput(outputId = "interspikeinterval"))
      ),
    )
  })
  output$graph_settings <- renderUI({
    sidebarPanel(
      conditionalPanel(
        condition = ("input.curr_tab == 'Spike Times' || (input.curr_tab == 'Firing Rate' && input.FRdisplay == 'Vertically split histogram')
                     || (input.curr_tab == 'Interspike Interval' && (input.ISIdisplay == 'Vertically split histogram'))"),
        selectInput(inputId = "color", label = "Color palette: ",
                    choices = c("YlOrRd", "YlGnBu", "RdYlBu","PiYG", "BuPu", "Greys")),
      ),
      conditionalPanel(
        condition = ("input.curr_tab == 'Firing Rate' && ((input.FRdisplay == 'Single histogram') || (input.FRdisplay == 'Single line-graph'))
                  || (input.curr_tab == 'Interspike Interval' && (input.ISIdisplay == 'Single histogram'))"),
        
        selectInput(inputId = "fillc", label = "Fill Color: ",
                    choices = c("Royal blue", "Light blue", "Fire brick", "Golden rod", "Spring green", "Custom (hex)"),
                    "Light blue"),
      ),
      conditionalPanel(
        condition = ("input.fillc == 'Custom (hex)'"),
        textInput(inputId = "customfill", label = "Custom Fill: ", value = "#FFFFFF"),
      ),
      conditionalPanel(
        condition = ("input.curr_tab != 'Info'"),
        selectInput(inputId = "background", label = "Background color: ",
                    choices = c("White", "Light Grey", "Dark Grey", "Black", "Custom (hex)")),
      ),
      conditionalPanel(
        condition = ("input.background == 'Custom (hex)'"),
        textInput(inputId = "customcolor", label = "Custom Background: ", value = "#FFFFFF"),
      ),
      conditionalPanel(
        condition = ("input.curr_tab != 'Info'"),
        textInput(inputId = "data", label = "Which datasets do you want to use (csv input | 1-25): ", value = "1")
      ),
      conditionalPanel(
        condition = ("input.curr_tab == 'Spike Times'"),
        selectInput(inputId = "spikedisplay", label = "How would you like to display the spikes? ",
                    choices = c("Vertical split", "Inline display")),
      ),
      conditionalPanel(
        condition = ("input.curr_tab == 'Firing Rate'"),
        
        selectInput(inputId = "FRdisplay", label = "How would you like to display the graphs? ",
                    choices = c("Single histogram", "Vertically split histogram", "Single line-graph")), # "Vertically split line-graph"
        
        conditionalPanel(
          condition = ("input.syn == 0"),
          sliderInput(inputId = "bins",
                      label = "What size bin would you like to use (s)?",
                      min = .01, max = 1.27,
                      value = .1)
        ),
        checkboxInput(inputId = "syn", "synchronize", FALSE)
      ),
      conditionalPanel(
        condition = ("input.curr_tab == 'Interspike Interval'"),
        selectInput(inputId = "ISIdisplay", label = "How would you like to display the graphs? ",
                    choices = c("Single histogram", "Vertically split histogram")),
        sliderInput(inputId = "bins2",
                    label = "What size bin would you like to use (ms)?",
                    min = .01, max = 1.27,
                    value = .1)
      ),
    )
  })
  output$spiketimes <- renderPlot({
    
    bcol = "#FFFFFF"
    if(input$background == "Light Grey"){bcol = "#e2e2e2"}
    else if(input$background == "Dark Grey"){bcol = "#4d4d4d"}
    else if(input$background == "Black"){bcol = "#000000"}
    else if(input$background == "Custom (hex)"){bcol = input$customcolor}
    
    d <- input$data
    data_sel = c()
    
    for(i in 1:nchar(d)){
      if(str_sub(d,i,i)!=',' && str_sub(d,i,i)!=' '){
        if(str_sub(d,i+1,i+1)==',' | str_sub(d,i+1,i+1)==" " | i == nchar(d)){
          if(str_sub(d,i-1,i-1)==',' | str_sub(d,i-1,i-1)==" ")
            data_sel <- c(data_sel,strtoi(str_sub(d,i,i)))
          else{
            data_sel <- c(data_sel,strtoi(str_sub(d,i-1,i)))
          }
        }
      }
    }
    # can condense this code (use p+)
    if(input$spikedisplay == "Vertical split"){
      st <- evoked_asOne %>%
        filter(trial %in% data_sel) %>%
        ggplot()+
        geom_tile(aes(x=spikes, y = trial, fill = trial))+ # facet grid
        labs(y="Trial #", x="Time (s)")+
        ggtitle("Spike Times")+
        theme_classic()+
        scale_fill_brewer(palette = input$color)+
        theme(panel.background = element_rect(fill = (bcol)))
    }
    else if(input$spikedisplay == "Inline display"){
      st <- evoked_asOne %>%
        filter(trial %in% data_sel) %>%
        ggplot()+
        geom_tile(aes(x=spikes, y = "", fill = trial))+
        labs(y="Trials", x="Time (s)")+
        ggtitle("Spike Times")+
        theme_classic()+
        scale_fill_brewer(palette = input$color)+
        theme(panel.background = element_rect(fill = (bcol)))
    }
    st
  })
  output$firingrate <- renderPlot({
    
    bcol = "#FFFFFF"
    if(input$background == "Light Grey"){bcol = "#e2e2e2"}
    else if(input$background == "Dark Grey"){bcol = "#4d4d4d"}
    else if(input$background == "Black"){bcol = "#000000"}
    else if(input$background == "Custom (hex)"){bcol = input$customcolor}
    
    T <- 1.3 # time window
    d <- input$data
    data_sel = c()
    
    for(i in 1:nchar(d)){
      if(str_sub(d,i,i)!=',' && str_sub(d,i,i)!=' '){
        if(str_sub(d,i+1,i+1)==',' | str_sub(d,i+1,i+1)==" " | i == nchar(d)){
          if(str_sub(d,i-1,i-1)==',' | str_sub(d,i-1,i-1)==" ")
            data_sel <- c(data_sel,strtoi(str_sub(d,i,i)))
          else{
            data_sel <- c(data_sel,strtoi(str_sub(d,i-1,i)))
          }
        }
      }
    }
    
    N = length(data_sel)
    
    yaxis_label = "Average Firing Rate"
    counter = 1
    if(input$syn == 0)
      dt <- input$bins # time interval
    else{
      diff.data <- evoked_asOne %>%
        group_by(trial) %>%
        filter(trial %in% data_sel) %>%
        summarize(diff = diff(spikes)) %>%
        filter(diff > 0)
      dt <- min(diff.data$diff)
      counter = dt
      yaxis_label = "Probability of firing"
    }
    
    cut.df <- evoked_asOne %>%
      filter(trial %in% data_sel) %>%
      mutate(bins = cut(spikes, breaks = seq(0,T,by = dt), dig.lab = 5)) %>%
      summarise(count = as.vector(table(bins)), bins = as.vector(names(table(bins))))%>% 
      mutate(L_time = substr(bins, as.numeric(lapply(gregexpr(pattern = ',', bins), min))+1, as.numeric(lapply(gregexpr(pattern = ']', bins), min))-1)) %>%
      mutate(countPerNeuron = count/N) %>%
      mutate(FR = count/(dt*N/counter))
    
    if(input$fillc == "Custom (hex)")
      col = input$customfill
    else
      col = input$fillc
    
    if(input$FRdisplay == "Single line-graph"){
      
      # plot firing rate as a curve instead of a histogram
      fr <- cut.df %>% 
        ggplot() +
        geom_line(aes(x = seq(0+dt,T, by = dt), y=FR), color = col) +
        xlab("Time (s)") + ylab(yaxis_label) +
        theme_classic()+
        theme(panel.background = element_rect(fill = (bcol)))
    }
    else if(input$FRdisplay == "Vertically split line-graph"){
      #plot firing rate as a curve instead of a histogram
      
      cut.df <- evoked_asOne %>%
        group_by(trial) %>%
        mutate(bins = cut(spikes, breaks = seq(0,1.3,by = dt), dig.lab = 5)) %>%
        summarise(count = as.vector(table(bins)), bins = as.vector(names(table(bins))))%>%
        mutate(countPerNeuron = count/N) %>%
        mutate(FR = count/(dt*N/counter))
      
      fr <- cut.df %>% 
        filter(trial %in% data_sel) %>%
        ggplot() +
        geom_line(aes(x = seq(0+dt,T, by = dt), y=FR)) +
        xlab("Time (s)") + ylab(yaxis_label) +
        facet_grid(~trial)+
        theme_classic()+
        scale_fill_brewer(palette = input$color)+
        theme(panel.background = element_rect(fill = (col)))
    }
    else if(input$FRdisplay == "Single histogram"){
      
      # make sure the y-axis label is accurate (avg or total)
      
      fr <- cut.df %>% 
        ggplot() +
        geom_col(aes(x = L_time, y = FR), fill = col, color = "black") +
        xlab("Time (s)") + ylab(yaxis_label) +
        theme_classic()+
        theme(panel.background = element_rect(fill = (bcol)))
      
      if(input$syn == 1){
        fr <- fr + theme(panel.background = element_rect(fill = (bcol)), axis.text.x=element_blank(),
                   axis.ticks.x=element_blank())
      }
      
    }
    else if(input$FRdisplay == "Vertically split histogram"){
      
      cut.df <- evoked_asOne %>%
        group_by(trial) %>%
        mutate(bins = cut(spikes, breaks = seq(0,1.3,by = dt), dig.lab = 5)) %>%
        summarise(count = as.vector(table(bins)), bins = as.vector(names(table(bins))))%>%
        mutate(L_time = substr(bins, as.numeric(lapply(gregexpr(pattern = ',', bins), min))+1, as.numeric(lapply(gregexpr(pattern = ']', bins), min))-1)) %>%
        mutate(countPerNeuron = count) %>%
        mutate(FR = count/(dt))
      
      fr <- cut.df %>%
        filter(trial %in% data_sel) %>%
        ggplot() +
        geom_col(aes(x = L_time, y = FR, fill = trial), color = "black") +
        xlab("Time (s)") + ylab(yaxis_label) +
        theme_classic()+
        facet_wrap(~trial)+
        scale_fill_brewer(palette = input$color)+
        theme(panel.background = element_rect(fill = (bcol)),
              axis.text.x = element_text(size = 5))
      if(input$syn == 1){
        fr <- fr + theme(panel.background = element_rect(fill = (bcol)), axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())
      }
      
    }
    fr
  })
  output$interspikeinterval <- renderPlot({
    
    if(input$fillc == "Custom (hex)")
      col = input$customfill
    else
      col = input$fillc
    
    bcol = "#FFFFFF"
    if(input$background == "Light Grey"){bcol = "#e2e2e2"}
    else if(input$background == "Dark Grey"){bcol = "#4d4d4d"}
    else if(input$background == "Black"){bcol = "#000000"}
    else if(input$background == "Custom (hex)"){bcol = input$customcolor}
    
    bins = 1.27 / input$bins2
    d <- input$data
    data_sel = c()
    
    for(i in 1:nchar(d)){
      if(str_sub(d,i,i)!=',' && str_sub(d,i,i)!=' '){
        if(str_sub(d,i+1,i+1)==',' | str_sub(d,i+1,i+1)==" " | i == nchar(d)){
          if(str_sub(d,i-1,i-1)==',' | str_sub(d,i-1,i-1)==" ")
            data_sel <- c(data_sel,strtoi(str_sub(d,i,i)))
          else{
            data_sel <- c(data_sel,strtoi(str_sub(d,i-1,i)))
          }
        }
      }
    }
    if(input$ISIdisplay == "Single histogram"){
      diff.data <- evoked_asOne %>%
        group_by(trial) %>%
        filter(trial %in% data_sel) %>%
        summarize(diff = diff(spikes)) %>%
        filter(diff > 0)
      
      isi <- ggplot(diff.data) + 
        # filter by trial for selected data
        geom_histogram(aes(x=diff), bins = bins, color = "black", fill = col)+
        labs(y="Total spike count", x="Time diff (s)")+
        ggtitle("Interspike Interval")+
        theme_classic()+
        theme(panel.background = element_rect(fill = (bcol)))
    }
    else if(input$ISIdisplay == "Vertically split histogram"){
      cut.df <- evoked_asOne %>%
        group_by(trial) %>%
        filter(trial %in% data_sel) %>%
        summarize(diff = diff(spikes)) %>%
        filter(diff > 0)
      
      isi <- cut.df %>%
        #filter(trial %in% data_sel) %>%
        ggplot() +
        geom_histogram(aes(x=diff, fill = trial), bins = bins, color = "black")+
        labs(y="Total spike count", x="Time diff (s)")+
        ggtitle("Interspike Interval")+
        theme_classic()+
        facet_wrap(~trial)+
        scale_fill_brewer(palette = input$color)+
        theme(panel.background = element_rect(fill = (bcol)))
    }
    if(input$ISIdisplay == "Single line-graph"){
      diff.data <- evoked_asOne %>%
        group_by(trial) %>%
        filter(trial %in% data_sel) %>%
        summarize(diff = diff(spikes)) %>%
        filter(diff > 0)
      # plot firing rate as a curve instead of a histogram
      isi <- diff.data %>% 
        ggplot() +
        geom_line(aes(x = seq(0+dt,T, by = dt), y=FR), color = col) +
        xlab("Time diff (s)") + ylab("Total spike count") +
        theme_classic()+
        theme(panel.background = element_rect(fill = (bcol)))
    }
    isi
  })
  output$graph_desc <- renderText({
    output = ""
    d <- input$data
    data_sel = c()
    
    for(i in 1:nchar(d)){
      if(str_sub(d,i,i)!=',' && str_sub(d,i,i)!=' '){
        if(str_sub(d,i+1,i+1)==',' | str_sub(d,i+1,i+1)==" " | i == nchar(d)){
          if(str_sub(d,i-1,i-1)==',' | str_sub(d,i-1,i-1)==" ")
            data_sel <- c(data_sel,strtoi(str_sub(d,i,i)))
          else{
            data_sel <- c(data_sel,strtoi(str_sub(d,i-1,i)))
          }
        }
      }
    }
    
    N = length(data_sel)
    
    filtered <- evoked_asOne %>%
      filter(trial %in% data_sel)
    
    if(input$curr_tab == "Spike Times"){
      output <- "Spike time graphs show the raw data from the trials of the experiment, and in this example we graph a line for each neuron spike."
      output <- paste0(output," The average firing rate over the 1.3 second interval is: ",round((nrow(filtered)/N)/1.3, 3), " spikes/sec")
    }
    else if(input$curr_tab == "Firing Rate"){
      output <- "Firing rate graphs show the average number of spikes for each time-binned section of the data."
      
      d <- input$data
      data_sel = c()
      
      for(i in 1:nchar(d)){
        if(str_sub(d,i,i)!=',' && str_sub(d,i,i)!=' '){
          if(str_sub(d,i+1,i+1)==',' | str_sub(d,i+1,i+1)==" " | i == nchar(d)){
            if(str_sub(d,i-1,i-1)==',' | str_sub(d,i-1,i-1)==" ")
              data_sel <- c(data_sel,strtoi(str_sub(d,i,i)))
            else{
              data_sel <- c(data_sel,strtoi(str_sub(d,i-1,i)))
            }
          }
        }
      }
      N = length(data_sel)
      
      if(input$syn == 1){ # synchronize
        diff.data <- evoked_asOne %>%
          group_by(trial) %>%
          filter(trial %in% data_sel) %>%
          summarize(diff = diff(spikes)) %>%
          filter(diff > 0)
        dt <- min(diff.data$diff)
      }
      else{ # dont synchronize
        dt <- input$bins
      }
      T = 1.3
      
      cut.df <- evoked_asOne %>%
        filter(trial %in% data_sel) %>%
        mutate(bins = cut(spikes, breaks = seq(0,T,by = dt), dig.lab = 5)) %>%
        summarise(count = as.vector(table(bins)), bins = as.vector(names(table(bins))))%>% 
        mutate(countPerNeuron = count/N)
      
      FanoFactor <- cut.df %>%
        summarize(var(countPerNeuron)/mean(countPerNeuron))
      
      output <- paste0(output, " The fano factor (measure of spiketrain regularity) for this set of data is: ", round(FanoFactor, 2), ".")
      
      if(FanoFactor > 1){
        output <- paste0(output, " A fano factor greater than 1 implies the spike train is less regular than a randomly generated spiketrain.")
      }
      else if(FanoFactor == 1){
        output <- paste0(output, " A fano factor equal to 1 implies the spike train is as regular as a randomly generated spiketrain.")
      }
      else if(FanoFactor < 1){
        output <- paste0(output, " A fano factor less than 1 implies the spike train is more regular than a randomly generated spiketrain.")
      }
    }
    else if(input$curr_tab == "Interspike Interval"){ # ISI
      d <- input$data
      data_sel = c()
      
      for(i in 1:nchar(d)){
        if(str_sub(d,i,i)!=',' && str_sub(d,i,i)!=' '){
          if(str_sub(d,i+1,i+1)==',' | str_sub(d,i+1,i+1)==" " | i == nchar(d)){
            if(str_sub(d,i-1,i-1)==',' | str_sub(d,i-1,i-1)==" ")
              data_sel <- c(data_sel,strtoi(str_sub(d,i,i)))
            else{
              data_sel <- c(data_sel,strtoi(str_sub(d,i-1,i)))
            }
          }
        }
      }
      N = length(data_sel)
      
      diff.data <- evoked_asOne %>%
        #group_by(trial) %>%
        filter(trial %in% data_sel) %>%
        summarize(diff = diff(spikes)) %>%
        filter(diff > 0)
      
      CV <- diff.data %>%
        summarize(sd(diff)/mean(diff))
      
      output <- "Interspike interval graphs show the distribution of wait times between spikes."
      output <- paste0(output," The coefficient of variation (measure of the variability of intervals between spikes) for this set of data is: ", round(CV, 2), ".")
      
      if(CV > 1){
        output <- paste0(output, " A CV greater than 1 implies the spike train is less regular than a randomly generated spiketrain.")
      }
      else if(CV == 1){
        output <- paste0(output, " A CV equal to 1 implies the spike train is as regular as a randomly generated spiketrain.")
      }
      else if(CV < 1){
        output <- paste0(output, " A CV less than 1 implies the spike train is more regular than a randomly generated spiketrain.")
      }
      
    }
    output
  })
}

shinyApp(ui, server)
