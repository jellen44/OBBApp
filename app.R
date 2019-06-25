data_date <- format(Sys.time(), "%Y-%m-%d")

#Packages
library(shiny)
library(plotrix)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(pander)
suppressMessages(library(tidyverse))
library(lubridate)
library(stringr)
library(stargazer)
library(tidytext)
library(quanteda)
library(leaflet)
library(geojsonio)
library(shinyWidgets)
library(shinydashboard)
library(wordcloud2)

#Reading in Data
#A <- str_sort(dir("/Users/jacobellen/dropbox/OBB/OBBData"))
#log <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/formcompletionlog.csv"))
#med <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/medication.csv"))
#gen2 <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/form-baseline.csv"))
#motif <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/motif_segmentvalue.csv"))
#symp <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/motif_segment.csv"))
#id <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/motif_motif.csv"))
#game <- read.csv(paste0("/Users/jacobellen/dropbox/OBB/OBBData/", A[length(A)], "/gamescore.csv"))
#pop <- read.csv("/Users/jacobellen/dropbox/OBB/PopSurvey/form-may-survey.csv")
#newsurvey <- read.csv('/Users/jacobellen/dropbox/OBB/PopSurvey/form-obbcommunity-viewpoints-survey.csv')

log <- read.csv("formcompletionlog.csv")
med <- read.csv("medication.csv")
gen <- read.csv("form-baseline.csv")
motif <- read.csv("motif_segmentvalue.csv")
symp <- read.csv("motif_segment.csv")
id <- read.csv("motif_motif.csv")

#Background Code
reg <- log %>%
  filter(Stage=="Registration")
reg <- distinct(reg, UserId, .keep_all=TRUE)
index = which(names(gen)=="userid")
names(gen)[index]<-paste("UserId")
survey <- merge(reg, gen, by="UserId")
sympfreq <- symp %>%
  group_by(Name) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
motif$uMotifTime <- ymd_hms(motif$uMotifTime)
motif <- motif %>%
  mutate(week_name = week(uMotifTime-days(4))-9)
sympmotif <- symp %>%
  select(Id, MotifId, SegmentCategoryId, Name, Text1, Text2, Text3, Text4, Text5)
colnames(id) <- c("MotifId", "UserId")
motif_motif <- motif %>% 
  select(DateCreated, SegmentId, Value, week_name)
colnames(motif_motif) <- c("DateCreated", "Id", "Value", "Weekname")
finalid <- merge(id, sympmotif, by="MotifId")
finalid <- merge(finalid, motif_motif, by="Id")
sympdata <- finalid %>%
  group_by(Name) %>%
  summarise(mv = mean(Value))
sympdata2 <- finalid %>%
  group_by(Name, Weekname) %>%
  summarise(mv = mean(Value))
survey <- survey %>%
  mutate(yearswglio = (howmanymonthshaveyoubeenlivingwithgbm/12))
survey$yearcat[survey$yearswglio>0] <- "Less than One Year"
survey$yearcat[survey$yearswglio>1] <- "1-2 Years"
survey$yearcat[survey$yearswglio>2] <- "2-5 Years"
survey$yearcat[survey$yearswglio>5] <- "More than 5 Years"
survey <- survey %>%
  mutate(yearcat = factor(yearcat,
                          levels = c("Less than One Year", "1-2 Years", "2-5 Years", "More than 5 Years")))
x <- unname(table(survey$yearcat))
lbl <-  c("Less than One Year", "1-2 Years", "2-5 Years", "More than 5 Years")
gen2 <- gen %>%
  filter(atwhatagewasyourglioblastomadiagnosed<100)
gengender <- gen %>%
  group_by(whatwasyourgenderatbirth) %>%
  summarise(gender = n())


symplabel <- symp %>%
  select(Name, Text1, Text2, Text3, Text4, Text5)
colnames(symplabel) <- c("Symptom_Name", "1", "2", "3", "4", "5")

#Medication
med$Name <- str_to_lower(med$Name)
str1 <- str_subset(med$Name, "keppra")
str2 <- str_subset(med$Name, "ativan")
str3 <- str_subset(med$Name, "lipitor")
str4 <- str_subset(med$Name, "decadron")
str5 <- str_subset(med$Name, "oxycodone")
str6 <- str_subset(med$Name, "citalopram")
str7 <- str_subset(med$Name, "multi")
str8 <- str_subset(med$Name, "temodar")
str9 <- str_subset(med$Name, "vitamin d")
numvec <- c(length(str1), length(str2), length(str3), length(str4)+1, length(str5), length(str6), length(str7)+1, length(str8), length(str9))
labvec <- c("Keppra", "Ativan", "Lipitor", "Decadron", "Oxycodone", "Citalophram", "MultiVitamin", "Temodar", "Vitamin D")
medfreq <- cbind.data.frame(labvec, numvec)
medfreq <- medfreq %>%
  arrange(desc(numvec))
medvec <- as.vector(medfreq$numvec)
labelz <-  as.vector(medfreq$labvec)
gengbm <- gen %>%
  select(whatisthemostimportantthingforyoutogetfromyourgbmtreatment, areyoucurrentlyreceivingtreatmentifsowhattreatment,haveyoureceivedothertreatmentsforglioblastomainthepast, whatothermedicationsareyoutaking)
gengbm$whatisthemostimportantthingforyoutogetfromyourgbmtreatment <- as.character(gengbm$whatisthemostimportantthingforyoutogetfromyourgbmtreatment)
gengbm$areyoucurrentlyreceivingtreatmentifsowhattreatment <- as.character(gengbm$areyoucurrentlyreceivingtreatmentifsowhattreatment)
gengbm$haveyoureceivedothertreatmentsforglioblastomainthepast <- as.character(gengbm$haveyoureceivedothertreatmentsforglioblastomainthepast)
gengbm$whatothermedicationsareyoutaking <- as.character(gengbm$whatothermedicationsareyoutaking)
colnames(gengbm) <- c("What is the goal of your GBM treatment", "What Treatment Are You Currently Receiving (If Any)?", "Which Treatments Have You Received in the Past (If Any)?", "What Other Medications are you Taking?")
removev <- c("treatment", "Treatment", "therapy", "treatments", "Treatments", 
             "applicable", "clinical", "specify", "trial", "drugs", "use", "please", "label", "past",
             "drug", "low", "vitamin", "mg", "generic", "dose", "pm")

#Map
genstate <- gen %>%
  select(whichstateoftheusdoyoulivein, whichcountrydoyouliveinnow)
colnames(genstate) <- c("NAME", "Country")
genstate$NAME <- as.character(genstate$NAME)
genstate$Country <- as.character(genstate$Country)
genstate <- genstate %>%
  filter(is.na(NAME)==FALSE & NAME!="0" & NAME!="F" & NAME!="QLD" & NAME!="USA" & NAME!= "o" & NAME!="I live in europa" & NAME!= "None" & NAME!="O" &NAME!="Test"&NAME!="h"&NAME!="La" & NAME != "United states") %>% 
  filter(Country!="Poland" & Country!="Belgique" & Country != "Portugal" & Country != "United kingdom" & Country!="Isle of Man" & Country!= "Netherlands" & Country!="Israel" & Country!="Norway" & Country!="India" & Country!="Uk" & Country!="No" & Country!="Lebanon" & Country!="india" & Country!="BC" & Country!="Canada")
for (i in 1:length(genstate$NAME)) {
  if(genstate$NAME[i]=="Pa" | genstate$NAME[i]=="PA") {
    genstate$NAME[i] <- "Pennsylvania"
  }
  if(genstate$NAME[i]=="Ak") {
    genstate$NAME[i] <- "Alaska"
  }
  if(genstate$NAME[i]=="Washington DC") {
    genstate$NAME[i] <- "District of Columbia"
  }
  if(genstate$NAME[i]=="VA" | genstate$NAME[i]=="Va") {
    genstate$NAME[i] <- "Virginia"
  }
  if(genstate$NAME[i]=="OH") {
    genstate$NAME[i] <- "Ohio"
  }
  if(genstate$NAME[i]=="NY" | genstate$NAME[i]=="Ny" | genstate$NAME[i]=="New york") {
    genstate$NAME[i] <- "New York"
  }
  if(genstate$NAME[i]=="WI" | genstate$NAME[i]=="Wi") {
    genstate$NAME[i] <- "Wisconsin"
  }
  if(genstate$NAME[i]=="NC" | genstate$NAME[i]=="Nc") {
    genstate$NAME[i] <- "North Carolina"
  }
  if(genstate$NAME[i]=="Ca" | genstate$NAME[i]=="CA") {
    genstate$NAME[i] <- "California"
  }
  if(genstate$NAME[i]=="NH") {
    genstate$NAME[i] <- "New Hampshire"
  }
  if(genstate$NAME[i]=="WA" | genstate$NAME[i]=="Wa") {
    genstate$NAME[i] <- "Washington"
  }
  if(genstate$NAME[i]=="TX" | genstate$NAME[i]=="Tx" | genstate$NAME[i]=="texas") {
    genstate$NAME[i] <- "Texas"
  }
  if(genstate$NAME[i]=="NJ" | genstate$NAME[i]=="Nj" | genstate$NAME[i]=="New jersey") {
    genstate$NAME[i] <- "New Jersey"
  }
  if(genstate$NAME[i]=="CO" | genstate$NAME[i]=="Denver") {
    genstate$NAME[i] <- "Colorado"
  }
  if(genstate$NAME[i]=="RI" | genstate$NAME[i]=="Rhode island") {
    genstate$NAME[i] <- "Rhode Island"
  }
  if(genstate$NAME[i]=="Ma" | genstate$NAME[i]=="MA") {
    genstate$NAME[i] <- "Massachusetts"
  }
  if(genstate$NAME[i]=="Mississipp") {
    genstate$NAME[i] <- "Mississippi"
  }
  if(genstate$NAME[i]=="IN") {
    genstate$NAME[i] <- "Indiana"
  }
  if(genstate$NAME[i]=="Mn" | genstate$NAME[i]=="MN") {
    genstate$NAME[i] <- "Minnesota"
  }
  if(genstate$NAME[i]=="Az") {
    genstate$NAME[i] <- "Arizona"
  }
  if(genstate$NAME[i]=="Ky" | genstate$NAME[i]=="KY") {
    genstate$NAME[i] <- "Kentucky"
  }
  if(genstate$NAME[i]=="Ga") {
    genstate$NAME[i] <- "Georgia"
  }
  if(genstate$NAME[i]=="Nyc" | genstate$NAME[i]=="new york") {
    genstate$NAME[i] <- "New York"
  }
  if(genstate$NAME[i]=="FL" | genstate$NAME[i]=="Fl") {
    genstate$NAME[i] <- "Florida"
  }
  if(genstate$NAME[i]=="Sc") {
    genstate$NAME[i] <- "South Carolina"
  }
  if(genstate$NAME[i]=="CT" | genstate$NAME[i]=="Ct") {
    genstate$NAME[i] <- "Connecticut"
  }
  if(genstate$NAME[i]=="IL" | genstate$NAME[i]=="Il") {
    genstate$NAME[i] <- "Illinois"
  }
  if(genstate$NAME[i]=="R.I.") {
    genstate$NAME[i] <- "Rhode Island"
  }
  if(genstate$NAME[i]=="Minneota") {
    genstate$NAME[i] <- "Minnesota"
  }
  if(genstate$NAME[i]=="MD") {
    genstate$NAME[i] <- "Maryland"
  }
}

genstate <- genstate %>%
  group_by(NAME) %>%
  summarise(FREQ=n())
states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"
states <- geojson_read(states.url, what= "sp")
states@data$NAME <- as.character(states@data$NAME)
states@data <- merge(genstate, 
                     states@data, 
                     by = "NAME", all=TRUE)
states@data$FREQ <- as.numeric(states@data$FREQ)
for (i in 1:52) {
  if (is.na(states@data$FREQ[i])==TRUE) {
    states@data$FREQ[i] <- 0
  }
}
states@data <- states@data %>%
  arrange(STATE)

labels <- sprintf(
  "<strong>%s</strong><br/>%g OBB Patients </sup>",
  states$NAME, states$FREQ
) %>% lapply(htmltools::HTML)

#Setting up year categories
surveyplot <- survey %>%
  group_by(yearcat) %>%
  summarise(freq=n()) %>%
  filter(is.na(yearcat)==FALSE) 

#Total to Proportion
gen2 <- gen2 %>%
  filter(atwhatagewasyourglioblastomadiagnosed > 0)
sum1 <- as.numeric(sympfreq[1,2])
sympfreq$n <- as.numeric(sympfreq$n)
sympfreq <- sympfreq %>%
  mutate(prop = (n/sum1))
sympfreq <- sympfreq %>%
  mutate(percent = prop*100)
#sympfreq$prop <- (sympfreq$prop*100)
surveyplot$freq <- as.numeric(surveyplot$freq)
sum2 <- sum(surveyplot$freq)
surveyplot <- surveyplot %>%
  mutate(prop = (freq/sum2))
surveyplot <- surveyplot %>%
  mutate(percent = prop*100)
gengender$gender <- as.numeric(gengender$gender)
sum3<- sum(gengender$gender)
gengender <- gengender %>%
  mutate(prop = (gender/sum3)) 
gengender <- gengender %>%
  mutate(percent=prop*100)

for (i in 1:length(sympdata2$Weekname)) 
  if(sympdata2$Weekname[i] <= 0) 
    sympdata2$Weekname[i] <- sympdata2$Weekname[i]+53

#Main App
ui <- dashboardPage(#setBackgroundColor(
  #  color = c("#73C2FB", "#F7FBFF"),
  #  gradient = "linear",
  # direction = "bottom"
  #),
  dashboardHeader(title="OBB's Interactive App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Symptoms", tabName = "SymptomTracking"),
      menuItem("Demographics", tabName="Demographics"),
      menuItem("WordClouds", tabName="WordClouds"),
      menuItem("Patient Map", tabName="PatientMap")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="Introduction", 
              box(width=12, h4(textOutput(outputId = "intro")), h4("OurBrainBank is a certified non-profit organization, and our study is certified by the New England Independent Review Board."), img(src='obblogo.png', align = "center", width=
                                                                      "50%"))),
    tabItem(tabName="SymptomTracking",
                 fluidRow(width=4, box(selectInput(inputId = "symptomname", 
                             label = "Choose a Symptom from the Menu to See Its Rating Information",
                             choices = levels(sympdata2$Name))),
               box(title=textOutput(outputId="tabletext"), tableOutput(outputId = "table"))),
             box(width=12, plotOutput(outputId = "symptom", height="320px"
                            )), 
             fluidRow(
                      box(width=6,plotOutput(outputId = "symptom1"#, width = "100%", height = "350px"
                                 )), 
                      box(width=6, plotOutput(outputId = "symptom2"#, width = "100%", height = "350px"
                                 )))
            ),
    
     tabItem(tabName="Demographics",  
              fluidRow(
                box(title="Fill in Your Information and then Click the Button Below!",
                            checkboxGroupInput(inputId = "yearswgbm", label = "How Many Years Have You Had GBM?", choices=levels(survey$yearcat), selected = 
                                                 "1-2 Years"),
                            checkboxGroupInput(inputId = "sex", label = "What is your sex?", choices=levels(gengender$whatwasyourgenderatbirth), selected = "male"),
                            textInput(inputId = "age", label="What was your Age of GBM Diagnosis?"),
                            actionButton("goButton", "Go!")),
                 box(plotOutput(outputId = "demo1"#, width="100%", height="250px"
                                         ))),
             fluidRow(
                 box(plotOutput(outputId = "demo3"#, width = "100%", height = "250px"
                                         )),
             box(plotOutput(outputId = "demo2")))
     ),
    tabItem(tabName="WordClouds", 
                 box(
                 selectInput(inputId="question",
                             label="Choose a Question To See A WordCloud Below",
                             choices=colnames(gengbm)),
                 sliderInput(inputId = "numwords",
                             label="Slide to Adjust the Number of Words in the Wordcloud",
                             max=100, min=1, value=50)),
                 box(plotOutput(outputId = "wordcloud")
               
                       )
              
    ),
    tabItem(tabName="PatientMap", leafletOutput(outputId="map")
    )
    #,
    #tabPanel("Medications", plotOutput(outputId="demo5"), plotOutput(outputId="demo4"))
     
    
    ) 
)
)


#Plots
server <- function(input, output, session) {
  output$table <- renderTable({
    symplabel <- symplabel %>%
      filter(Symptom_Name==input$symptomname)
    symplabel[1,2:6]
  }#, caption = paste0(name, " Table"),
  #caption.placement = getOption("xtable.caption.placement", "top"), 
  #caption.width = getOption("xtable.caption.width", NULL)
  )
  output$tabletext <- renderText({
    name <- as.String(input$symptomname)
    paste0(name, " Rating Table")
  })
  output$symptom <- renderPlot({
    symp1 <- sympdata2 %>%
      filter(Name==input$symptomname)
    symp1 %>%
      ggplot(aes(x=Weekname, y=mv)) + geom_point() + geom_smooth(method="lm", fill=NA) + 
      labs(x="Week Name", y="Average Symptom Rating", 
           title = paste0("Mean ", 
                          input$symptomname, " Ratings"), subtitle = paste0("*5 is the most favorable ", input$symptomname, " rating")) + 
      theme_classic() + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=14), plot.title = element_text(size=16), axis.title=element_text(size=14))
  })
  output$symptom1 <- renderPlot({
    sympfreq %>%
      mutate(ToHighlight = ifelse(Name == input$symptomname, "yes", "no")) %>%
      ggplot(aes(x=reorder(Name, -n), y=prop, fill=ToHighlight)) + geom_col() + 
      scale_fill_manual(values = c("yes"="dark red", "no"="light blue"), guide = FALSE) +
      labs(x=NULL, y="Proportion of Each Symptom", title = "Frequency of Tracking Each Symptom") + 
      theme_classic() + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = .5, size=9), plot.title = element_text(size=16), axis.text.y = element_text(size=12), axis.title=element_text(size=14)) 
  })
  output$symptom2 <- renderPlot({
    sympdata %>%
      mutate(ToHighlight = ifelse(Name == input$symptomname, "yes", "no" )) %>%
      ggplot(aes(x=reorder(Name, -mv), y = mv, fill=ToHighlight)) + geom_col() + 
      scale_fill_manual(values = c("yes"="dark red", "no"="light blue"), guide = FALSE) +
      geom_text(aes(label = round(mv, 1), vjust="outward"), size=2.5) + 
      labs(x=NULL, y="Average Symptom Rating", title = "Average Symptom Ratings") + theme_classic() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = .5, size=9), plot.title = element_text(size=16), axis.text.y = element_text(size=12), axis.title=element_text(size=14))
  })
  output$demotext1 <- renderText({
    if(is.null(input$yearswgbm)==TRUE) {
      print("No Boxes Checked")
    }
  })
  output$demo1 <- renderPlot({
    if (input$goButton == 0) {
      return()
    }
    if (is.null(input$yearswgbm)==TRUE) {
      return() 
    }
    surveyplot %>%
      mutate(ToHighlight = ifelse(yearcat == input$yearswgbm, "yes", "no")) %>%
      ggplot(aes(x=yearcat, y= prop, fill=ToHighlight)) + geom_bar(stat="identity", color="black") + geom_text(aes(label = paste0(round(percent, 1), "%"),
                                                                                                                   vjust=-.35))  + 
      scale_fill_manual(values = c("yes"="dark red", "no"="light blue"), guide = FALSE) +
      labs(x=NULL, y="Proportion of Respondents", title="Number of Years Since Diagnosis") + theme_classic() +
      theme(legend.position = "none", axis.text.x = element_text(size=9), plot.title = element_text(size=16), axis.text.y = element_text(size=12), axis.title=element_text(size=14)) 
  })
  output$demo2 <- renderPlot({
    if (input$goButton == 0)
      return()
    if (is.null(input$age)==TRUE) {
      return() 
    }
    gen2 %>%
      ggplot(aes(x=atwhatagewasyourglioblastomadiagnosed)) + geom_histogram(fill="light blue") + 
      geom_vline(xintercept = as.numeric(input$age), linetype="dashed", color = "dark red", size=2) + 
      labs(x="Age", y="Number of Respondents", title="Age of GBM Diagnosis") + 
      theme_classic() + theme(axis.text.x = element_text(size=14), plot.title = element_text(size=16), axis.title=element_text(size=15))
  })
  output$demo3 <- renderPlot({
    if (input$goButton == 0)
      return()
    if (is.null(input$sex)==TRUE) {
      return() 
    }
    gengender %>%
      mutate(ToHighlight = ifelse(whatwasyourgenderatbirth == input$sex, "yes", "no")) %>%
      ggplot(aes(x=reorder(whatwasyourgenderatbirth, -gender), y=prop, fill=ToHighlight)) + geom_col(width = .50, color="black") + 
      geom_text(aes(label = paste0(round(percent,1), "%"),
                    vjust=-.35)) +
      scale_fill_manual(values = c("yes"="dark red", "no"="light blue"), guide = FALSE) +
      labs (x="Sex", y="Proportion of Respondents", title="Sex of OurBrainBank Participants") + theme_classic() + 
      theme(legend.position = "none", plot.title = element_text(size=16), axis.text.x = element_text(size=14), axis.text.y = element_text(size=10), axis.title=element_text(size=15))
  })
  output$demo4 <- renderPlot({
    stepfreq %>% 
      ggplot(aes(x=reorder(steplabvec, -stepvec), y=stepvec)) + geom_col(fill="blue", color="black", width=.7) + 
      labs (x=NULL, y="Number of Respondents", title= "Are You Taking Any of These Other Steps?") + 
      theme_classic()
  })
  output$demo5 <- renderPlot({
    pie(medvec,labels = labelz, main = "Most Common Medications for OBB Patients")
  })
  output$wordcloud <- renderPlot({
    gengbm <- gengbm %>%
      select(input$question)
    gbmcorpus1 <- corpus(gengbm, text_field = as.String(input$question))
    dfm.matrix <- dfm(gbmcorpus1,
                      ngrams = 1,
                      remove = c(stopwords("english"), removev),
                      stem = FALSE,
                      remove_punct = TRUE)
    gbmcorpus2 <- convert(dfm.matrix, to = "matrix")
    v <- sort(colSums(gbmcorpus2), decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=input$numwords, random.order=FALSE, 
              colors=brewer.pal(8, "Dark2")
              ,
              scale = c(4.5,1)
              )
  })
  output$map <- renderLeaflet({
    bins <- c(0, 1, 3, 5, 10, 20, 50)
    colors <- colorBin(palette = "YlOrRd", domain = states@data$FREQ, bins=bins)
    states %>%
      leaflet() %>%
      setView(-96, 28, 3) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(fillColor = ~colors(FREQ), weight =2, opacity =1, 
                  color="white", dashArray = "3", fillOpacity = .7, highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),   label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal=colors, values=states@data$FREQ, title = "Concentration of OurBrainBank Residents", 
                position="bottomright", opacity=2)
  })
  output$intro <- renderText({
    paste("OurBrainBank is an innovative, patient-led movement designed to move glioblastoma (GBM) from terminal to treatable, powered by patients. Our key tool is our free app. It enables people affected by GBM to manage their disease by tracking their symptoms—an essential path to coping and surviving. It also allows patients to donate their data to medical researchers. Data is vital in the search for new life-saving treatments. This interactive element of the app allows you to see how everyone using the app is doing, so you can compare symptoms, demographic information, and even see where everyone is located. We hope this is helpful!")
  })
  
}

shinyApp(ui, server)