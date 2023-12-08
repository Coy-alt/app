library(shiny)
library(visreg)
library(tidyverse)
library(ggplot2)
library(plsdepot)
library(dplyr)
library(modelr)
library(caret)
library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)


alldata <- read.csv("table02.csv", skip=3) # import all the data, skips the first three because they were filled with words mainly

firstsubdataset = alldata[2:9,] #takes only the 2 through 9 rows data, to cover the first section of data "percent races from 1973 to 2016"
secondsubdataset = alldata[11:18,]#takes only the 11 through 18 rows data, to cover the second section of data "percent races from 1973 to 2016"
thirdsubdataset = alldata[20:27,]#takes only the 20 through 27 rows data, to cover the third section of data "percent races from 1973 to 2016"


colnames<-colnames(firstsubdataset) #step1 to renaming the titles 
colnames(firstsubdataset)<-c("Race", "1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016") #renaming the titles

Years = c("1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016") #setting up the range of years

longer_data <- firstsubdataset %>%pivot_longer('1973':'2016', names_to = "Years", values_to = "Poverty_Percent") #rearanging the data to fit better on a plot
any(is.na(longer_data)) #changing missing data spots to na
print(longer_data) # checking to make sure the change and na works


longer_data$Poverty_Percent  <- as.double(longer_data$Poverty_Percent) #setting the poverty_percents to type double
longer_data$Years  <- as.double(longer_data$Years)#setting the Years to type double


All_races_data1 = longer_data[1:35,] #calling just the data for all races
White_only_data1 = longer_data[36:70,] #calling just the data for those who put white
Black_or_African_American_only_data1 = longer_data[71:105,] #calling just the data for those who put black or african american
Asian_only_data1 = longer_data[106:140,] #calling just the data for those who put asian
Hispanic_or_Latino_data1 = longer_data[141:175,] #calling just the data for those who put hispanic or latino
Mexican_data1 = longer_data[176:210,] #calling just the data for those who put mexican
Puerto_Rican_data1 = longer_data[211:245,] #calling just the data for those who put puerto rican
White_only_not_Hispanic_or_Latino_data1 = longer_data[246:280,] #calling just the data for those who put white with no hispanic or latino



colnames<-colnames(secondsubdataset) #step1 to renaming the titles 
colnames(secondsubdataset)<-c("Race", "1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016") #renaming the titles

Years = c("1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016") #setting up the range of years

longer_data <- secondsubdataset %>%pivot_longer('1973':'2016', names_to = "Years", values_to = "Poverty_Percent") #rearanging the data to fit better on a plot
any(is.na(longer_data)) #changing missing data spots to na
print(longer_data) # checking to make sure the change and na works


longer_data$Poverty_Percent  <- as.double(longer_data$Poverty_Percent) #setting the poverty_percents to type double
longer_data$Years  <- as.double(longer_data$Years)#setting the Years to type double


All_races_data2 = longer_data[1:35,] #calling just the data for all races
White_only_data2 = longer_data[36:70,] #calling just the data for those who put white
Black_or_African_American_only_data2 = longer_data[71:105,] #calling just the data for those who put black or african american
Asian_only_data2 = longer_data[106:140,] #calling just the data for those who put asian
Hispanic_or_Latino_data2 = longer_data[141:175,] #calling just the data for those who put hispanic or latino
Mexican_data2 = longer_data[176:210,] #calling just the data for those who put mexican
Puerto_Rican_data2 = longer_data[211:245,] #calling just the data for those who put puerto rican
White_only_not_Hispanic_or_Latino_data2 = longer_data[246:280,] #calling just the data for those who put white with no hispanic or latino



colnames<-colnames(thirdsubdataset) #step1 to renaming the titles 
colnames(thirdsubdataset)<-c("Race", "1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016") #renaming the titles

Years = c("1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016") #setting up the range of years

longer_data <- thirdsubdataset %>%pivot_longer('1973':'2016', names_to = "Years", values_to = "Poverty_Percent") #rearanging the data to fit better on a plot
any(is.na(longer_data)) #changing missing data spots to na
print(longer_data) # checking to make sure the change and na works


longer_data$Poverty_Percent  <- as.double(longer_data$Poverty_Percent) #setting the poverty_percents to type double
longer_data$Years  <- as.double(longer_data$Years)#setting the Years to type double


All_races_data3 = longer_data[1:35,] #calling just the data for all races
White_only_data3 = longer_data[36:70,] #calling just the data for those who put white
Black_or_African_American_only_data3 = longer_data[71:105,] #calling just the data for those who put black or african american
Asian_only_data3 = longer_data[106:140,] #calling just the data for those who put asian
Hispanic_or_Latino_data3 = longer_data[141:175,] #calling just the data for those who put hispanic or latino
Mexican_data3 = longer_data[176:210,] #calling just the data for those who put mexican
Puerto_Rican_data3 = longer_data[211:245,] #calling just the data for those who put puerto rican
White_only_not_Hispanic_or_Latino_data3 = longer_data[246:280,] #calling just the data for those who put white with no hispanic or latino

choices1 = c("All races","White only","Black or African American only","Asian only", 
              "Hispanic or Latino", "Mexican", "Puerto Rican","White only not Hispanic or Latino") #layout all the choices for a drop down menu
choices2 = c("All races","White only","Black or African American only","Asian only", 
             "Hispanic or Latino", "Mexican", "Puerto Rican","White only not Hispanic or Latino") #layout all the choices for a drop down menu
choices3 = c("All races","White only","Black or African American only","Asian only", 
             "Hispanic or Latino", "Mexican", "Puerto Rican","White only not Hispanic or Latino") #layout all the choices for a drop down menu

set.seed(164) # set so the numbers don't change
  
#start the ui section for the app
ui <- fluidPage( #start here to make a layout

    titlePanel("Poverty Analysis Based on Self-Reported Ethnicity"), #set a title for the whole page
    
    wellPanel(titlePanel("Overall Poverty Percent Analysis"), #set a wellpanel and state the title
      plotOutput("Plot2"), #layout a plot on this wellpanel
      
      
              ),
      
    wellPanel(#set another wllpanel 
      titlePanel("Overall Poverty Percent Analysis"),#set a title for this wellpanel
            #selectInput("datasets", #start a drop down menu
                        #label = "Select a Dataset", #set a title for the drop down menu
                        #choices = choices2), #set the values for the drop down menu
            selectInput("Races", #start a drop down menu
                        label = "Select a Race to have on the x-axis", #set a title for the drop down menu
                        choices = choices1), #set the values for the drop down menu
            #sliderInput("years", "Range of years on the x-axis", 1973, 2016, value = c(1973, 2016),
                        #sep = ""), #set a slider that controls the x-axis values
      fluidRow(#arrange another set of things
            column(6,plotOutput("Plot"), #set a plot
            ),
            column(6, verbatimTextOutput("two")), #set some text
            column(12, verbatimTextOutput("a")), #set some text
            #column(9, verbatimTextOutput("code")) #set some more text
      
      )),
    wellPanel(#set another wllpanel 
      titlePanel("Households with Kids Under 18 Poverty Percent Analysis"),#set a title for this wellpanel

      selectInput("Races2", #start a drop down menu
                  label = "Select a Race to have on the x-axis", #set a title for the drop down menu
                  choices = choices2), #set the values for the drop down menu

      fluidRow(#arrange another set of things
        column(6,plotOutput("Plot3"), #set a plot
        ),
        
        column(6, verbatimTextOutput("three")), #set some text
        column(12, verbatimTextOutput("b")), #set some text
        

        
      )),
    
    wellPanel(#set another wllpanel 
      titlePanel("Households with Kids Under 18 and a Single Mother Poverty Percent Analysis"),#set a title for this wellpanel
      
      selectInput("Races3", #start a drop down menu
                  label = "Select a Race to have on the x-axis", #set a title for the drop down menu
                  choices = choices3), #set the values for the drop down menu
      
      fluidRow(#arrange another set of things
        column(6,plotOutput("Plot4"), #set a plot
        ),
        column(6, verbatimTextOutput("four")), #set some text
        column(12, verbatimTextOutput("c")), #set some text
        
        
      )),
    
    #column(width = 10, rows = 15, 
    wellPanel(fluidRow(
      textOutput("explain"),
      textOutput("explain2"),
      textOutput("explain3"),
      textOutput("explain4")
      ))
)



#start the server info for the app
server <- function(input, output) { 
  
      output$Plot2 <- renderPlot({#setup another plot
    
            ggplot(data = longer_data, aes(x=Years,y = Poverty_Percent)) + geom_line(aes(color = Race), group =1)
           
            #print out the plot
      })
      output$explain <- renderPrint({"*Linear regression can be summed as the link between independent variables and a dependent variable or multiple dependent variables. This formation is then made into a line that shows the link between the two and can be used to understand all the variables better. It can also be used to try to guess what future variables could be. The math behind it starts with the equation Y=g(x). Y is the independent variable, (x) is the dependent variable, and g is the regression of the slope. The g is used to help predict the possible future values of (x). Normally there is a set of know Y and (x) variables that would make up a testing dataset. To find the g, it is assumed that it is a linear function and therefore the equation g(x)=∝x+β is used. The goal then is to get a ∝ and a β that best fit into the linear line. Using a sum equation helps here, because ∑_i((∝(x_i)+β-(y_i)))^2  is used to find the fit. Then, these two equations ∝=(∑_i ((x_i) (y_i))-∑_i ((x_i) ∑_i(y_i) ))/(∑_i (x_i)^2 -(∑_i(x_i))^2 ) and β=(1/n) ∑_i((y_i)-α (1/n)) ∑_i(x_i) are used to find the alpha and beta that make the fit as small as possible. All that will then allow you to predict the next possible values."})      
      output$a <- renderPrint({"*Mexican and Puerto Rican datasets have too much missing data to run a full analysis on."})
      output$b <- renderPrint({"*Mexican and Puerto Rican datasets have too much missing data to run a full analysis on."})
      output$c <- renderPrint({"*Mexican and Puerto Rican datasets have too much missing data to run a full analysis on."})

      choices12 = reactive({switch(input$Races, "All races" = All_races_data1, #create a list that ties the names to data sets
                               "White only" = White_only_data1,
                               "Black or African American only" = Black_or_African_American_only_data1,
                               "Asian only" = Asian_only_data1, 
                               "Hispanic or Latino" = Hispanic_or_Latino_data1, 
                               "Mexican" = Mexican_data1, 
                               "Puerto Rican" = Puerto_Rican_data1,
                               "White only not Hispanic or Latino" = White_only_not_Hispanic_or_Latino_data1)})
     

      output$two <- renderPrint({ #start the output for data, it will switch between the data sets
        correlation1 = cor(choices12()$Years, choices12()$Poverty_Percent, use = "complete.obs")#calculate the correlation between poverty percent and years
        print(paste("Correlation between race and years =", correlation1))#print out this info on correlation
        separationOfdata <- sample.split(Y=choices12()$Poverty_Percent, SplitRatio=0.75)#split of the data 75 percent in one and 25 percent in another
        trainningData <- subset(x=choices12(), separationOfdata==TRUE) #set the data that need to be trained with
        testingData <- subset(x=choices12(), separationOfdata==FALSE)#set the data to test with
        buildModel <- lm(formula=Poverty_Percent ~ Years, data=trainningData)#build a model
        print(summary(buildModel)) #print out the model on the app
        ResidualsOfbuildModel <- as.data.frame(residuals(buildModel)) #finds the leftovers
        predictionOfData <- predict(buildModel, testingData) #try to think of what numbers will come next
        checkingModel <- cbind(testingData$Poverty_Percent, predictionOfData) #check how the model is doing
        colnames(checkingModel) <- c('Real Numbers', 'Guess Numbers') #change the names of the columns
        checkingModel <- as.data.frame(checkingModel) #set the checked model into a data frame
        print(head(checkingModel)) #print the checked model
        MeanSquaredError <- mean((checkingModel$Real - checkingModel$Guess)^2) #find the mean squared error
        print(paste("MeanSquaredError =", MeanSquaredError))#print the mean squared error
        RootMeanSquaredError <- sqrt(MeanSquaredError)#find the root mean squared error
        print(paste("RootMeanSquaredError =",RootMeanSquaredError))#print the root mean squared error
        linearRegression = lm(Poverty_Percent ~ Years, data=testingData) #find the linear regression
        fitted(linearRegression)#fit linear regression
        resid(linearRegression)#resid linear regression
        #print(summary(linearRegression))#print the linear regression
        #Years = c("1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016")
        #Years1 = data.frame(1973, 1980, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013.5, 2013.6, 2014, 2015, 2016)
        
        #x1    <- years[1]
        #x2    <- years[2]
       #years <- reactive({
          #seq(input$years[1], input$years[2], by = 1)})
          
        output$Plot <- renderPlot({# set up a plot
          #bins <- seq(input$years[1], input$years[2]) #set the range for the x-axis
          #years <- seq(input$years[1], input$years[2], length.out = input$years + 1)
          ggplot(data = choices12(), aes(x=Years,y = Poverty_Percent)) + geom_line(aes(color = Race), group =1)+ geom_abline(intercept = linearRegression[1]$coefficients[1], slope = linearRegression[1]$coefficients[2], color="blue")
          #print out the plot
          })
  
        
      })
      choices21 = reactive({switch(input$Races2, "All races" = All_races_data2, #create a list that ties the names to data sets
                                   "White only" = White_only_data2,
                                   "Black or African American only" = Black_or_African_American_only_data2,
                                   "Asian only" = Asian_only_data2, 
                                   "Hispanic or Latino" = Hispanic_or_Latino_data2, 
                                   "Mexican" = Mexican_data2, 
                                   "Puerto Rican" = Puerto_Rican_data2,
                                   "White only not Hispanic or Latino" = White_only_not_Hispanic_or_Latino_data2)})
      
      output$three <- renderPrint({ #start the output for data, it will switch between the data sets
        correlation1 = cor(choices21()$Years, choices21()$Poverty_Percent, use = "complete.obs")#calculate the correlation between poverty percent and years
        print(paste("Correlation between race and years =", correlation1))#print out this info on correlation
        separationOfdata <- sample.split(Y=choices21()$Poverty_Percent, SplitRatio=0.75)#split of the data 75 percent in one and 25 percent in another
        trainningData <- subset(x=choices21(), separationOfdata==TRUE) #set the data that need to be trained with
        testingData <- subset(x=choices21(), separationOfdata==FALSE)#set the data to test with
        buildModel <- lm(formula=Poverty_Percent ~ Years, data=trainningData)#build a model
        print(summary(buildModel)) #print out the model on the app
        ResidualsOfbuildModel <- as.data.frame(residuals(buildModel)) #finds the leftovers
        predictionOfData <- predict(buildModel, testingData) #try to think of what numbers will come next
        checkingModel <- cbind(testingData$Poverty_Percent, predictionOfData) #check how the model is doing
        colnames(checkingModel) <- c('Real Numbers', 'Guess Numbers') #change the names of the columns
        checkingModel <- as.data.frame(checkingModel) #set the checked model into a data frame
        print(head(checkingModel)) #print the checked model
        MeanSquaredError <- mean((checkingModel$Real - checkingModel$Guess)^2) #find the mean squared error
        print(paste("MeanSquaredError =", MeanSquaredError))#print the mean squared error
        RootMeanSquaredError <- sqrt(MeanSquaredError)#find the root mean squared error
        print(paste("RootMeanSquaredError =",RootMeanSquaredError))#print the root mean squared error
        linearRegression = lm(Poverty_Percent ~ Years, data=testingData) #find the linear regression
        fitted(linearRegression)#fit linear regression
        resid(linearRegression)#resid linear regression
        #print(summary(linearRegression))#print the linear regression
        #Years = c("1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016")
        #Years1 = data.frame(1973, 1980, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013.5, 2013.6, 2014, 2015, 2016)
        
        #x1    <- years[1]
        #x2    <- years[2]
        #years <- reactive({
        #seq(input$years[1], input$years[2], by = 1)})
        
        output$Plot3 <- renderPlot({# set up a plot
          #bins <- seq(input$years[1], input$years[2]) #set the range for the x-axis
          #years <- seq(input$years[1], input$years[2], length.out = input$years + 1)
          ggplot(data = choices21(), aes(x=Years,y = Poverty_Percent)) + geom_line(aes(color = Race), group =1)+ geom_abline(intercept = linearRegression[1]$coefficients[1], slope = linearRegression[1]$coefficients[2], color="blue")
          #print out the plot
        })
      })
        
        choices34 = reactive({switch(input$Races3, "All races" = All_races_data3, #create a list that ties the names to data sets
                                     "White only" = White_only_data3,
                                     "Black or African American only" = Black_or_African_American_only_data3,
                                     "Asian only" = Asian_only_data3, 
                                     "Hispanic or Latino" = Hispanic_or_Latino_data3, 
                                     "Mexican" = Mexican_data3, 
                                     "Puerto Rican" = Puerto_Rican_data3,
                                     "White only not Hispanic or Latino" = White_only_not_Hispanic_or_Latino_data3)})
        
        output$four <- renderPrint({ #start the output for data, it will switch between the data sets
          correlation1 = cor(choices34()$Years, choices34()$Poverty_Percent, use = "complete.obs")#calculate the correlation between poverty percent and years
          print(paste("Correlation between race and years =", correlation1))#print out this info on correlation
          separationOfdata <- sample.split(Y=choices34()$Poverty_Percent, SplitRatio=0.75)#split of the data 75 percent in one and 25 percent in another
          trainningData <- subset(x=choices34(), separationOfdata==TRUE) #set the data that need to be trained with
          testingData <- subset(x=choices34(), separationOfdata==FALSE)#set the data to test with
          buildModel <- lm(formula=Poverty_Percent ~ Years, data=trainningData)#build a model
          print(summary(buildModel)) #print out the model on the app
          ResidualsOfbuildModel <- as.data.frame(residuals(buildModel)) #finds the leftovers
          predictionOfData <- predict(buildModel, testingData) #try to think of what numbers will come next
          checkingModel <- cbind(testingData$Poverty_Percent, predictionOfData) #check how the model is doing
          colnames(checkingModel) <- c('Real Numbers', 'Guess Numbers') #change the names of the columns
          checkingModel <- as.data.frame(checkingModel) #set the checked model into a data frame
          print(head(checkingModel)) #print the checked model
          MeanSquaredError <- mean((checkingModel$Real - checkingModel$Guess)^2) #find the mean squared error
          print(paste("MeanSquaredError =", MeanSquaredError))#print the mean squared error
          RootMeanSquaredError <- sqrt(MeanSquaredError)#find the root mean squared error
          print(paste("RootMeanSquaredError =",RootMeanSquaredError))#print the root mean squared error
          linearRegression = lm(Poverty_Percent ~ Years, data=testingData) #find the linear regression
          fitted(linearRegression)#fit linear regression
          resid(linearRegression)#resid linear regression
          #print(summary(linearRegression))#print the linear regression
          #Years = c("1973", "1980", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013.5", "2013.6", "2014", "2015", "2016")
          #Years1 = data.frame(1973, 1980, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013.5, 2013.6, 2014, 2015, 2016)
          
          #x1    <- years[1]
          #x2    <- years[2]
          #years <- reactive({
          #seq(input$years[1], input$years[2], by = 1)})
          
          output$Plot4 <- renderPlot({# set up a plot
            #bins <- seq(input$years[1], input$years[2]) #set the range for the x-axis
            #years <- seq(input$years[1], input$years[2], length.out = input$years + 1)
            ggplot(data = choices34(), aes(x=Years,y = Poverty_Percent)) + geom_line(aes(color = Race), group =1)+ geom_abline(intercept = linearRegression[1]$coefficients[1], slope = linearRegression[1]$coefficients[2], color="blue")
            #print out the plot
          })
        
      })
      
      
}


# Run the application 
shinyApp(ui = ui, server = server)
