
require(shiny)
require(babynames)
require(ggplot2)
library(reshape2)

ui <- fluidPage(
  titlePanel("Wykres ilości nadawanych imion w zależności od czasu."),
  sidebarLayout(
    sidebarPanel(
      textInput("input1", "Wpisz imię:", "Mary"),
      textOutput("tekst"),
      textInput("input2", "Płeć:", "F"),
      textOutput("tekst2"),
      textInput("input3", "Wpisz imiona:", "Mary, Sara")),
    mainPanel(
      textOutput("info"),
      textOutput("tekstplot"),
      plotOutput("plot"),
      textOutput("tekstplot2"),
      plotOutput("plot2"),
      textOutput("tekstplot3"),
      plotOutput("plot3"))
  )
)



server <- function(input,output) {
  output$info <- renderText({print("Jak dużo dziewczynek o imieniu Mary urodziło się w ostatnich latach? Czy kiedyś ktoś miał na imię Boa? Używając tej aplikacji możesz poszukać odpowiedzi na te oraz inne pytania.")})
  output$tekst <- renderText({print("Wpisz F dla kobiety, M dla mężczyzny.")})
  output$tekst2 <- renderText({print("Chcąc porównać ilość dzieci o różnych imionach wpisz imiona w okienko ponieżej przedzielając je przecinkiem.")})

  output$tekstplot <- renderText({paste("Wykres dla imienia:", input$input1)})
  output$plot <- renderPlot({
    N <- babynames[babynames$name == input$input1,]
    X = N[N$sex == input$input2,]
    rok = X$year
    ilosc = X$n
    df=data.frame(rok, ilosc)
    ggplot(df, aes(x = rok, y = ilosc)) + geom_col()
  })
  
  output$tekstplot2 <- renderText({paste("Wykres dla wszystkich imion zaczynających się na:", input$input1)})
  output$plot2 <- renderPlot({
    
    L = input$input1==substr(babynames$name, 1, nchar(input$input1))
    rok = c(1880:2015)
    ilosc=c()
    for (j in rok) {
      R = babynames[L,]$year==j
      ilosc = c(ilosc, c(sum(babynames[L,][R,]$n)))
    }
    df=data.frame(rok, ilosc)
    ggplot(df, aes(x = rok, y = ilosc)) + geom_col()
    
  })
  
  output$tekstplot3 <- renderText({paste("Wykres dla imion:", input$input3)})
  output$plot3 <- renderPlot({
    imiona = input$input3
    imiona = stringi::stri_extract_all_words(imiona)[[1]]
    rok = c(1880:2015)
    df = data.frame(rok)
    for (k in imiona) {
      N <- babynames[babynames$name == k,]
      ilosc=c()
      for (j in rok) {
        R = N$year==j
        ilosc = c(ilosc, c(sum(N[R,]$n)))
      }
      df[,k] <- ilosc
    }
    
    df2 <- melt(data = df, id.vars = "rok")
    ggplot(data = df2, aes(x = rok, y = value, colour = variable)) + geom_line()
    
  })
  
}

shinyApp(ui, server)
