library(git2rdata)
library(tidyverse)
library(googlesheets4)
library(digest)
library(shiny)

lees_vertalingen <- function(
  sheet = "1w85ll_qVEPkybDEPPovji_bmLAS5TXXFNqMRGOY0TTs",
  taal = "nederlands-frans"
) {
  read_sheet(sheet, taal) %>%
    mutate(
      hash = pmap_chr(
        list(
          type = .data$type, werkwoord = .data$werkwoord, persoon = .data$persoon,
          werkwoordstijd = .data$werkwoordstijd
        ),
        function(...) {
          sha1(list(...))
        }
      ),
      type = factor(.data$type),
      werkwoord = factor(.data$werkwoord),
      persoon = factor(.data$persoon),
      werkwoordstijd = factor(.data$werkwoordstijd)
    )
}
lees_antwoorden <- function(taal = "nederlands-frans") {
  if(!is_git2rmeta(taal)) {
    return(
      data.frame(
        hash = character(0), tijdstip = as.POSIXct(integer(0)),
        juist = integer(0), fout = character(0)
      )
    )
  }
  read_vc(taal)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    textOutput("vraag"),
    textInput("antwoord", label = "vertaling"),
    actionButton("controleer", label = "Controleer vertaling"),
    textOutput("feedback")
  )
)
kies_vraag <- function(vertalingen, antwoorden) {
  if (sum(antwoorden$juist) < 100) {
    return(sample_n(vertalingen, 1))
  }
  stop("voldoende juiste antwoorden om meer gericht te werken")
}


# Define server logic required to draw a histogram
server <- function(session, input, output) {
  data <- reactiveValues(
    taal = "nederlands-frans", vertalingen = NULL, antwoorden = NULL,
    hash = NULL, vraag = NULL, correct = NULL, feedback = NULL
  )
  observeEvent(data$taal, {
    data$antwoorden <- lees_antwoorden(taal = data$taal)
    data$vertalingen <- lees_vertalingen(taal = data$taal)
  })
  observeEvent(data$vertalingen, {
    if (is.null(data$vertalingen)) {
      return(NULL)
    }
    vraag <- kies_vraag(
      vertalingen = data$vertalingen, antwoorden = data$antwoorden
    )
    data$hash <- vraag$hash
    data$vraag <- vraag$nederlands
    data$correct <- vraag$vertaling
    output$vraag <- renderText({data$vraag})
  })
  observeEvent(input$controleer, {
    if (is.null(data$hash) || input$antwoord == "") {
      return(NULL)
    }
    if (input$antwoord == data$correct) {
      data$feedback <- isolate(
          sprintf("Correct! `%s` vertaal je als `%s`", data$vraag, data$correct)
        )
      extra <- data.frame(
        hash = data$hash, tijdstip = Sys.time(), juist = 1L, fout = ""
      )
    } else {
      data$feedback <- isolate(
        sprintf(
          "Fout! `%s` vertaal je als `%s`, je schreef `%s`", data$vraag,
          data$correct, input$antwoord
        )
      )
      extra <- data.frame(
        hash = data$hash, tijdstip = Sys.time(), juist = 0L,
        fout = input$antwoord
      )
    }
    data$antwoorden <- rbind(data$antwoorden, extra)
    write_vc(data$antwoorden, data$taal, sorting = c("hash", "tijdstip"))
    vraag <- kies_vraag(
      vertalingen = data$vertalingen, antwoorden = data$antwoorden
    )
    data$hash <- vraag$hash
    data$vraag <- vraag$nederlands
    data$correct <- vraag$vertaling
    output$vraag <- renderText({data$vraag})
    updateTextInput(session = session, inputId = "antwoord", value = "")
  })
  observeEvent(data$feedback, {
    if (is.null(data$feedback)) {
      return(NULL)
    }
    output$feedback <- renderText({data$feedback})
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
