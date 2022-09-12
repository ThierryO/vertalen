renv::consent(TRUE)
renv::restore(prompt = FALSE)
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
          type = .data$type, werkwoord = .data$werkwoord,
          persoon = .data$persoon, werkwoordstijd = .data$werkwoordstijd
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
selectiekans_bijwerken <- function(vragen, antwoord) {
  if ((sum(antwoord$juist) < 20) || (sum(1 - antwoord$juist) < 20)) {
    return(mutate(vragen, voorspelling = 1))
  }
  vragen %>%
    inner_join(antwoord, by = "hash") %>%
    mutate(fout = 1 - .data$juist) -> dataset
  formule <- "persoon"
  if (length(levels(vragen$type)) > 1) {
    formule <- c(formule, "type")
  }
  if (length(levels(vragen$werkwoordstijd)) > 1) {
    formule <- c(formule, "werkwoordstijd")
  }
  if (length(levels(vragen$werkwoord)) > 1) {
    formule <- c(formule, "werkwoord")
  }
  formule <- sprintf(
    paste(
      "f(\n  %s, model = 'iid',",
      "  hyper = list(theta = list(prior = 'pc.prec', param = c(3, 0.01)))\n)",
      sep = "\n"
    ),
    formule
  )
  formule <- as.formula(paste("fout ~\n", paste(formule, collapse = " +\n")))
  model <- inla(formule, family = "binomial", data = dataset)
  dataset$voorspelling <- model$summary.fitted.values$mean
  dataset %>%
    distinct(across(unique(c(colnames(vragen), "voorspelling"))))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    textOutput("vraag"),
    textInput("antwoord", label = "vertaling"),
    actionButton("controleer", label = "Controleer vertaling"),
    textOutput("feedback"),
    plotOutput("inspanning")
  )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  data <- reactiveValues(
    taal = "nederlands-frans", vertalingen = NULL, antwoorden = NULL,
    hash = NULL, vraag = NULL, correct = NULL, feedback = NULL
  )
  observeEvent(data$taal, {
    data$antwoorden <- lees_antwoorden(taal = data$taal)
    data$vertalingen <- selectiekans_bijwerken(
      lees_vertalingen(taal = data$taal), data$antwoorden
    )
  })
  observeEvent(data$vertalingen, {
    if (is.null(data$vertalingen)) {
      return(NULL)
    }
    vraag <- slice_sample(
      data$vertalingen, n = 1, weight_by = .data$voorspelling
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
    if (str_squish(input$antwoord) == data$correct) {
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
    vraag <- slice_sample(
      data$vertalingen, n = 1, weight_by = .data$voorspelling
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
  observeEvent(data$antwoorden, {
    if (is.null(data$antwoorden)) {
      return(NULL)
    }
    if (nrow(data$antwoorden) %% 25 == 0) {
      data$vertalingen <- selectiekans_bijwerken(
        vragen = data$vertalingen, antwoord = data$antwoorden
      )
    }

    output$inspanning <- renderPlot(
      data$antwoorden %>%
        filter(
          difftime(Sys.time(), data$antwoorden$tijdstip, units = "days") <= 28
        ) %>%
        mutate(
          dag = trunc(.data$tijdstip, units = "days") %>%
            as.Date(),
          juist = factor(.data$juist, 0:1, c("fout", "juist"))
        ) %>%
        ggplot(aes(x = dag, fill = juist)) +
        geom_bar() +
        scale_x_date(date_labels = "%d %b") +
        theme(axis.title = element_blank()) +
        ggtitle("inspanning van de laatste vier weken")
    )
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
