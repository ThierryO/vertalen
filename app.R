renv::consent(TRUE)
renv::restore(prompt = FALSE)
library(git2rdata)
library(tidyverse)
library(googlesheets4)
library(digest)
library(shiny)
library(INLA)
library(scales)

source("helper.R")

selectiekans_bijwerken <- function(vragen, antwoord) {
  if ((sum(antwoord$juist) < 20) || (sum(1 - antwoord$juist) < 20)) {
    vragen %>%
      left_join(antwoord, by = "hash") %>%
      group_by(
        .data$type, .data$werkwoord, .data$persoon, .data$werkwoordstijd,
        .data$nederlands, .data$vertaling, .data$hash
      ) %>%
      summarise(laatste = max(.data$tijdstip), .groups = "drop") %>%
      mutate(
        voorspelling = 1,
        laatste = replace_na(
          .data$laatste,
          min(c(.data$laatste, Sys.time()), na.rm = TRUE) - 3600 * 24
        )
      ) -> resultaat
    return(resultaat)
  }
  vragen %>%
    left_join(antwoord, by = "hash") %>%
    mutate(fout = 1 - .data$juist) -> dataset
  formule <- "persoon"
  if (length(levels(vragen$type)) > 1) {
    stop("meer dan een type")
    formule <- c(formule, "type")
  }
  if (length(levels(vragen$werkwoordstijd)) > 1) {
    stop("meer dan een werkwoordstijd")
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
  model <- inla(
    formule, family = "binomial", data = dataset,
    control.predictor = list(link = 1)
  )
  dataset$voorspelling <- model$summary.fitted.values$`0.975quant`
  dataset %>%
    group_by(
      .data$type, .data$werkwoord, .data$persoon, .data$werkwoordstijd,
      .data$nederlands, .data$vertaling, .data$hash
    ) %>%
    summarise(
      laatste = max(.data$tijdstip),
      voorspelling = max(.data$voorspelling),
      .groups = "drop"
    ) %>%
    mutate(
      laatste = replace_na(
        .data$laatste, min(.data$laatste, na.rm = TRUE) - 3600 * 24
      )
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    textOutput("vraag"),
    textInput("antwoord", label = "vertaling"),
    actionButton("controleer", label = "Controleer vertaling"),
    textOutput("feedback")
  ),
  fluidRow(
    column(plotOutput("inspanning"), width = 6),
    column(plotOutput("inspanning_detail"), width = 6)
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
    data$vertalingen %>%
      slice_min(.data$laatste, n = -5) %>%
      slice_sample(n = 1, weight_by = .data$voorspelling) -> vraag
    data$hash <- vraag$hash
    data$vraag <- vraag$nederlands
    data$correct <- vraag$vertaling
    output$vraag <- renderText({data$vraag})
  })
  observeEvent(input$controleer, {
    if (is.null(data$hash) || input$antwoord == "") {
      return(NULL)
    }
    nu <- Sys.time()
    data$vertalingen$laatste[data$vertalingen$hash == data$hash] <- nu
    if (str_squish(input$antwoord) == data$correct) {
      data$feedback <- isolate(
          sprintf("Correct! `%s` vertaal je als `%s`", data$vraag, data$correct)
        )
      extra <- data.frame(
        hash = data$hash, tijdstip = nu, juist = 1L, fout = ""
      )
    } else {
      data$feedback <- isolate(
        sprintf(
          "Fout! `%s` vertaal je als `%s`, je schreef `%s`", data$vraag,
          data$correct, input$antwoord
        )
      )
      extra <- data.frame(
        hash = data$hash, tijdstip = nu, juist = 0L, fout = input$antwoord
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
    if (nrow(data$antwoorden) %% 10 == 0) {
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

    output$inspanning_detail <- renderPlot(
      data$antwoorden %>%
        inner_join(data$vertalingen, by = "hash") %>%
        mutate(
          dag = round.POSIXt(.data$tijdstip, units = "day") %>%
            as.Date()
        ) %>%
        group_by(.data$werkwoord, .data$dag) %>%
        summarise(aantal = n(), juist = mean(.data$juist), .groups = "drop") %>%
        ggplot(aes(x = dag, y = juist, size = aantal)) + geom_point() +
        facet_wrap(~werkwoord) +
        scale_size_area()
    )
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
