lees_vertalingen <- function(
  sheet = c(
    vervoeging = "1w85ll_qVEPkybDEPPovji_bmLAS5TXXFNqMRGOY0TTs",
    woorden = "1G6NBi9FE5wJnooKL1eIQyDCqFNUmWTi-SEyXF7PlEMQ"
  ),
  taal = "nederlands-frans"
) {
  if (grepl(".+-.+", taal)) {
    read_sheet(sheet["vervoeging"], taal) |>
      transmute(
        hash = pmap_chr(
          list(
            type = .data$type, werkwoord = .data$werkwoord,
            persoon = .data$persoon, werkwoordstijd = .data$werkwoordstijd
          ),
          ~sha1(list(...))
        ),
        type = factor(.data$type), werkwoord = factor(.data$werkwoord),
        persoon = factor(.data$persoon),
        werkwoordstijd = factor(.data$werkwoordstijd),
        vraag = sprintf("Geef de vertaling van `%s`", .data$nederlands),
        antwoord = .data$vertaling
      )
  } else {
    read_sheet(sheet["woorden"], taal) |>
      transmute(
        hash = pmap_chr(
          list(type = .data$type, woord = .data$woord, kenmerk = .data$kenmerk),
          ~sha1(list(...))
        ),
        type = factor(.data$type), kenmerk = factor(.data$kenmerk),
        vraag = sprintf(
          "Geef `%s` van `%s`", .data$kenmerk, .data$woord
        ),
        antwoord = .data$waarde
      )
  }
}
lees_antwoorden <- function(taal = "nederlands-frans") {
  if (!is_git2rmeta(taal)) {
    return(
      data.frame(
        hash = character(0), tijdstip = as.POSIXct(integer(0)),
        juist = integer(0), fout = character(0)
      )
    )
  }
  read_vc(taal)
}
