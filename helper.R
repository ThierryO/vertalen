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
