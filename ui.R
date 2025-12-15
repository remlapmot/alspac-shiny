library(shiny)
library(shinyBS)
library(shinyLP)
library(alspac)
library(shinythemes)
library(markdown)


processFile = function(filepath) {
  l <- list()
  i <- 1
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    l[[i]] <- line
    i <- i + 1
  }
  close(con)
  return(unlist(l))
}

convert_item_to_html <- function(item) {
  title <- gsub("# ", "", item[1])
  body <- markdown::markdownToHTML(
    text = paste(item[-1], collapse = "\n"),
    fragment.only = TRUE
  )
  Encoding(body) <- "UTF-8"
  body <- HTML(body)
  return(panel_div(class_type = "warning", panel_title = title, content = body))
}

parse_news <- function(path) {
  x <- processFile(path)
  item_starts <- grep("^# ", x)
  item_ends <- c(item_starts[-1] - 1, length(x))
  l <- list()
  for (i in 1:length(item_starts)) {
    l[[i]] <- convert_item_to_html(x[item_starts[i]:item_ends[i]])
  }
  return(l)
}


aboutpage <- function() {
  tabPanel(
    title = "About",
    value = "aboutpage",
    icon = icon("cog", verify_fa = FALSE), # nolint
    tags$head(
      # nolint
      includeScript(
        "https://www.googletagmanager.com/gtag/js?id=UA-53610054-3"
      ),
      includeScript("google-analytics.js")
    ),

    # Jumbotron
    div(
      class = "jumbotron",
      position = "bottom",
      h1("Search for variables collected in the ALSPAC study"),
      p(
        tags$strong("App version: "),
        paste0(
          paste(scan("version.txt", what = character())[1], collapse = " "),
          " (",
          format(as.Date(file.info("version.txt")$mtime, ), "%d %B %Y"),
          ")"
        ),
        tags$br(),
        tags$strong("Data version: "),
        as.character(packageVersion('alspac'))
      )
    ),

    # Main body
    fluidRow(
      column(
        6,
        fluidRow(column(
          12,
          panel_div(
            class_type = "primary",
            panel_title = "The ALSPAC data resource",
            content = div(
              fluidRow(
                column(
                  12,
                  tags$p(
                    "The Avon Longitudinal Study of Parents and Children (ALSPAC), also known as Children of the 90s, is a world-leading birth cohort study, charting the health of 14,500 families in the Bristol area. You can find full information about the cohort ",
                    tags$a("here", href = "https://www.bristol.ac.uk/alspac/")
                  ),
                  HTML(
                    "<button class='btn btn-default action-button' onclick=\"window.location.href='https://www.bristol.ac.uk/alspac/'\">ALSPAC webpage</button>"
                  ),
                  tags$p()
                )
              ),
              fluidRow(column(
                12,
                tags$p(
                  "For more than two decades clinic and questionnaire data has been collected from the participants."
                ),
                tags$p(
                  "This search tool is a quick and abridged version of a much more extensive data dictionary. You can find a lot more detail about these data",
                  tags$a(
                    "here",
                    href = "http://www.bristol.ac.uk/alspac/researchers/our-data/"
                  )
                ),
                HTML(
                  "<button class='btn btn-default action-button' onclick=\"window.location.href='http://www.bristol.ac.uk/alspac/researchers/our-data/'\">Main data page</button>"
                )
              ))
            )
          )
        )),

        fluidRow(column(
          12,
          panel_div(
            class_type = "primary",
            panel_title = "The variable search tool",
            content = div(
              uiOutput("variablecount"),
              tags$p(
                "Use this website to search through the variables, obtain their identifiers and the number of records with that measure"
              ),
              tags$p(
                "You can use download lists of selected variables. For direct users, this can be used to extract variables directly. Alternatively, add this list to your data request form to specify the data that you require."
              ),
              bsButton(inputId = "variablespage", label = "Search variables")
            )
          )
        )),

        fluidRow(column(
          12,
          p(
            "Source code at ",
            tags$a(
              "https://github.com/explodecomputer/alspac-shiny",
              href = "https://github.com/explodecomputer/alspac-shiny"
            )
          )
        ))
      ),
      column(
        6,
        panel_div(
          class_type = "primary",
          panel_title = "News",
          content = div(parse_news(system.file("NEWS.md", package = "alspac")))
        )
      )
    )
  )
}


variablespage <- function() {
  tabPanel(
    title = "Variables",
    value = "variablespage",
    icon = icon("search", verify_fa = FALSE),
    fluidRow(
      column(
        9,
        # fluidRow(
        # 	column(12,
        # 		h3("Categories"),
        # 		tags$div(class="labeltags text-center",
        # 			uiOutput("laba")
        # 		)
        # 	)
        # ),
        h2("Freely browse available ALSPAC data by variable name"),
        tags$br(),
        fluidRow(column(12, DT::dataTableOutput('x3')))
      ),
      column(
        3,
        fluidRow(
          column(
            12,
            h3("Instructions"),
            p(
              "Use the search box to filter on keywords or terms. Click on rows to select those variables. Click the Download button to download a csv of the variables that you have selected."
            ),
            p(
              "If there is anything you cannot find, please consult the ",
              tags$a(
                "data dictionary or variables catalog",
                href = "http://www.bristol.ac.uk/alspac/researchers/our-data/"
              ),
              ". You can also get help from the team at",
              tags$a(
                "ALSPAC-data@bristol.ac.uk",
                href = "mailto:alspac-data@bristol.ac.uk"
              ),
              "."
            ),
            p(
              strong("Note:"),
              " Sometimes the sample counts presented here will be inaccurate - consult the ",
              tags$a(
                "data dictionary",
                href = "http://www.bristol.ac.uk/alspac/researchers/our-data/"
              ),
              " data dictionary for accurate summary data for each of the variables"
            ),
            p(
              strong("Note:"),
              " High dimensional biological data including genetic variants, DNA methylation measures and gene expression measures are not included in this tool. You can find out more about sample availability ",
              tags$a(
                "here",
                href = "http://www.bristol.ac.uk/alspac/researchers/our-data/biological-resources/"
              )
            ),
            br(),
            p("These rows are currently selected:")
          )
        ),
        fluidRow(
          column(12, verbatimTextOutput('x4'), uiOutput("makebutton"))
        )
      )
    )
  )
}


shinyUI(navbarPage(
  id = "mainnav",
  title = "ALSPAC variables search tool",
  inverse = FALSE, # for diff color view
  theme = "alspac-united.css",
  # theme = shinytheme("united"),
  aboutpage(),
  variablespage()
))
