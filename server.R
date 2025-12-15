library(shiny)
library(DT)
library(alspac)
library(dplyr)

data(current)
# data(useful)


current <- subset(current, ! name %in% c("aln", "qlet"), select=c(obj, name, lab, counts, type, cat1, cat2, cat3, cat4))
# useful <- subset(useful, ! name %in% c("aln", "qlet"), select=c(obj, name, lab, counts, type, cat1, cat2, cat3, cat4))

# dat <- rbind(current, useful)
dat <- current
dat$obj <- gsub(".dta", "", dat$obj)
names(dat) <- c("Dataset", "Variable", "Details", "Counts", "Type", "Release", "Label 1", "Label 2", "Label 3")

rownames(dat) <- NULL

labs <- table(c(
	dat$"Label 1", dat$"Label 2", dat$"Label 3"
))
labs <- labs[labs > 10] %>% sort(decreasing=TRUE)


print_rows <- function(s, dat)
{
	if(length(s) == 0)
	{
		cat("(none)")
	} else {
		cat(paste0(dat$Variable[s], ": ", dat$Details[s]), sep='\n')
	}
}

shinyServer(function(input, output, session) {

	output$variablecount <- renderUI({
		HTML("<p>There are <strong>", nrow(dat), "</strong> variables that are currently available to researchers.")
	})

	output$laba <- 	renderUI({
		HTML(sapply(names(labs),
			function(x) paste0("<button id='category", which(names(labs) == x)[1], "' action='toggle' class='btn btn-default action-button btn-xs' style='margin-bottom: 3px'>", x, " (", labs[names(labs) == x], ")</button>")) %>%
			paste(collapse=" "))
	})

	observeEvent(input$typeahead_search,
	{
		updateNavbarPage(session, "mainnav", selected="variablespage")
	})

	observeEvent(input$variablespage,
	{
		updateNavbarPage(session, "mainnav", selected="variablespage")
	})


	output$x3 = DT::renderDataTable(dat, rownames = FALSE)


	observeEvent(input$category1, {
		output$x3 = DT::renderDataTable(subset(dat, `Label 1` == "Quest"), rownames = FALSE)

	})


	output$x4 = renderPrint({
		s = input$x3_rows_selected
		# cat('These rows are currently selected:\n\n')
		print_rows(s, dat)
		output$makebutton <- renderUI(
			downloadButton('downloadData', 'Download variable list')
		)
	})


	output$downloadData <- downloadHandler(
		filename = function() {
			paste('data-', Sys.Date(), '.csv', sep='')
		},
		content = function(con) {
			index <- as.numeric(input$x3_rows_selected)
			write.csv(dat[index,], con, row.names=FALSE)
		})

})
