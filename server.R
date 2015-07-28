### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0G

Sys.setenv(TZ="GMT")

shinyServer(function(input, output) {

	#read in data
	datasetInput <- reactive({
	    inFile <- input$files
	    loc_types <- sapply(strsplit(inFile, ' '),'[[',1)
	    mid_input <- input$mid
		dta <- all[mid==mid_input & stove_loc %in% loc_types]
	})

	datasetName <- reactive({
	    inFile <- input$files
    	if (is.null(inFile)){return(NULL)} 
		inFile$name[1]
	})

	data_cleaned <- reactive({
		if (is.null(datasetInput())) return(NULL)
		data_d <- datasetInput()[,with=F]
	})

	####################
	##### datasets ##### 
	####################
	dataXTS.plainplot <- reactive({
		dta<-data_cleaned()
		dta.wide <- dcast.data.table(dta, datetime~stove_loc+buffer+device_id, value.var='temp')
		cols <- colnames(dta.wide)[colnames(dta.wide)!='datetime']
		dta.wide <- dta.wide[, lapply(.SD, as.numeric), by=datetime]
		cols <- colnames(dta.wide)[colnames(dta.wide)!='datetime']
		as.xts(dta.wide[,c(cols), with=F], order.by=dta.wide$datetime, tz="GMT")
	})

	####################
	##### dygraphs ##### interactivity - to subset data to user-selected range
	####################
	#threshold plots
	from <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[1]])), "%Y-%m-%d %H:%M:%S"))
	})
  
	to <- reactive({
		if (!is.null(input$thresholdPlot_date_window))
		ymd_hms(strftime(gsub(".000Z","",gsub("T"," ",input$thresholdPlot_date_window[[2]])), "%Y-%m-%d %H:%M:%S"))
	})  
	output$from <- renderText({from()})
  	output$to <- renderText({to()})
	output$graphTitle <- renderText({paste("Time Series Graph:", paste(from(), to(), sep=" to ")," ")}) 

	#UI OUTPUTS
	fileMin <- reactive({data_cleaned()[,min(temp)]})

	fileMax <- reactive({data_cleaned()[,max(temp)]})

	fileSamplingInterval <- reactive({as.numeric(data_cleaned()[10,'datetime',with=F]-data_cleaned()[9,'datetime',with=F])})

	output$selectMID <- renderUI({
		selectInput('mid', "Maternal ID", all[!is.na(mid),unique(mid)])
	})

	output$selectFiles <- renderUI({
    	if(is.null(input$mid)) return()
	    files <- all[mid==input$mid, paste(unique(stove_loc), " (", unique(paste(as.Date(min(datetime)), as.Date(max(datetime)), sep=" to ")), ")", sep="")]
	    checkboxGroupInput("files", "Choose stoves", choices  = files, selected = files)
  	})

	####################
	####### Boxes ###### 
	####################
	# Overview Page 	
	output$maxTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		maxTemp <- data_cleaned()[,max(temp)]
		maxThreshold <- 85
		deviceIDs <- data_cleaned()[temp>maxThreshold, unique(device_id)]
		infoBox(
			value = if (maxTemp >= maxThreshold) paste(deviceIDs, collapse=", ") else formatC(maxTemp, digits = 2, format = "f"),
			title = if (maxTemp >= maxThreshold) "Warning: 85C Exceeded" else "Max Temp",
			icon = if (maxTemp >= maxThreshold)icon("warning") else icon("chevron-circle-up"),
			color = if (maxTemp >= maxThreshold) "red" else "green"
		)
	})

	output$minTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		minTemp <- data_cleaned()[,min(temp)]
		minThreshold <- 0
		deviceIDs <- data_cleaned()[temp<=minThreshold,unique(device_id)]
		infoBox(
			value = if (minTemp <= minThreshold) paste(deviceIDs, collapse=", ") else formatC(minTemp, digits = 2, format = "f"),
			title = if (minTemp <= minThreshold) "Warning: Values Below 0" else "Min Temp",
			icon = if (minTemp <= minThreshold) icon("warning") else icon("chevron-circle-down"),
			color = if (minTemp <= minThreshold) "red" else "green"
		)
	})

	####################
	###### Tables ###### 
	####################
	output$allDataTable<-renderPrint({
		orig <- options(width = 1000)
		print(melt(data_cleaned(),  id.var=c('datetime','device_id', 'stove_loc', 'buffer')), 1000)
		options(orig)
	})

	diagnostics <- reactive({
		datasetInput()[, list(
			Start_Date =as.Date(min(datetime)),
			End_Date = as.Date(max(datetime)),
			Max_Temp = max(temp),
			Min_Temp = min(temp)
		), by='device_id,stove_loc,buffer']
	})

	output$diagnosticsOutput <- renderDataTable(diagnostics(), options=list(searchable = FALSE, searching = FALSE, pageLength = 20,rowCallback = I('function(row, data) {
			if (data[5] >= 85) 
				$("td", row).css({"background" : "red", "color" : "white"});
			else if (data[6] < 0) 
				$("td", row).css({"background" : "blue", "color" : "white"});
			;}')))
	# ,columnDefs = list(list(width = '100px', targets = c(0:4)))

	####################
	####### PLOTS ###### 
	####################
	output$plainPlot<- 
	renderDygraph({
		# if (is.null(datasetInput())) return(NULL)
		dygraph(dataXTS.plainplot()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T, strokeWidth=1, connectSeparatedPoints=T) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "Date & Time") %>%
        dyLegend(labelsDiv = "legendARY")
	})

	##########################
	####### DL HANDLERS ###### 
	##########################
	output$downloadCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.cleaned.csv', sep='') },
		content = function(file) {
			write.csv(melt(data_cleaned(), id.var=c('datetime','device_id')), file, row.names=F)
		}
	)

	output$downloadThresholdCSV <- downloadHandler(
		filename = function() {paste(datasetName(), '.threshold.output.csv', sep='') },
		content = function(file) {
			write.csv(thresholdData(), file, row.names=F)
		}
	)	
})