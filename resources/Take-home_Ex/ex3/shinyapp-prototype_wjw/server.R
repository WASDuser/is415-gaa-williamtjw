function(input, output, session) {


    dataset_reactive <- reactive({
        req(input$dataset)
        data <- switch(input$dataset,
            "crime_boundary" = crime_boundary,
            "WEST_MSIA" = WEST_MSIA,
            "EAST_MSIA" = EAST_MSIA
            # "crime_district" = crime_district,
            # "pop_data" = pop_data,
            # "adm2_sf" = adm2_sf
            )
    })


    esda_dataset_reactive <- reactive({
        req(input$esda_dataset)
        data <- switch(input$esda_dataset,
            "crime_boundary" = crime_boundary,
            "WEST_MSIA" = WEST_MSIA,
            "EAST_MSIA" = EAST_MSIA)
    })


    # Preview the dataset with up to 20 rows (when "Preview" is selected)
    output$data_table <- DT::renderDataTable({
        req(input$display_option == "preview")
        req(input$dataset)

        data <- dataset_reactive()
        
        data_display_options <- list( # default display
            dom = 'Bfrtip',
            buttons = c('copy', 'excel', 'pdf'),
            
            pageLength = 20,
            scrollX = TRUE,
            scrollY = '333px',
            fixedHeader = TRUE)
        # modify edge cases of problematic displays
        # if (input$dataset == "") {
        #     data_display_options$columnDefs <- list())}
        
        DT::datatable(
            data, 
            options=data_display_options, 
            extensions = 'Buttons',
            rownames = FALSE)
    })


    # Render code output for str() or glimpse()
    output$code_output <- renderPrint({
        data <- dataset_reactive()
        
        if (input$display_option == "str") {
            str(data)
        } else if (input$display_option == "glimpse") {
            glimpse(data)
        }
    })


    # Dynamic description based on the dataset
    output$description_text <- renderText({
        switch(input$dataset,
            "crime_boundary" = "Description for crime_boundary",
            "WEST_MSIA" = "Description for WEST_MSIA",
            "EAST_MSIA" = "Description for EAST_MSIA",
            # "rate_crime_district" = "Description for rate_crime_district...",
            # "adm2_sf" = "Description for adm2_sf dataset...",
            # "crime_district" = "Description for crime_district dataset...",
            # "pop_data" = "Description for pop_data dataset..."
            )
    })


    # Update variable choices based on selected dataset
    observe({
        req(input$dataset)
        data <- dataset_reactive()
        
        # Get numeric variable names
        numeric_vars <- names(data)[sapply(data, is.numeric)]
        
        updateSelectInput(session, "variable", 
            choices = numeric_vars,
            selected = NULL) # Reset selection
    })


    # Dynamic UI for variable selection based on continuous/categorical
    output$var_select <- renderUI({
        data <- dataset_reactive()
        if (input$var_type == "cat") {
            selectInput("variable", "Choose Categorical Variable:", 
                choices = names(data)[sapply(data, is.character)])
        } else {
            selectInput("variable", "Choose Continuous Variable:", 
                choices = names(data)[sapply(data, is.numeric)])
        }
    })


    # Dynamic chart type options
    observeEvent(input$var_type, {
        updateSelectInput(session, "chart_type", choices = if (input$var_type == "cat") {
            c("Bar Chart")
        } else {
            c("Histogram", "Boxplot", "Time Series")
        })
    })


    # Render EDA plot/statistics upon submit button click 
    observeEvent(input$submit_eda, {
        req(input$submit_eda)
        output$eda_plot <- renderPlot({
            data <- dataset_reactive()
            
            if (input$chart_type == "Bar Chart") {
                ggplot(data, aes(x = .data[[input$variable]])) + geom_bar() + 
                    theme(axis.text.x = element_text(angle = 30))
                
            } else if (input$chart_type == "Histogram") {
                ggplot(data, aes(x = .data[[input$variable]])) + 
                    geom_histogram(binwidth = 5, color="black", fill="light blue")

            } else if (input$chart_type == "Boxplot") {
                ggplot(data, aes(y = .data[[input$variable]])) + geom_boxplot()
                
            } else if (input$chart_type == "Time Series") {
                ggplot(data, aes(x = "date", y = .data[[input$variable]])) + geom_line()
            }
        })})


    output$summary_table <- renderPrint({
        req(input$submit_eda)    # Ensure that the submit button has been clicked
        req(input$variable)       # Ensure that a variable has been selected
        
        data <- dataset_reactive()
        variable <- data[[input$variable]] # Get the selected variable
        summary_output <- summary(as.data.frame(variable)) # Get summary statistics
        colnames(summary_output) <- input$variable # Set the selected variable as column name
        
        summary_output
    })


    # Dynamic choro var options
    observe({
        data <- esda_dataset_reactive()
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        
        updateSelectInput(session, "esda_variable", 
            choices = numeric_cols,
            selected = numeric_cols[1]  # Set a default selection
        )
    })


    # Dynamic crime type options
    observe({
        data <- esda_dataset_reactive()
        crime_types <- unique(data$type)
        
        updateSelectInput(session, "crime_type",
            choices = crime_types,
            selected = crime_types[1])
    })


    # Dynamic crime type options - local
    observe({
        data <- esda_dataset_reactive()
        crime_types <- unique(data$type)
        
        updateSelectInput(session, "local_crime_type",
            choices = crime_types,
            selected = crime_types[1])
    })
    
    
    observe({
        data <- esda_dataset_reactive()
        crime_types <- unique(data$type)
        
        updateSelectInput(session, "global_crime_type",
            choices = crime_types,
            selected = crime_types[1])
    })


    # Dynamic region options
    observe({
        data <- esda_dataset_reactive()
        regions <- unique(data$region)
        
        updateSelectInput(session, "region",
            choices = regions,
            selected = regions[1])
    })


    observe({
        data <- esda_dataset_reactive()
        regions <- unique(data$region)
        
        updateSelectInput(session, "local_region",
            choices = regions,
            selected = regions[1])
    })
    
    
    observe({
        data <- esda_dataset_reactive()
        regions <- unique(data$region)
        
        updateSelectInput(session, "global_region",
            choices = regions,
            selected = regions[1])
    })


    # Dyanmic classification options
    observe({
        styles <- list(
            "sd" = "sd", 
            "equal" = "equal", 
            "pretty" = "pretty", 
            "quantile" = "quantile", 
            "kmeans" = "kmeans", 
            "hclust" = "hclust", 
            "bclust" = "bclust", 
            "fisher" = "fisher", 
            "jenks" = "jenks",
            "cont" = "cont",
            "order" = "order",
            "log10" = "log10") # TODO: handle log(0) error
        
        updateSelectInput(session, 'classification',
            choices = styles,
            selected = styles[1])
    })


    # Dynamic colors
    observe({
        c_palette <- list(
            "blues" = "Blues", 
            "reds" = "Reds", 
            "greens" = "Greens",
            "Yellow-Orange-Red" = "YlOrRd",
            "Yellow-Orange-Brown" = "YlOrBr",
            "Yellow-Green" = "YlGn",
            "Orange-Red" = "OrRd")
        
        updateSelectInput(session, 'colors',
            choices = c_palette,
            selected = c_palette[1])
    })


    observe({
        data <- esda_dataset_reactive()
        
        if ("date.x" %in% colnames(data)){ # hardcode
            years <- unique(year(data$date.x))
        } else{years <- unique(year(data$date))}
        
        updateSelectInput(session, 'local_time_period',
            choices = years,
            selected = years[1])
    })
    
    observe({
        data <- esda_dataset_reactive()
        
        if ("date.x" %in% colnames(data)){ # hardcode
            years <- unique(year(data$date.x))
        } else{years <- unique(year(data$date))}
        
        updateSelectInput(session, 'global_time_period',
            choices = years,
            selected = years[1])
    })


    choropleth_map <- eventReactive(input$submit_esda,{
        req(input$esda_variable)
        req(input$esda_dataset)
        req(input$crime_type)
        req(input$region)
        req(input$classification)
        req(input$n_classes)
        req(input$colors)
        
        chosen_data <- input$esda_dataset
        chosen_var <- input$esda_variable
        chosen_crime <- input$crime_type
        chosen_region <- input$region
        chosen_class <- input$classification
        chosen_n <- input$n_classes
        chosen_color <- input$colors
        
        data <- esda_dataset_reactive() %>% ungroup() %>% st_as_sf() %>%
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime)
        
        # for debugging ---------------------------------------------------
        print(chosen_data)
        print('---------------------------------------------------------------')
        print(chosen_var)
        print('---------------------------------------------------------------')
        print(chosen_crime)
        print('---------------------------------------------------------------')
        print(chosen_region)
        print('---------------------------------------------------------------')
        print(chosen_class)
        print('---------------------------------------------------------------')
        print(chosen_n)
        print('---------------------------------------------------------------')
        print(chosen_color)
        print('---------------------------------------------------------------')
        
        str(data)
        # for debugging ---------------------------------------------------
        tmap_mode('view')
        tmap_options(check.and.fix = TRUE)
        tm_shape(data) +
            tm_fill(
                col = chosen_var,
                palette = chosen_color,
                style = chosen_class,
                n = chosen_n,
            ) +
            tm_layout(
                main.title = paste(chosen_crime))
    })

    output$choro_map <- renderTmap({
        choropleth_map()
    })


    globalMIResults <- eventReactive(input$submit_global,{
        chosen_crime <- input$global_crime_type
        chosen_region <- input$global_region
        chosen_year <- input$global_time_period
        
        data <- esda_dataset_reactive() %>% ungroup() %>% st_as_sf() %>% 
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime) %>% 
            # missing data if any
            filter(!is.na(crime_rate))
        
        if ("date.x" %in% colnames(data)){ # hardcode
            data <- data %>% filter(year(data$date.x) == chosen_year)
        } else{data <- data %>% filter(year(data$date) == chosen_year)}
        
        nb <- st_contiguity(data$geometry, queen = as.logical(input$global_contiguity))
        
        # thanks to santhya~
        nb[17]<- as.integer(19) #handle case for langkawi, which is not connected to the others by admin boundary
        
        # empty nb indices
        # crime_boundary: 17, 63
        # west: 17
        # east: 20
        
        # Computing Contiguity Spatial Weights
        wm_q <- data %>%
            mutate(
                nb = nb,
                wt = st_weights(nb, style = input$global_MoranWeights))
        
        global_moran_results <- global_moran_perm(
            wm_q$crime_rate,
            wm_q$nb,
            wm_q$wt,
            nsim = as.numeric(input$global_MoranSims))
        
        return(global_moran_results)
    })


    globalHist <- eventReactive(input$submit_global,{
        global_moran_results <- globalMIResults()
        
        hist_plot <- hist(
            global_moran_results$res,
            freq=TRUE,
            breaks=10,
            xlab="Histogram of Simulated Moran's Is"
        ) 
        abline(v=0, col='red')
    })
    
    output$global_hist <- renderPlot({
        globalHist()
    })


    output$global_output <- renderDataTable({
        
        data_display_options <- list(
            pageLength = 1,
            scrollX = TRUE,
            scrollY = '333px',
            fixedHeader = TRUE)

        data <- globalMIResults() %>% as.data.frame()

        DT::datatable(
            data, 
            options=data_display_options, 
            rownames = FALSE)
    })


    #==========================================================
    # Local Measures of Spatial AutoCorrelation
    #==========================================================

    localMIResults <- eventReactive(input$MoranUpdate,{
        
        chosen_crime <- input$local_crime_type
        chosen_region <- input$local_region
        chosen_year <- input$local_time_period

        data <- esda_dataset_reactive() %>% ungroup() %>% st_as_sf() %>% 
            filter(region == chosen_region) %>% 
            filter(type == chosen_crime) %>% 
            # missing data if any
            filter(!is.na(crime_rate))

        if ("date.x" %in% colnames(data)){ # hardcode
            data <- data %>% filter(year(data$date.x) == chosen_year)
        } else{data <- data %>% filter(year(data$date) == chosen_year)}

        nb <- st_contiguity(data$geometry, queen = as.logical(input$Contiguity))
        
        # thanks to santhya~
        nb[17]<- as.integer(19) #handle case for langkawi, which is not connected to the others by admin boundary
        
        # empty nb indices
        # crime_boundary: 17, 63
        # west: 17
        # east: 20
        
        # Computing Contiguity Spatial Weights
        wm_q <- data %>%
            mutate(
                nb = nb,
                wt = st_weights(nb, style = input$MoranWeights))

        # Computing Local Moran's I

        lisa <- wm_q %>%
            mutate(
                local_moran = local_moran(
                    wm_q$crime_rate,
                    wm_q$nb,
                    wm_q$wt,
                nsim = as.numeric(input$MoranSims)),.before = 1) %>%
            unnest(local_moran)

        lisa <- lisa %>%
            rename(
                "local moran(ii)" = "ii", "expectation(eii)" = "eii",
                "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
                "p_value" = "p_ii")

        # glimpse(data)
        # View(nb)
        # View(wm_q)
        # View(lisa)
        return(lisa)
    })

    #==========================================================
    # Render output maps
    #==========================================================

    #Render local Moran I statistics
    output$LocalMoranMap <- renderTmap({
        df <- localMIResults()
        # df

        if(is.null(df) || nrow(df) == 0) return()  # Exit if no data

        # Map creation using tmap
        localMI_map <- tm_shape(df) +
            tm_fill(col = input$localmoranstats,
                style = "pretty",
                palette = "RdBu",
                title = input$localmoranstats) +
            tm_borders() +
            tm_view(set.zoom.limits = c(6, 7))

        localMI_map
    })

    #Render LISA map
    output$LISA <- renderTmap({
        df <- localMIResults()

        if(is.null(df)) return()

        lisa_sig <- df  %>%
            filter(p_value < as.numeric(input$MoranConf))

        lisamap <- tm_shape(df) +
            tm_polygons() +
            tm_borders() +

            tm_shape(lisa_sig) +
            tm_fill(col = input$LisaClass,
                palette = "-RdBu",
                title = (paste("Significance:", input$LisaClass))) +
            tm_borders(alpha = 0.4) +
            tm_view(set.zoom.limits = c(6, 7))

        lisamap
    })
}