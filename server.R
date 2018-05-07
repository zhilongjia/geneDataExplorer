library(GEOquery)
library(dplyr)
library(DT)

#get key colnames based on having different text
key_col_filter <- function(raw_table) {
    wanted_colnames <- names(which(apply(raw_table, 2, function(x) { length(table(x)) > 1 } )))
    unwanted_colnames <- colnames(raw_table)[grep("relation|file", colnames(raw_table) )]
    
    other_colnames <- colnames(raw_table)[na.omit(pmatch(c("last_update_date", "source", "organism"), colnames(raw_table)))]
    
    # other_colnames <- intersect(c("last_update_date", "source_name", "molecule", "library_strategy"), colnames(raw_table) ) 
    
    wanted_colnames <- c(setdiff(wanted_colnames, unwanted_colnames), other_colnames)
    return(wanted_colnames)
}

# all_row_same_col
other_col_table_filter <- function(raw_table) {
    unique_value_cols <- names(which(apply(raw_table, 2, function(x) { length(table(x)) == 1 } )))
    return(raw_table[1, unique_value_cols])
}

server <- function(input, output) {
    
    phenoData <- reactive({
        raw_input <- input$dataset_ID
        
        switch(gsub("[[:digit:]]", "", raw_input),
        GSE = {
            GSEseries <- getGEO(raw_input, destdir="./.data")
                
            # GSEseries <- getGEO("GSE68939", destdir="./.data")
                
            pheno_rawTable <- pData(GSEseries[[1]])
            wanted_colnames <- key_col_filter(pheno_rawTable)

            show_Table <- dplyr::select(pheno_rawTable, wanted_colnames )
            },
            
        GPL = {
            GPL_raw <- getGEO(raw_input, destdir="./.data")
            # GPL_raw <- getGEO("GPL13607", destdir="./.data")
            show_Table <- Table(GPL_raw)
            }
              
        )
        return (show_Table)
    })
    

    output$pheno_view <- renderDT({
        datatable(phenoData(), rownames = TRUE, 
                  filter = "bottom", 
                  class = 'nowrap display',
                  extensions = c('Buttons', 'ColReorder', 'FixedColumns', 'Scroller'),
                  options = list(dom = 'frBtip', 
                                 colReorder = TRUE,
                                 buttons = list(list(extend = 'colvis'), 
                                                'pageLength',
                                                list(extend = 'csv', filename=input$dataset_ID) ),
                                 fixedHeader = TRUE,
                                 lengthMenu = c(10, 5, 25, 100),
                                 columnDefs = list(list(targets = 3:ncol(phenoData()), render = JS( "function(data, type, row, meta) {", 
                                                                                  "return type === 'display' && data.length > 50 ?", 
                                                                                  "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;", 
                                                                                  "}") )),

                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 2) ) )
    },
        options = list(autoWidth = TRUE, 
                       initComplete = I("function(settings, json) {alert('Done.');}") )
    )
    
}
