library(GEOquery)
library(dplyr)
library(DT)

#get key colnames based on having different text
key_col_filter <- function(raw_table) {
    wanted_colnames <- names(which(apply(raw_table, 2, function(x) { length(table(x)) > 1 } )))
    unwanted_colnames <- colnames(raw_table)[grep("relation|file", colnames(raw_table) )]
    
    other_colnames <- intersect(c("last_update_date", "source_name", "molecule", "library_strategy"), 
                                colnames(raw_table) ) 
    
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
                
            # GSEseries <- getGEO("GSE107871", destdir="./.data")
                
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
        datatable(phenoData(), rownames = FALSE, 
                  extensions = c('Buttons', 'ColReorder'),
                  options = list(dom = 'Bfrtip', colReorder = TRUE, 
                                 buttons = list(list(extend = 'colvis'))) )
    },
        options = list(autoWidth = TRUE,initComplete = I("function(settings, json) {alert('Done.');}") )
    )
    
}
