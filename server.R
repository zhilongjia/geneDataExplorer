library(GEOquery)
library(dplyr)
library(DT)

server <- function(input, output) {
    
    
    phenoData <- reactive({
        
        switch(gsub("[[:digit:]]", "", input$dataset_ID),
            GSE = {
                GSEseries <- getGEO(input$dataset_ID, destdir="./.data")
                
                # GSEseries <- getGEO("GSE107871", destdir="./.data")
                
                pheno_rawTable <- pData(GSEseries[[1]])
                
                #TODO: function it
                pheno_filter <- names(which(apply(pheno_rawTable, 2, function(x) { length(table(x)) > 1 } )))
                unwanted_cols <- colnames(pheno_rawTable)[grep("relation|file", colnames(pheno_rawTable) )]
                
                #TODO: function it and show it as text
                # unique_value_cols <- names(which(apply(pheno_rawTable, 2, function(x) { length(table(x)) == 1 } )))
                # pheno_rawTable[1, unique_value_cols]
                
                pheno_filter <- setdiff(pheno_filter, unwanted_cols)
                
                pheno_tidyTable <- dplyr::select(pheno_rawTable, pheno_filter )
                
                # pheno_filter <- "title|geo_accession|characteristics|last_update_date|source_name|molecule|library_strategy"
                # pheno_tidyTable <- dplyr::select(pheno_rawTable, matches(pheno_filter) )
            },
            
            GPL = {
                GPL_raw <- getGEO(input$dataset_ID, destdir="./.data")
                # GPL_raw <- getGEO("GPL13607", destdir="./.data")
                GPL_rawTable <- Table(GPL_raw)
            }
               
              )

    })
    
    
    output$pheno_view <- renderDT({
        datatable(phenoData(), rownames = FALSE, 
                  # extensions = c('Responsive', 'Buttons'), 
                  extensions = 'Buttons',
                  options = list(dom = 'Bfrtip', buttons = list(list(extend = 'colvis'))) )
    },
        options = list(autoWidth = TRUE,initComplete = I("function(settings, json) {alert('Done.');}") )
    )
    
}
