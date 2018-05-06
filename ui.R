
library(DT)

ui <- fluidPage(
    
    # titlePanel("Open Omics Data Explorer"),
    
    sidebarLayout(
        
        sidebarPanel(

            textInput(inputId = "dataset_ID",
                      label = "Enter a dataset ID:",
                      value="GSE107871"),
            width=2
        ),
        

        mainPanel(
            
            DTOutput("pheno_view")
            
        )
    )
)
