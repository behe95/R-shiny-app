#load functions and define paths of reference files and data directory
library(openxlsx)
library(shinyBS)

source("./singleTraitComponent.R")
trait_file<-"./data/2020-04-02_trait_overview.xlsx"
traits <- read.xlsx(trait_file,colNames = T,rowNames = T)


traitComponent <- function(study_id) {
  fluidPage(
    beginRow(),
    beginPanel("1/2"), #start of left column
    
      beginRow(),
        beginPanel(),
          HTML(paste0('<h3 style="font-weight:bold;">',traits[study_id,"trait"],'<h3>')),
        endPanel(),
      endRow(),
    
      beginRow(),
        beginPanel(),
          HTML('<h5 style="font-weight:bold">RESUMEN</h5>'),
          HTML("[trait_summary] lorem impsu blablablablablablabl
      lblblalbalbalbalkasnasonf nasals sn slnsf o ln dsln l lans lsd lnaslsdn ksdk nasaal s lasndfkq2en a’sof aanga aso; a;ow lslkd nasnsod wneo lsd vos lnedid ls; oa’sldn o owkd aso ena’wl lsodj  jygj idken kdosl e doslkehfyv b bssdkdlslidla’slsidlakdnfifkdlsidlsido"),
        endPanel(),
      endRow(),
    
      marginTop("1"),
    
      beginRow(),
        beginPanel("1/2"),
          HTML('<h5 style="font-weight:bold">REFERENCIA</h5>'),
         HTML(paste(traits[study_id,"first_author"],' , ',traits[study_id,"publication_date"])),
        endPanel(),
          
        beginPanel("1/2"),
          HTML('<h5 style="font-weight:bold">RIESGO GENETICO</h5>'),
          HTML("[GRS]"),
        endPanel(),
      endRow(),
    
    endPanel(),#end of left column
    
    beginPanel("1/2"), #start of right column (graph)
      beginRow(),
        # h2("Genetic risk score:"),
        
        plotOutput(paste0("plot_1",traits[study_id,"id"]),height = 230),
      endRow(),
    
      beginRow(),
        beginPanel(),
          htmlOutput(paste0("text_2",traits[study_id,"id"])),
        endPanel(),
      endRow(),
    endPanel(), #end of right column (graph)
    
    
    endRow(),
    
    marginTop("1"),
    
    beginRow(),
      beginPanel(),
        actionButton(inputId = paste0("reporteCompletoTrait",traits[study_id,"id"]), label = "Reporte Completo", 
                     style="display: block;
            margin: auto;
            color: #fff;
            background: #4E1EC5;
            font-weight:bold;
            padding: 10px 20px;
            border-radius: 0px;"),
      endPanel(),
    endRow(),
    
    marginBottom("2"),
    
    endLine(),
    bsModal(paste0("modal",traits[study_id,"id"]), "", paste0("reporteCompletoTrait",traits[study_id,"id"]) , size = "large", renderUI(singleTraitComponent(study_id))),
  )
}