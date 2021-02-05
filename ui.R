library(shiny)
library(shinyBS)
library(shinyjs)
library(V8)


source("./uifunctions.R")

source("./traitComponent.R")

#load functions and define paths of reference files and data directory
library(openxlsx)
trait_file<-"./data/2020-04-02_trait_overview.xlsx"
traits <- read.xlsx(trait_file,rowNames=F)
rownames(traits) <- traits[,"study_id"]
traits<-traits[!is.na(traits[,"omit"]) & !traits[,"omit"],]




#traits to omit ad-hoc (because they don't work or because they are actively selected against)
traits<-traits[!traits[,"omit"],]


#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")

#Define the trait groups
trait_groups<-c("all","disease","biometrics","biomarker","response","other")
names(trait_groups)<-c("All","Disease","Biometrics","Biomarker","Response","Other")


#create overview of what is selectable in the UI - 
#note there wil be one object per trait group (the 6 groups defined above), 
#and each of the will be duplicated into newest and all
selections_all<-traits[,"study_id"]
names(selections_all)<-traits[,"trait"]

selections_disease<-traits[traits[,"disease"],"study_id"]
names(selections_disease)<-traits[traits[,"disease"],"trait"]

selections_biometrics<-traits[traits[,"biometrics"],"study_id"]
names(selections_biometrics)<-traits[traits[,"biometrics"],"trait"]

selections_biomarker<-traits[traits[,"biomarker"],"study_id"]
names(selections_biomarker)<-traits[traits[,"biomarker"],"trait"]

selections_response<-traits[traits[,"response"],"study_id"]
names(selections_response)<-traits[traits[,"response"],"trait"]

selections_other<-traits[traits[,"other"],"study_id"]
names(selections_other)<-traits[traits[,"other"],"trait"]

selections_all_newest<-traits[traits[,"most_recent"],"study_id"]
names(selections_all_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"most_recent"],"trait"])



selections_disease_newest<-traits[traits[,"disease"] & traits[,"most_recent"],"study_id"]
names(selections_disease_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"disease"] & traits[,"most_recent"],"trait"])

selections_biometrics_newest<-traits[traits[,"biometrics"] & traits[,"most_recent"],"study_id"]
names(selections_biometrics_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"biometrics"] & traits[,"most_recent"],"trait"])

selections_biomarker_newest<-traits[traits[,"biomarker"] & traits[,"most_recent"],"study_id"]
names(selections_biomarker_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"biomarker"] & traits[,"most_recent"],"trait"])

selections_response_newest<-traits[traits[,"response"] & traits[,"most_recent"],"study_id"]
names(selections_response_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"response"] & traits[,"most_recent"],"trait"])

selections_other_newest<-traits[traits[,"other"] & traits[,"most_recent"],"study_id"]
names(selections_other_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"other"] & traits[,"most_recent"],"trait"])

jsCode <- "shinyjs.scrolltop = function() {window.scrollTo(0, 0)};" 

ui <- bootstrapPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("scrolltop")),
  head(),
  navigation(),
  
  beginPage(),
  
  
  
  marginTop(),
  
  #checkboxInput("advanced", label ="Advanced options", value = FALSE),
  actionButton("advanced",label = "SORTEAR",style="display: block;
            margin-left: auto;
            color: #fff;
            background: #4E1EC5;
            font-weight:bold;
            padding: 10px 20px;
            border-radius: 0px;
            position: fixed;
            top: 5px;
            z-index: 2000;
            right: 70px;"),
  conditionalPanel(
    condition = "input.advanced%2==1",
    checkboxInput("only_show_newest", label ="Only show newest study", value = TRUE),
    radioButtons("trait_group", "Trait categories:", trait_groups, selected = "disease"),
    radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "automatic", inline = FALSE,width = NULL),
    checkboxInput("use_all_snp_score", label ="Show all-SNP score if possible (experimental)", value = TRUE),
    checkboxInput("plot_heritability", label ="Plot variability explained", value = TRUE),
    checkboxInput("real_dist", label ="Plot user distribution (experimental)", value = FALSE)
    
  ),
  
  
  uiOutput("traitComponent"),
  
  marginTop(),
  

  
  beginRow(),
  beginPanel(),
  HTML("<div style='display:flex;justify-content:center;align-items:center'>"),
  actionButton("prevBtn","Previous Page",style="display: block;
            margin: auto;
            color: #fff;
            background: #4E1EC5;
            font-weight:bold;
            padding: 10px 20px;
            border-radius: 0px;"),
  
  htmlOutput("paginationNumber"),
  
  actionButton("nextBtn","Next Page",style="display: block;
            margin: auto;
            color: #fff;
            background: #4E1EC5;
            font-weight:bold;
            padding: 10px 20px;
            border-radius: 0px;"),
  HTML("</div>"),
  endPanel(),
  endRow(),
  
  marginBottom(),
  
  
  
  endPage()
)



