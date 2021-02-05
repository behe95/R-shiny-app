library("shiny")


#load functions and define paths of reference files and data directory
source("./functions.R")
dataFolder<-"./data/"
snps_file<-"./data/2020-04-02_snp_weights.rdata"
trait_file<-"./data/2020-04-02_trait_overview.xlsx"
all_snp_trait_file <- "./prs/2019-03-05_study_list.xlsx"



#defining 1000 genomes populations
ethnicities_labels<-c("Automatic guess","global","African","Ad Mixed American","East Asian","European","South Asian")
names(ethnicities_labels)<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")


#establish ordinal number ending
ordinals<-rep("th",1,100)
ordinals[c(1,seq(21,91,10))] <- "st"
ordinals[c(2,seq(22,92,10))] <- "nd"
ordinals[c(3,seq(23,93,10))] <- "rd"


#preload data
library(openxlsx)
load(snps_file)
traits <- read.xlsx(trait_file,colNames = T)
all_snp_traits<-read.xlsx(all_snp_trait_file,rowNames=T)

rc_traits <- read.xlsx(trait_file,colNames = T,rowNames = T)


server <- function(input, output) {
  
 
  
  
  
  #The main data gathereing function, defined as reactive because it's used in several different calls
  get_data <- function(x_study_id){
    
    
    
    
    #initial UI data gathering and user-check
    
    if(input$only_show_newest){
      ui_selector <- paste0("trait_",input$trait_group,"_newest")
    }else{
      ui_selector <- paste0("trait_",input$trait_group)
    }
    study_id<- x_study_id #input[[ui_selector]]
    #print(study_id)
    
    
    uniqueID<- "id_6N2f72J26" #gsub(" ","",input$uniqueID)
    ethnicity_group<-input$ethnicity_group
    use_all_snp_score <- input$use_all_snp_score
    plot_heritability <- input$plot_heritability
    real_dist<-input$real_dist
    
    #If trait is not available in all_snp_traits we override the use_all_snp_score and set to FALSE
    #(this would be the most frequent case actually. For now)
    if(!study_id %in% rownames(all_snp_traits)){
      use_all_snp_score <- FALSE
    }else{
      if(is.na(all_snp_traits[study_id,"file_to_read"])){
        use_all_snp_score <- FALSE
      }
    }
    
    
    #If use_all_snp_score is TRUE real_dist must be TRUE as well, so override that
    if(use_all_snp_score){
      real_dist<-TRUE
    }
    
    
    #print(paste(dataFolder,uniqueID,sep=""))
    
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError(paste("Did not find a user with this id",uniqueID)))
    }
    
    
    
    
    
    
    
    #getting the relevant trait name, pmid and SNPs to analyze
    trait<-rc_traits[study_id,"trait"]
    pmid<-rc_traits[study_id,"pmid"]
    if(!pmid%in%data[,"pmid"])stop(paste("PMID",pmid,"was not found in system"))
    SNPs_to_analyze<-data[data[,"study_id"]%in%study_id ,]
    
    #setting up back-ground frequency sources
    #The default behavior is to try to guess ethnicity. If this fails it should revert to 'global' distribution but prepare to make a note of it in the methods.
    if(ethnicity_group == "automatic"){
      json_path<-paste0(dataFolder,uniqueID,"/",uniqueID,"_data.json")
      if(!file.exists(json_path))stop(safeError("Json file not found (So cannot do automatic reference population guess)"))
      library(jsonlite)
      d1<-fromJSON(json_path)
      e<-try(d1[["ethnicity"]][["guessed_super_pop"]],silent=F)
      if(is.null(e) || is.na(e) ||  !e %in% c("AFR", "AMR", "EAS", "EUR", "SAS")){
      # ethnicity_explanation_text<-paste0(ethnicity_explanation_text," This was done because we could not automatically guess your ethnicity, but you can use the advanced tab to set it yourself.")
        ethnicity_group<-"global"
      }else{
        # ethnicity_explanation_text<-paste0(ethnicity_explanation_text," This was done based on an automated guess.")
        ethnicity_group <- e
      }
    }
    ethnicity_group<-"global"
    if(ethnicity_group == "global"){
      #do nothing. Note the density curve location.
      densityCurvePath<-"./data/2020-04-17_densities_ALL.rdata"
    }else{
      #then replace the MAF with the correct superpopulation group
      SNPs_to_analyze[,"minor_allele_freq"] <- SNPs_to_analyze[,paste0(ethnicity_group,"_AF")]
      #note the density curve location
      densityCurvePath<-paste0("./data/2020-04-17_densities_",ethnicity_group,".rdata")
    }
    
    
    
    
    
    #gathering some background info for the study		
    link<-paste0("www.ncbi.nlm.nih.gov/pubmed/",traits[study_id,"pmid"])
    author<-traits[study_id,"first_author"]
    sampleSize<-traits[study_id,"sampleSize"]
    publication_date<-traits[study_id,"publication_date"]
    textToReturn <- paste0("Retrieved ",nrow(SNPs_to_analyze)," SNPs from <u><a target='_blank' href='http://",link,"'>",author," et al (PMID ",pmid,")</a></u>, which were reported to be associated with ",tolower(trait),".")
    textToReturn <- paste0(textToReturn," This study reports a total sample size of ",sampleSize,", as entered on date ",publication_date,".")
    
    
    
    #if any of the SNPs are duplicated we have to merge them (this by the way is an odd situation
    #why would a SNP be listed twice in the results for the same trait - let's aim to merge only in GRS
    #but list all in table for the sake of transparency)
    if(any(duplicated(SNPs_to_analyze[,"SNP"]))){
      #Handle the duplications and make a proper warning
      duplicates<-sort(unique(SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"SNP"]))
      warnForDiscrepancyInBeta<-FALSE
      for(duplicate in duplicates){
        s1<-SNPs_to_analyze[SNPs_to_analyze[,"SNP"]%in%duplicate,]
        if(
          length(unique(s1[,"effect_allele"]))!=1|
          length(unique(s1[,"non_effect_allele"]))!=1|
          length(unique(s1[,"effect_size"]))!=1){
          warnForDiscrepancyInBeta<-TRUE
        }
      }
      duplicates_example<-paste(duplicates[1:min(c(5,length(duplicates)))],collapse=", ")			
      if(warnForDiscrepancyInBeta){
        textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNP(s) were entered twice for this GWAS, and the effect-size and direction was <b>not consistent</b>. The beta from the first listed SNP was used, but please cross-check the results table with the original study carefully for details (e.g. ",duplicates_example,"). Duplicates are indicated in the end of table but don't contribute to results.")
      }else{
        textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNP(s) were entered twice for this GWAS, but the effect-size and direction was consistent (e.g. ",duplicates_example,"). Duplicates are indicated in the end of table but don't contribute to results.")
      }
      
      #then we handle it so that the extra lines are removed and inserted in the end instead
      SNPs_to_analyze_duplicates<-SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),]
      rownames(SNPs_to_analyze_duplicates) <- paste0("duplicate_",1:nrow(SNPs_to_analyze_duplicates))
      for(col in c("genotype","personal_score","population_score_average","population_score_sd","score_diff")){
        SNPs_to_analyze_duplicates[,col]<-NA
      }
      SNPs_to_analyze<-SNPs_to_analyze[!duplicated(SNPs_to_analyze[,"SNP"]),]
    }else{
      SNPs_to_analyze_duplicates<-SNPs_to_analyze[0,]
    }
    
    
    rownames(SNPs_to_analyze)<-SNPs_to_analyze[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze, namingLabel="cached.all_gwas")
    SNPs_to_analyze[,"genotype"] <- genotypes[rownames(SNPs_to_analyze),"genotype"]
    SNPs_to_analyze <-get_GRS_2(SNPs_to_analyze,mean_scale=T, unit_variance=T)
    population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
    if(population_sum_sd == 0)stop(safeError("For some reason we couldn't analyse this particular trait from your genomic data."))
    
    GRS <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
    
    
    
    
    #check for question marks in risk-allele
    c1<-apply(SNPs_to_analyze[,c("minor_allele","major_allele","effect_allele","non_effect_allele")]=="?",1,sum)
    if(sum(c1>0) & !use_all_snp_score){
      textToReturn <- paste0(textToReturn," Also note that ",sum(c1>0)," SNP(s) had missing or discrepant allele information, meaning that risk-allele or minor/major allele could not be correctly assigned. This is indicated with a '?' in the results table and causes the SNP to be omitted from the GRS-calculation. This is likely due to strand-reporting issues, and may be fixable by checking the original study.")
    }
    
    
    
    
    #add the overall population SD value
    if(!use_all_snp_score){
      textToReturn <- paste0(textToReturn,"<br><br>For you, we calculated an ethnicity-corrected trait Z-score of ",signif(GRS,2),".")  
    }
    
    
    
    #add the final summary 
    percentage<-floor(pnorm(GRS,mean=0,sd=1)*100)
    if(percentage < 20){
      summary <- " This is a lower score than the average person."
    }else if(percentage > 90){
      summary <- " This is a higher score than the average person. <br><br>But keep in mind that additional calculation is necessary to determine a real life-time risk. For example having a very high genetic score for something that is not very heritable may make very little difference. These additional calculations typically require further studies, not always available. If you would like someone to discuss these findings with we recommend <u><a href='https://www.greygenetics.com/'>greygenetics.com</a></u>."
      summary <- paste0(summary,"<br><br><div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'><p>PLEASE NOTE:<br>The information provided in this module:<br>1) CANNOT definitively tell you whether or not you currently have - or whether you will develop - any of the traits for which information is provided.<br>2) is about RISK for the conditions listed. There are genetic variations that may contribute to risk that are NOT included in the scores provided. Remember that behavior, experience, and lifestyle factors may contribute to your risk, and these are not included in the scores provided.<br></div>")
    }else{
      summary <- " This is a fairly average score."
    }
    
    
    
    #return a read-out
    if(!use_all_snp_score){
      if(percentage > 50){
        textToReturn <- paste0(textToReturn," This means that your genetic risk score for <b>this trait is higher than ",percentage,"% of and lower than ",100-percentage,"% of the general population</b>.",summary)
      }else{
        textToReturn <- paste0(textToReturn," This means that your genetic risk score for <b>this trait is lower than ",100-percentage,"% of and higher than ",percentage,"% of the general population</b>.",summary)
      }
    }
    
    
    
    #add that breast cancer PRS is no substitute for a BRCA sequencing
    if(length(grep("breast_cancer_",study_id))>0){
      textToReturn<-paste0(textToReturn," Note that this breast cancer PRS <b>is no substitute for a BRCA-gene sequencing</b>. It only includes info from common variation, and in breast cancer there are particularly strong nonsense-mutations in the BRCA-genes that have high impact on risk, but are not detectable with microarray data.")
    }
    
    #write the methods text for GWAS-significant hits
    methodsToReturn<-paste0("<small><br><b>Methods</b><br>The polygenic risk score is calculated by combining your genotype data with trait association data from ",author," et al (PMID ",pmid,"). This is done by counting how many risk-alleles you have for each SNP (column <i>'Your genotype'</i>) and multiplying that count by the reported effect-size of the SNP (column <i>'Effect Size'</i>). This gives a SNP-score for each row (column <i>'SNP-score'</i>). The SNP-score is then centered so that the <i>average</i> score in the general population would be zero (column <i>'SNP-score population normalized'</i>). All of these population normalized values are then summarized to get an overall score. This sum is further scaled so that its standard-deviation in the general population would be equal to 1 ('unit-variance'). This effectively makes it a <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-score</a></u>. The scaling and centering is based on the minor-allele frequencies (MAF) taken from the 1000 genomes project, using the ",ethnicities_labels[ethnicity_group]," frequency distribution. This gives an ethnicity-specific standard deviation of ",signif(population_sum_sd,2),". If you summarize the population normalized SNP-score column and divide by this number, you too will obtain the reported Z-score of ",signif(GRS,2),", illustrating how your score is a combination of many SNPs working together. Further details of all calculations can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/03c51c63b262f600d509469e361db35bd2a8a5fb/functions.R#L1295-L1455'>source code</a></u>. 

                            <br><br>The advantage of this approach is that it only requires a list of GWAS-significant SNPs, their frequency, effect-size and effect-alleles.  This makes it possible to implement the calculation systematically for many diseases and traits. 
                            
                            <br><br>One weakness in this approach is that it assumes that individuals from the 1000 genomes project are a reasonably normal reference group. For some traits or diseases this may not be true. As an alternative, you can select the <i>'plot user distribution'</i> option in the advanced options sections. This will overlay the plot with distribution of all ethnicity-matched impute.me users. The weakness of that approach, however, is the assumption that most users of impute.me are reasonably normal. Another potential issue is that in some cases the term genetic <i>risk</i> score may be unclear. For example in the case of GWAS of biological quantities where it is not clear if higher values are <i>more</i> or <i>less</i> risk-related, e.g. HDL-cholesterol or vitamin-levels. In most cases, higher score means high level - but it is recommended to consult with the original GWAS publication if there is any doubt. Thirdly, it is important to note that many of these scores only explain very small proportions of the overall risk. How much is illustrated in the blue pie chart below the bell curve. The more dark blue, the more predictive the score is. In the <u><a href='https://www.impute.me/prsExplainer'>PRS-explanatory module</a></u> you can further explore what this predivtiveness means in a sandbox setting.
                            
                            <br><br>Finally, instead of scrolling through all the alphabetical entries here, then check out the <u><a href='https://www.impute.me/diseaseNetwork/'>Precision-medicine module</a></u>. The data in that module is based on the calculations made here, but the information is instead given as a view of scores relevant only to a specific disease-scope. The intention is to give relevant information for a given context, while avoiding risk-sorted lists bound to produce spurious and wrongful observations (see <u><a href='https://github.com/lassefolkersen/impute-me/issues/8'>discussion</a></u> here).</small>")		
    
    
    
    #add in the (damn) duplicates
    SNPs_to_analyze<-rbind(SNPs_to_analyze,SNPs_to_analyze_duplicates)
    
    
    #if asked for distribution then get the distribution
    if(real_dist & !use_all_snp_score){
      load(densityCurvePath)
      if(!paste0(study_id,"_y") %in% rownames(densities))stop(safeError(paste("This",study_id,"trait was not found to have density-plotting available")))
      
      distributionCurve<-list(x=densities[paste0(study_id,"_x"),],y=densities[paste0(study_id,"_y"),])
    }else{
      distributionCurve<-NULL
    }
    
    
    
    #if asked for all-SNP override then try to get that instead, modify methods text and 
    #SNP count as relevant, but keep main table with top SNPs to illustrate
    if(use_all_snp_score){
      
      
      if(!study_id %in% rownames(all_snp_traits))stop(safeError("All SNP trait data not available for this study"))
      
      file_to_read <- all_snp_traits[study_id,"file_to_read"]
      
      #re-read json for robustness (can be optimized later)
      if(!exists("d1")){
        json_path<-paste0(dataFolder,uniqueID,"/",uniqueID,"_data.json")
        if(!file.exists(json_path))stop(safeError("Json file not found (So cannot do automatic reference population guess)"))
        library(jsonlite)
        d1<-fromJSON(json_path)
      }
      
      #check and load prs
      if(!"prs"%in%names(d1))stop(safeError("No all-SNP scores were available for this sample. It was probably uploaded before implementation."))
      d2<-d1[["prs"]]
      
      #check and load study
      if(!file_to_read%in%names(d2))stop(safeError("No all-SNP scores were available for this study for this sample. It was probably uploaded before implementation."))
      d3<-d2[[file_to_read]]
      
      
      #check and react to alleles_checked
      alleles_checked <- d3[["alleles_checked"]]
      qc_limit <- 300000 # number determined in QC to fix the really bad outliers (it's a compeltely separate top below this, and there's none in-between)
      if(alleles_checked < qc_limit){ 
        stop(safeError("We detected a problem with the number of SNPs used in this all-SNP PRS score. It is available in your json file, but will not displayed here due to calculation-quality concerns. Switch off the 'show all-SNP score' option to revert to basic score plotting."))
      }
      
      
      #replace GRS
      GRS <- d3[["SCORESUM_PLINK_1_9"]] 
      
      #replace SNP count
      new_snp_count <- d3[["alleles_observed"]]
      textToReturn<-sub("Retrieved [0-9]+ SNPs from",paste("Retrieved",new_snp_count,"SNPs from"),textToReturn)
      
      #replace the distribution curves
      if(ethnicity_group == "global"){
        #do nothing. Note the density curve location.
        densityCurvePath<-"./prs/2019-09-17_densities_ALL.rdata"
      }else{
        #note the density curve location
        densityCurvePath<-paste0("./prs/2019-09-17_densities_",ethnicity_group,".rdata")
      }
      
      #write the methods text for all-SNP scores
      methodsToReturn<-paste0("<small><br><b>Methods</b><br>The all-SNP polygenic risk score is calculated by combining your genotype data with complete trait association data from <u><a href='",link,"'>",author," et al</a></u>. This is done by counting how many risk-alleles you have for each SNP (column <i>'Your genotype'</i>) and multiplying that count by the reported effect-size of the SNP (column <i>'Effect Size'</i>). This gives a SNP-score for each row (column <i>'SNP-score'</i>). Note that the table only shows the most significant SNPs as an example, even though a total of ",new_snp_count," SNPs are used in the calculation. This is done according to the <u><a href='https://www.cog-genomics.org/plink2'>plink</a></u> <i>score</i> method. For missing SNPs, the average frequency for ",ethnicities_labels[ethnicity_group]," ethnicity is used, based on data from the 1000 genomes project.  Further details of all calculations can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/03c51c63b262f600d509469e361db35bd2a8a5fb/prs/export_script.R#L96-L97'>source code</a></u>. 
                              <br><br>The all-SNP polygenic risk score is still a semi-experimental functionality of impute.me. You can switch it off by un-selecting <i>'Show all-SNP score'</i> in advanced options. The main advantage is that explains more of the risk variation than the default score types that are based only on GWAS-significant 'top' SNPs. You can see the difference as how much dark-blue there is in the bar-plot of variability explained. The difference can also be further explored in the sandbox <u><a href='https://www.impute.me/prsExplainer'>PRS-explanatory module</a></u>.
                              <br><br>A main disadvantage of the all-SNP score is that it is difficult to implement it systematically for many diseases and traits, which is the reason it is only available for few selected studies. This is likely to change in the future as more and more studies release their full summary-stats without access-conditions. Check this <u><a href='https://github.com/lassefolkersen/impute-me/issues/9'>github issue</a></u> for further perspectives. Remaining disadvantages mainly relate to how we implement the calculations. For example, the current implementation only have the option to compare to previous ethnicity-matched users of impute.me. This is something we are working actively on.</small>")		
      
      
      
      
      
      #get density curve      
      load(densityCurvePath)
      if(!paste0(file_to_read,"_y") %in% rownames(densities))stop(safeError(paste("This",study_id,"trait was not found to have density-plotting available")))
      distributionCurve<-list(x=densities[paste0(file_to_read,"_x"),],y=densities[paste0(file_to_read,"_y"),])
      
      
      #scale to zero-mean and unit-variance
      x <-distributionCurve[["x"]]
      y <- distributionCurve[["y"]]
      mean <- x[order(y,decreasing=T)[1]]
      proportion <- cumsum(y[order(y,decreasing=T)]) / sum(y)
      one_sd_range_index<-range(as.numeric(names(proportion[proportion < 0.6827])))
      sd <- mean(abs(x[one_sd_range_index] - mean))
      
      #execute scaling
      GRS <- (GRS - mean) / sd
      distributionCurve[["x"]] <- (distributionCurve[["x"]] - mean) / sd
      
      
      #Insert summary score text
      textToReturn <- paste0(textToReturn," For you, we calculated an all-SNP polygenic risk score of ",signif(GRS,2),". You can compare this score with that of other users in above plot. The table below shows the strongest of the SNPs to illustrate the polygenic nature of the trait.")  
      
    }
    
    
    #write the score to the log file
    log_function<-function(uniqueID,study_id,genotypes){
      user_log_file<-paste("./data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"AllDisease",uniqueID,study_id,GRS,ethnicity_group,use_all_snp_score,real_dist,plot_heritability)
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID,study_id,genotypes))
    
    #then return the overall list		
    return(list(
      SNPs_to_analyze=SNPs_to_analyze,
      textToReturn=textToReturn,
      methodsToReturn=methodsToReturn,
      GRS=GRS,
      distributionCurve=distributionCurve,
      study_id=study_id,
      use_all_snp_score=use_all_snp_score,
      ethnicity_group=ethnicity_group))
  }
  
  
  
  current_page<-reactiveValues(number = 1)
  show_total_data<-reactiveValues(number = 20) 
  
  observeEvent(input$nextBtn, 
               {
                 current_page$number <<- current_page$number + 1
                 if (current_page$number < 1) {
                   current_page$number <<- 1
                 }
               })
  
  observeEvent(input$prevBtn, 
               {
                 current_page$number <<- current_page$number - 1
               })
 
  
  
  
  output$traitComponent<-renderUI({
    
    lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(traitName){
      traitComponent(traitName)
    })
    
    
  })
  
  
  observe(lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(study_id){
   output[[paste0("plot_1",rc_traits[study_id,"id"])]] <- renderPlot({
      
      #print("XXXXXXXXXXXXXXXXXXXds")
      
      o<-get_data(study_id)
      #print("ALSHJDLKASDLKASJLDKJASL")
      #print(o)
      SNPs_to_analyze<-o[["SNPs_to_analyze"]]
      GRS_beta<-o[["GRS"]]
      distributionCurve <- o[["distributionCurve"]]
      use_all_snp_score <- o[["use_all_snp_score"]]
      
      if(is.na(GRS_beta))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
      
      
      
      #plotting for top-hit scores
      if(!use_all_snp_score){
        
        #get xlim, ylim and x and y
        #since scores are mean=0 and SD=1 we draw a reference curve
        reference_mean<-0
        reference_sd<-1
        xlim<-c(reference_mean - reference_sd*3, reference_mean + reference_sd*3)
        reference_x<-seq(xlim[1],xlim[2],length.out=100)
        reference_y<-dnorm(reference_x,mean=reference_mean,sd=reference_sd)
        ylim <- range(reference_y)
        
        #draw curve
        plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
        par(mai=c(0.2,0.1,0.1,0.1))
        lines(x=reference_x,y=reference_y,lty=1,col="blue",lwd=2)
        
        #fill in shading
        if(!all(!reference_x<GRS_beta)){
          max_GRS_i<-max(which(reference_x<GRS_beta))
          upper_x<-reference_x[1:max_GRS_i]
          upper_y<-reference_y[1:max_GRS_i]
          x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
          y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
          polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
        }
        
        #draw the main line
        abline(v=GRS_beta,lwd=3)
        
        #optionally add real distribution curve
        if(!is.null(distributionCurve)){
          real_x <- distributionCurve[["x"]]
          real_y <- distributionCurve[["y"]]
          adj_y<-real_y * (max(reference_y) / max(real_y))
          lines(x=real_x,y=adj_y,lty=2)
        }
        
        #add legend depending on real-dist score
        if(is.null(distributionCurve)){
          legend("topleft",legend=c("Population distribution","Your genetic risk score"),lty=c(1,1),lwd=c(2,3),col=c("blue","black"))
        }else{
          legend("topleft",legend=c("Population distribution","Impute.me user distribution","Your genetic risk score"),lty=c(1,2,1),lwd=c(2,2,3),col=c("blue","black","black"))
        }
        
        #plotting for all-SNP scores
      }else{
        
        #get xlim, ylim and x and y
        #since mean and sd is not standardized we get only according to previous users
        distributionCurve <- o[["distributionCurve"]]
        real_x <- distributionCurve[["x"]]
        real_y <- distributionCurve[["y"]]
        xlim<-range(c(real_x,GRS_beta))
        xlim[1] <- xlim[1]-0.1
        xlim[2] <- xlim[2]+0.1
        ylim <- c(0,max(real_y))
        
        
        #draw curve
        plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
        par(mai=c(0.2,0.1,0.1,0.1))
        lines(x=real_x,y=real_y,lty=2,col="black",lwd=1)
        
        #fill in shading
        if(!all(!real_x<GRS_beta)){
          max_GRS_i<-max(which(real_x<GRS_beta))
          upper_x<-real_x[1:max_GRS_i]
          upper_y<-real_y[1:max_GRS_i]
          x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
          y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
          polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
        }
        
        #draw the main line
        abline(v=GRS_beta,lwd=3)
        
        legend("topleft",legend=c("Impute.me user distribution","Your genetic risk score"),lwd=c(1,3),col=c("black","black"),lty=c(2,1))
      }
    })
  })
  )
  
  observe(lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(study_id){
    output[[paste0("plot_1modals",rc_traits[study_id,"id"])]] <- renderPlot({
      
      #print("XXXXXXXXXXXXXXXXXXXds")
      
      o<-get_data(study_id)
      #print("ALSHJDLKASDLKASJLDKJASL")
      #print(o)
      SNPs_to_analyze<-o[["SNPs_to_analyze"]]
      GRS_beta<-o[["GRS"]]
      distributionCurve <- o[["distributionCurve"]]
      use_all_snp_score <- o[["use_all_snp_score"]]
      
      if(is.na(GRS_beta))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
      
      
      
      #plotting for top-hit scores
      if(!use_all_snp_score){
        
        #get xlim, ylim and x and y
        #since scores are mean=0 and SD=1 we draw a reference curve
        reference_mean<-0
        reference_sd<-1
        xlim<-c(reference_mean - reference_sd*3, reference_mean + reference_sd*3)
        reference_x<-seq(xlim[1],xlim[2],length.out=100)
        reference_y<-dnorm(reference_x,mean=reference_mean,sd=reference_sd)
        ylim <- range(reference_y)
        
        #draw curve
        plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
        par(mai=c(0.2,0.1,0.1,0.1))
        lines(x=reference_x,y=reference_y,lty=1,col="blue",lwd=2)
        
        #fill in shading
        if(!all(!reference_x<GRS_beta)){
          max_GRS_i<-max(which(reference_x<GRS_beta))
          upper_x<-reference_x[1:max_GRS_i]
          upper_y<-reference_y[1:max_GRS_i]
          x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
          y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
          polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
        }
        
        #draw the main line
        abline(v=GRS_beta,lwd=3)
        
        #optionally add real distribution curve
        if(!is.null(distributionCurve)){
          real_x <- distributionCurve[["x"]]
          real_y <- distributionCurve[["y"]]
          adj_y<-real_y * (max(reference_y) / max(real_y))
          lines(x=real_x,y=adj_y,lty=2)
        }
        
        #add legend depending on real-dist score
        if(is.null(distributionCurve)){
          legend("topleft",legend=c("Population distribution","Your genetic risk score"),lty=c(1,1),lwd=c(2,3),col=c("blue","black"))
        }else{
          legend("topleft",legend=c("Population distribution","Impute.me user distribution","Your genetic risk score"),lty=c(1,2,1),lwd=c(2,2,3),col=c("blue","black","black"))
        }
        
        #plotting for all-SNP scores
      }else{
        
        #get xlim, ylim and x and y
        #since mean and sd is not standardized we get only according to previous users
        distributionCurve <- o[["distributionCurve"]]
        real_x <- distributionCurve[["x"]]
        real_y <- distributionCurve[["y"]]
        xlim<-range(c(real_x,GRS_beta))
        xlim[1] <- xlim[1]-0.1
        xlim[2] <- xlim[2]+0.1
        ylim <- c(0,max(real_y))
        
        
        #draw curve
        plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
        par(mai=c(0.2,0.1,0.1,0.1))
        lines(x=real_x,y=real_y,lty=2,col="black",lwd=1)
        
        #fill in shading
        if(!all(!real_x<GRS_beta)){
          max_GRS_i<-max(which(real_x<GRS_beta))
          upper_x<-real_x[1:max_GRS_i]
          upper_y<-real_y[1:max_GRS_i]
          x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
          y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
          polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
        }
        
        #draw the main line
        abline(v=GRS_beta,lwd=3)
        
        legend("topleft",legend=c("Impute.me user distribution","Your genetic risk score"),lwd=c(1,3),col=c("black","black"),lty=c(2,1))
      }
    })
  })
  )
  
  observe(lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(study_id){
    #The table of SNPs and their effects
    output[[paste0("table1",rc_traits[study_id,"id"])]] <- DT::renderDataTable({ 
#      if(input$goButton == 0){
#      return(NULL)
#     }else if(input$goButton > 0) {
        
        #getting data
        o<-get_data(study_id)
        SNPs_to_analyze<-o[["SNPs_to_analyze"]]
        GRS<-o[["GRS"]]
        
        
        #summarising allele info into single-columns
        SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
        SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
        
        
        
        
        #rounding MAF and effect_size
        SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
        SNPs_to_analyze[,"effect_size"] <- signif(SNPs_to_analyze[,"effect_size"], 3)
        
        #removing duplicate GRS
        # SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"GRS"]<-""
        
        #shortening the reported gene count
        SNPs_to_analyze[,"reported_genes"]<-sapply(strsplit(SNPs_to_analyze[,"reported_genes"],", "),function(x){
          paste(x[1:min(c(2,length(x)))],collapse=", ")
        })
        
        
        #marking duplicates
        for(col in c("genotype","personal_score","score_diff")){
          SNPs_to_analyze[is.na(SNPs_to_analyze[,col]),col] <- ""
        }
        
        
        keep<-c("SNP","genotype","Risk/non-risk Allele","effect_size","personal_score","score_diff"
                ,"p_value","Major/minor Allele","minor_allele_freq","reported_genes")
        SNPs_to_analyze<-SNPs_to_analyze[,keep]
        colnames(SNPs_to_analyze)<-c("SNP","Your Genotype","Risk/ non-risk Allele","Effect Size","SNP-score","SNP-score (population normalized)","P-value","Major/ minor Allele","Minor Allele Frequency","Reported Gene")
        return(SNPs_to_analyze)
#      }
    },options = list(searching = FALSE,paging = FALSE),rownames= FALSE)
  })
  )
  
  observe(lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(study_id){
    output[[paste0("text_2",rc_traits[study_id,"id"])]] <- renderText({ 
        o<-get_data(study_id)
        m<-paste0("<br><br>",o[["textToReturn"]],"<br><br>")
        
       # print(m)
        
      
      return(m)
    })
  })
  )
  
  observe(lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(study_id){
    output[[paste0("text_2modals",rc_traits[study_id,"id"])]] <- renderText({ 
      o<-get_data(study_id)
      m<-paste0("<br><br>",o[["textToReturn"]],"<br><br>")
      
      #print(m)
      
      
      return(m)
    })
  })
  )
  
  observe(lapply(traits[(((current_page$number-1) * show_total_data$number) + 1 ): (((current_page$number-1) * show_total_data$number) + show_total_data$number),"study_id"], function(study_id){
    #The long methods section written after the SNPs-table
    output[[paste0("text_3",rc_traits[study_id,"id"])]] <- renderText({ 
        o<-get_data(study_id)
        methodsToReturn<-o[["methodsToReturn"]]
      
      return(methodsToReturn)
    })
  })
  )
}
  
