
isdebug<-FALSE

initialize<-function(name, isDebug = FALSE)
{
 isdebug <<-isDebug
}

head<-function()
{
	if(!isdebug)
	tags$head(includeHTML('./layout/pageparts/head.html'))
	else
	tags$head(HTML(paste('<style>',includeHTML('./layout/styling/base.css'),'</style>')))
}




navigation<-function()
{
	includeHTML('./layout/pageparts/navigation.html')
}

footer<-function()
{
	includeHTML('./layout/pageparts/footer.html')
}

beginPage<-function()
{
	includeHTML('./layout/pageparts/beginpage.html')
}

endPage<-function()
{
	HTML('</div>')
}


titlePanel<-function(title)
{
	HTML(paste0('<div class="title-block">
	<div class="container">
		<h1>' , title , '</h1>
	</div>
</div>'))
}

beginRow <- function() {
  HTML('<div class="row">')
}

endRow <- function() {
  HTML('</div>')
}

beginPanel<-function(size = '' ,id='')
{
	class<-'col-xs-12'

	if(size == '1/2') class<-'col-md-6 layout-panel'
	if(size == '1/3') class<-'col-md-4 layout-panel'
	if(size == '2/3') class<-'col-md-8 layout-panel'

	HTML(paste0('<div class="',class,'" id="',id,'">'))
}

marginTop <- function(offset = '') {
  class<- 'mt-1'
  if(offset == '2') class<- 'mt-2'
  if(offset == '3') class<- 'mt-3'
  if(offset == '4') class<- 'mt-4'
  if(offset == '5') class<- 'mt-5'
  HTML(paste0('<hr style="border-top:0px;" class="',class,'"/>'))
}

marginBottom <- function(offset = '') {
  class<- 'mb-1'
  if(offset == '2') class<- 'mb-2'
  if(offset == '3') class<- 'mb-3'
  if(offset == '4') class<- 'mb-4'
  if(offset == '5') class<- 'mb-5'
  HTML(paste0('<hr style="border-top:0px;" class="',class,'"/>'))
}

endLine <- function() {
  HTML('<hr style="border-top:1px solid #707070;"/>')
}

endPanel<-function()
{
	HTML('</div>')
}