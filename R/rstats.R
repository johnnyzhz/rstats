rate<-function(download=NULL, like=NULL, rating=NULL, comment=NULL, meta=TRUE, package, email, name, lib.loc = NULL){
	## check the package name
	if (missing(package)){
		if (is.null(options()$rstats)){
			stop('Package name is needed. You can provide it in the function or set it use setRstats().')
		}else{
			rstats.op<-options()$rstats
			package <- rstats.op[1]
			email <- rstats.op[2]
			user <- rstats.op[3]
		}
	}
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only rate a package you have installed. Thanks. ", sQuote(package))
	}else{
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
		if (!is.null(download)) download <- '1'
		if (!is.null(like)) like <- '1'
		if (!is.null(rating)){
			if (!(rating %in% 1:5)) stop('The rating has to be 1 from 5.')
			rating <- as.character(rating)
		}			
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rateGet.php?name=', package, '&download=', download, '&like=', like, '&rating=', rating, '&comment=',URLencode(comment,TRUE), '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/rate.php", name=package, download=download, like=like, rating=rating, comment=comment, meta=meta)
			cat(out)
		}
	}
}

view<-function(comment=FALSE, ncomment=1:5, package, lib.loc = NULL){
	comment<-ifelse(comment, 1, 0)
	dir <- system.file(package = "RCurl", lib.loc = lib.loc)
	if (dir == ""){
		URL<-paste('http://rstats.psychstat.org/comments.php?name=',  package, sep='')
		browseURL(URL)
	}else{
		library('RCurl')
		rating<-getURL(paste('http://rstats.psychstat.org/view.php?name=',  package,  '&comment=', comment, sep=''))
		rate<-strsplit(rating, "\n")[[1]]
		nrate<-length(rate)
		if (nrate>2){
			for (i in 1:3) cat(rate[i], "\n")
			cat("\n")		
			if (comment){
				if (nrate==3) stop("No comment available yet")
				totalcomment<-nrate-3
				cat("There are ", totalcomment, " comments in total. The number in parenthesis is # of replies.\n")
				if (max(ncomment)>totalcomment) ncomment<-1:totalcomment
				for (i in (ncomment+3)) cat(rate[i], "\n")
			}
		}else{
			cat(rating)	
		}
	}		
}

ask<-function(comment=NULL, package,  email, name, meta=TRUE, lib.loc = NULL){
	dir <- system.file(package = package, lib.loc = lib.loc)
    if (dir == "") {
		gettextf("You have not installed the package %s. You can only ask a question about a package you have installed. Thanks. ", sQuote(package))
	}else{
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
			
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/rateGet.php?name=', package, '&comment=',URLencode(comment,TRUE), '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/ask.php", name=package, comment=comment, email=email, meta=meta)
			cat(out)
		}
	}
}


reply<-function(id, comment=NULL, package, email, name, meta=TRUE, lib.loc = NULL){
	
		if (missing(id)) stop('The id of the question or comment is needed! Please use view() function to find out the id.')
		if (meta){
			meta <- packageDescription(pkg = package)
			meta <- paste(meta$Maintainer, ";", meta$Version, ";", meta$Built, ";", meta$Repository)
		}else{
			meta <- "Not provided"
		}
			
		dir <- system.file(package = "RCurl", lib.loc = lib.loc)
		if (dir == ""){	
			URL<-paste('http://rstats.psychstat.org/replyGet.php?name=', package, '&comment=',URLencode(comment,TRUE), '&meta=', URLencode(meta,TRUE), sep='')
			browseURL(URL)
		}else{
			library('RCurl')
			out<-postForm("http://rstats.psychstat.org/reply.php", name=package, id=id, comment=comment, meta=meta)
			cat(out)
		}
}

viewreply<-function(id, package, lib.loc = NULL){
	if (missing(id)) stop('The id of the question or comment is needed! Please use view() function to find out the id.')
	
	dir <- system.file(package = "RCurl", lib.loc = lib.loc)
	if (dir == ""){
		URL<-paste('http://rstats.psychstat.org/comments.php?name=',  package, sep='')
		browseURL(URL)
	}else{
		library('RCurl')
		rating<-getURL(paste('http://rstats.psychstat.org/viewreply.php?name=',  package,  '&id=', id, sep=''))		
		cat(rating)	
	}		
}

setRstats<-function(package='base', email='', name=''){
	options(rstats=c(package, email,name))
}
