
  ##' Reads Shimadzu Spectra Files (.txt) as exported by Shimadzu Chrome Solution (v. 2.73) 
  ##'
  ##' @note This is a first rough import function and the functions may change without notice.
  ##' @param filename file name and path of the .txt file
  ##' @param encoding encoding of the txt file (used by \code{\link[base]{readLines}})
  ##' @param quiet suppress printing of progress
  ##' @return hyperSpec object
  ##' @author Bjoern Egert
  ##' @export
  read.txt.Shimadzu <- function(filename, encoding = "", quiet = TRUE)
  {    
  
    # A file consists of several sections ([Headers])
    # Each Section consists of:
    # [Header]
    #   [MS Spectrum]
    #   [MC Peak Table]
    #   [MS Similarity Search Results for Spectrum Process Table]      
      
    parsed <- readLines(con = filename, n = -1L, ok = TRUE, warn = TRUE, encoding = encoding)
    length(parsed)
  
  	# number of pos1 and pos2 and pos3 are equal
  	pos1 <- which(parsed=="[Header]")															                        # row positions of Headers
  	pos2 <- which(parsed=="[MC Peak Table]")  											                      # row positions of peak info tables
  	pos3 <- which(parsed=="[MS Similarity Search Results for Spectrum Process Table]")    # row positions of peak annotations
  	pos4 <- which(parsed=="[MS Spectrum]")													                      # row positions of peak spectra
  
  	headers <- length(pos1) # number of header sections  
  
  	# link spectra to headers
  	pos4Li = list()
  	for (i in 1:(length(pos1)))
  	{
  		header 			<- pos1[i] 
  		headerNext	<- pos1[i+1] 
  		tmp	<- (pos4>header & pos4<headerNext)	
  		pos4Li[[i]] <- pos4[tmp] 
  	}
  	# last Header information section separately ...
  	headerLast <- pos1[length(pos1)]
  	tmp	<- pos4>headerLast
  	pos4Li[[length(pos1)]] = pos4[tmp]

  	for (i in 1:(length(pos4Li)-1))
  	{
  		tmp <- length(pos4Li[[i]])
  		vec	<- pos4Li[[i]]
  		pos4Li[[i]] <- c(vec,pos1[i+1])
  	}
  	#End position
  	pos4Li[[headers]] <- c(pos4Li[[headers]], length(parsed))
  
  
  	# Check
    stopifnot(parsed[1] == "[Header]") 
  	stopifnot(length(pos1) == length(pos2))
  	stopifnot(length(pos2) == length(pos3))
  	stopifnot(length(pos3) == length(pos4Li))
  
  
  	# ----------------- 1. Import: get section informations
  
  	# gather in lists
  	res2Li <- list()	  # Peak Info
  	res3Li <- list()    # Similarity
  	res4Li <- list()    # Spektren
  
  	for(header in 1:headers)
  	{

      if(!quiet) cat("header: ", header, "\n")
      
  		# ----------------- 1a. Import: "[MC Peak Table]"
  
  		start <- pos2[header]+3
  		stop  <- pos3[header]-2
  		peakMat <- read.table(file = file, skip = start-1, header = T, sep = ";", dec =".", nrows = stop-start, comment.char = "", stringsAsFactors = FALSE, quote = "\"'")
  		res2Li[[header]] <- peakMat
  		
  		# ----------------- 1b. Import: "[MS Similarity Search Results for Spectrum Process Table]"
  
  		start <- pos3[header]+2
  		stop  <- pos4Li[[header]][1]-1
  		if(stop-start!=0)	# no annotation hits
  		{
  			simMat <- read.table(file = file, skip = start-1, header = T, sep = ";", dec =".", nrows = stop-start, comment.char = "", stringsAsFactors = FALSE, quote = "\"'")
  		}else simMat <- NA
  		res3Li[[header]] <- simMat
  		
  		# ----------------- 1c. Import: "[MS Spectrum]"
  
  		specLi <- list()	# Liste aller Spektren der gefundenen Peaks des gegenwärtigen headers
  		for(i in 1:(length(pos4Li[[header]])-1))	
  		{  		
        
  		  # debug
        #cat("header: ", header, "start:", start, "stop", stop, "\n")
  			
        # Spektrum extrahieren
  			start <- pos4Li[[header]][i]+5			# 5 Zeilen später fangen die Daten an
  			stop  <- pos4Li[[header]][i+1]-1	  # Die letzte Datenzeile, bevor ein neues Spektrum anfängt
  			
  			spec	<- scan(file = file, sep=";", skip = start-1, nlines = (stop-start)+1, dec = ".", quiet = TRUE)
  			spec	<- matrix(spec,ncol = 3, byrow = T)
  			colnames(spec) <- c("m/z", "Absolute Intensity", "Relative Intensity")
  			specLi[[i]] <- spec	
        
  		}
      
  		res4Li[[header]] <- specLi     
      
  	} # for(headers)
  
  
  	# ----------------- 2. combine all headers sections
  
  	# res2Li --> m2
  	m2 <- as.data.frame(res2Li[[1]])
  	m2 <- cbind(header=1,m2)
  	for(header in 2:headers) 
  	{
  		tmp <- as.data.frame(res2Li[[header]])
  		tmp <- cbind(header=header,tmp)
  		m2  <- rbind(m2,tmp)
  	}
    
    # res3Li --> m3    
    # In a header section there may be not annotation tables
    m3 <- do.call("rbind", res3Li) 
    # add header Nr. 
    tmpMat <- lapply(X = res3Li, FUN = nrow)  
    tmpMat <- as.matrix(tmpMat)
    tmp <- vector(length = 0)
    for(i in 1:nrow(tmpMat))
    {
      if(tmpMat[i,1] == "NULL" ) tmp <- c(tmp,i)
      if(tmpMat[i,1] != "NULL" ) tmp <- c(tmp,rep(x = i, times = tmpMat[i,1]))
    }    
    m3 <- cbind(header= tmp, m3) 
    m3 <- m3[,c("header","Spectrum.","Hit..","SI","CAS..","Mol.Weight","Mol.Form","Retention.Index")]  # select most important columns
    tmp <- complete.cases(m3)
    m3 <- m3[tmp,]
    
  	# res4Li --> m4
  	m4 <- as.matrix(res4Li[[1]][[1]])
  	m4 <- cbind(header=1,spectra = 1, m4)
  	for(header in 1:headers)
  	{  
  		for(spectra in 2:length(res4Li[[header]]))
  		{
  			tmp <- as.matrix(res4Li[[header]][[spectra]])	
  			tmp <- cbind(header,spectra,tmp)
  			m4	<- rbind(m4,tmp)
  		}# spectras
  			
  	}# header
    mode(m4) <- "numeric"    
    
    return(list(peakInfo = m2, peakAnnotate = m3, peakMasses = m4))  
    
  } 
     
  
