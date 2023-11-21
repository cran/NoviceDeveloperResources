#' checkBuildInstallSourcePackage
#'
#' @import utils
#' @import devtools
#' 
#' @description check, build, and install local source package
#'
#' @param dir character string containing the path name of the directory holding the package folders
#' @param packs character vector of the names of the packages
#'
#' @details
#' I wanted to include "library(packs)" in the program, but this is not allowed.
#' The user can "cat" the return value and copy and paste to facilitate doing
#'  this manually.
#'
#' @return returns a character string that can be printed using "echo" and then
#' copy and paste by the user to load or update the packages
#'
#' @export
checkBuildInstallSourcePackage<-
  function(dir,packs) {
    l<-list()
    returnStr<-""
    for(pack in packs) {
      p<-sprintf("%s/%s",dir,pack)
      oldwd <- getwd()
      on.exit(setwd(oldwd))
      setwd(p)
      
      # check local source package
      devtools::check()
      
      # build local .tar.gz source file
      devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
      b<-devtools::build()
      
      # install package from local .tar.gz file
      install.packages(b,repos=NULL,type='source')
      
      # returns a character string that can be printed using
      # cat [return value of checkBuildInstallSourcePackage()]
      #  and then copy and paste by the user to load or update the packages
      returnStr<-sprintf("%s\ndetach(%s)",returnStr,pack)
      returnStr<-sprintf("%s\nlibrary(%s)",returnStr,pack)
    }
    return(returnStr)
  }

#' conflictOfInterest
#'
#' @description determine if there are any conflicts
#' between the functions in the attached packages and the R global environment
#'
#' @examples
#' l<-conflictOfInterest()
#'
#' @return returns a list identifying the conflicts for each conflicted function
#'
#' @export
conflictOfInterest<-
  function() {
    # retrieve functions in the global environment
    globf<-lsf.str(pos=1)
    globfSS<-unlist(strsplit(globf," : ",fixed=TRUE))

    # retrieve functions in attached packages
    l<-list()
    packs<-search()
    for(p in 2:(length(packs))) {
      ss<-strsplit(packs[p],":",fixed=TRUE)
      if(ss[[1]][1]=="package") {
        funs<-getNamespaceExports(ss[[1]][2])
        l[[ss[[1]][2]]]<-funs
      }
    }
    
    # tabulate number of occurrences of each function
    # when number of occurrences > 1, indicates conflict
    x<-c(unlist(l),globfSS)
    t<-table(x)
    W<-which(t>1)
    
    l2<-list()
    for(w in W) {
      f<-names(t[w])
      l2[[f]]<-list()
      # retrieve the function package name containing the conflicted function
      for(p in names(l)) {
        w2<-which(l[[p]]==f)
        if(length(w2)>0) {
          l2[[f]][p]<-p # add package name to conflict list for current function
        }
      } # for(p in names(l))
      
      # determine if GLOBAL ENVIRONMENT contains the conflicted function
      w2<-which(globfSS==f)
      if(length(w2)>0) {
        # add "GLOBAL ENVIRONMENT" to conflict list for current function
        l2[[f]]["GLOBAL ENVIRONMENT"]<-"GLOBAL ENVIRONMENT"
      }
    } # for(w in W)
    
    return(l2)
  }

#' conflictOfInterestRestricted
#' 
#' @description restrict the conflicted functions (retrieved by conflictOfInterest())
#' to those in user-specified packages
#' 
#' @param packs character vector of user-specified packages
#' 
#' @return returns a subset of the return value of conflictOfInterest()
#' 
#' @export
conflictOfInterestRestricted<-
  function(packs) {
    x<-conflictOfInterest() # names(x) are conflicted functions
    xu<-unlist(x) # xu are packages containing conflicted functions

    # restrict to conflicted functions that are in packs
    wp<-which(xu %in% packs)
    # restrict to conflicted functions that are in GLOBAL ENVIRONMENT
    wg<-which(xu == "GLOBAL ENVIRONMENT")

    # names(xu) are like "mapfile.cardUtils" (i.e., function.package)
    ssp<-strsplit(names(xu[wp]),".",TRUE)
    ssg<-strsplit(names(xu[wg]),".",TRUE)

    # we are only interested on examining conflicted functions in BOTH packs AND in GLOBAL ENVIRONMENT
    fp<-vector("character",length(ssp))
    for(i in 1:length(ssp))
      fp[i]<-ssp[[i]][1]
    # fp is like "mapfile"   "norm"      "relaxCols"

    fg<-vector("character",length(ssg))	
    for(i in 1:length(ssg))
      fg[i]<-ssg[[i]][1]
    # fg is like "conflicts" "mapfile"   "relaxCols"

    foi<-intersect(fp,fg) # functions of interest
    w<-which(names(x) %in% foi)
    
    return(x[w]) # conflictOfInterest() restricted to functions of interest
}