#' checkBuildInstallSourcePackage
#'
#' @import utils
#' @import devtools
#' 
#' @description check, build, and install local source package
#'
#' @param dir character string containing the path name of the directory holding the package folders
#' @param packs character vector of the names of the packages
#' @param packCheck character vector of the names of the packages to check()
#' @param autoLibrary Boolean if TRUE automate library() command altering search path
#'
#' @details
#' I wanted to include "library(packs)" in the program, but this is not allowed.
#' The user can "cat" the return value and copy and paste to facilitate doing
#'  this manually.
#'
#' @examples
#' \dontrun{
#' # you need to specify dir, packs that are on your own computer !!
#' dir1<-"~/personal/hearts/hearts_card_game_bayesian_inference"
#' dir2<-"packages/inference_packages/inference_packages/"
#' packs<-c("cardUtils","clickableImageMap")
#' l<-checkBuildInstallSourcePackage(sprintf("%s/%s",dir1,dir2),packs,packs,TRUE)
#' }
#'
#' @return returns a list whose components are
#' \itemize{
#' \item
#' character string that can be printed using "echo" and then
#' copy and paste by the user to load or update the packages
#' \item
#' return value of dateTable()
#' }
#'
#' @export
checkBuildInstallSourcePackage<-
  function(dir,packs,packCheck,autoLibrary=FALSE) {
    l<-list()
    dt<-list()
    returnStr<-""
    dir<-path.expand(dir)
    
    lib<-getSysLib()
    
    for(pack in packs) {
      p<-sprintf("%s/%s",dir,pack)
      message(c("Processing: ",pack))
      pattern<-sprintf("%s.+%s",pack,".tar.gz")
      p.tar.gz<-list.files(dir,pattern=pattern)
      if(length(p.tar.gz)>0)
        dt<-dateTable(dt,sprintf("%s/%s",dir,p.tar.gz),"before")
      oldwd <- getwd()
      on.exit(setwd(oldwd))
      setwd(p)

      # check() local source package
      # to save time, check() only those packs in packCheck
      if(pack %in% packCheck) {
        devtoolsCheckErrors<-devtools::check()
        if(devtoolsCheckErrors$status!=0)
          stop(c("devtoolsCheckErrors",pack,devtoolsCheckErrors$stderr,devtoolsCheckErrors$status,devtoolsCheckErrors$errors,devtoolsCheckErrors$warnings))
      }
      
      # build local .tar.gz source file
      devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))

      b<-"buildError"
      b<-devtools::build()
      # return value b is "a string giving the location (including file name) of the built package"
      # an extra "/" at the end of dir will cause a problem in (b %in% lf)
      #if(grep("/$",dir)==1)
      if(length(grep("/$",dir))>0)
        dir<-substr(dir,1,nchar(dir)-1)
      lf<-list.files(dir,pattern=sprintf("%s.+.tar.gz",pack),full.names=TRUE)
      if(!(b %in% lf))
        stop(c("buildError: ",pack))

      fExistsAge(b)
      
      dt<-dateTable(dt,b,"after")
      
      # install package from local .tar.gz file
      f<-sprintf("%s/%s",lib,pack)
      dt<-dateTable(dt,f,"before")
      # not well documented anywhere, but I fortuitously noticed that verbose=TRUE generates a message with the word "succeeded"
      # https://stackoverflow.com/questions/64797849/how-to-save-verbose-output
      installReturnStr<-capture.output(install.packages(b,repos=NULL,type='source',lib=lib,verbose=TRUE),type="message")
      installReturnCode<-strsplit(installReturnStr[2],"[[:space:]]+")[[1]][2]
      print(c("installReturnCode:",pack,installReturnCode))
      if(installReturnCode!="succeeded")
        stop(c("installReturnCode!='succeeded'"),pack)
      
      dt<-dateTable(dt,f,"after")
      fExistsAge(f)
      
      
      # https://stackoverflow.com/questions/46916283/how-to-reinstall-and-reload-a-local-r-package
      # "If a package is already loaded library() doesn't load it again"
      # "You have to unload the current version of the package for the update to take effect when you try to load it again."
      # https://stackoverflow.com/questions/64737686/why-library-or-require-should-not-be-used-in-a-r-package
      # instead returns a character string that can be printed using
      # cat [return value of checkBuildInstallSourcePackage()]
      # and then copy and paste by the user to load or update the packages
      returnStr<-sprintf("%s\ndetach(\"package:%s\")",returnStr,pack)
      returnStr<-sprintf("%s\nlibrary(%s)",returnStr,pack)
      
      # workaround for not being able to directly include library() in package code
      if(autoLibrary)
        dt<-zload(lib,pack,dt)
    }
    
    l$dt<-dt
    l$returnStr<-returnStr
    
    return(l)
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
#' @examples
#' \dontrun{
#' # you need to specify packs that are on your own computer !!
#' pack<-c("retrieve","tcpflow")
#' conflictOfInterestRestricted(pack)
#' }
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
    if(length(ssp)>1 & length(ssg)>1) {
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
    
     if(length(w)>0)
      return(x[w]) # conflictOfInterest() restricted to functions of interest
    else
        return(NULL)
    }
    return(NULL)
  }

#' getSysLib
#' 
#' @description For consistency, make sure that we are always using the designated library rather than a random library
#' 
#' @return returns a character string containing the path name for the designated library
#' 
#' @examples
#' if(interactive()) {
#'   sysLib<-getSysLib()
#' }
#' 
#' @export
#' 
getSysLib<-
  function() {
    libs<-.libPaths()
    nlib<-length(libs)
    if(nlib==0)
      stop("getSysLib: you have no libraries")
    cat("Here are your libraries:\n\n")
    print(libs,quote=FALSE)
    while(TRUE) {
      lib<-as.integer(readline(prompt="Enter the number of the library that you wish to designate: "))
      if(lib>=1 & lib<=nlib) {
        cat(sprintf("\nYou chose %s\n",libs[lib]))
        cat("Remember that in case you need to access the packages in the designated library in the future\n")
        return(libs[lib])
      }
    }
  }
#' fExistsAge
#' 
#' @description check for newly created file existence and age
#' 
#' @param f character string containing the path name of the file
#' 
#' @examples
#' \dontrun{
#' # you need to specify f that is on your own computer !!
#' dir1<-"/Users/barryzeeberg/personal/hearts"
#' dir2<-"hearts_card_game_bayesian_inference/packages"
#' f<-"NoviceDeveloperResources_1.1.0.tar.gz"
#' fExistsAge(sprintf("%s/%s/%s",dir1,dir2,f))
#' }
#' 
#' @return returns no values,but has side effect of terminating if file is not valid
#' 
#' @export
fExistsAge<-
  function(f) {
    delt<-10 # max acceptable age in seconds
    units<-"secs"
    if(is.null(f))
      stop("fExistsAge: argument f is NULL")
    
    if(length(f)==0)
      stop("fExistsAge: length of argument f is 0")
    
    if(!file.exists(f))
      stop(c("fExistsAge file does not exist: ",f))
    age<-as.numeric(difftime(Sys.time(),file.info(f)$ctime,units=units))
    if(age>delt)
      stop(c("fExistsAge file too old: ",f," ",age," ",units))
  }

#' dateTable
#' 
#' @description list of file dates
#' 
#' @param dt date table in list format
#' @param f character string containing the full path name of the file
#' @param when character string either "before" or "after"
#' 
#' @details allows the user to confirm that the .tar.gz and the library packages are not left-overs
#' 
#' @examples
#' \dontrun{
#' # you need to specify dir, f that is on your own computer !!
#' dt<-list()
#' dir<-"~/personal/hearts/hearts_card_game_bayesian_inference/packages"
#' f<-sprintf("%s/%s",dir,"NoviceDeveloperResources_1.1.0.tar.gz")
#' when<-"before"
#' dateTable(dt,f,when)
#' }
#' 
#' @return updated version of dt
#' 
#' @export
dateTable<-
  function(dt,f,when) {
    if(is.null(f))
      stop("dateTable: argument f is NULL")
    
    if(length(f)==0)
      stop("dateTable: length of argument f is 0")
    
   if(!file.exists(f)) {
      dt[[f]]["now"]<-system("date",intern=TRUE)
      dt[[f]]["error"]<-"file does not exist"
      return(dt)
    }
    sys<-sprintf("ls -ld %s",f)
    x<-system(sys,intern=TRUE)
    ss<-strsplit(x,"[[:space:]]+")
    dt[[f]]["now"]<-system("date",intern=TRUE)
    dt[[f]][when]<-sprintf("%s %s %s",ss[[1]][6],ss[[1]][7],ss[[1]][8])
    
    return(dt)
  }

#' zload
#' @description detach old version of package from search path, load new version, and validate
#'
#' @param lib character string containing the name of the user-designated library
#' @param pack character string containing the name of a package
#' @param dt date table in list format
#'
#' @examples
#' \dontrun{
#' # you need to specify packs that are on your own computer !!
#' lib<-getSysLib()
#' dt<-list()
#' zload(lib,"NoviceDeveloperResources",dt)
#' }
#'
#' @return returns updated version of date table
#'
#' @export
zload<-
  function(lib,pack,dt) {
    # if package with the same name is already in the search path, detach it
    if(inSearchPath(pack)) {
      # version that is permitted in CRAN package)))
      eval(parse(text = sprintf("detach(\"package:%s\", force=TRUE)",pack)))
      if(inSearchPath(pack))
        stop(c("zload detach() failed: ",pack))
    }
    # package is now guaranteed to be detached
    # so we know that we are adding the intended version to the search path
    # version that is permitted in CRAN package
    eval(parse(text = sprintf("library(%s,lib.loc='%s', verbose=TRUE)",pack,lib)))
    
    # confirm that the package was successfully added to the search path
    if(!inSearchPath(pack))
      stop(c("zload library() failed: ",pack))
    # double check to provide tangible proof that this package was
    # newly added to the search path
    # and that it came from the intended library
    return(dateTable(dt,sprintf("%s/%s",lib,pack),"attach"))
  }

#' inSearchPack
#' @description is the package listed in the search path
#' 
#' @param pack list of character strings containing the name of a package
#' 
#' @examples
#' \dontrun{
#' # you need to specify packs that are on your own computer !!
#' pack<-c("retrieve","tcpflow")
#' inSearchPath(pack)
#' }
#' 
#' @return returns list of Booleans TRUE if the package is listed in the search path
#' 
#' @export
inSearchPath<-
  function(pack) {
    # is pack in the search path?
    return(formatSearchPack(pack) %in% search())
  }

#' formatSearchPack
#' @description given a package name, reformat it as listed in the search path
#' 
#' @param pack list of character strings containing the names of a package
#' 
#' @examples
#' pack<-c("retrieve","tcpflow")
#' formatSearchPack(pack)
#' 
#' @return returns list of package names formatted it as listed in the search path
#' 
#' @export
formatSearchPack<-
  function(pack) {
    return(sprintf("%s:%s","package",pack))
  }