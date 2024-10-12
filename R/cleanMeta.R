#' Parsing meta file
#'
#' This function cleans up the metadata table, and allows you to specify roles for columns.
#' @param in.table A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param scan.prefix Which column specifies the unique name for each timecourse of images. Defaults to "scan".
#' @param scan.dir FULL path to the directory containing the scanner images, if it exists.
#' @param scan.dir FULL path to the directory containing the cropped images and/or histogram tables, if it exists.
#' @param scan.position If there are multiple plates per scan, this column specifies which crop on the original scan corresponds to the sample. Defaults to "position", but "", NA, NULL, "none" are also good options
#' @param group.factors Column(s) to group images by such that each group corresponds to a single sample. For example, c("strain", "dose", "replicate").
#' @param facet.wrap.factor Column(s) to facet wrap the plots by, if any
#' @param facet.grid.factors A list of two vectors of column names, the first specifying facet rows and the second specifying facet columns.
#' @param color.factor Column to color plots by, if any.
#' @param linetype.factor Column to set linetype by, if any. 
#' @param filters Logical columns(s) to filter samples based on, if any.
#' @param timefed Column that specifies the worms met food on each plate. Date format = yy-mm-dd_HH_MM
#' @param trimbefore Column that specifies the timepoint before which data for plate should be ignored. Date format = yy-mm-dd_HH_MM
#' @param trimafter Column that specifies the timepoint after which data for plate should be ignored. Date format = yy-mm-dd_HH_MM
#' @export
#' @examples
#' cleanMeta()
 
cleanMeta <- function(in.table,
                      scan.prefix = "scan",
                       scandir = NA,
                      crop.hist.dir = NA,
                       scan.position = NA,
                       group.factors = c(),
                      facet.wrap.factor = c(), #"strain", # c("rep", "strain"), # or "strain", or c("strain", ".")
                      facet.grid.factors = list(), #"strain", # c("rep", "strain"), # or "strain", or c("strain", ".")
                      color.factor = NA, #"dose", # or "", NA, NULL, "none"
                       linetype.factor = NA, #"rep", # or "", NA, NULL, "none"
                       filters = c(), #c("clean", "complete"), # or "", NA, NULL, "none"
                       timefed = "time_fed", 
                       trimbefore = NA, #"trim_before", # or "", NA, NULL, "none"
                       trimafter = NA, #"trim_after" # or "", NA, NULL, "none",
                      additional.columns = NA # any other columns that don't have a function here but that we want to maintain
                       
){
    # A quick check that the two required inputs look ok.
    if(!scan.prefix %in% colnames(in.table)){
        print(paste0("ERROR: \'", scan.prefix, "\' is not a column in table. Options are: ", colnames(in.table)))
        stop()
    }
    
    # Make a new dataframe, with column names that will be meaningful to the plotting scripts
    expDesign = data.frame( scan.prefix = in.table[,scan.prefix] )
    
    # Add information about original images, if applicable
    if(!is.na(scandir)){ 
        expDesign$scan.dir = scandir 
        expDesign$path.to.scan = apply(expDesign, 1, function(x){paste0(x["scan.dir"], "/", x["scan.prefix"])})
    }
    
    # If the original scans need cropping, the scans, crop positions, cropped image names, and paths need to be specified.
    if(!is.na(scan.position) & scan.position %in% colnames(in.table)){ 
        expDesign$scan.position = in.table[,scan.position] 
        
        expDesign$crop.name = apply(in.table[,c(scan.position, scan.prefix)], 1, function(x){paste(x, collapse = "_")})
        if(is.na(crop.hist.dir)){
            expDesign$path.to.crop = apply(expDesign, 1, function(x){
                tmp.path = x["scan.dir"]
                if(substr(tmp.path, nchar(tmp.path),nchar(tmp.path))=="/"){
                    tmp.path = substr(tmp.path, 1,nchar(tmp.path)-1)
                }
                paste0(tmp.path, "_cropped/", x["crop.name"])})
        }
    } else {
    # If no cropping, just copy the scan information to the crop columns
        if(!scan.position %in% c(NA, NULL, "NA", "NULL", "none", "")) {
            stop(paste0("scan.position column name \'", scan.position, "\' not recognized"))
        }
        expDesign$crop.name = expDesign$scan.prefix
        expDesign$path.to.crop = expDesign$path.to.scan
    } 
    
    # Add information about cropped images, if applicable
    if(!is.na(crop.hist.dir)){
        expDesign$path.to.crop = paste0(crop.hist.dir, "/", expDesign$crop.name)
    }
    
    # 
    if(length(group.factors)>0){ expDesign$grouper = apply(as.data.frame(in.table[,c(group.factors)]),1,function(x){paste(x,collapse = "_")}) }
    if(!is.na(color.factor)){ expDesign$colorer = in.table[,color.factor] }
    if(length(filters)>0){ expDesign$filterer = apply(as.data.frame(in.table[,filters]),1,function(x){all(x, na.rm = TRUE)}) }
    if(!is.na(linetype.factor)){ expDesign$linetyper = in.table[,linetype.factor] }
    if(!is.na(timefed)){ expDesign$time.fed = in.table[,timefed] }
    if(!is.na(trimbefore)){ expDesign$trim.before = in.table[,trimbefore] }
    if(!is.na(trimafter)){ expDesign$trim.after = in.table[,trimafter] }
    if(length(facet.wrap.factor)>0){ expDesign$facet_wrap = apply(as.data.frame(in.table[,c(facet.wrap.factor)]),1,function(x){paste(x,collapse = "_")}) }
    if(length(facet.grid.factors)>0){
        expDesign$facet_row = apply(as.data.frame(in.table[,c(facet.grid.factors[[1]])]),1,function(x){paste(x,collapse = "_")})
        expDesign$facet_col = apply(as.data.frame(in.table[,c(facet.grid.factors[[2]])]),1,function(x){paste(x,collapse = "_")}) }
    
    return(expDesign)
}
