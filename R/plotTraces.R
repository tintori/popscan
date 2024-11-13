#' Takes a growth curve table and meta_with_minmax table and outputs an assortment of plots
#'
#' @param sd.table A table with one row per image captured, with standard deviation of pixel intensity already calculated.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param save.to.folder The folder where images will be saved. Defaults to "./tmp_plots".
#' @param plot.untrimmed.traces Plot each entire growth curve. Defaults to TRUE.
#' @param plot.trimmed.traces Plot just the lowest point to the highest point of each growth curve. Defaults to TRUE.
#' @param plot.ghost.curve Plot the trimmed curves on top of the faded out full curves. Defaults to TRUE.
#' @param plot.time.to.starve Plot just the number of hours from time fed to time starved. Defaults to TRUE.
#' @param p.width Force plot width. Defaults to NA.
#' @param p.height Force plot height. Defaults to NA.
#' @param p.suffix If running this multiple ways, include a suffix for the exported files, to distinguish them from each other. Defaults to "" (nothing).
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' plotTraces()

plotTraces <- function(sd.table, exp.design, 
                       save.to.folder="./tmp_plots", 
                       plot.untrimmed.traces = T,
                       plot.trimmed.traces = T,
                       plot.ghost.curve = T,
                       plot.time.to.starve = T,
                       p.width = NA,
                       p.height = NA,
                       p.suffix = ""){
    library(ggplot2)
    library(dplyr)
    theme_sophie = theme_bw()+
        theme(axis.line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              strip.background = element_rect(fill="white"),
              axis.text.x = element_text(angle = 60, hjust=1))

    if(!dir.exists(save.to.folder)){dir.create(save.to.folder)}
    
    # set how many/which colors
    pinkgreen8 = c("#4d9221", "#7fbc41", "#b8e186", "#e6f5d0", "#fde0ef", "#f1b6da", "#de77ae", "#c51b7d", "#821252", "#960EBE", "#6600FF", "#5555FF", "#44AAFF", "#33FFFF")
    color_adding_order = c(1,8,6,3,7,2,5,4,9,10,11,12,13,14)
    custom_color = NULL
    tmp.color.ln = length( unique(c(exp.design$colorer, NA))[!is.na(unique(c(exp.design$colorer, NA)))])
    if(tmp.color.ln==1){ 
        custom_color = scale_color_manual(values = "black", limits=c(ref.color))}
    if(tmp.color.ln %in% c(2:14)) {
        custom_color = scale_color_manual(values = pinkgreen8[sort(c(color_adding_order[1:tmp.color.ln]))],
                                          limits=unique(c(exp.design$colorer, NA))[!is.na(unique(c(exp.design$colorer, NA)))], 
                                          drop = FALSE)
    }
    
    plotter.table = NULL
    plot.width = 7
    plot.height = 5
    
    if(plot.untrimmed.traces|plot.trimmed.traces|plot.ghost.curve|plot.time.to.starve){
        plotter.table = sd.table %>%
            ungroup() %>%
            select(grouper, timepoint, sd) %>% full_join(ungroup(exp.design)) %>%
            mutate(tp_in_hours = as.numeric(difftime(tm(timepoint), tm(time.fed), units = "hours")))
        if(!"colorer" %in% colnames(plotter.table)){plotter.table = plotter.table %>% mutate(colorer = "NA")}
        if(!"linetyper" %in% colnames(plotter.table)){plotter.table = plotter.table %>% mutate(linetyper = "NA")}

        if("facet_wrap" %in% colnames(sd.table)){
            plot.width = ceiling(2*sqrt(length(unique(exp.design$facet_wrap)))+2)
            plot.height = ceiling(2*sqrt(length(unique(exp.design$facet_wrap))))
        }
        if("facet_row" %in% colnames(sd.table)){
            plot.width = ceiling(2*length(unique(exp.design$facet_row))+2)
            plot.height = ceiling(2*length(unique(exp.design$facet_col)))
        }
    }
    if(!is.na(p.width)){plot.width = p.width}
    if(!is.na(p.height)){plot.height = p.height}
    
    # Untrimmed traces
    if(plot.untrimmed.traces){
        p = plotter.table %>% full_join(ungroup(exp.design)) %>% 
            ggplot(aes(x=tp_in_hours, y=sd, group=grouper,
                       color=factor(colorer), 
                       linetype=factor(linetyper)))+
            geom_line() +
            custom_color +
            labs(x="Hours", y="Worm density\n(Standard deviation of pixel intensity)", 
                 color=NULL) +
            theme_sophie
        if("facet_wrap" %in% colnames(sd.table)){ p = p + facet_wrap(~facet_wrap) }
        if("facet_row" %in% colnames(sd.table)){ p = p + facet_wrap(facet_row~facet_col) }
        if(!"linetyper" %in% colnames(exp.design)){p = p + guides(linetype="none")}
        if(!"colorer" %in% colnames(exp.design)){p = p + guides(color="none")}
        
        print(p)
        ggsave(paste0(save.to.folder, "/untrimmed_sd_curve", p.suffix, ".pdf"), device = "pdf",
               width = plot.width, height = plot.height) 
    }
    
    # Trimmed curves
    if(plot.trimmed.traces){
        p = ggplot() +
            geom_line(data = plotter.table %>% 
                          filter(!tm(timepoint)<sd_time_of_valley, !tm(timepoint)>sd_time_of_peak) %>% 
                          full_join(ungroup(exp.design)),
                      aes(x=tp_in_hours, y=sd, group=grouper,
                       color=factor(colorer), 
                       linetype=factor(linetyper)))+
            custom_color +
            labs(x="Hours", y="Worm density\n(Standard deviation of pixel intensity)", 
                 color=NULL) +
            theme_sophie
        if("facet_wrap" %in% colnames(sd.table)){ p = p + facet_wrap(~facet_wrap) }
        if("facet_row" %in% colnames(sd.table)){ p = p + facet_wrap(facet_row~facet_col) }
        if(!"linetyper" %in% colnames(exp.design)){p = p + guides(linetype="none")}
        if(!"colorer" %in% colnames(exp.design)){p = p + guides(color="none")}
        
        print(p)
        ggsave(paste0(save.to.folder, "/trimmed_sd_curve", p.suffix, ".pdf"), device = "pdf",
               width = plot.width, height = plot.height) 
    }
    
    # Ghost curve
    if(plot.ghost.curve){
        p = ggplot() +
            geom_line(data = plotter.table,
                      aes(x=tp_in_hours, y=sd, group=grouper,
                          color=factor(colorer), 
                          linetype=factor(linetyper)), alpha=.1)+
            geom_line(data = plotter.table%>% 
                          filter(!tm(timepoint)<sd_time_of_valley, !tm(timepoint)>sd_time_of_peak)  %>% 
                          full_join(ungroup(exp.design)) ,
                      aes(x=tp_in_hours, y=sd, group=grouper,
                          color=factor(colorer), 
                          linetype=factor(linetyper)))+
            custom_color +
            labs(x="Hours", y="Worm density\n(Standard deviation of pixel intensity)", 
                 color=NULL) +
            theme_sophie
        if("facet_wrap" %in% colnames(sd.table)){ p = p + facet_wrap(~facet_wrap) }
        if("facet_row" %in% colnames(sd.table)){ p = p + facet_wrap(facet_row~facet_col) }
        if(!"linetyper" %in% colnames(exp.design)){p = p + guides(linetype="none")}
        if(!"colorer" %in% colnames(exp.design)){p = p + guides(color="none")}
        
        print(p)
        ggsave(paste0(save.to.folder, "/ghost_sd_curve", p.suffix, ".pdf"), device = "pdf",
               width = plot.width, height = plot.height) 
    }
    
    # Time to starve
    if(plot.time.to.starve){
        p = plotter.table %>% 
            select(intersect(colnames(plotter.table),
                             c("grouper", "colorer", "hours_to_starve", "facet_wrap", "facet_row", "facet_col"))) %>% 
            distinct() %>% 
            ggplot(aes(x=colorer,
                       y=hours_to_starve, 
                       color=colorer))+
            geom_point(size=3)+
            custom_color +
            geom_point(shape=1, color="black", size=3)+
            facet_wrap(~facet_wrap, dir = "v")+
            labs(x=NULL, y="Hours to consume food", color=NULL)+
            theme_sophie
        print(p)
        ggsave(paste0(save.to.folder, "/hours_to_consume_food", p.suffix, ".pdf"), device = "pdf",
               width = plot.width, height = plot.height )
    }
}
