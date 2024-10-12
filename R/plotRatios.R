#' Takes a growth curve table and meta_with_minmax table and outputs an assortment of plots
#'
#' @param sd.table A table with one row per image captured, with standard deviation of pixel intensity already calculated.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param save.to.folder The folder where images will be saved. Defaults to "./tmp_plots".
#' @param ratio.trait Name of the column which describes the variable used in sensitivity score ratios. Defaults to "colorer", which might be dose, depending on how you set up your meta.
#' @param plot.by.sens.score Plot ratios by sensitivity. Defaults to TRUE.
#' @param plot.ctrl.by.exp Plot ctrl values on one axis and experimental values on the other. Defaults to TRUE.
#' @param wide.view For ctrl-by-exp plot, make the x and y axes limits match, and add a 1:1 reference line. Makes square dotplot. Defaults to TRUE
#' @param plot.unfaceted.tts Plot time to starvation, all on one plot, with samples ordered by sensitivity score. Defaults to TRUE.
#' @param p.width Width of ratio plots. Defaults to 7.
#' @param p.height Height of ratio plots. Defaults to 5.
#' @import ggplot2
#' @import dplyr
#' @import reshape2
#' @import ggrepel
#' @export
#' @examples
#' plotRatios()

plotRatios <- function(sd.table, exp.design, 
                       save.to.folder="./tmp_plots", 
                       ratio.trait="colorer",
                       plot.by.sens.score = TRUE,
                       plot.ctrl.by.exp = TRUE,
                       wide.view = TRUE,
                       plot.unfaceted.tts = TRUE,
                       p.width = 7,
                       p.height = 5){
    library(ggplot2)
    library(dplyr)
    library(reshape2)
    theme_sophie = theme_bw()+
        theme(axis.line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              strip.background = element_rect(fill="white"),
              axis.text.x = element_text(angle = 60, hjust=1))

    if(!dir.exists(save.to.folder)){dir.create(save.to.folder)}
    
    # set colors
    if(length(unique(exp.design$colorer))>2){
        quit("ERROR: I can't handle multiple experimental conditions yet. If you want to make these plots, just give me two treatments you want ratios for.")
    }
    
    # Make ratio plot
    ratio.table = NULL
    ratio.group = "facet_wrap"
    trait.levels = unique(sd.table[,ratio.trait])
    avg.levels = paste0("avg_", trait.levels[[1]])
    sd.levels = paste0("sd_", trait.levels[[1]])
    
    ratio.table = sd.table %>% ungroup() %>%
        left_join(exp.design %>% ungroup()) %>% 
        filter(filterer==T) %>%   
        select(-c(intersect(c("sd_peak_value", "sd_time_of_peak", "sd_valley_value", "sd_time_of_valley", 
                              "scan.prefix", "scan.position", "scan.dir", "trim.before", "trim.after","path.to.scan", 
                              "path.to.crop"), colnames(sd.table))))
    ratio.table = ratio.table %>% 
        mutate(!!sym(ratio.trait) := factor(!!sym(ratio.trait)),
               !!sym(ratio.group) := factor(!!sym(ratio.group)) )
    ratio.table.long = ratio.table %>% group_by_at(c(ratio.trait, ratio.group)) %>%
        summarise(avg_time_to_starve = mean(hours_to_starve, na.rm = T),
                  ci_sd = sd(hours_to_starve, na.rm=T)) 
    
    ratio.table = full_join(ratio.table.long %>% 
                                select(-ci_sd) %>% 
                                dcast(get(ratio.group) ~ get(ratio.trait)) %>%
                                rename("facet_wrap" = "get(ratio.group)",
                                       !!avg.levels[1] := trait.levels[[1]][1],
                                       !!avg.levels[2] := trait.levels[[1]][2]),
                            ratio.table.long %>% 
                                select(-avg_time_to_starve) %>% 
                                dcast(get(ratio.group) ~ get(ratio.trait)) %>%
                                rename("facet_wrap" = "get(ratio.group)",
                                       !!sd.levels[1] := trait.levels[[1]][1],
                                       !!sd.levels[2] := trait.levels[[1]][2])) %>%
        # Calculate sensitivity score and confidence interval (error propagation of standard deviation
        # Error prop is ()
        mutate(sens_score = get(avg.levels[2])/get(avg.levels[1]),
               sd_error_prop = sens_score * sqrt(((get(sd.levels[1])/get(avg.levels[1]))^2)+((get(sd.levels[2])/(get(avg.levels[2])))^2)))
    
    # Ordered by sensitivity score
    if(plot.by.sens.score){
        ratio.table$facet_wrap = factor(ratio.table$facet_wrap, 
                                        levels = ratio.table %>% arrange(sens_score) %>% 
                                            select(facet_wrap) %>% unlist())
        
        p = ggplot()+
            geom_segment(data=ratio.table, aes(x=facet_wrap, xend = facet_wrap, 
                                               y=sens_score-sd_error_prop, yend = sens_score+sd_error_prop), color="gray")+
            geom_point(data = ratio.table, aes(x=facet_wrap, y=sens_score))+
            geom_hline(yintercept = 1)+
            labs(y="Treatment / Ctrl (hours to starvation)", x=NULL)+
            theme_sophie
        print(p)
        
        ggsave(paste0(save.to.folder, "/ratios_by_sens_score.pdf"), device = "pdf",
               width = p.width, height = p.height )
        
    }
    
    # Plot by avg ctrl vs avg experiment for each strain
    if(plot.ctrl.by.exp){
       
        library(ggrepel)
        p = ggplot(data = ratio.table %>% filter(!is.na(sens_score)))+
            geom_segment(aes(x = !!sym(avg.levels[1]) - !!sym(sd.levels[1]),
                                                 xend = !!sym(avg.levels[1]) + !!sym(sd.levels[1]),
                                                 y = !!sym(avg.levels[2]), yend = !!sym(avg.levels[2])), color="gray")+
            geom_segment(aes(x = !!sym(avg.levels[1]), xend = !!sym(avg.levels[1]),
                                                 y = !!sym(avg.levels[2]) - !!sym(sd.levels[2]),
                                                 yend = !!sym(avg.levels[2]) + !!sym(sd.levels[2])), color="gray")+
            geom_point(aes(x= !!sym(avg.levels[1]), 
                           y= !!sym(avg.levels[2])))+

            xlab("Ctrl time to consume food")+
            ylab("Treatment time to consume food")+
            geom_text_repel(aes(label = !!sym(ratio.group),
                                 x= !!sym(avg.levels[1]), 
                                 y= !!sym(avg.levels[2])))+  # hjust=0.5, vjust=0, force = .5, nudge_y = .005, nudge_x = 5, size=2
            theme_sophie
  
        print(p)
        
        ggsave(paste0(save.to.folder, "/ratios_ctrl_by_exp.pdf"), device = "pdf",
               width = p.width, height = p.height )
        
        if(wide.view){
            tmp.min = min(c(ratio.table[,avg.levels[1]]-ratio.table[,sd.levels[1]],
                            ratio.table[,avg.levels[2]]-ratio.table[,sd.levels[2]] ), na.rm = T)
            tmp.max = max(c(ratio.table[,avg.levels[1]]+ratio.table[,sd.levels[1]],
                            ratio.table[,avg.levels[2]]+ratio.table[,sd.levels[2]] ), na.rm = T)
            p = p +
                xlim(c(tmp.min, tmp.max))+
                ylim(c(tmp.min, tmp.max))+
                geom_abline(intercept = 0, slope = 1, linetype="dotted", color = "gray")+
                coord_fixed()
            p.width = p.height
        }
        print(p)
        
        ggsave(paste0(save.to.folder, "/ratios_ctrl_by_exp_wide.pdf"), device = "pdf",
               width = p.width, height = p.height )
        
    }
    
    
    
    
    # Time to starve, unfaceted, ordered by sensitivity score
    
    if(plot.unfaceted.tts){
        ratio.table$facet_wrap = factor(ratio.table$facet_wrap,
                                        levels = ratio.table %>% arrange(sens_score) %>%
                                            select(facet_wrap) %>% unlist())
        
        plotter.table = sd.table %>%
            select(grouper, timepoint, sd) %>% left_join(exp.design)
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
        plotter.table = plotter.table %>%
            select(intersect(colnames(plotter.table),
                             c("grouper", "colorer", "hours_to_starve", "facet_wrap", "facet_row", "facet_col"))) %>%
            distinct()
        plotter.table$facet_wrap = factor(plotter.table$facet_wrap,
                                          levels = ratio.table %>% arrange(sens_score) %>%
                                              select(facet_wrap) %>% unlist())
        ratio.table.long$facet_wrap = factor(ratio.table.long$facet_wrap, 
                                             levels = ratio.table %>% arrange(sens_score) %>%
                                                 select(facet_wrap) %>% unlist())
        
        p = ggplot()+
            geom_segment(data = ratio.table.long, 
                         aes(x = facet_wrap, group = colorer, 
                             y = (avg_time_to_starve - ci_sd),
                             yend = (avg_time_to_starve + ci_sd)),
                         position = position_dodge(width = .25), color="black", alpha = .4, linewidth = 1)+
            geom_point(data = plotter.table,
                       aes(x=facet_wrap,
                           y=hours_to_starve,
                           color=colorer, 
                           group = colorer),
                       size=3, position = position_dodge(width = .25))+
            scale_color_manual(values = c("#4d9221", "#c51b7d"), limits=unique(exp.design$colorer))+
            geom_point(data = plotter.table,
                       aes(x=facet_wrap,
                           y=hours_to_starve,
                           group = colorer),shape=1, color="black", size=3, position = position_dodge(width = .25))+
             labs(y="Hours to consume food", color=NULL, x = NULL)+
            geom_segment(data = ratio.table.long, 
                         aes(x = facet_wrap, group = colorer, 
                             y = (avg_time_to_starve - ci_sd),
                             yend = (avg_time_to_starve + ci_sd)),
                         position = position_dodge(width = .25), color="black", alpha = .2, linewidth = 1)+
            
            theme_sophie
        print(p)
        
        ggsave(paste0(save.to.folder, "/timetostarve_by_sensscore.pdf"), device = "pdf",
               width = 1.3*p.width, height = .8*p.height )
        
    }
    
}

