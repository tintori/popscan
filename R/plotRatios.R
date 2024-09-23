#' Takes a growth curve table and meta_with_minmax table and outputs an assortment of plots
#'
#' @param sd.table A table with one row per image captured, with standard deviation of pixel intensity already calculated.
#' @param exp.design A table with one row per culture/plate/petri dish. Each column adds info about how to group, facet, color, etc the data points.
#' @param save.to.folder The folder where images will be saved. Defaults to "./tmp_plots".
#' @param plot.by.sens.score Plot ratios by sensitivity. Defaults to TRUE.
#' @param plot.ctrl.by.exp Plot ctrl values on one axis and experimental values on the other. Defaults to TRUE.
#' @param p.width Width of ratio plots. Defaults to 7.
#' @param p.height Height of ratio plots. Defaults to 5.
#' @export
#' @examples
#' plotRatios()

plotRatios <- function(sd.table, exp.design, 
                       save.to.folder="./tmp_plots", 
                       plot.by.pval = T,
                       plot.by.ctrl = T,
                       plot.by.sens.score = T,
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
    
    # set how many/which colors
    pinkgreen8 = c("#4d9221", "#7fbc41", "#b8e186", "#e6f5d0", "#fde0ef", "#f1b6da", "#de77ae", "#c51b7d", "#821252", "#960EBE", "#6600FF", "#5555FF", "#44AAFF", "#33FFFF")
    color_adding_order = c(1,8,6,3,7,2,5,4,9,10,11,12,13,14)
    custom_color = NULL
    if(length(unique(exp.design$colorer))%in%c(1,2)){ custom_color = scale_color_manual(values = "black", limits=unique(exp.design$colorer))}
    if(length(unique(exp.design$colorer)) %in% c(3:14)) {
        custom_color = scale_color_manual(values = pinkgreen8[sort(c(color_adding_order[2:length(unique(exp.design$colorer))]))],
                                          limits=unique(exp.design$colorer))
    }
    
    if(length(unique(exp.design$colorer))>2){
        quit("ERROR: I can't handle multiple experimental conditions yet. If you want to make these plots, just give me two treatments you want ratios for.")
    }
    
    # Make ratio plot
    ratio.table = NULL
    ratio.trait = "colorer"
    ratio.group = "facet_wrap"
    trait.levels = unique(sd.table[,ratio.trait])
    avg.levels = paste0("avg_", trait.levels)
    sd.levels = paste0("sd_", trait.levels)
    
    ratio.table = sd.table %>%
        left_join(exp.design) %>% 
        filter(filterer==T) %>%   
        select(-c(sd_peak_value, sd_time_of_peak, sd_valley_value, sd_time_of_valley, scan.prefix, scan.position, scan.dir, trim.before, trim.after,path.to.scan, path.to.crop))
    ratio.table[,ratio.trait] = as.factor(ratio.table[,ratio.trait])
    ratio.table[,ratio.group] = as.factor(ratio.table[,ratio.group])
    ratio.table = ratio.table %>% group_by_at(c(ratio.trait, ratio.group)) %>%
        summarise(avg_time_to_starve = mean(hours_to_starve, na.rm = T),
                  ci_sd = sd(hours_to_starve, na.rm=T)) 
    ratio.table = full_join(ratio.table %>% 
                                select(-ci_sd) %>% 
                                dcast(get(ratio.group) ~ get(ratio.trait)) %>%
                                rename("facet_wrap" = "get(ratio.group)",
                                       !!avg.levels[1] := trait.levels[1],
                                       !!avg.levels[2] := trait.levels[2]),
                            ratio.table %>% 
                                select(-avg_time_to_starve) %>% 
                                dcast(get(ratio.group) ~ get(ratio.trait)) %>%
                                rename("facet_wrap" = "get(ratio.group)",
                                       !!sd.levels[1] := trait.levels[1],
                                       !!sd.levels[2] := trait.levels[2])) %>%
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
            xlab("Strain")+
            ylab("Treatment / Ctrl (hours to starvation)")+
            theme_sophie
        print(p)
        
        ggsave(paste0(save.to.folder, "/ratios_by_sens_score.pdf"), device = "pdf",
               width = p.width, height = p.height )
        
    }
    
    # Eventually, add one that plots avg ctrl on the x axis and avg exp on the y axis.
    
    
    
}

