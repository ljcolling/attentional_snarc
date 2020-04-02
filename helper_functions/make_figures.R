# Make the Figures for Model 1
library(ggplot2)
library(reshape2)
library(forestplot)
require(tidyverse)
require(glue)
#setwd("/Users/blakemcshane/Documents/Academic Research/MetaAnalysis/RRR/fischer_att_snarc/analysis")
ReplicationProjectTools::GetLabNames()
require(ReplicationProjectTools)
EyeTrackerTable =  read_csv(here::here("data/processed_data/EyeTrackerTable.csv"))
EyeTrackerTable %>% group_by(Node) %>% summarise(LabsWithEye = sum(HasEye)) %>% filter(LabsWithEye > 0) %>% pull(Node) -> LabsWithEye
LabNames[Nodes %in% LabsWithEye] -> LabsWithEye 

load(here::here("data/processed_rdata/results1.Rdata"))
Fischer.dat = read.csv(here::here("other_info/Original_estimates.csv"))

Fischer.dat %>% mutate(lower = RT - (qnorm(.95) * se), upper = RT + (qnorm(.95) * se)) -> Fischer.dat

# Get estimates for best model fit for Model 1
m1.idx <- which(fit.stats$AIC[,2] == min(fit.stats$AIC[,2]))
estimates$EAMMCS[[m1.idx]]

dat = read.csv(here::here("data/meta_data/model1.meta.csv"))

DependentVariables = c("d250","d500","d750","d1000")



for(this.DependentVariable in DependentVariables){
  
  this.DependentVariable.idx = grepl(estimates$EAMMCS[[m1.idx]]$fe.estimates %>% row.names(),pattern = this.DependentVariable) %>% which()
  
  
  summary.mean = estimates$EAMMCS[[m1.idx]]$fe.estimates$Estimate[this.DependentVariable.idx] 
  summary.se = estimates$EAMMCS[[m1.idx]]$fe.estimates$SE[this.DependentVariable.idx]
  
  select<- dplyr::select
  dat %>% filter(DependentVariable == this.DependentVariable) %>% mutate(se = sqrt(v)) %>% select(LabID,y,se,Freq) %>% mutate(lower = y - (qnorm(.95) * se), upper = y + (qnorm(.95) * se)) %>% rename(mean = y) %>% select(LabID,mean,lower,upper,Freq,se) %>% rename(n = Freq) %>%
    mutate(t = mean / se, df = n - 1, p = 2 * pt(abs(t), df = df, lower.tail = FALSE), 
           `t-test` = glue("t({df}) = {round(t,2)}, p = {round(p,3)}")) %>% select(LabID,mean,lower,upper,n,`t-test`) -> meta.dat
  GetLabNames()
  LabIDNames = data.frame(LabID = Nodes, names = LabNames)
  LabIDNames %>% mutate(names = ifelse(names %in% LabsWithEye, paste0(names,"*"), paste0(names))) -> LabIDNames
  
  merge(meta.dat,LabIDNames) %>% select(names,mean,lower,upper,n,`t-test`) %>% rename(LabID = names) -> meta.dat
  meta.dat %>% arrange(LabID) -> meta.dat
  
  summary.n = meta.dat$n %>% sum()
  
  Fischer.dat %>% filter(delay == this.DependentVariable %>% substr(start = 2, stop = 99) %>% as.numeric()) %>% 
    mutate(LabID = "Fischer et al. (2003)", mean = RT) %>% mutate(n = df + 1, `t-test` = glue("t({df}) = {round(t,2)}, p = {round(p.value,3)}")) %>% select(LabID,mean,lower,upper,n,`t-test`)  -> Fischer.sub
  
  rbind(Fischer.sub,meta.dat) -> meta.dat
  
  n.labs = dim(meta.dat)[1]
  
  as.padded.text<-function(x){
    if (is.na(x) == FALSE) {
      return(ifelse(x > 0,glue(" {sprintf('%.2f',x)}"),sprintf('%.2f',x)))
    } else {
      return(NA)
    }
  }
  
  rbind(data.frame(LabID = "Study", mean = NA, lower = NA, upper = NA, n = NA),
        meta.dat %>% select(LabID, mean, lower, upper, n),
        data.frame(LabID = c(NA,NA,"Summary",NA), 
                   mean = c(NA,NA,summary.mean,NA), 
                   lower = c(NA,NA,summary.mean - (qnorm(.95) * summary.se),NA), 
                   upper = c(NA,NA,summary.mean + (qnorm(.95) * summary.se),NA),
                   n     = c(NA,NA,summary.n,NA))) -> meta.dat
  
  map_chr(1:dim(meta.dat)[1],function(x) as.padded.text(x = (meta.dat  %>% pull(mean))[x])) -> means
  map_chr(1:dim(meta.dat)[1],function(x) as.padded.text(x = (meta.dat  %>% pull(upper))[x])) -> uppers
  map_chr(1:dim(meta.dat)[1],function(x) as.padded.text(x = (meta.dat  %>% pull(lower))[x])) -> lowers
  meta.dat$meantext = means
  meta.dat$lowertext = lowers
  meta.dat$uppertext = uppers
  meta.dat %>% mutate(estimate = glue("{meantext} ms [{lowertext}, {uppertext}]")) -> meta.dat
  meta.dat$estimate[1] = "Estimate [90% CI]"
  meta.dat$n[1] = "n"
  #meta.dat$estimate[dim(meta.dat)[1]-1] = ""
  meta.dat$estimate[dim(meta.dat)[1]-2] = ""
  meta.dat$estimate[dim(meta.dat)[1]-3] = ""
  meta.dat$estimate[dim(meta.dat)[1]] = ""
  
  min.value = floor(meta.dat$lower[meta.dat$lower %>% is.na() == FALSE] %>% min())
  max.value = ceiling(meta.dat$upper[meta.dat$upper %>% is.na() == FALSE] %>% max())
  
  nearestEvenDown<-function(x){
    if(mod(x,10) != 0){
      x = x - mod(x,10)
      return(x)
    } else {
      return(x)
    }
  }
  
  nearestEvenUp<-function(x){
    if(mod(x,10) != 0){
      x = x + mod(x,10)
      return(x)
    } else {
      return(x)
    }
  }
  
  xticks = seq(nearestEvenDown(min.value), nearestEvenUp(max.value), 10)
  
  hrzl_lines = list(upper = gpar(lty =1),
                    middle = gpar(lty =2),
                    lower = gpar(lty =1))
  names(hrzl_lines) <- c("1","3",as.character(n.labs  + 3))
  png(filename = glue("{here::here('manuscript_files')}/{this.DependentVariable}.png"), width =20, height = 15, units = "cm",res = 300)
  #cairo_pdf(file = glue("manuscript_files/{this.DependentVariable}.pdf"), width =8, height = 6, onefile = F, pointsize = 10)
  
  forestplot(labeltext = meta.dat %>% select(LabID,estimate,n) %>% mutate(LabID = as.vector(LabID), estimate = as.vector(estimate)),
             align = c("l","r","r"),
             graph.pos = 2,xticks = xticks,
             hrzl_lines = hrzl_lines,
             meta.dat %>% select(mean,lower,upper), 
             is.summary = c(TRUE,rep(FALSE,n.labs),TRUE,TRUE),
             xlab = "Estimate (ms)",txt_gp  = fpTxtGp(xlab = gpar(cex= 1), ticks = gpar(cex = .9)))#,
  #title = glue("{substr(this.DependentVariable,2,99)} ms ISI Condition"))
  dev.off()
  
}


#library(magick)
#d250 = image_read("d250.png")  %>% image_annotate("A", size = 85, font = "Helvetica")
#d500 = image_read("d500.png")  %>% image_annotate("B", size = 85, font = "Helvetica")
#d750 = image_read("d750.png")  %>% image_annotate("C", size = 85, font = "Helvetica")
#d1000 = image_read("d1000.png")%>% image_annotate("D", size = 85, font = "Helvetica")
#image_append(c(d250,d500)) -> topRow
#image_append(c(d750,d1000)) -> bottomRow
#image_append(c(topRow,bottomRow),stack = T) -> model1
#image_write(model1,"manuscript_files/model1.png",format = "png")
#system("rm d250.png")
#system("rm d500.png")
#system("rm d750.png")
#system("rm d1000.png")


# NEW FIGURES
theme_blank <- function (base_size = 12, base_family = "") {
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family) + 
     ggplot2::theme(axis.line.x = ggplot2::element_line(colour = "white"), 
                    axis.line.y = ggplot2::element_line(colour = "white"), 
                    axis.text = ggplot2::element_text(colour = "grey50"), 
                    axis.title = ggplot2::element_text(colour = "grey30"), 
                    panel.grid.minor = ggplot2::element_line(colour = "white", 
                                                             linetype = 1), panel.grid.major = ggplot2::element_line(colour = "white", 
                                                                                                                     linetype = 1)))
}

require(ReplicationProjectTools)
require(glue)
require(tidyverse)

meta.files = c(here::here("data/meta_data/model1.meta.csv"),
               here::here("data/meta_data/model2.meta.csv"),
               here::here("data/meta_data/model3.meta.csv"),
               here::here("data/meta_data/model4.meta.csv"))



map(meta.files, function(x) read_csv(x) %>% group_by(ConditionDescription, DependentVariable) %>% summarise(n = sum(Freq))) %>% bind_rows() -> subj.nums

results.files = c("results1.Rdata",
                  "results2.Rdata",
                  "results3.Rdata",
                  "results4.Rdata")


estimates.list = list()

for (x in 1:length(results.files)) {
  load(results.files[x])
  eval(parse(text = glue("estimates{x} = estimates")))
  eval(parse(text = glue("fit.stats{x} = fit.stats")))
  
  m.idx <- which(fit.stats$AIC[, 2] == min(fit.stats$AIC[, 2]))
  estimates$EAMMCS[[m.idx]]
  
  estimates.list[[x]] = estimates$EAMMCS[[m.idx]]$fe.estimates %>% as.data.frame() %>% rownames_to_column("Condition.Full")
  
  
}

estimates.list = bind_rows(estimates.list)

merge.data.frame(subj.nums %>% unite(Condition.Full,c("DependentVariable","ConditionDescription"), sep = " * "),estimates.list, by = "Condition.Full") -> estimates.list

estimates.list %>% mutate(
  Model = case_when(
    grepl(Condition.Full, pattern = "Not.Applicable") == TRUE ~ "Model 1",
    grepl(Condition.Full, pattern = "LS") == TRUE ~ "Model 2",
    grepl(Condition.Full, pattern = "RS") == TRUE ~ "Model 2",
    grepl(Condition.Full, pattern = "LH") == TRUE ~ "Model 3",
    grepl(Condition.Full, pattern = "RH") == TRUE ~ "Model 3",
    grepl(Condition.Full, pattern = "LTR") == TRUE   ~ "Model 4",
    grepl(Condition.Full, pattern = "NLR")  == TRUE ~ "Model 4"
  )
) %>%
  mutate(
    Condition = case_when(
      grepl(Condition.Full, pattern = "d250") == TRUE ~ "250 ms",
      grepl(Condition.Full, pattern = "d500") == TRUE ~ "500 ms",
      grepl(Condition.Full, pattern = "d750") == TRUE ~ "750 ms",
      grepl(Condition.Full, pattern = "d1000") == TRUE ~ "1000 ms"
    )
  ) %>%
  mutate(
    Moderator = case_when(
      grepl(Condition.Full, pattern = "Not.Applicable") == TRUE ~ "No moderators",
      grepl(Condition.Full, pattern = "LS") == TRUE ~ "Left-starter",
      grepl(Condition.Full, pattern = "RS") == TRUE ~ "Right-starter",
      grepl(Condition.Full, pattern = "LH") == TRUE ~ "Left-handed",
      grepl(Condition.Full, pattern = "RH") == TRUE ~ "Right-handed",
      grepl(Condition.Full, pattern = "LTR") == TRUE   ~ "Left-to-right",
      grepl(Condition.Full, pattern = "NLR")  == TRUE ~ "Not left-to-right"
    )
  ) -> estimates.list


sprintf.p<-function(fmt,value){
  ReplicationProjectTools::as.padded.text(value, pad.length = 6)  
}




read_csv(here::here("other_info/Original_estimates.csv")) -> Orginal.estimates

Orginal.estimates %>% mutate(Condition.Full = glue("d{delay} * Not.Applicable"), 
                             ConditionDescription = "Not.Applicable",
                             Estimate = RT, 
                             SE = se, 
                             n = 10, 
                             Model = "Fischer et al. (2003)",
                             Moderator = "No moderators",
                             DependentVariable = glue("d{delay}"),
                             Condition = glue("{delay} ms")) %>% select(Condition.Full, n, Estimate, SE, Model, Condition, Moderator) -> Orginal.estimates

rbind(Orginal.estimates,estimates.list) -> estimates.list
estimates.list = as.data.frame(estimates.list)

estimates.list$Condition = as.factor(estimates.list$Condition)

estimates.list$Model = as.factor(estimates.list$Model)

estimates.list$Condition <- factor(estimates.list$Condition, 
                                   levels = c("250 ms","500 ms","750 ms","1000 ms"))

estimates.list$Model <- factor(estimates.list$Model,
                               levels = c("Fischer et al. (2003)","Model 1","Model 2","Model 3","Model 4"))

estimates.list$mid = pmap_chr(as.list(estimates.list)[c("Estimate")], function(Estimate,SE) ifelse(Estimate - (qnorm(.95) * 0) >0, paste0(" " ,sprintf('%.2f',Estimate - (qnorm(.95) * 0))), sprintf('%.2f',Estimate - (qnorm(.95) * 0))))
estimates.list$ll = pmap_chr(as.list(estimates.list)[c("Estimate","SE")], function(Estimate,SE) ifelse(Estimate - (qnorm(.95) * SE) >0, paste0(" " ,sprintf('%.2f',Estimate - (qnorm(.95) * SE))), sprintf('%.2f',Estimate - (qnorm(.95) * SE))))
estimates.list$ul = pmap_chr(as.list(estimates.list)[c("Estimate","SE")], function(Estimate,SE) ifelse(Estimate + (qnorm(.95) * SE) >0, paste0(" " ,sprintf('%.2f',Estimate + (qnorm(.95) * SE))), sprintf('%.2f',Estimate + (qnorm(.95) * SE))))

estimates.list$mid[1] <- paste0(" ",estimates.list$mid[1]) 

estimates.list$ll[2] <- paste0(" ",estimates.list$ll[2]) 
estimates.list$ll[3] <- paste0(" ",estimates.list$ll[3]) 
estimates.list$ll[4] <- paste0(" ",estimates.list$ll[4]) 


estimates.list$ul[1] <- paste0(" ",estimates.list$ul[1]) 

estimates.list$text = as.list(estimates.list)[c("mid","ll","ul")] %>% glue_data("{mid} ms [{ll},{ul}]")


##### DRAW THE PLOTS For the remaining models 



##### VERSION 4

glue("{estimates.list$mid[1] %>% stringr::str_trim()} ms [{estimates.list$ll[1] %>% stringr::str_trim()}, {estimates.list$ul[1] %>% stringr::str_trim()}]") -> estimates.list$text[1]
glue("{estimates.list$mid[2] %>% stringr::str_trim()} ms [{estimates.list$ll[2] %>% stringr::str_trim()}, {estimates.list$ul[2] %>% stringr::str_trim()}]") -> estimates.list$text[2]
glue("{estimates.list$mid[3] %>% stringr::str_trim()} ms [{estimates.list$ll[3] %>% stringr::str_trim()}, {estimates.list$ul[3] %>% stringr::str_trim()}]") -> estimates.list$text[3]
glue("{estimates.list$mid[4] %>% stringr::str_trim()} ms [{estimates.list$ll[4] %>% stringr::str_trim()}, {estimates.list$ul[4] %>% stringr::str_trim()}]") -> estimates.list$text[4]

estimates.list %>% mutate(y = case_when(Model == "Fischer et al. (2003)" & Moderator == "No moderators" ~ 8,
                                        Model == "Model 1" & Moderator == "No moderators" ~ 7,
                                        Model == "Model 2" & Moderator == "Left-starter" ~ 6,
                                        Model == "Model 2" & Moderator == "Right-starter" ~ 5,
                                        Model == "Model 3" & Moderator == "Left-handed" ~ 4,
                                        Model == "Model 3" & Moderator == "Right-handed" ~ 3,
                                        Model == "Model 4" & Moderator == "Left-to-right" ~ 2,
                                        Model == "Model 4" & Moderator == "Not left-to-right" ~ 1,
                                        FALSE ~ NA_real_)) %>%
  mutate(label2 = pmap_chr(.l = list(Ml = as.character(Model), Mr = as.character(Moderator)), .f = function(Ml,Mr) glue("{Ml} ({Mr})"))) %>% ggplot() + 
  geom_errorbarh(aes(xmax = Estimate + (qnorm(.95) * SE), xmin = Estimate - (qnorm(.95) * SE), y = y), height = 0) + 
  geom_vline(xintercept = 0, linetype = 2, size = .2, colour = "grey")  +
  facet_grid(Condition ~ .) + scale_y_continuous(labels = NULL, limits = c(0,9)) + geom_point(aes(x = Estimate, y = y), shape = 15) + 
  xlab("Estimate (ms)") + geom_text(aes(65, y = y, label = text, hjust = 1)) + 
  geom_text(aes(-40, y = y, label = label2), size = 3, hjust = 0) +
  scale_x_continuous(limits = c(-40,65), breaks = seq(-20,40,5)) +  theme_blank() + ylab(NULL) + 
  theme(axis.title.x.bottom = element_text(hjust = (1/abs(-40-65)) * (abs(-40-10)))) + ggtitle(label = "Estimate [90% CI]", subtitle = "Model (Moderator group)") + 
  theme(plot.title = element_text(hjust = .94,size = 12, vjust = -6), plot.subtitle = element_text(vjust = 0, hjust = 0.05), legend.position = "none", text = element_text(family = "Helvetica", colour = "black")) + theme(panel.border  = element_rect(fill = NA, colour = 'black'),
                                                                                                                                                                                                                            panel.spacing.y = unit(x = 1.1, units = "picas")) ->p 
p

ggsave(plot = p, filename = here::here("manuscript_files/meta_summaryv4.png"), height = 12, width = 10)
#ggsave(plot = p, filename = "manuscript_files/meta_summaryv4.pdf", height = 12, width = 10, device = cairo_pdf)


