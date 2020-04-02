require(reshape2)
require(lme4)
require(tidyverse)
setwd(here::here(""))
read_csv(here::here("data/meta_data/model1.meta.csv")) %>% mutate(se = sqrt(v))  %>% select(-v) %>%  mutate(lower = y - (qnorm(.95) * se), upper = y + (qnorm(.95) * se)) %>% rename(mean = y) %>% mutate(sig = sign(lower) == 1) -> individual.replications

individual.replications %>% select(LabID,DependentVariable,sig) %>% spread(DependentVariable,sig) %>% select(LabID,d250,d500,d750,d1000) %>% mutate(perfect = d250 == FALSE & d500 == TRUE & d750 == TRUE & d1000 == FALSE) -> perfect.replications

perfect.replications %>% filter(perfect == FALSE) %>% mutate(partial =  d500 == TRUE | d750 == TRUE) -> partial.replications

individual.replications %>% select(LabID,DependentVariable,sig) %>% spread(DependentVariable,sig) %>% select(LabID,d250,d500,d750,d1000) %>% mutate(any = d250 == TRUE | d500 == TRUE | d750 == TRUE | d1000 == TRUE) -> any.effects




data.files = c(here::here("data/processed_rdata/results1.Rdata"),
               here::here("data/processed_rdata/results2.Rdata"),
               here::here("data/processed_rdata/results3.Rdata"),
               here::here("data/processed_rdata/results4.Rdata"),
               here::here("data/processed_rdata/results1c.Rdata"),
               here::here("data/processed_rdata/results1b.Rdata"))

mdxs = c("m1.idx",
         "m2.idx",
         "m3.idx",
         "m4.idx",
         "m1c.idx",
         "m1b.idx")

fitnames = c("fit1.stats",
             "fit2.stats",
             "fit3.stats",
             "fit4.stats",
             "fit1c.stats",
             "fit1b.stats")

estimatenames = c("estimates1",
                  "estimates2",
                  "estimates3",
                  "estimates4",
                  "estimates1c",
                  "estimates1b")

pwalk(list(data.files = data.files, 
           mdxs = mdxs,
           fitnames = fitnames,
           estimatenames = estimatenames),
    function(data.files,mdxs,fitnames,estimatenames) 
      {load(data.files)
      eval(parse( text = glue("{fitnames} <<- fit.stats")))
      eval(parse( text = glue("{estimatenames} <<- estimates")))
      eval(parse(text = glue("{mdxs} <<- which({fitnames}$AIC[,2] == min({fitnames}$AIC[,2]))")))})

dat = read.csv(here::here("data/final_data_csv/AllFischerData.csv"))


# Model 1
read_csv(here::here("data/meta_data/model1.meta.csv")) %>% group_by(ConditionDescription,DependentVariable) %>% summarise(N = sum(Freq)) %>% 
  unite("Condition",c("DependentVariable","ConditionDescription"), sep = " * ") %>% 
  ungroup() %>% select(Condition,N) %>% 
  merge.data.frame(estimates1$EAMMCS[[m1.idx]]$fe.estimates %>% 
                     tibble::rownames_to_column("Condition")) %>% 
  mutate(Condition = glue("{substr(Condition,2,5)} ms")) %>% mutate(`z` = Estimate / SE) %>% 
  mutate(Estimate = round(Estimate,2), SE = round(SE,2), `z` = round(`z`,2))  %>% 
  arrange(c(4,1,2,3)) %>% rename(`Std. Err.` = SE, `$z$` = z, `$n$` = N, `ISI Condition` = Condition)-> model1.fe

model1.fe %>% mutate(sig = ifelse(abs(`$z$`) > qnorm(.95), ">","<")) %>% mutate(stat = glue("|{`$z$`}| {sig} {round(qnorm(.95),2)}")) %>% 
  mutate(ISI = case_when(grepl(`ISI Condition`,pattern = "250") == TRUE ~ 250,
                         grepl(`ISI Condition`,pattern = "500") == TRUE ~ 500,
                         grepl(`ISI Condition`,pattern = "750") == TRUE ~ 750,
                         grepl(`ISI Condition`,pattern = "1000") == TRUE ~ 1000)) -> model1.fe.stats


# Model 2

dat %>% group_by(Lab,FingerGroup) %>% summarise(n = n()) %>% spread(FingerGroup,n) %>% ungroup() %>% summarise(LS = sum(LS), RS = sum(RS)) %>% 
  walk({LS.n <<- .$LS; RS.n <<- .$RS})

 read_csv(here::here("data/meta_data/model2.meta.csv")) %>% group_by(ConditionDescription,DependentVariable) %>% summarise(N = sum(Freq)) %>% 
  unite("Condition",c("DependentVariable","ConditionDescription"), sep = " * ") %>% 
  ungroup() %>% select(Condition,N) %>% merge.data.frame(estimates2$EAMMCS[[m2.idx]]$fe.estimates %>% tibble::rownames_to_column("Condition")) %>% mutate(A = str_trim(substr(Condition,2,5), "both"), A = as.numeric(A)) %>% mutate(A2 =  ifelse(grepl(Condition, pattern = "LS"), "B","A")) %>% arrange(A,A2) %>% mutate(`Delay condition` = glue("{substr(Condition,2,5)} ms")) %>% mutate(`Finger counting group` = ifelse(grepl(Condition, pattern = "LS"), "Left-starter","Right-starter")) %>% select(`Delay condition`,`Finger counting group`,N,Estimate,SE) %>% mutate(Estimate = round(Estimate,2), SE = round(SE,2), z = round(Estimate/SE,2)) %>% rename(`Std. Err.` = SE, `$z$` = z, `$n$` = N, `ISI Condition` = `Delay condition`) -> model2.fe

model2.fe %>% mutate(sig = ifelse(abs(`$z$`) > qnorm(.95), ">","<")) %>% mutate(stat = glue("|{`$z$`}| {sig} {round(qnorm(.95),2)}")) %>% 
  mutate(ISI = case_when(grepl(`ISI Condition`,pattern = "250") == TRUE ~ 250,
                         grepl(`ISI Condition`,pattern = "500") == TRUE ~ 500,
                         grepl(`ISI Condition`,pattern = "750") == TRUE ~ 750,
                         grepl(`ISI Condition`,pattern = "1000") == TRUE ~ 1000)) %>%
  rename(Group = `Finger counting group`)-> model2.fe.stats


# Model 3

dat %>% group_by(Lab,ReadingDir) %>% summarise(n = n()) %>% filter(n >= 5) %>% spread(ReadingDir,n) %>% ungroup() %>%
  walk({LTR.n <<- sum(.$LTR, na.rm = T)
        NLR.n <<- sum(.$NLR, na.rm = T)})

read_csv(here::here("data/meta_data/model3.meta.csv")) %>% group_by(ConditionDescription,DependentVariable) %>% summarise(N = sum(Freq)) %>% 
  unite("Condition",c("DependentVariable","ConditionDescription"), sep = " * ") %>% 
  ungroup() %>% select(Condition,N) %>% merge.data.frame(estimates3$EAMMCS[[m3.idx]]$fe.estimates %>% tibble::rownames_to_column("Condition")) %>% mutate(A = str_trim(substr(Condition,2,5), "both"), A = as.numeric(A)) %>% mutate(A2 =  ifelse(grepl(Condition, pattern = "LH"), "B","A")) %>% arrange(A,A2)  %>% mutate(`Delay condition` = glue("{substr(Condition,2,5)} ms")) %>% mutate(`Reading direction` = ifelse(grepl(Condition, pattern = "LTR"), "Exclusively LTR","Not exclusively LTR"))  %>% select(`Delay condition`,`Reading direction`,N,Estimate,SE) %>% mutate(Estimate = round(Estimate,2), SE = round(SE,2), z = round(Estimate/SE,2))  %>% rename(`Std. Err.` = SE, `$z$` = z, `$n$` = N, `ISI Condition` = `Delay condition`) -> model3.fe

model3.fe %>% mutate(sig = ifelse(abs(`$z$`) > qnorm(.95), ">","<")) %>% mutate(stat = glue("|{`$z$`}| {sig} {round(qnorm(.95),2)}")) %>% 
  mutate(ISI = case_when(grepl(`ISI Condition`,pattern = "250") == TRUE ~ 250,
                         grepl(`ISI Condition`,pattern = "500") == TRUE ~ 500,
                         grepl(`ISI Condition`,pattern = "750") == TRUE ~ 750,
                         grepl(`ISI Condition`,pattern = "1000") == TRUE ~ 1000)) %>%
  rename(Group = `Reading direction`, Condition = `ISI Condition`) -> model3.fe.stats


# Model 4

dat %>% group_by(Lab,Hand) %>% summarise(n = n()) %>% spread(Hand,n) %>% ungroup() %>% mutate(LH.labs = LH > 4, RH.labs = RH > 4, LH.include = LH * LH.labs, RH.include = RH * RH.labs) %>% 
  walk({LH.n <<- .$LH.include %>% sum(); RH.n <<- .$RH.include %>% sum()})

read_csv(here::here("data/meta_data/model4.meta.csv")) %>% group_by(ConditionDescription,DependentVariable) %>% summarise(N = sum(Freq)) %>% 
  unite("Condition",c("DependentVariable","ConditionDescription"), sep = " * ") %>% 
  ungroup() %>% select(Condition,N) %>% merge.data.frame(estimates4$EAMMCS[[m4.idx]]$fe.estimates %>% tibble::rownames_to_column("Condition")) %>% mutate(A = str_trim(substr(Condition,2,5), "both"), A = as.numeric(A)) %>% mutate(A2 =  ifelse(grepl(Condition, pattern = "LH"), "B","A")) %>% arrange(A,A2)  %>% mutate(`Delay condition` = glue("{substr(Condition,2,5)} ms")) %>% mutate(`Handedness group` = ifelse(grepl(Condition, pattern = "LH"), "Left-handed","Right-handed")) %>% select(`Delay condition`,`Handedness group`,N,Estimate,SE) %>% mutate(Estimate = round(Estimate,2), SE = round(SE,2), z = round(Estimate/SE,2)) %>% rename(`Std. Err.` = SE, `$z$` = z, `$n$` = N, `ISI Condition` = `Delay condition`) -> model4.fe

model4.fe %>% mutate(sig = ifelse(abs(`$z$`) > qnorm(.95), ">","<")) %>% mutate(stat = glue("|{`$z$`}| {sig} {round(qnorm(.95),2)}")) %>% 
  mutate(ISI = case_when(grepl(`ISI Condition`,pattern = "250") == TRUE ~ 250,
                         grepl(`ISI Condition`,pattern = "500") == TRUE ~ 500,
                         grepl(`ISI Condition`,pattern = "750") == TRUE ~ 750,
                         grepl(`ISI Condition`,pattern = "1000") == TRUE ~ 1000)) %>%
  rename(Group = `Handedness group`)-> model4.fe.stats


# Model 1 b

Model1b.n = read_csv(here::here("data/meta_data/model1b.meta.csv")) %>% select(LabID,Freq) %>% distinct() %>% pull(Freq) %>% sum()

Model1b.nlabs = read_csv(here::here("data/meta_data/model1b.meta.csv")) %>% pull(LabID) %>% unique() %>% length() %>% english::as.english()

read_csv(here::here("data/meta_data/model1b.meta.csv")) %>% group_by(ConditionDescription,DependentVariable) %>% summarise(N = sum(Freq)) %>% 
  unite("Condition",c("DependentVariable","ConditionDescription"), sep = " * ") %>% 
  ungroup() %>% select(Condition,N) %>% 
  merge.data.frame(estimates1b$EAMMCS[[m1.idx]]$fe.estimates %>% 
                     tibble::rownames_to_column("Condition")) %>% 
  mutate(Condition = glue("{substr(Condition,2,5)} ms")) %>% mutate(`z` = Estimate / SE) %>% 
  mutate(Estimate = round(Estimate,2), SE = round(SE,2), `z` = round(`z`,2))  %>% 
  arrange(c(4,1,2,3)) %>% rename(`Std. Err.` = SE, `$z$` = z, `$n$` = N, `ISI Condition` = Condition)-> model1b.fe

model1b.fe %>% mutate(sig = ifelse(abs(`$z$`) > qnorm(.95), ">","<")) %>% mutate(stat = glue("|{`$z$`}| {sig} {round(qnorm(.95),2)}")) %>% 
  mutate(ISI = case_when(grepl(`ISI Condition`,pattern = "250") == TRUE ~ 250,
                         grepl(`ISI Condition`,pattern = "500") == TRUE ~ 500,
                         grepl(`ISI Condition`,pattern = "750") == TRUE ~ 750,
                         grepl(`ISI Condition`,pattern = "1000") == TRUE ~ 1000)) -> model1b.fe.stats

# Model 1 c

Model1c.n  = read_csv(here::here("data/meta_data/model1c.meta.csv")) %>% select(LabID,Freq) %>% distinct() %>% pull(Freq) %>% sum()
Model1c.nlabs = read_csv(here::here("data/meta_data/model1c.meta.csv")) %>% pull(LabID) %>% unique() %>% length() %>% english::as.english()


read_csv(here::here("data/meta_data/model1c.meta.csv")) %>% group_by(ConditionDescription,DependentVariable) %>% summarise(N = sum(Freq)) %>% 
  unite("Condition",c("DependentVariable","ConditionDescription"), sep = " * ") %>% 
  ungroup() %>% select(Condition,N) %>% 
  merge.data.frame(estimates1c$EAMMCS[[m1.idx]]$fe.estimates %>% 
                     tibble::rownames_to_column("Condition")) %>% 
  mutate(Condition = glue("{substr(Condition,2,5)} ms")) %>% mutate(`z` = Estimate / SE) %>% 
  mutate(Estimate = round(Estimate,2), SE = round(SE,2), `z` = round(`z`,2))  %>% 
  arrange(c(4,1,2,3)) %>% rename(`Std. Err.` = SE, `$z$` = z, `$n$` = N, `ISI Condition` = Condition)-> model1c.fe

model1c.fe %>% mutate(sig = ifelse(abs(`$z$`) > qnorm(.95), ">","<")) %>% mutate(stat = glue("|{`$z$`}| {sig} {round(qnorm(.95),2)}")) %>% 
  mutate(ISI = case_when(grepl(`ISI Condition`,pattern = "250") == TRUE ~ 250,
                         grepl(`ISI Condition`,pattern = "500") == TRUE ~ 500,
                         grepl(`ISI Condition`,pattern = "750") == TRUE ~ 750,
                         grepl(`ISI Condition`,pattern = "1000") == TRUE ~ 1000)) -> model1c.fe.stats


# Model 5 
# This section was written by Blake

dat <- read.csv(here::here("data/final_data_csv/AllFischerData.csv"), header=TRUE)

# Center and scale MathTest and AMAS to improve convergence:
dat$MathTest <- scale(dat$MathTest)
dat$AMAS <- scale(dat$AMAS)

# Prepare data for individual-level models:
dd <- melt(dat[,c("d250","d500","d750","d1000")])
dd$SubjectID <- rep(dat$Code, 4)
dd$LabID <- rep(dat$Lab, 4)
dd$FingerGroup <- rep(dat$FingerGroup, 4)
dd$Hand <- rep(dat$Hand, 4)
dd$ReadingDir <- rep(dat$ReadingDir, 4)
dd$MathTest <- rep(dat$MathTest, 4)
dd$AMAS <- rep(dat$AMAS, 4)
dd$varLabID <- paste(dd$variable, dd$LabID, sep="*")
dd$varSubjectID <- paste(dd$variable, dd$SubjectID, sep="*")

###################
# Fit lmer models #
###################


m2a <- lmer(value~variable*MathTest*AMAS-1-MathTest-AMAS-MathTest*AMAS + 
              (1|varLabID) + (MathTest*AMAS-1||LabID) + (1|SubjectID), data=dd)

Estimates = summary(m2a)$coefficients %>% as.tibble() %>% add_column(Name = row.names(summary(m2a)$coefficients), .before = 1) 
Estimates$Lower = Estimates$Estimate - (qnorm(.975) * Estimates$`Std. Error`)
Estimates$Upper = Estimates$Estimate + (qnorm(.975) * Estimates$`Std. Error`)


Estimates$Name %>% stringr::str_replace_all(pattern = "variabled250", "250 ms ISI") %>%
  stringr::str_replace_all(pattern = "variabled500", "500 ms ISI") %>%
  stringr::str_replace_all(pattern = "variabled750", "750 ms ISI") %>%
  stringr::str_replace_all(pattern = "variabled1000", "1000 ms ISI") %>%
  stringr::str_replace_all(pattern = ":MathTest", " $\\\\times$ Maths test") %>%
  stringr::str_replace_all(pattern = ":AMAS", " $\\\\times$ AMAS") -> Estimates$Name 

m2a.n = dd$SubjectID %>% unique() %>% length()

Estimates %>% select(-Lower,-Upper) %>% rename(`$t$` = `t value`, `Std. Err.` = `Std. Error`) %>% 
  mutate(Estimate = round(Estimate,2), `Std. Err.` = round(`Std. Err.`,2), `$t$` = round(`$t$`,2)) -> Estimates
Estimates %>% add_column(`$n$` = m2a.n, .before = "Estimate") -> Estimates
RandomEffect  = summary(m2a)$varcor

model5.fe.stats = Estimates
model5.fe.stats %>% mutate(critical = qnorm(p = .975), sig = ifelse(`$t$` > critical, '>','<'), stat = glue("|{`$t$`}| {sig} {round(critical,2)}")) -> model5.fe.stats



ReplicationProjectTools::GetLabNames()


save.image(here::here("manuscript_files/manuscript.RData"))
