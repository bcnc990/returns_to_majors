library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(officer)
library(scales)
library(matrixStats)

programData=readRDS("programDataAnalysis.rds") %>%
  select(UNITID, INSTNM, STABBR, CIP2, CIP.Family, CIPCODE, CIPDESC, CREDLEV, CREDDESC, MAIN, CONTROL, OPEID6,
         starts_with("DEBT_ALL_STGP"), starts_with("DEBT_MALE_STGP"), starts_with("DEBT_NOTMALE_STGP"), 
         starts_with("DEBT_PELL_STGP"), starts_with("DEBT_NOPELL_STGP"), starts_with("EARN_"), starts_with("BBRR3_"),
         starts_with("debtPayment"), starts_with("monthly"), starts_with("annual"), reportAsGroup) %>%
  mutate(CIP.Family = str_replace(str_wrap(CIP.Family, 1000), "/ ", "/"),
         CIPDESC = str_replace(str_wrap(CIPDESC, 1000), "/ ", "/")) # unwrap for datatables
cipcts2 = readRDS("cip2cts.rds")
cipcts4 = readRDS("cip4cts.rds")

all = programData %>%
  filter(CREDLEV <= 3 | CREDLEV == 5) %>%
  group_by(CREDLEV, CREDDESC) %>%
  summarise(medianEarnings = weightedMedian(EARN_NE_MDN_3YR, EARN_COUNT_NE_3YR, na.rm = TRUE),
            medianDebt = weightedMedian(DEBT_ALL_STGP_EVAL_MDN, DEBT_ALL_STGP_EVAL_N, na.rm = TRUE),
            medianDebtPayments = weightedMedian(DEBT_ALL_STGP_EVAL_MDN10YRPAY*12, DEBT_ALL_STGP_EVAL_N, na.rm = TRUE),
            medianEarningsNetDebt = weightedMedian(annualEarningsNetDebt, EARN_COUNT_NE_3YR, na.rm = TRUE),
            numProgs = n()) %>%
  mutate(across(starts_with("median"), ~round(.x, -2)))

prepData = function(l) {
  cipcts2_ = cipcts2 %>%
    filter(CREDLEV == l, totalProgramsA >= 5)
  cipcts4_ = cipcts4 %>%
    filter(CREDLEV == l, totalProgramsA >= 5)
  
  cip2_lbls = cipcts2_ %>%
    ungroup() %>%
    select(CIP2, CIP.Family) %>%
    distinct()
  cip4_lbls = cipcts4_ %>%
    ungroup() %>%
    select(CIP2, CIP.Family, CIPCODE, CIPDESC) %>%
    distinct()
  
  Progs_ = programData %>%
    filter(CREDLEV == l) %>%
    mutate(debtPayments = DEBT_ALL_STGP_EVAL_MDN10YRPAY * 12) %>%
    select(UNITID, CIP2, CIP.Family, INSTNM, STABBR, CONTROL, CIPCODE, CIPDESC, medianAnnualEarnings = EARN_NE_MDN_3YR,
           medianDebt = DEBT_ALL_STGP_EVAL_MDN, medianDebtPayments = debtPayments, medianEarningsNetDebt = annualEarningsNetDebt) %>%
    mutate(across(starts_with("median"), ~round(.x, -2)))
  
  Majors2_ = programData %>%
    filter(CREDLEV == l) %>%
    group_by(CIP2, CIP.Family) %>%
    summarise(medianEarnings = weightedMedian(EARN_NE_MDN_3YR, EARN_COUNT_NE_3YR, na.rm = TRUE),
              medianDebt = weightedMedian(DEBT_ALL_STGP_EVAL_MDN, DEBT_ALL_STGP_EVAL_N, na.rm = TRUE),
              medianDebtPayments = weightedMedian(DEBT_ALL_STGP_EVAL_MDN10YRPAY*12, DEBT_ALL_STGP_EVAL_N, na.rm = TRUE),
              medianEarningsNetDebt = weightedMedian(annualEarningsNetDebt, EARN_COUNT_NE_3YR, na.rm = TRUE),
              numProgs = n()) %>%
    mutate(across(starts_with("median"), ~round(.x, -2))) %>%
    filter(numProgs >= 5) 
  
  Majors4_ = programData %>%
    filter(CREDLEV == l) %>%
    group_by(CIP2, CIP.Family, CIPCODE, CIPDESC) %>%
    summarise(medianEarnings = weightedMedian(EARN_NE_MDN_3YR, EARN_COUNT_NE_3YR, na.rm = TRUE),
              medianDebt = weightedMedian(DEBT_ALL_STGP_EVAL_MDN, DEBT_ALL_STGP_EVAL_N, na.rm = TRUE),
              medianDebtPayments = weightedMedian(DEBT_ALL_STGP_EVAL_MDN10YRPAY*12, DEBT_ALL_STGP_EVAL_N, na.rm = TRUE),
              medianEarningsNetDebt = weightedMedian(annualEarningsNetDebt, EARN_COUNT_NE_3YR, na.rm = TRUE),
              numProgs = n()) %>%
    mutate(across(starts_with("median"), ~round(.x, -2))) %>%
    filter(numProgs >= 5)

  return (list(cip2_lbls, cip4_lbls, Majors2_, Majors4_, Progs_))
  
}

ba = prepData(l=3)

ciplbls = data.frame(ba[1]) %>%
  mutate(CIP.Family = str_trim(CIP.Family)) %>%
  arrange(CIP.Family) %>%
  pull(CIP.Family)

baCip2 = all %>%
  ungroup() %>%
  mutate(CIP.Family = paste(CREDDESC, "(All)"), color="red") %>%
  bind_rows(data.frame(ba[3])) %>%
  select(CIP2, CF = CIP.Family, me = medianEarnings, md = medianDebt, mdp = medianDebtPayments, ne = medianEarningsNetDebt, N = numProgs, color)

baCip4 = all %>%
  ungroup() %>%
  mutate(CIP.Family = paste(CREDDESC, "(All)"), CIPDESC = paste(CREDDESC, "(All)"), color="red") %>%
  bind_rows(data.frame(ba[4])) %>%
  select(CIP2, CF = CIP.Family, CC = CIPCODE, CD = CIPDESC,
         me = medianEarnings, md = medianDebt, mdp = medianDebtPayments, ne = medianEarningsNetDebt, N = numProgs, color)

baProgs = data.frame(ba[5]) %>%
  select(inst = INSTNM, s = STABBR, c = CONTROL, CIP2, CF = CIP.Family, CC = CIPCODE, CD = CIPDESC,
         me = medianAnnualEarnings, md = medianDebt, mdp = medianDebtPayments, ne = medianEarningsNetDebt) %>%
  group_by(CIP2, CF) %>%
  mutate(rank = min_rank(-ne),
         instCD = paste0(inst, "\n(", CD, ")")) %>%
  filter(rank <= 10)

for (i in 1:length(ciplbls)) {
  d = baCip4 %>%
    filter(CF %in% ciplbls[i] | str_detect(CF, "All"))
  nr = nrow(d)
  if (nr > 0) {
    baCip4img = d %>%
      ggplot(aes(y=ne, x=fct_reorder(CD, ne), fill=color)) +
      geom_col() +
      geom_text(aes(y=100, label=paste(CD, dollar(ne))), size=3, hjust=0) +
      labs(x="", y="Earnings net of debt payments", fill="") +
      scale_y_continuous(labels = label_dollar()) +
      guides(fill="none") +
      ggtitle(ciplbls[i]) + 
      coord_flip() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    if (nr<6) {
      ggsave(paste0("images/bacip4_", i, ".jpg"), baCip4img, width=6, height=3)
    } else if (nr >= 6 && nr <= 10) {
      ggsave(paste0("images/bacip4_", i, ".jpg"), baCip4img, width=6, height=5)
    } else {
      ggsave(paste0("images/bacip4_", i, ".jpg"), baCip4img, width=6, height=6)
    }
    }
}

for (i in 1:length(ciplbls)) {
  d = baProgs %>%
    filter(CF %in% ciplbls[i])
  nr = nrow(d)
  if (nr > 0) {
    baProgsimg = d %>%
      ggplot(aes(y=ne, x=fct_reorder(instCD, ne))) +
      geom_col(fill="light blue") +
      geom_text(aes(y=100, label=paste(instCD, dollar(ne))), size=3, hjust=0) +
      labs(x="", y="Earnings net of debt payments", fill="") +
      scale_y_continuous(labels = label_dollar()) +
      ggtitle(ciplbls[i]) + 
      coord_flip() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    if (nr<6) {
      ggsave(paste0("images/baprog_", i, ".jpg"), baProgsimg, width=6, height=3)
    } else if (nr >= 6 && nr <= 10) {
      ggsave(paste0("images/baprog_", i, ".jpg"), baProgsimg, width=6, height=5)
    } else {
      ggsave(paste0("images/baprog_", i, ".jpg"), baProgsimg, width=6, height=6)
    }
  }
}

baCip2img = baCip2 %>%
  ggplot(aes(y=ne, x=fct_reorder(CF, ne), fill=color)) +
  geom_col() +
  geom_text(aes(y=100, label=paste(CF, dollar(ne))), size=3, hjust=0) + 
  labs(x="", y="Earnings net of debt payments", fill="") +
  scale_y_continuous(labels = label_dollar()) +
  guides(fill="none") +
  coord_flip() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) 

ggsave("images/bacip2.jpg", baCip2img, width=6, height=10)

# baCip2 %>% mutate(valstr = paste(CF, ":", dollar(ne))) %>%
#   summarise(str_c(valstr, collapse = "\n"))

prompt = paste("Below is a table of net earnings after debt for a list of bachelor's degree majors. Summarize the table and describe the variation in earnings.", baCip2 %>% mutate(valstr = paste(CF, ":", dollar(ne))) %>%
                 summarise(str_c(valstr, collapse = "\n")))

library(rgpt3)
gpt3_authenticate("../../access_key.txt")
#response1 = gpt3_single_completion(prompt_input = prompt, max_tokens = 256, temperature = 0.7)
saveRDS(response1, "text/gptcip2.rds")

prompts = c()
id = c()
for (i in 1:length(ciplbls)) {
  p = baCip4 %>% 
    filter(CF %in% ciplbls[i] | str_detect(CF, "All")) %>%
    mutate(valstr = paste(CD, ":", dollar(ne))) %>%
    summarise(str_c(valstr, collapse = "\n")) 
  prompts = c(prompts, paste("Below is a table of net earnings after debt within the field of ", ciplbls[i], ".  Fields with the phrase 'All' are included for reference purposes. Summarize the table and describe the variation in earnings.", p)) 
  id = c(id, paste0("C", i))
}
promptsCIP4 = data.frame('prompts' = prompts, prompt_id = id)
#responseCIP4 = gpt3_completions(prompt_var = promptsCIP2$prompts, id_var = promptsCIP2$prompt_id, param_max_tokens = 256, param_temperature = 0.7)
saveRDS(responseCIP2, "text/gptcip4.rds")

prompts = c()
id = c()
for (i in 1:length(ciplbls)) {
  p = baProgs %>% ungroup() %>%
    filter(CF %in% ciplbls[i]) %>%
    mutate(valstr = paste(inst, ",", CD, ":", dollar(ne))) %>%
    summarise(str_c(valstr, collapse = "\n")) 
  prompts = c(prompts, paste("Below is a table of net earnings after debt for various colleges and the major ", ciplbls[i], ". Summarize the table and describe the variation in earnings.", p)) 
  id = c(id, paste0("C", i))
}
promptsInst = data.frame('prompts' = prompts, prompt_id = id)
#responseInst = gpt3_completions(prompt_var = promptsInst$prompts, id_var = promptsInst$prompt_id, param_max_tokens = 256, param_temperature = 0.7)
saveRDS(responseInst, "text/gptInst.rds")
# test = promptsInst[1:2,]
# testresult = gpt3_completions(prompt_var = test$prompts, id_var = test$prompt_id, param_max_tokens = 256, param_temperature = 0.7)

responseInst = readRDS("text/gptInst.rds")
responseCIP4 = readRDS("text/gptcip4.rds")

ftitle = shortcuts$fp_bold(font.size=16)
doc = read_docx()
doc = doc %>%
  body_add_fpar(fpar(ftext("Which Majors Pay Off", prop=ftitle))) %>%
  body_add_fpar(fpar(ftext("The Economic Value of Bachelor’s Degrees in 34 Majors", prop=shortcuts$fp_bold(font.size=14)))) %>%
  body_add_par("Introduction", style="heading 1") %>%
  body_add_par("While a college degree is undoubtedly valuable, the choice of major can be even more critical in determining an individual's career trajectory and overall success. While a degree provides a foundation of knowledge and skills, the specific major can have a significant impact on an individual's career opportunities, earning potential, and job satisfaction.") %>%
  body_add_par("") %>% 
  body_add_par("Choosing a major that aligns with one's interests, skills, and career goals can lead to a fulfilling and successful career. In contrast, selecting a major that is not well-suited to one's interests or career aspirations may result in a lack of engagement, frustration, and a diminished sense of purpose.") %>%
  body_add_par("") %>% 
  body_add_par("Furthermore, the job market is continually evolving, and certain industries may experience growth or decline over time. Therefore, a degree in a particular field may not guarantee long-term career success. It is crucial to select a major that provides transferrable skills and knowledge that can adapt to changing job market demands and future career opportunities.") %>%
  body_add_par("") %>% 
  body_add_par("In summary, while a college degree is a valuable asset, the choice of major can significantly impact an individual's career trajectory and overall life satisfaction. A well-informed and thoughtful decision can lead to a fulfilling and successful career, while a hasty or misguided choice can have long-term consequences.")

doc = doc %>%
  body_add_par("Ranking 34 Major Groups", style="heading 1") %>%
  body_add_par(response1[[1]]$gpt3) %>%
  body_add_img("images/bacip2.jpg", width = 6, height = 10) 

for (i in 1:length(ciplbls)) {
  doc = doc %>%
    body_add_par(ciplbls[i], style="heading 1") %>%
    body_add_par(responseCIP4[[1]]$gpt3[i]) %>%
    body_add_par("") %>% 
    body_add_img(paste0("images/bacip4_", i, ".jpg"), width = 5, height = 5) %>%
    body_add_par(responseInst[[1]]$gpt3[i]) %>%
    body_add_par("") %>% 
    body_add_img(paste0("images/baprog_", i, ".jpg"), width = 5, height = 5)
}

print(doc, target = "text/ba.docx")

