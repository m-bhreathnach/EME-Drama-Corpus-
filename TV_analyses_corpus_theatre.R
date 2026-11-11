### T/V Analysis in EME drama ###

#libraries
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(udpipe)
library(readxl)
library(ggplot2)
library(DescTools)

#loading the corpus 

corpus_folder = "theatre corpus/"
corpus_file_names = list.files(corpus_folder)
texts = c()
for(file_name in corpus_file_names){
  file_path = paste0(corpus_folder, file_name)
  text = read_file(file_path)
  texts = c(texts, text)
}

metadata=tibble(read_xlsx("theatre metadata.xlsx"))
file_names=c()
for (i in length(texts)){
  name=str_remove(corpus_file_names[i], ".txt")
  file_names=append(file_names, name)
}
names_vector=c()
for (i in 1:length(texts)){
  name=paste(as.character(metadata$year[i]),metadata$author[i])
  names_vector=append(names_vector,name)
}
names_vector
names(texts)=names_vector

annotation_language=udpipe_load_model("models/english-gum-ud-2.5-191206.udpipe")
corpus=corpus(texts)

#General corpus information
corpus_summary=textstat_summary(corpus)
median_tokens=median(corpus_summary$tokens)
average_tokens=mean(corpus_summary$tokens)  
median_tokens
average_tokens

median_types=median(corpus_summary$types)
average_types=mean(corpus_summary$types)
median_types
average_types



tokens_obj = tokens(texts)
summary(tokens_obj)


#Ts and Vs


requete_thee_thou = "^(thee|thou)$"
kwic_obj_thee_thou = kwic(tokens_obj, requete_thee_thou, valuetype="regex")
thee_thou_df=as_tibble(kwic_obj_thee_thou)


requete_thy= "^(thy|thine)$"
kwic_obj_thy = kwic(tokens_obj, requete_thy, valuetype="regex")
thy_df=as_tibble(kwic_obj_thy)

requete_you= "^(you|ye)$"
kwic_obj_you = kwic(tokens_obj, requete_you, valuetype="regex")
you_df=as_tibble(kwic_obj_you)

requete_your= "^yours?$"
kwic_obj_your = kwic(tokens_obj, requete_your, valuetype="regex")
your_df=as_tibble(kwic_obj_your)


requete_verbes_st= "^(art)$|^(do|ha|would|could)(st)$"
kwic_obj_verbes_st = kwic(tokens_obj, requete_verbes_st, valuetype="regex")
verbs_df=as_tibble(kwic_obj_verbes_st) 

requete_verbes_normal= "^(are|have|do|would|could)$"
kwic_obj_verbes_normal = kwic(tokens_obj, requete_verbes_normal, valuetype="regex")
verbs_normal_df=as_tibble(kwic_obj_verbes_normal) 

requete_wert= "^(wert|wast)$"
kwic_obj_wert = kwic(tokens_obj, requete_wert, valuetype="regex")
wert_df=as_tibble(kwic_obj_wert) 

#création des graphiques pour la distribution des formes T et v 

formes_pronom_T_plot=textplot_xray(kwic_obj_thee_thou)
formes_poss_T_plot=textplot_xray(kwic_obj_thy)
formes_pronom_V_plot=textplot_xray(kwic_obj_you)
formes_poss_V_plot=textplot_xray(kwic_obj_your)
formes_verbales_t_plot=textplot_xray(kwic_obj_verbes_st)
formes_verbs_normal_plot=textplot_xray(kwic_obj_verbes_normal)

formes_wert=textplot_xray(kwic_obj_wert)

formes_pronom_T_plot
formes_poss_T_plot
formes_pronom_V_plot
formes_poss_V_plot
formes_verbales_t_plot
formes_verbs_normal_plot

thee_thou_count=count(thee_thou_df, docname, keyword, sort=T)

thy_count=count(thy_df, docname, keyword, sort=T)

verbs_count=count(verbs_df, docname, keyword, sort=T)

you_count=count(you_df, docname, keyword, sort=T)

your_count=count(your_df, docname, keyword, sort=T)

verbs_count=count(verbs_df, docname, keyword, sort=T)

verbs_normal_count=count(verbs_normal_df, docname, keyword, sort=T)

drawplots<-function(a_data_frame){
  ggplot(a_data_frame)+
    geom_col(aes(x=docname, y=n, fill = keyword))+
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) 
}

drawplots(verbs_count)
drawplots(verbs_normal_count)
###first lot of graphs###
  
#T/V ratios
you_counter=count(you_count, docname, wt=n)
thou_counter=count(thee_thou_count, docname, wt=n)
thy_counter=count(thy_count, docname, wt=n)
your_counter=count(your_count, docname, wt=n)
verbs_counter=count(verbs_count, docname, wt=n)
verbs_normal_counter=count(verbs_normal_count,docname, wt=n)

overall_df=tibble('play'=names_vector,
                  'year'=metadata$year,
                  'author'=metadata$author,
                  'genre'=metadata$genre,
                  'region'=metadata$`author region`,
                  'thou/thee count'=thou_counter$n,
                  'you count'=you_counter$n,
                  'thy/thine count'= thy_counter$n,
                  'your count'=your_counter$n,
                  'verbs count'=verbs_counter$n,
                  'non st verbs count'=verbs_normal_counter$n,
                  'thou/you ratio'=thou_counter$n/you_counter$n, 
                  'thy/your ratio'=thy_counter$n/your_counter$n, 
                  'verbs ratio'= verbs_counter$n/verbs_normal_counter$n,
                  'length'=corpus_summary$tokens,
                
                 )
overall_df

#fix this pls, difference_poss_pron needs to be before overall_df, and not circular

tv_pron_ratio=tibble(thou_counter$n/you_counter$n*100, metadata$author, metadata$year)
tv_poss_ratio=tibble(thy_counter$n/your_counter$n*100, metadata$author, metadata$year)

difference_poss_pron=overall_df$`thy/your ratio`-overall_df$`thou/you ratio`

ggplot(overall_df)+
  geom_col()+
  aes(play,`thou/you ratio`,`thy/your ratio`,fill=genre)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(overall_df)+
  geom_col()+
  aes(play,`thy/your ratio`,fill=genre)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


ggplot(overall_df)+
  geom_col()+
  aes(play,`verbs ratio`,fill=genre)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

tot_t_count=sum(overall_df$`thou/thee count`)
tot_v_count=sum(overall_df$`you count`)
total_ratio_tv=tot_t_count/tot_v_count
total_ratio_tv

tot_poss_t=sum(overall_df$`thy/thine count`)
tot_poss_v=sum(overall_df$`your count`)
ratio_poss=tot_poss_t/tot_poss_v
ratio_poss

tot_verbs_t=sum(overall_df$`verbs count`)
tot_verbs_v=sum(overall_df$`non st verbs count`)
ratio_verbs_tot=tot_verbs_t/tot_verbs_v
ratio_verbs_tot*100
#stats
#descriptive 

you_sd=sd(overall_df$`you count`)
thou_sd=sd(overall_df$`thou/thee count`)
your_sd=sd(overall_df$`your count`) 
thy_sd=sd(overall_df$`thy/thine count`)
verbs_sd=sd(overall_df$`verbs count`)
verbs_normal_sd=sd(overall_df$`non st verbs count`)
standard_deviations=c(you_sd,thou_sd,your_sd,thy_sd, verbs_sd, verbs_normal_sd)
standard_deviations

you_mean=mean(overall_df$`you count`)
thou_mean=mean(overall_df$`thou/thee count`)
your_mean=mean(overall_df$`your count`)
thy_mean=mean(overall_df$`thy/thine count`)
verbs_mean=mean(overall_df$`verbs count`)
verbs_normal_mean=mean(overall_df$`non st verbs count`)

forms_means=c(you_mean,thou_mean,your_mean,thy_mean,verbs_mean,verbs_normal_mean)
forms_medians=c(median(overall_df$`you count`),median(overall_df$`thou/thee count`),median(overall_df$`your count`),median(overall_df$`thy/thine count`),median(overall_df$`verbs count`),median(overall_df$`non st verbs count`))
 
forms_medians
you_distance_from_sd=c(abs(overall_df$`you count`-you_mean))
you_comp_sd=c()

you_sd/you_mean
thou_sd/thou_mean
your_sd/your_mean
thy_sd/thy_mean
verbs_sd/verbs_mean
verbs_normal_sd/verbs_normal_mean

tv_poss_mean=mean(overall_df$`thy/your ratio`)
tv_pron_mean=mean(overall_df$`thou/you ratio`)
tv_pron_median=median(overall_df$`thou/you ratio`)
tv_poss_median=median(overall_df$`thy/your ratio`)
verbs_ratio_mean=mean(overall_df$`verbs ratio`)
verbs_ratio_median=median(overall_df$`verbs ratio`)

sd_comparisons=function(count_vector, mean_value, sd_value) {
  subtraction_res=c()
  comparaison_res=c()
  
  for (i in seq_along(count_vector)) {
    subtraction_res=append(subtraction_res, abs(count_vector[i] - mean_value))
    comparaison_res=append(comparaison_res, subtraction_res[i] < sd_value)
  }
  
  result=tibble::tibble(
    subtraction = subtraction_res,
    comparison = comparaison_res
  )
  
  return(result)
}

you_compared=sd_comparisons(overall_df$`you count`,you_mean,you_sd)
thou_compared=sd_comparisons(overall_df$`thou/thee count`,thou_mean,thou_sd)
your_compared=sd_comparisons(overall_df$`your count`,your_mean,your_sd)
thy_compared=sd_comparisons(overall_df$`thy/thine count`,thy_mean,thy_sd)
verbs_compared=sd_comparisons(overall_df$`verbs count`,verbs_mean,verbs_sd)
verbs_non_st_compared=sd_comparisons(overall_df$`non st verbs count`,verbs_normal_mean,verbs_normal_sd)

sd_pronoms=tibble(overall_df$play,
                  you_compared$comparison,
                  thou_compared$comparison,
                  your_compared$comparison,
                  thy_compared$comparison,
                  verbs_non_st_compared$comparison,
                  verbs_compared$comparison)




#visualising ratios
#making a usable table
ratios_table=tibble('play'=overall_df$play,'genre'=overall_df$genre,'year'=metadata$year,'region'=overall_df$region,'thou/you'=overall_df$`thou/you ratio`,'thy/your'=overall_df$`thy/your ratio`)%>%
  pivot_longer(
    cols = c('thou/you','thy/your'),
    names_to = 'type',
    values_to = 'ratio',
  )
ratios_table_verbs=tibble('play'=overall_df$play,'genre'=overall_df$genre,'year'=metadata$year,'region'=overall_df$region,'verbal ratio'=overall_df$`verbs ratio`)

#making the graph
ggplot(ratios_table)+
  geom_point(aes(x=play,y=ratio, color=type))+
  ggtitle('T/V ratios per play (chronologically)')+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

#graph but with years
ggplot(ratios_table)+
  geom_point(aes(x=year,y=ratio,color=type))+
  ggtitle('T/V ratios by year')+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

#verbal linear regressions by genre


ratios_table_verbs_tragedy=filter(ratios_table_verbs, genre=='tragedy')
ratios_table_verbs_comedy=filter(ratios_table_verbs, genre=='comedy')

ggplot(ratios_table_verbs_tragedy)+
  aes(`year`,`verbal ratio`)+
  geom_point()+
  ylab("verbal ratio tragedies")+
  geom_smooth(method="lm", se=FALSE)

ggplot(ratios_table_verbs_comedy)+
  aes(`year`,`verbal ratio`)+
  geom_point()+
  ylab("verbal ratio comedies")+
  geom_smooth(method="lm", se=FALSE)



                 

#linear regressions

#reg_verbes=lm(n_verbes~annee_verbes, data=verbs_flipped)

reg_ratios=lm(ratio~year, data=ratios_table)
summary(reg_ratios)

reg_verbs=lm(`verbal ratio`~year, data=ratios_table_verbs)
summary(reg_verbs)


#graphs

ggplot(ratios_table, aes(x=year,y=ratio))+
  geom_point(aes(x=year, y=ratio, color=type))+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title="Linear regression for T/V ratios by year")+
  xlab("year")+
  ylab("T/V ratio")

ggplot(ratios_table_verbs, aes(x=year,y=`verbal ratio`))+
  geom_point(aes(x=year, y=`verbal ratio`, colour=genre))+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title="Linear regression for 2nd person verbal inflections")+
  xlab("year")+
  ylab("ratio")

ggplot(ratios_table, aes(x=type,y=ratio))+
  geom_boxplot()

ggplot(ratios_table)+
  aes(x=genre,y=ratio)+
  geom_boxplot()

ggplot(ratios_table)+
  aes(x=region,y=ratio)+
  geom_boxplot()
ggplot(ratios_tragedies)+
  aes(x=region, y=ratio)+
  geom_boxplot()+
  labs(title = "Geographic distribution of tragedies")
ggplot(ratios_comedies)+
  aes(x=region, y=ratio)+
  geom_boxplot()+
  labs(title = "Geographic distribution of comedies")

#Region with just the comedies?



#comparing other variables 
###---Genre---###
##Tragedies##

ratios_tragedies=ratios_table[ratios_table$genre == 'tragedy',]
ratios_tragedies


ggplot(ratios_tragedies, aes(x=year,y=ratio))+
  geom_point(aes(x=year, y=ratio, color=type))+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title="T/V ratios in tragedies")+
  xlab("year")+
  ylab("T/V ratio")

reg_tragedies=lm(ratio~year, data=ratios_tragedies)
summary(reg_tragedies)

ggplot(overall_df)+
  geom_boxplot()+
  aes(genre,`thou/you ratio`)

mean(ratios_tragedies$ratio)

ggplot(ratios_tragedies)+
  aes(region, ratio)+
  geom_boxplot()

##Comedies##
 
ratios_comedies=ratios_table[ratios_table$genre == 'comedy',]
ratios_comedies

ggplot(ratios_comedies, aes(x=year,y=ratio))+
  geom_point(aes(x=year, y=ratio, color=type))+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title="T/V ratios in comedies")+
  xlab("year")+
  ylab("T/V ratio")

mean(ratios_comedies$ratio)

reg_comedies=lm(ratio~year, data=ratios_comedies)
summary(reg_comedies)

ggplot(ratios_comedies)+
  aes(region, ratio)+
  geom_boxplot()

#fun with verbs
ggplot(ratios_table_verbs, aes(x=year,y=`verbal ratio`))+
  geom_point(aes(x=year, y=`verbal ratio`))+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title="")+
  xlab("year")+
  ylab("T/V ratio")

tragic_verbs=ratios_table_verbs[ratios_table_verbs$genre == 'tragedy',]
comic_verbs=ratios_table_verbs[ratios_table_verbs$genre == 'comedy',]


ggplot(overall_df)+
  aes(x=`thou/you ratio`, y=`verbs ratio`,colour=`genre`)+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

  aes(x=`year`, y=`ratio`,colour=`genre`)+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(title = "Corrleations less Marlowe")

cor.test(overall_df$`verbs ratio`,overall_df$`thou/you ratio`)


#geography

ggplot(overall_df)+
  geom_point()+
  aes(play,`thou/you ratio`,`thy/your ratio`,fill=region)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

ggplot(ratios_tragedies)+
  geom_boxplot()+
  aes(x=region, y=ratio)



#inferential?

SpearmanRho(ratios_table$year,ratios_table$ratio)
cor.test(ratios_table$ratio,ratios_table$year)
overall_df$`thou/you ratio` 

SpearmanRho(ratios_table_verbs$`verbal ratio`,ratios_table_verbs$year)




tragedy_time_correlation=cor.test(ratios_tragedies$ratio,ratios_tragedies$year)
comedy_time_correlation=cor.test(ratios_comedies$ratio,ratios_comedies$year)

wilcox.test(ratios_tragedies$ratio,ratios_comedies$ratio, method=kendall)
wilcox.test(tragic_verbs$`verbal ratio`,comic_verbs$`verbal ratio`)

cor.test(ratios_tragedies$ratio,ratios_tragedies$year,method="spearman")
cor.test(comic_verbs$`verbal ratio`,comic_verbs$year)


cor.test(tragic_verbs$`verbal ratio`,tragic_verbs$year)

cor.test(overall_df$`verbs ratio`,overall_df$`thou/you ratio`,method="spearman")
cor.test(ratios_table_verbs$`verbal ratio`,ratios_table_verbs$year)

kruskal.test(ratios_table$ratio,ratios_table$region)
kruskal.test(ratios_tragedies$ratio,ratios_tragedies$region)
kruskal.test(ratios_comedies$ratio,ratios_comedies$region)

kruskal.test(ratios_table_verbs$region,ratios_table_verbs$`verbal ratio`)



###---DIFFERENCE BETWEEN PRONOMINAL AND POSSESSIVE T/V RATIOS---###
difference_poss_pron=overall_df$`thy/your ratio`-overall_df$`thou/you ratio`

ggplot(overall_df)+
  aes(x=`year`,y=`diff poss - pron`, colour=genre)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  geom_smooth(method="lm")

### --- normalising things I should have normalised previousl --- ###

normalised_ratios=tibble('play'=overall_df$play, 'normalised count'=overall_df$`thou/thee count`/overall_df$length,'genre'=overall_df$genre)
normalised_ratios


ggplot(normalised_ratios)+ 
  aes(x=`play`, y=`normalised count`,colour=`genre`)+
  geom_point()+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
  
  

  