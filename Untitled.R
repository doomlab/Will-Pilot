

##you will run this for each foundation AFTER running the MTMM project 
##through MTMM set up
#Harm
harm_all = data.frame("words" = c(original_mfd$h2, harm_words, harm_words_reduce, 
                                  harm_words_reduce_exp2))
harm_all$dataset = c(rep("orig", length(original_mfd$h2)),
                           rep("all_exp1", length(harm_words)),
                           rep("reduce_exp1", length(harm_words_reduce)),
                           rep("reduce_exp2", length(harm_words_reduce_exp2)))
write.csv(harm_all, "harm_words_total.csv", row.names = F)

#Fair
fair_all = data.frame("words" = c(original_mfd$f2, fair_words, fair_words_reduce, 
                                  fair_words_reduce_exp2))
fair_all$dataset = c(rep("orig", length(original_mfd$f2)),
                     rep("all_exp1", length(fair_words)),
                     rep("reduce_exp1", length(fair_words_reduce)),
                     rep("reduce_exp2", length(fair_words_reduce_exp2)))
write.csv(fair_all, "fair_words_total.csv", row.names = F)

#Ingroup
ingroup_all = data.frame("words" = c(original_mfd$i2, ingroup_words, ingroup_words_reduce, 
                                     ingroup_words_reduce_exp2))
ingroup_all$dataset = c(rep("orig", length(original_mfd$i2)),
                     rep("all_exp1", length(ingroup_words)),
                     rep("reduce_exp1", length(ingroup_words_reduce)),
                     rep("reduce_exp2", length(ingroup_words_reduce_exp2)))
write.csv(ingroup_all, "ingroup_words_total.csv", row.names = F)

#Authority
authority_all = data.frame("words" = c(original_mfd$a2, authority_words, authority_words_reduce, 
                                     authority_words_reduce_exp2))
authority_all$dataset = c(rep("orig", length(original_mfd$a2)),
                        rep("all_exp1", length(authority_words)),
                        rep("reduce_exp1", length(authority_words_reduce)),
                        rep("reduce_exp2", length(authority_words_reduce_exp2)))
write.csv(authority_all, "authority_words_total.csv", row.names = F)

#Purity
purity_all = data.frame("words" = c(original_mfd$p2, purity_words, purity_words_reduce, 
                                       purity_words_reduce_exp2))
purity_all$dataset = c(rep("orig", length(original_mfd$p2)),
                          rep("all_exp1", length(purity_words)),
                          rep("reduce_exp1", length(purity_words_reduce)),
                          rep("reduce_exp2", length(purity_words_reduce_exp2)))
write.csv(purity_all, "purity_words_total.csv", row.names = F)
