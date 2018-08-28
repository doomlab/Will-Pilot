

##you will run this for each foundation AFTER running the MTMM project 
##through MTMM set up
harmstuffthing = data.frame("words" = c(original_mfd$h2, harm_words, harm_words_reduce, harm_words_reduce_exp2))
harmstuffthing$dataset = c(rep("orig", length(original_mfd$h2)),
                           rep("all_exp1", length(harm_words)),
                           rep("reduce_exp1", length(harm_words_reduce)),
                           rep("reduce_exp2", length(harm_words_reduce_exp2)))
write.csv(harmstuffthing, "harm_words_total.csv", row.names = F)


