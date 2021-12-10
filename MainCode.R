# set working directory
setwd("C:/Users/HP/Desktop/master/Computer science For business Analytics")

# # # link to other files 
source("Functies_cs.R")

# # # usefull libraries
library(tidyverse)
library(lsa)
library(bootstrap)
library(ggplot2)
library("rjson")
# # # import data 

JsonData <- fromJSON(file = "TVs-all-merged.json")

n_bootstraps = 5

bands = c(300,150,100,80,60, 50, 40, 30,20,10,1)

PQ_save = c()
PC_save = c()
F1_star_save = c()
save_fraction_of_comparisons = c()
F1_save = c()

for(n_bands in bands){
  
  PQ_boot = c()
  PC_boot = c()
  F1_star_boot = c()
  boot_fraction_of_comparisons = c()
  F1_boot = c()
  
  for(strap in 1:n_bootstraps){
    
    # build training and test set with bootstrapping 
    list_of_products = build_dataset(JsonData)
    
    # # # # # training # # # # #
    
    # tune delta
    vector_model_words = create_vector_model_words(list_of_products$train)
    binary_matrix = create_binary_matrix(vector_model_words, list_of_products$train)
    signature_matrix = create_signature_matrix(binary_matrix,n_permutations = floor(length(vector_model_words)* 0.5) , n_rows = length(vector_model_words))


    grid_values_delta = c(0.2,0.4,0.5,0.6,0.8)
    save_f1_star_grid = replicate(length(grid_values_delta),0)

    neighbor_matrix = create_neighbor_matrix(list_of_products$train,signature_matrix,bands = n_bands)
    dissimilarity_list = create_dissimilarity_matrix(neighbor_matrix, list_of_products$train)

    for(d in 1:length(grid_values_delta)){
        cm = confusion_matrix(neighbor_matrix,list_of_products$train)
        save_f1_star_grid[d] =   2/((1/((cm$TP) / (cm$TP + cm$FP)))+(1/(cm$TP / (cm$TP + cm$FN))))
    }
    best_value = which(save_f1_star_grid==max(save_f1_star_grid),arr.ind=T)
    optimal_delta = grid_values_delta[best_value[1]]
  
    
    # # # # testing 
    # # extract model words and put them in a vector 
    vector_model_words = create_vector_model_words(list_of_products$test)
    binary_matrix = create_binary_matrix(vector_model_words, list_of_products$test)
    n_permutations = floor(length(vector_model_words) * 0.5)
    signature_matrix = create_signature_matrix(binary_matrix,n_permutations = n_permutations, n_rows = length(vector_model_words))
    
    
    neighbor_matrix = create_neighbor_matrix(list_of_products$test,signature_matrix,bands = n_bands) 
    
    cm = confusion_matrix(neighbor_matrix,list_of_products$test)
    PQ = (cm$TP) / (cm$TP + cm$FP)
    PC = cm$TP / (cm$TP + cm$FN)
    F1_star = 2/((1/PQ)+(1/PC))
    
    # # # compare 2 canditate pairs => implement TF-IDF method 
    dissimilarity_list = create_dissimilarity_matrix(neighbor_matrix, list_of_products$test)
    dissimilarity_matrix = dissimilarity_list[[1]]
    counter_number_of_comparisons = dissimilarity_list[[2]]
    
    F1 = compute_F1_measures(list_of_products$test, dissimilarity_matrix, TRUE,counter_number_of_comparisons,optimal_delta)
    fraction_of_comparisons = (cm$TP + cm$FP)/(cm$TP + cm$FP + cm$TN + cm$FN)
    
    # # # saving metrics # # # 
    PQ_boot = append(PQ_boot, PQ)
    PC_boot = append(PC_boot, PC)
    F1_star_boot = append(F1_star_boot, F1_star)
    F1_boot = append(F1_boot, F1)
    boot_fraction_of_comparisons = append(boot_fraction_of_comparisons, fraction_of_comparisons)

  } 
  
  # # # saving metrics # # # 
  PQ_save = append(PQ_save, mean(PQ_boot))
  PC_save = append(PC_save,mean(PC_boot))
  F1_star_save = append(F1_star_save, mean(F1_star_boot))
  F1_save = append(F1_save, mean(F1_boot))
  save_fraction_of_comparisons = append(save_fraction_of_comparisons, mean(boot_fraction_of_comparisons))
  
}

# # # summarize all evaluation metric # # # 
evaluation = data.frame(bands = bands, PQ = PQ_save, f_o_c = save_fraction_of_comparisons,PC = PC_save, F1_star = F1_star_save, F1 = F1_save)

# # # making plots # # # 
qplot(evaluation$f_o_c, evaluation$F1_star, xlab = "fraction of Comparisons", ylab = "F1*-measure", geom=c("point", "line"))
qplot(evaluation$f_o_c, evaluation$PQ, xlab = "fraction of Comparisons", ylab = "Pair Quality", geom=c("point", "line")) 
qplot(evaluation$f_o_c, evaluation$PC, xlab = "fraction of Comparisons", ylab = "Pair Completeness", geom=c("point", "line"))
qplot(evaluation$f_o_c, evaluation$F1, xlab = "fraction of Comparisons", ylab = "F1-measure", geom=c("point", "line"))

