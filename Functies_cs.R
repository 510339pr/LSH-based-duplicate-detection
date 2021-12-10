# data

build_dataset = function(JsonData){
  
  list_of_all_products = list()
  for (x in 1:1624) {
    list_of_all_products = append(list_of_all_products,JsonData[x][[1]])
  }
  
  list_of_products_train = unique(sample(list_of_all_products, 1624, replace = T))
  list_of_products_test = setdiff(list_of_all_products, list_of_products_train)

  return(list('train' = list_of_products_train, 'test' = list_of_products_test))
}

# LSH 
create_vector_model_words = function(list_of_products){
  
  # # extract model words and put them in a vector
  
  n_products = length(list_of_products) 
  
  vector_model_words = c()
  for (i in 1:n_products) {
    title = list_of_products[[i]]$title
    # split words into a list 
    words_list =  str_split(title, " ")
    # with code line below we get true or false if a word follows a certain pattern 
    wordMatches = grepl("[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*", words_list[[1]], perl = F) 
    # add those words to a vector:
    for(j in 1:length(wordMatches)){
      # wordMatches[j] is a boolean: true if word fits format  
      if(wordMatches[j] && !(words_list[[1]][j] %in% vector_model_words)){ 
        vector_model_words = append(vector_model_words, words_list[[1]][j])
      }
    }
  }
  return(vector_model_words)
  
}

create_binary_matrix = function(vector_model_words, list_of_products){
  n_products = length(list_of_products)
  binary_matrix = matrix(0, length(vector_model_words),n_products)
  for (j in 1:n_products) {
    title = list_of_products[[j]]$title
    words_list =  str_split(title, " ")[[1]]
    for(i in 1:length(words_list)){
      if(words_list[i] %in% vector_model_words){ 
        binary_matrix[match(words_list[i],vector_model_words),j] = 1
      }
    }
  }
  return(binary_matrix)
}

create_signature_matrix = function(binary_matrix, n_permutations, n_rows){
  set.seed(2)
  n_products = dim(binary_matrix)[2]
  permutation_matrix = matrix(0,n_permutations,n_rows)
  
  for (i in 1:n_permutations) {
    permutation_matrix[i,] = sample(c(1:n_rows))
  }
  
  signature_matrix = matrix(0,n_permutations, n_products)
  
  # # calculate the signature matrix 
  for (i in 1:dim(signature_matrix)[1]){
    for(j in 1:dim(signature_matrix)[2]){
      signature_matrix[i,j] = index_first_1(permutation_matrix[i,], binary_matrix[,j])
    }
  }
  return(signature_matrix)
}

create_neighbor_matrix = function(list_of_products,signature_matrix,bands) {
  
  n_products = length(list_of_products)
  neighbor_matrix = matrix(NA,n_products,n_products)
  counter_number_of_comparisons = 0
  brand_vector = get_brand_vector(list_of_products)
  
  for (i in 1:n_products){
    # compare product i with every product j 
    for(j in 1:n_products){
      if(j>i){
        
        product_i = list_of_products[[i]]
        product_j = list_of_products[[j]]
        
        # extract brand name for both product 
        brand_i = get_brand_name(list_of_products[[i]], brand_vector)
        brand_j = get_brand_name(list_of_products[[j]], brand_vector)
        brand_i = ifelse(is.null(brand_i),"",brand_i)
        brand_j = ifelse(is.null(brand_j),"",brand_j)
        
        # extract model id from title for both products
        extracted_model_id_i = get_model_id(product_i, brand_i)
        extracted_model_id_j = get_model_id(product_j, brand_j)
        
        # check if NULL
        continue = F
        if(!is.null(extracted_model_id_i) && !is.null(extracted_model_id_j)){
          continue = T
        }
        # check if j and i have different shops if so then they cannot be neighbors
        if(list_of_products[[i]]$shop == list_of_products[[j]]$shop){
          neighbor_matrix[i,j] = 0
        }
        # if j and i have different brands than products cannot be the same
       if(brand_i != brand_j){
          neighbor_matrix[i,j] = 0
        }
        else if(continue){
          if(extracted_model_id_i==extracted_model_id_j){
            neighbor_matrix[i,j] = 1
          } else{
            neighbor_matrix[i,j] = 0
          }
        }
        else{
          # now compare products i with products j 
          neighbor_matrix[i,j] = check_if_neighbors(signature_matrix[,i],signature_matrix[,j],bands) 
          if(neighbor_matrix[i,j]==1){
            counter_number_of_comparisons = counter_number_of_comparisons + 1
          }
       }
      }
    }
  }
  return(neighbor_matrix)
  
}

check_if_neighbors = function(sign_prod_i_vec,sign_prod_j_vec, bands){
  
  rows = length(sign_prod_i_vec)/bands
  
  for (b in 1:bands){
    check_difference = TRUE
    for (i in (1+rows*(b-1)):(rows*b)){
      # if not similar for one row we know that not similar for entire band 
      # hence break loop and go to next band 
      if(!(sign_prod_i_vec[i]==sign_prod_j_vec[i])){
        check_difference = FALSE 
        break
      }
    }
    # if true hence one of the bands are similar for two products 
    # hence we can define both products as neighbors and don't need to look 
    # further 
    if(check_difference){
      return(1)
      break
    }
  }
  # the loop did not break hence both vectors don't have anything in common in 
  # one of the bands hence they are not neighbors 
  return(0) 
}

index_first_1 = function(permutation_vector,binary_vector){
  for(i in 1:length(permutation_vector)){
    position = permutation_vector[i]
    if(binary_vector[position]==1){
      return(i)
    }
  }
}

# TF.IDF

create_dissimilarity_matrix = function(neighbor_matrix, list_of_products){
  n_products = length(list_of_products) 
  dissimilarity_matrix = matrix(NA,n_products,n_products)
  counter_number_of_comparisons = 0
  brand_vector = get_brand_vector(list_of_products)
  for (i in 1:n_products){
    for(j in 1:n_products){
      if(j>i && neighbor_matrix[i,j]==1){
        
        product_i = list_of_products[[i]]
        product_j = list_of_products[[j]]
        
        # extract brand name for both product 
        brand_i = get_brand_name(list_of_products[[i]], brand_vector)
        brand_j = get_brand_name(list_of_products[[j]], brand_vector)
        brand_i = ifelse(is.null(brand_i),"",brand_i)
        brand_j = ifelse(is.null(brand_j),"",brand_j)
        
        # extract model id from title for both products
        extracted_model_id_i = get_model_id(product_i, brand_i)
        extracted_model_id_j = get_model_id(product_j, brand_j)
        
        # check if NULL
        continue = F
        if(!is.null(extracted_model_id_i) && !is.null(extracted_model_id_j)){
          continue = T
        }
        
        if(continue  && extracted_model_id_i == extracted_model_id_j){
          dissimilarity_matrix[i,j] = 0
        }
        else{
          dissimilarity_matrix[i,j] = calculate_dissimilarity(i,j,list_of_products)
        }
        counter_number_of_comparisons = counter_number_of_comparisons + 1
      } 
    }
  }
  return(list(dissimilarity_matrix,counter_number_of_comparisons))
}

calculate_TF_IDF_scores = function(word_list_i, word_list_j, union_list){
  
  TF_IDF_scores_each_word = replicate(0,length(union_list))
  N = 2 # total amount of documents compares => always a pairwise comparison
  c = 1 # counter
  for(w in union_list){
    n_w = 0 
    f_wi = 0 # amount of times word w appears in document i 
    f_max_ki = 1 # how many times the each words appears in document i and then pick the maximum of that => we know unique therefore value 1
    if(w %in% word_list_i){
      n_w = n_w + 1
      f_wi = f_wi + 1
      }
    if(w %in% word_list_j){n_w = n_w + 1}
    TF.IDF = (f_wi / f_max_ki) * log2(N/n_w)
    TF_IDF_scores_each_word[c] = TF.IDF
    c = c + 1
  }
  return(TF_IDF_scores_each_word)
}

calculate_dissimilarity = function(i,j,list_of_products){
  
  # put all the words in the title in a vector for each product 
  words_list_product_i =  str_split(list_of_products[[i]]$title, " ")[[1]]
  words_list_product_j =  str_split(list_of_products[[j]]$title, " ")[[1]]

    
  # compute union of all 
  union_words_list = union(unique(words_list_product_i), unique(words_list_product_j))
  
  # Calculate TF.IDF score for each word in product i and save the TF.IDF score in a vector
  TF.IDF_product_i = calculate_TF_IDF_scores(words_list_product_i, words_list_product_j, union_words_list) 
  TF.IDF_product_j = calculate_TF_IDF_scores(words_list_product_j, words_list_product_i, union_words_list) 
  
  # # convert TF.IDF_product list to a vector to determine the cosine value 
  vector_i = c(1:length(union_words_list))
  vector_j = c(1:length(union_words_list))
  for(iter in 1:length(union_words_list)){
    vector_i[iter] = ifelse(TF.IDF_product_i[[iter]]==0,1,0) # # convert 0 to 1 and vice versa 
    vector_j[iter] = ifelse(TF.IDF_product_j[[iter]]==0,1,0) 
  }
  
  # determine cosine dissimilarity 
  return(1-cosine(vector_i,vector_j))
  
}

# evaluation metrics

compute_F1_measures = function(list_of_products,dissimilarity_matrix,calculate_F1_star, total_number_of_comparisons, delta){
  
  # get products that are predicted to be duplicates 
  TP = 0; FP = 0; TN = 0; FN = 0
  n_products = length(list_of_products)
  
  # make a adjusted dissimilarity matrix 
  for (i in 1:n_products){
    # compare product i with every product j 
    for(j in 1:n_products){
      # if this holds than product i and j are similar 
      if(j>i && !is.na(dissimilarity_matrix[i,j]) && dissimilarity_matrix[i,j]<0.5){
        # find out if they are truly similar
        if(list_of_products[[i]]$modelID == list_of_products[[j]]$modelID){
          TP = TP + 1
        } else{FP = FP + 1}
      }
      # else the products are not classified as similar
      else if(j>i){
        if(list_of_products[[i]]$modelID == list_of_products[[j]]$modelID){
          FN = FN + 1
        } else{TN = TN + 1}
      }
    }
  }
  # # calculate F1 measure 
  precision = TP/(TP + FP); recall = TP/(TP+ FN); 
  F1_measure = 2/((1/precision) + (1/recall))
  
  return(F1_measure) 
}

confusion_matrix = function(neighbor_matrix, list_of_products){

  TP = 0; FP = 0; TN = 0; FN = 0
  n_products = length(list_of_products)
  
  # make a adjusted dissimilarity matrix 
  for (i in 1:n_products){
    # compare product i with every product j 
    for(j in 1:n_products){
      # if this holds than product i and j are similar 
      if(j>i && neighbor_matrix[i,j]==1){
        # find out if they are truly similar
        if(list_of_products[[i]]$modelID == list_of_products[[j]]$modelID){
          TP = TP + 1
        } else{FP = FP + 1}
      }
      # else the products are not classified as similar
      else if(j>i){
        if(list_of_products[[i]]$modelID == list_of_products[[j]]$modelID){
          FN = FN + 1
        } else{TN = TN + 1}
      }
    }
  }
  
  return(list('TP'= TP, 'FP'=FP, 'TN'=TN, 'FN'=FN))
  
}

# extracting brand name from key_value pairs 

get_brand_vector = function(list_of_products){
  
  brand_vector = c()
  for(i in 1:length(list_of_products)){
    brand_name = list_of_products[[i]]$featuresMap$Brand
    if(!is.null(brand_name)){
      brand_vector = append(brand_vector, brand_name)
    }
  }
  return(unique(brand_vector))
  
}

get_brand_name = function(product, brand_vector){
  
  brand_name = product$featuresMap$Brand
  
  if(is.null(brand_name)){
    title = product$title 
    words_list =  str_split(title, " ")
    
    for(w in words_list[[1]]){
      find_brand = match(w, brand_vector)
      if(!is.na(find_brand)){
        brand_name = brand_vector[find_brand]
        break
      }
    }
  }
  if(!is.null(brand_name)){
    brand_name = standarized_brandname(brand_name)
  }
  return(brand_name)
}

standarized_brandname = function(brand_name){
  
  brand_name = tolower(brand_name)
  if(brand_name == "lg electronics"){brand_name = "lg"}
  else if(brand_name == "sceptre inc"){brand_name = "sceptre"}
  else if(brand_name == "jvc tv"){brand_name = "jvc"}
  return(brand_name)
  
}

# extracting model_id's from the titles 

get_model_id = function(product, brand_name){
  
  common_brands = c('samsung','lg','toshiba', 'panasonic', 'vizio', 'sharp', 'sony', 'philips')
  if(brand_name %in% common_brands){
    return(eval(parse(text=paste("get_model_id_",brand_name, sep="")))(product))
  }
  return(NULL)
}

get_model_id_samsung = function(product){
  
  words_list =  str_split(product$title, " ")
  test_list = list('UN', 'HN', 'PN', 'LN', 'HG')
  
  for(w in words_list[[1]]){
    if(nchar(w)>7 && substr(w, 1, 2) %in% test_list){
      return(w)
    }
  }
}

get_model_id_lg = function(product){
  
  words_list =  str_split(product$title, " ")
  wordMatches = grepl("[0-9][0-9][a-zA-Z][a-zA-Z]", words_list[[1]], perl = F)
  
  for(j in 1:length(wordMatches)){
    # wordMatches[j] is a boolean: true if word fits format  
    w = words_list[[1]][j]
    if(wordMatches[j] && nchar(w)>6){ 
      return(w)
    }
  }
}

get_model_id_toshiba = function(product){
  
  words_list =  str_split(product$title, " ")
  wordMatches = grepl("[0-9][0-9][a-zA-Z]", words_list[[1]], perl = F)
  
  for(j in 1:length(wordMatches)){
    # wordMatches[j] is a boolean: true if word fits format  
    w = words_list[[1]][j]
    if(wordMatches[j] && nchar(w)>5){ 
      return(w)
    }
  }
}

get_model_id_panasonic = function(product){
  
  words_list =  str_split(product$title, " ")
  test_list = list('TC', 'TH')
  
  for(w in words_list[[1]]){
    if(nchar(w)>7 && substr(w, 1, 2) %in% test_list){
      return(w)
    }
  }
}

get_model_id_vizio = function(product){
  
  words_list =  str_split(product$title, " ")
  test_list = list('E', 'M', 'X')
  avoid_list = list('MSeries','ESeries', 'E-series', "E-Series")
  
  for(w in words_list[[1]]){
    if(nchar(w)>5 && substr(w, 1, 1) %in% test_list){
      if(!(w %in% avoid_list)){
        return(w)
      }
    }
  }
}

get_model_id_sharp = function(product){
  

  
  words_list =  str_split(product$title, " ")
  in_list = list('LC')
  
  for(w in words_list[[1]]){
    if(nchar(w)>5 && substr(w, 1, 2) %in% in_list){
      return(w)
    }
  }
} 

get_model_id_sony = function(product){
  
  words_list =  str_split(product$title, " ")
  in_list = list('KDL', 'XBR', 'NSX')
  
  for(w in words_list[[1]]){
    if(nchar(w)>5 && substr(w, 1, 3) %in% in_list){
      return(w)
    }
  }
}

get_model_id_philips = function(product){
  
  words_list =  str_split(product$title, " ")
  in_list = list('PFL')
  
  for(w in words_list[[1]]){
    if(nchar(w)>5 && substr(w, 3, 5) %in% in_list){
      
      return(w)
    }
  }
}




