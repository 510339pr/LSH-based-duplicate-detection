In this program a scalable duplicate detection algorithm is 
implemented. For this LSH is used as well as TF.IDF. However, LSH is enhanced 
by implementing the following rules: if two products are from the same webshop, have different brand names or 
have different extracted model-id's we do not consider them pairs. However, if 2 products do have the same 
extracted model-id's we do identify them as neighbors. Here the extracted model-id refers to the model-id that is found 
from the titles of products of common brand names that include Samsung, LG, Toshiba, Panasonic, Vizio, Sharp, Sony and Philips. 

Structure of the code:

The code is written in R. There are two R-code files:
1. MainCode
	In here the all the methods/functions are used to implement the algorithm in the dataset
	Furthermore, the plots are made and the performance is evaluated. 
2. Functies_cs
	Contains the functions that are used in the file MainCode. Every function is categorized in what it is implemented 
	for. There are 6 categories: 
	Data => constructs the training and test set
	LSH => All the functions are listed that are used to implement LSH
	TF.IDF =>All the functions are listed that are used to implement TF.IDF
	Evaluation metrics => functions for computing the evaluation metrics 
	extracting brand name from key_value pairs <= functions that do this 
	extracting model_id's from the titles <= functions that do this 

The data that is used is obtained from:
from: https://personal.eur.nl/frasincar/datasets/TVs-allmerged.zip as

To Run the code 
1. change the working directory in setwd to a directory that contains the dataset as well as the Functies_cs file.
2. Run it.
