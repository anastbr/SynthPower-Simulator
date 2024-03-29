library(limma) 
library(CpGassoc)
unzip_file = function(item){
#working_directory = getwd()
temp = list.files(path=getwd(), pattern = paste0("SimMethyl_run_",item))
list_of_txts<-unzip(temp,exdir = getwd()) #,list=TRUE
file1 = read.table(list_of_txts[1])
file2 = read.table(list_of_txts[2])
file3 = read.table(list_of_txts[3], header = TRUE, sep = "")
workflow = list(file1, file2, file3)
return(workflow)
}

get_all_zip = function(n_zip){
all_runs_output = list()
for (i in 1:n_zip){
  run = unzip_file(i)
  all_runs_output[[i]] = run
}
return(all_runs_output)
}

preprocess_simulated_data = function(all_runs_output, zip_order){
simulated_data = all_runs_output[[zip_order]][[1]]
parameters = all_runs_output[[zip_order]][[3]]

healthy_proportion = parameters$Value[parameters$Parameter == "Healthy proportion"]
n_healthy = ncol(simulated_data)*healthy_proportion
healthy = simulated_data[,1:n_healthy]
diseased = simulated_data[,(n_healthy+1):ncol(simulated_data)]

colnames(simulated_data)[(1:n_healthy)] = "Group1"
colnames(simulated_data)[(n_healthy+1):ncol(simulated_data)] = "Group2"
colnames(simulated_data) = factor(colnames(simulated_data))
return(preprocessed_simulated_data)
}

calculate_means <- function (beta_values_matrix) {
  # check stop if not() to make sure there are no non numeric values.
  means <- rowMeans(beta_values_matrix, na.rm=TRUE)# if all values within a row are NA produces NaN
  means <- na.omit(means) #add handling of NAs if values in the the vector are NAs
  return(means)
}

healthy_mean = calculate_means(healthy)
diseased_mean = calculate_means(diseased)

### Run statistics ####
#t_test


t_test = function(simulated_data){
  phenotype = as.data.frame(colnames(simulated_data))
  names(phenotype) = "Group"
  t_test_output<-cpg.assoc(beta.val = simulated_data,indep=phenotype$Group, logit.transform = TRUE)
  t_test_p_val = t_test_output$results$P.value
  return(t_test_p_val)
}

####
#Kolmogorov-Smirnov Tests - non-parametric
ks_test = function(group1_mean,group2_mean){
ks_test_output = ks.test(group1_mean, group2_mean)
return(ks_test_output)
}

####
limma_test = function(beta_matrix){
Group1 <- ifelse(colnames(simulated_data)=="Group1", 1, 0)
Group2 <- ifelse(colnames(simulated_data)=="Group2", 1, 0)
design_matrix = as.data.frame(cbind(Group1,Group2))
contrast_matrix = makeContrasts(Diff = Group2-Group1, levels = design_matrix)
#design <- model.matrix(~simulated_data$Group2)
fit1 <- lmFit(simulated_data, design_matrix)
fit2 <- contrasts.fit(fit1,contrast_matrix)
fit3 <- eBayes(fit2)
limma_output = as.data.frame(fit3)
limma_p_val = limma_output$p.value
return(limma_p_val)
}

##
logit_trasformation = function(beta_matrix){
  beta_matrix <- ifelse(as.matrix(beta_matrix)>=1, 0.99,as.matrix(beta_matrix))
  beta_matrix <- ifelse(as.matrix(beta_matrix)<=0, 0,0000000000001,as.matrix(beta_matrix))
  beta_transformed = log2(beta_matrix/(1-beta_matrix))
  return(beta_transformed)
}
  

