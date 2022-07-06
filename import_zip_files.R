#library(stringr)
#First obtain contents of your archive:   

#user_path = "C:/Users/anast/Documents/Master 2021/Summer project 2022"

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


all_runs_output = list()
for (i in 1:2){
  run = unzip_file(i)
  all_runs_output[[i]] = run
}

simulated_data = all_runs_output[[1]][[1]]
parameters = all_runs_output[[1]][[3]]
healthy_proportion = parameters$Value[parameters$Parameter == "Healthy proportion"]
n_healthy = ncol(simulated_data)*healthy_proportion
healthy = simulated_data[,1:n_healthy]
diseased = simulated_data[,(n_healthy+1):ncol(simulated_data)]
calculate_means <- function (beta_values_matrix) {
  # check stop if not() to make sure there are no non numeric values.
  means <- rowMeans(beta_values_matrix, na.rm=TRUE)# if all values within a row are NA produces NaN
  means <- na.omit(means) #add handling of NAs if values in the the vector are NAs
  return(means)
}
healthy_mean = calculate_means(healthy)
diseased_mean = calculate_means(diseased)

### Run statistics ####
test = t.test(healthy_mean,diseased_mean)
(test)  

####
library(limma)
colnames(simulated_data)[(1:n_healthy)] = "Group1"
colnames(simulated_data)[(n_healthy+1):ncol(simulated_data)] = "Group2"
colnames(simulated_data) = factor(colnames(simulated_data))


Group1 <- ifelse(colnames(simulated_data)=="Group1", 1, 0)
Group2 <- ifelse(colnames(simulated_data)=="Group2", 1, 0)
design_matrix = as.data.frame(cbind(Group1,Group2))
                              
contrast_matrix = makeContrasts(Diff = Group2-Group1, levels = design_matrix)


#design <- model.matrix(~simulated_data$Group2)
fit1 <- lmFit(simulated_data, design_matrix)
fit2 <- contrasts.fit(fit1,contrast_matrix)
fit3 <- eBayes(fit2)
topTable(fit)


