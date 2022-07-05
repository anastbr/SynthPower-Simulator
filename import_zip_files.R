library(stringr)
#First obtain contents of your archive:   

user_path = "C:/Users/anast/Documents/Master 2021/Summer project 2022"

unzip_file = function(item){
#working_directory = getwd()

temp = list.files(path=getwd(), pattern = paste0("SimMethyl_run_",item))
list_of_txts<-unzip(temp,exdir = getwd()) #,list=TRUE
file1 = read.table(list_of_txts[1])
file2 = read.table(list_of_txts[2])
file3 = read.table(list_of_txts[3])

workflow = list(file1, file2, file3)
return(workflow)
}


all_runs_output = list()
for (i in 1:2){
  run = unzip_file(i)
  all_runs_output[[i]] = run
}
all_runs_output

  
  



