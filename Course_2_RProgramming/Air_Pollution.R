# Ejercicio 1 Semana 2. Coursera
# Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' 
#takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 
#'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 
# directory' argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA
#wd= "C:/Users/mabar/OneDrive - Universidad de Los Andes/Coursera/Data Science/Prueba1/COURSERA_DATASCIENCE"



pollutantmean <- function(directory='specdata',pollutant,id){
#Especifica la carpeta

if(dir.exists(directory)==TRUE & (pollutant=='sulfate'|pollutant=='nitrate'))
  {
  index <- 1
  prom=0
    
      for (i in id)
        {
        id_num     <-paste(as.character(c(rep(0,times=3-nchar(i)),i)), collapse ="") #Convierte el id en un caracter
        name       <-paste(directory, "/", id_num,".csv",sep = "", collapse =""); #crea el nombre del archivo
        data_file  <-read.csv(name) #lee el archivo y quita las filas con NA
        data_mean  <-na.omit(data_file[pollutant]) #Extrae la columna de el pollulant
        ans        <-mean(data_mean[,1]) #calcula el promedio
        print(paste(name,ans, sep = " "))
        
          if (index==1)
            {
            prom<-data_mean[,1] #recopila los promedios
            index      <- index+1
            }else { prom<-c(prom,data_mean[,1])} # concatena los datos
        }
    prom<-na.omit(prom)
    ans<-mean(prom)
    ans
    
} else {print("Esta carpeta no existe")}
  
}

#Parte 2
# Write a function that reads a directory full of files and reports the number of
# completely observed cases in each data file. The function should return a data 
# frame where the first column is the name of the file and the second column is 
# the number of complete cases. A prototype of this function follows

complete <-function(directory="specdata",id){
if(dir.exists(directory)==TRUE)
{
  index <- 1
  
  for (i in id)
  {
    id_num     <-paste(as.character(c(rep(0,times=3-nchar(i)),i)), collapse ="") #Convierte el id en un caracter
    name       <-paste(directory, "/", id_num,".csv",sep = "", collapse =""); #crea el nombre del archivo
    data_file  <-read.csv(name) #lee el archivo y quita las filas con NA
    ans<-sum(complete.cases(data_file[,2],data_file[,3]))
    #print(paste(name,ans, sep = " "))
    
    if (index==1)
    {
     Table<-data.frame("id"=i,nobs=ans) #recopila los promedios
      index      <- index+1
    }else {Table<-rbind(Table,c(i,ans))} # concatena los datos
    
  }
  print(Table)
} else {print("Esta carpeta no existe")}
}







