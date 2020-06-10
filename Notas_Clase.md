## Class Notes
Notas para la futura Alejandra. Recuerda **SIEMPRE** estos pasos: Guardar->Commit->Push (asi se hayan desaparecido los archivos) y ya. Puedes usar esta pagina donde estan las referencias de R Markdown.

[PAGINA_Referencias](https://rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)


> There are different types of classes of objetcs  
- character  
- numeric  
- integer  
- complex  
- Logical  

La excepcion es una lista que puede tener cualquier objeto. 

### Formas de crear vectores:  
> Con la funcion c()  

```{r}
x<- c(0.5, 0.6) #Como si c significara concatenar
print(x)
```
Tambien pueden tener nombres propios
```{r}
names(x)<-c("Food","Bar")
x
```
En este caso, pueden haber distintos objetos (numericos, logicos, enteros, etc)

> Con la funcion vector() 

```{r}
x<-vector("numeric", length=10)
x
```
***
Para saber la clase entonces se usa *class()*
```{r}
class(x)
```
Se puede forzar la conversion entre clases de numeros. Por ejemplo:
```{r}
x<-0:6
class(x)
as.numeric(x)
as.character(x)
as.logical(x)
```
 
### Listas
```{r}
x<-list(1, "a", TRUE, 1+4i) #Recuerda que los numeros que cambian son los indices de los vectores, tienen dobles corchetes cuadrados
print(x)
```
Tambien pueden tener nombres
```{r}
x<-list(a=1,b= "a",c= TRUE,d= 1+4i)
x
```
### Matrices
```{r}
m<- matrix(1:6,nrow=2, ncol=3) # Se construye column wise
print(m)
dim(m)
attributes(m) #Todo lo que se le pueda sacar
```
Tambien pueden tener nombres:
```{r}
dimnames(m) <- list(c("fila1","fila2"),c("col1","col2","col3")) #Se usa una listaaa
m
```

* Otra forma es redimensionar un vector
```{r}
m<-1:10
dim(m) <- c(2,5) #dos filas, dos columnas
m
```
* Cbind-ing and rbind-ing
```{r}
x<-1:3
y<-10:12
cbind(x,y) # Los une por columnas
print("Tambien se puede unir por filas")
rbind(x,y)
```

### Factors
Para tener datos categoricos:
```{r}
x<-factor(c("yes","no","yes", "no"), levels = c("yes","no")) #Para determinar el orden de los niveles.

x
table(x) #Conteo
class(x)

```

### Data Frame
```{r}
x<-data.frame(food=1:4, bar=c(T,T,F,F))
x
nrow(x)
ncol(x)
```

### Reading Data
Hay diferentes funciones para leer los datos  
- read.table, read.csv  
- readLines, para leer lineas de texto    
- source, Para leer R code files  
- dget, para leer R code files  
- load, para cargar los datos  
- unserialize, para leer en forma binaria
