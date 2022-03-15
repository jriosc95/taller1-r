hembras <- c(183.2 ,184.1 ,183.0,204.3,176.5 ,179.0 ,188.3 ,186.8 ,202.2, 182.5 ,190.0 ,178.1 
             ,193.2 ,180.4 ,184.3 ,189.2 ,189.1 ,203.1, 166.8 ,196.3 ,193.3 ,187.3 ,185.8 
             ,189.3 ,195.5 ,202.4 ,210.8 );
machos <- c(140.9 	,173.9 	,118.9 
            ,121.7 	,177.4 	,140.0 
            ,173.8 	,154.8 	,192.7 
            ,154.5 	,177.5 	,134.4 
            ,109.2 	,153.4 	,175.0 
            ,150.7 	,138.7 	,169.8 
            ,203.3 	,136.7 	,153.9 
            ,163.0 	,165.3 	,176.7 
            ,137.7 	,126.7 	,150.0);
#a-

hist(hembras,breaks = 5, main = "Distribución de tamaño en especies de langostinos hembra", 
     xlab = "tamaño mm", ylab = "Frecuencia",
     col = "green")

hist(machos,breaks = 5, main = "Distribución de tamaño en especies de langostinos macho", 
     xlab = "tamaño mm", ylab = "Frecuencia",
     col = "red", add=TRUE)
#segun el resultado del histograma, podemos deducir que los datos del tamaño de
#los machos están mas concentrados entre los 160-180 (asimetrica positiva), y los datos de las
#hembras están más concentrados hacia la mediana y su distribución es normal, 

#b-
  #promedio
  promedioHembras = mean(hembras);
  promedioHembras

  promedioMachos = mean(machos);
  promedioMachos
  
  sdHembras <- sd(hembras);
  sdHembras
  
  sdMachos <- sd(machos);
  sdMachos
  
  #Se puede concluir que los datos de las hembras están más agrupados con respecto
  #a la media, por lo tanto son más cercanos al valor esperado, en cambio, las muestras
  #obtenidas de los machos, están más dispersas respecto a su media.
  
  #C
  intervalo <- t.test(hembras,
                      conf.level = 0.97)$conf.int;
  intervalo
  