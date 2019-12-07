fx5<-read.csv("fecu.csv",header=T)
names(fx5)[2:47]<-c(1970:2015)


####Fecundidad########################################################



#Funcion para todos los años que se va modificando el año que se necesita en la fx.prueba
asfr<-function (fx5,year){ 
  fx0<-fx5[fx5$Year==year,"fx"]
  
  Fx<-5*cumsum(fx0)
  #Es oigual al ultiimo elemento de la suma acumulada
  TGF<-Fx[7]
  #proporcion de las fecundidades acumuluadas con respecto a la TGF (hasta los 30 años se ha acumulado el 70% 
  FxF<-Fx/TGF
  
  #Definir las edades. Secuencia que comienza en 17 al 47, marca de clase de edades quinquenales el cual es el punto medio
  x5<-seq(17.5,47.5,5)
  
  #establecer funcion oara desagregar edades simples. Se multiplica por menos para que sea positivo
  Yx<-log(-log(FxF))
  #sale un infinito pero se omite de la regresion
  
  #Regresion de la varX menos el infinito con respecto a edad quinquenal menos la ultima (seran los primeros 6 valores)
  Yx.lm<-lm(Yx[-7]~x5[-7])
  Yx.lm
  
  #ordenada al origen 
  a<-Yx.lm$coefficients[1]
  #pendiente
  b<-Yx.lm$coefficients[2]
  
  #alfas y betas estimadas
  A<-exp(-exp(a))
  B<-exp(b)
  
  #F(x) estimadas para edades simples
  x1<-c(15:50)
  (Fx.estim<-TGF*A^(B^(x1)))
  
  #para desacumular las tasas especificas de fecundidad (nfx)
  fx<-Fx.estim[2:36]-Fx.estim[1:35]
  #para el calculo del primer valor
  (fx<-c(Fx.estim[1],fx))
}

#Para hacer todos los años juntos para edades simples (36 renglones-36 edades, 35 años-35 columnas)
fx1<-data.frame(matrix(0,36,46))
row.names(fx1)<-c(15:50)
names(fx1)<-c(1970:2015)

#Para rellenar matriz de edades simples
for(i in 1:46){
  fx1[,i]<-asfr(fx5,1969+i)
}

#Tasas globales de fecundidad
colSums(fx1)
