

library(plumber)
library(dplyr)

#* @apiTitle API Equipo Azul Oscuro 
#* @apiDescription API Equipo Azul Oscuro Reto 4


#*@serializer json
#*@param usuario_id
#*@post recomendaciones
function(usuario_id=0){
  matriz<-
  if(usuario_id == 0){
    print("No has introducido usuario")}
      else{
        predict(usuario_id,getData(1,),type = "ratings")
      }
    }
  
  