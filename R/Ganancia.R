#' Genera la ganancia total
#'
#' Realiza una suma de la columna venta
#'
#' @param Ganancia (data.frame) Datos respuesta
#' @param carne (data.frame) Datos que explican la variable respuesta
#' @return Un solo dato, con la ganancia en pesos de un año
#' @export
#'
#' @examples
#' \dontrun{
#' #Directorio del trabajo
#' ruta<- "C:/Users/dmake/OneDrive/Escritorio/DatosProg"
#'
#'#------------------------------------------------------
#'#Ejemplo
#'datos1<- openxlsx::read.xlsx("parametrosR.xlsx")
#'#cargar libreria
#'library(ProgAnual)
#'programacion(par=datos1$parametros, x=datos1$x )
#' }
Ganancia<- function(Ganancia, carne){
  par = datos1$parametros
  x = datos1$x
  # Reproduccion
  x_vientre = x[par == "vientre"]
  x_gazappar = x[par == "gazpar"]
  x_desquin = x[par == "des_qui"]
  x_mortgaza = x[par == "mortgaz"]
  x_mortconpq = x[par == "mortconpq"]
  x_pesokg = x[par == "pesokg"]
  x_rendcanal = x[par == "rendcanal"]
  x_preciokg = x[par == "preciokg"]



  conges1<- rep(x_vientre, 24)
  conges2<- x_vientre *(1 - x_desquin )
  conlac1<- conges2*(1 - x_desquin )
  conlac2<- conlac1*(1 - x_desquin )

  #produccion
  gaz_par1<- conges2 * x_gazappar
  gaz_par2<- gaz_par1 - (gaz_par1 * x_mortgaza)

  con1<- gaz_par2
  con2<-con1-(con1* x_mortconpq)
  con3<-con2-(con2* x_mortconpq)
  con4<-con3-(con3* x_mortconpq)
  con5<-con4-(con4* x_mortconpq)

  #Ganancias
  carne<- (con5*x_pesokg * x_rendcanal)

  resultado<- data.frame(venta= c(rep(NA, 9),rep(ganancia, 15)))
  Ganancia<- sum(resultado, na.rm = TRUE)

  return(Ganancia)
}
