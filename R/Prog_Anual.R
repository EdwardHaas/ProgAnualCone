#' Programación Anual De Una Granja Cunicola
#'
#' Realiza una tabla con datos de hembras gestantes, lactantes; gazapos "1", gazapos "2"; conejos en fase de engorda; conejos finalizados; kg de carne obtenidos; dinero generado por la venta de esos kg de carne. Todo esto obtenido de la utilizacion de diferentes parametros reproductivos y productivos dados de una manera quincenal
#'
#' @param par (cuadro) nombre de las variables
#' @param x (cuadro) datos numericos de la variable explicativa
#' @return un data.frame (calendarizacion) con valores de produccion y reproduccion de manera quincenal en un año
#' @export
#'
#' @examples
#' \dontrun{
#' #Directorio del trabajo
#' ruta<- "C:/Users/dmake/OneDrive/Escritorio/DatosProg"
#'
#'#------------------------------------------------------
#'#Ejemplo1
#'datos1<- openxlsx::read.xlsx("parametrosR.xlsx")
#'#cargar libreria
#'library(ProgAnual)
#'programacion(par=datos1$parametros, x=datos1$x )
#' }
programacion<- function(par, x){
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

  calendarizacion<- data.frame(quincenas= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                           coneges= c(conges1),
                           coneges2= c(NA, rep(conges2,23)),
                           conlac1= c(rep(NA,2), rep(conlac1,22)),
                           conlac2= c(rep(NA, 3),rep(conlac2 ,21)),
                           Gazapos1= c(rep(NA, 2),rep(gaz_par1 ,22)),
                           Gazapos2= c(rep(NA,3), rep(gaz_par2,21)),
                           Conejos1= c(rep(NA, 4), rep(con1, 20)),
                           Conejos2= c(rep(NA, 5),rep(con2 ,19)),
                           Conejos3= c(rep(NA, 6),rep(con3 , 18)),
                           Conejos4= c(rep(NA, 7),rep(con4 ,17)),
                           Conejos5= c(rep(NA, 8),rep(con5 , 16)),
                           Confin= c(rep(NA, 9),rep( con5, 15)),
                           Carnekg= c(rep(NA, 9),rep(carne,15)))


  return(calendarizacion)
}


