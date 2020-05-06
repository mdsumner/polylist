#' Read 'MAP' vector polylist
#'
#' See https://stackoverflow.com/questions/61614314/how-to-read-a-map-file-extension-in-r
#' @param filename
#'
#' @return polylist
#' @export
#'
#' @examples
#' ## WIP doesn't work generally needs finish
#' x <- read.map(system.file("extdata/se_municip.MAP", package = "polylist", mustWork = TRUE))
#' df <- setNames(as.data.frame(do.call(rbind, x)), c("x", "y"))
#' df$region.name <- rep(attr(x, "region.name"), unlist(lapply(x, nrow)))
#' ## in case there are multi-rings
#' df$linestring_id <- cumsum(c(0, diff(is.na(df$x))))
#' df$polygon_id <- as.integer(factor(df$region.name))
#' df <- df[!is.na(df$x), ]
#' sfx <- sfheaders::sf_polygon(df, x = "x", y = "y", "linestring_id" = "linestring_id",
#'   polygon_id = "polygon_id", keep = TRUE)
#' #sf::st_crs(sfx) <- sf::st_crs(<whatever it is probably 4326>)
read.map = function(filename){
  zz=file(filename,"rb")
  #
  # header of .map
  #
  versao = readBin(zz,"integer",1,size=2)  # 100 = versao 1.00
  #Bounding Box
  Leste = readBin(zz,"numeric",1,size=4)
  Norte = readBin(zz,"numeric",1,size=4)
  Oeste = readBin(zz,"numeric",1,size=4)
  Sul   = readBin(zz,"numeric",1,size=4)

  geocodigo = ""
  nome = ""
  xleg = 0
  yleg = 0
  sede = FALSE
  poli = list()
  i = 0

  #
  # repeat of each object in file
  #
  repeat{
    tipoobj = readBin(zz,"integer",1,size=1) # 0=Poligono, 1=PoligonoComSede, 2=Linha, 3=Ponto

    if (length(tipoobj) == 0) break
    i = i + 1

    Len = readBin(zz,"integer",1,size=1)  # length byte da string Pascal
    geocodigo[i] = readChar(zz,10)
    Len = readBin(zz,"integer",1,size=1)  # length byte da string Pascal
    nome[i] = substr(readChar(zz,25),1,Len)
    xleg[i] = readBin(zz,"numeric",1,size=4)
    yleg[i] = readBin(zz,"numeric",1,size=4)
    numpontos = readBin(zz,"integer",1,size=2)

    sede = sede || (tipoobj = 1)

    x=0
    y=0
    for (j in 1:numpontos){
      x[j] = readBin(zz,"numeric",1,size=4)
      y[j] = readBin(zz,"numeric",1,size=4)
    }


    # separate polygons
    xInic = x[1]
    yInic = y[1]
    for (j in 2:numpontos){
      if (x[j] == xInic & y[j] == yInic) {x[j]=NA; y[j] = NA}
    }

    poli[[i]] = c(x,y)
    dim(poli[[i]]) = c(numpontos,2)
  }

  class(poli) = "polylist"
  attr(poli,"region.id") = geocodigo
  attr(poli,"region.name") = nome
  attr(poli,"centroid") = list(x=xleg,y=yleg)
  attr(poli,"sede") = sede
  attr(poli,"maplim") = list(x=c(Oeste,Leste),y=c(Sul,Norte))

  close(zz)
  return(poli)
}
