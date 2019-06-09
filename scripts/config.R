# @__author__ = 'RYALI93'
# @__copyright__ = 'RYALI 2018'
# @__credits__ = ['Roy Yali S.']
# @__version__ = '1.0'
# @__maintainer__ = ['Roy Yali S.']
# @__mail__ = 'ryali93@gmail.com'
# @__status__ = 'Development'

# Carpeta Principal
BASE_DIR = dirname(dirname(rstudioapi::getSourceEditorContext()$path)) 

DATOS    = file.path(BASE_DIR, "datos")  # Carpeta de datos

IMG      = file.path(DATOS, "img")    # Carpeta de raster
SHP      = file.path(DATOS, "shp")    # Carpeta de shapefiles
NC       = file.path(DATOS, "nc")     # Carpeta de netcdf

# Importar librarias
librerias = c("ncdf4","xts","zoo","rts","raster","sf","raster")
lapply(librerias, require, character.only = TRUE)

