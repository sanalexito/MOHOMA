# MOHOMA
Se presenta el código para calcular las estimaciones del servicio de agua en los hogares de México que provienen del Módulo de Hogares y Medio Ambiente (MOHOMA) 2017.

Se incluyen las funciones necesarias (archivos llamados funcs.R y Tab_2.R) para obtener los resultados publicados por el INEGI, con la novedad de que se presentan las estimaciones de totales que no se incluyen en los tabulados básicos de dicha institución. Si bien la encuesta está diseñada para obtener representatividad a nivel nacional, se lleva a cabo el cálculo por entidad (variable llamada "ENT") que puede emplearse para realizar análisis cualitativos.

Es necesario tener la carpeta general llamada "MOHOMA" en donde se almacenarán los demás archivos. 
Carpetas que deben crearse en MOHOMA: "Bases_mohoma", "Calculadores" y "Codigos"
                            
Lo primero que se debe hacer es correr el código que aparece en "dscrg_base_MOHOMA". Esto nos va a descargar el ZIP de las bases de datos en "MOHOMA".
De igual manera aparecerán las bases en formato CSV en la carpeta "Bases_mohoma". Nótese que todos los archivos se guardan en la unidad D.
Si lo consideran necesario solo cambian la ubicación a la unidad C.

Posteriormente se corren los scripts que generan los cuadros correspondientes y que se ubican en la carpeta "Codigos". Los cuadros que se generan son "Cuadro 1" y "Cuadro 2", así como sus desagregaciones por entidad federativa.


