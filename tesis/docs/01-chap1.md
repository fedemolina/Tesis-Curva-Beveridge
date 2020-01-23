<!--
This is for including Chapter 1.  Notice that it's also good practice to name your chunk.  This will help you debug potential issues as you knit.  The chunk above is called intro and the one below is called chapter1.  Feel free to change the name of the Rmd file as you wish, but don't forget to change it here from chap1.Rmd.
-->

<!--
The {#rmd-basics} text after the chapter declaration will allow us to link throughout the document back to the beginning of Chapter 1.  These labels will automatically be generated (if not specified) by changing the spaces to hyphens and capital letters to lowercase.  Look for the reference to this label at the beginning of Chapter 2.
-->

# Introducción {#intro}

Este trabajo se plantea 3 objetivos: 1) Generar una serie de vacantes laborales de frecuencia trimestral desde 1980 hasta 2018. 2) En base a los datos obtenidos estimar una curva de Beveridge para el mismo periodo. 3) Responder si existe algún corrimiento, cambio de pendiente y/o movimiento sobre la curva de Beveridge. El análisis en los tres casos se restringe al departamento de Montevideo, Uruguay.

Como sugieren \cite{Bergara2017} las transformaciones sufridas por la economía uruguaya en los 15 años han sido de carácter estructural, al igual que lo fueron las implementadas luego de la dictadura y hasta el año 2000 analizadas por \cite{Antia2001}. Por tanto, la hipótesis es que al menos uno de los 3 sucesos respecto a la curva de Beveridge se materializó. Para responder la pregunta se construye una serie de vacantes laborales para el periodo de análisis 1980-2018 en base \cite{Rama1988}, \cite{Urrestarazu1997}, \cite{Ceres2012} y datos propios, siendo siempre la fuente principal el Diario El País, clasificados laborales, ``Gallito''. Se utiliza como estrategia empírica vectores bayesianos autorregresivos con parámetros variables y volatilidad estocástica (TVP-VAR) siguiendo a \cite{Nakajima2011, Benati2013, Primiceri2005, Lubik2016b}, por dos motivos básicos 1) permite levantar el supuesto de una relación invariante en la relación vacantes-desempleo y 2) modelizar de forma no lineal. El marco de análisis para identificar shocks estructurales es la teoría de búsqueda y emparejamiento resumida en \cite{Pissarides2000}, siguiendo a \cite{Benati2013} quienes construyen sobre \cite{Shimer2005}.

\cite{Blanchard1989} remarcan que los pensamientos de los macroeconomistas respecto a la dinámica agregada del mercado laboral han sido organizados en base dos relaciones, la curva de Phillips y la curva de Beveridge. Sin embargo, la curva de Beveridge, entendida como la relación entre vacantes laborales y desempleo, ha jugado un rol notoriamente secundario. El caso uruguayo no es ajeno al no existir trabajos académicos al respecto en los últimos 20 años.%, de allí surge una primera motivación. Buscar llenar un vacío de la economía laboral uruguaya.
 
La curva de Beveridge o curva UV plantea una relación gráfica convexa hacia el origen entre dos variables, vacantes laborales y desempleo\footnote{Ambas expresadas como tasas con respecto a una tercera variable Población Económicamente Activa (PEA), Población en Edad de Trabajar (PET) o trabajadores totales (L). Gráficamente las vacantes suelen estar en las ordenadas y la tasa de desempleo en las abscisas}. El desempleo se entiende como personas que buscan trabajo remunerado activamente pero no logran obtenerlo. Mientras las vacantes laborales, como aquella posición dentro de la firma que el empleador busca llenar activamente en un periodo a determinar, lo cual suele usarse como proxy de la demanda laboral.
Mientras el desempleo suele ser una variable cuya definición y calculo esta generalizado, las vacantes son raramente calculadas bajo una metodología sistemática, no suelen ser recolectadas, ni existen encuestas al respecto a excepción de algunos pocos países de la OCDE\footnote{Para una lista de países con índices de vacantes laborales y que se ha trabajo sobre la curva de Beveridge, ver los trabajores de \cite{Hobijn2013, Nickell2002}}.

Su nacimiento se debe principalmente a los trabajos seminales de \cite{Beveridge} y \cite{Dicks-Mireaux1958}, estos últimos quienes plantean la relación gráfica por primera vez y, remarcan la robustez del indicador de vacantes laborales en cuanto a su uso de forma cualitativa, permitiendo fuera ampliamente utilizada durante los años 1960-1970. \cite{Rodenburg2007} analiza la evolución de la curva de Beveridge y muestra como ocupó un rol secundario en la macroeconomía entre 1970 y 1980, lo cual entiende pudo deberse entre otros factores, a su ausencia de microfundamentos y al asentamiento de la escuela neoclásica como mainstream. Sin embargo, con el nacimiento de la teoría de búsqueda y emparejamiento bajo los trabajos seminales de \cite{Pissarides1985}, \cite{Mortensen1994} y \cite{Diamond1982} conocido popularmente como el marco de análisis DMP, resumido en \cite{Pissarides2000}, y especialmente el trabajo de \cite{Blanchard1989} la curva de Beveridge recobró importancia en el análisis macroeconómico.\footnote{Para ver la evolución hasta 1986, ver \cite{Mortensen1986}. Para una investigación actual ver \cite{Elsby2015}}  

Respecto a su relevancia \cite{Blanchard1989} plantean que la curva de Beveridge conceptualmente viene primero que la curva de Phillips, y contiene información esencial sobre el funcionamiento del mercado de trabajo y los shocks que afectan al mismo. \cite{Elsby2015} muestran que se utiliza a nivel macroeconómico como un marco de análisis para el entendimiento de los mercados laborales tanto a nivel agregado como desagregado, para el análisis de la volatilidad y coexistencia de desempleo y vacantes, a la vez que como una aproximación al proceso de matching del mercado laboral.

El proceso de matching es el siguiente, un trabajador activo busca un trabajo que cumpla sus requisitos, para ello incurre en costos de búsqueda, gasta tiempo, dinero y deja de realizar actividades con beneficios potenciales. Una vez encontrado el puesto deseado, puede obtenerlo o no. Ello se debe a que el trabajador no solamente valora el ingreso salarial y las condiciones laborales presentes, lo cual es una decisión estática, sino que toma en cuenta al menos dos factores extras, el flujo de ingresos futuros y las condiciones laborales futuras (factores no monetarios).

La firma con vacantes disponibles, busca un trabajador acorde al puesto, en su búsqueda incurre en costos. Una vez encontrado el trabajador deseado, puede ocurrir que no se efectivice su contratación debido a que la firma al momento de ofrecer el contrato laboral debe internalizar no solamente los costos de búsqueda sino también los costos de capacitación. Además debe tomar en cuenta que la relación contractual puede finalizar porque el empleado decide renunciar o bien sea la firma quién finaliza la misma por shocks adversos o porque el candidato elegido quien parecía el indicado, realmente no lo era.

Esto muestra que el matching, en el cual se encuentra la oferta con la demanda laboral son procesos de decisión inherentemente dinámicos y descoordinados por parte del trabajador y la firma que conllevan fricciones, riesgos, asimetrías de información e incertidumbre, generando una diferencia sistemática entre demanda y oferta laboral más allá del desempleo voluntario.

El proceso de matching se relaciona con los diferentes componentes del desempleo: voluntario, desequilibrio o estructural y segmentación o friccional analizados en \cite{Rama1988}. 
El análisis de la curva de Beveridge mediante la teoría de búsqueda y emparejamiento puede explicar los 3 componentes, al menos de forma cualitativa\footnote{Es importante remarcar que la cuantificación que se haga de los efectos siempre va a estar sujeta a la calidad de la serie de vacantes, la cual suele estar sujeta a errores de medición y este trabajo no es la excepción. Sin embargo, su capacidad de análisis cualitativa suele ser confiable, ver \cite{Dicks-Mireaux1958} a la vez que existen posibles mejoras, ver \cite{Abraham1987}}.

Respecto al desempleo voluntario, es decir, trabajadores dispuestos a trabajar que no trabajan porque consideran el salario ofrecido insuficiente. Puede verse mediante cambios en la PEA, shocks que generen ingresos o egresos de personas dispuestas a trabajar provocan corrimiento hacia el origen (caída de la PEA) o hacia afuera (aumento de la PEA). 

Cambios en el desempleo friccional, falta de correspondencia entre los puestos y candidatos, en especial explicado por factores institucionales que logren afectar el proceso de contratación como los costos de contratación y despido, el salario mínimo, el derecho a huelga y ocupación, movilidad de la mano de obra, menores tasas de sindicalización, mejora en la tecnología del matching debido al uso de la tecnología como avisos web, entre otros. Se puede analizar como cambios en la función de matching, implicando un posible mejoramiento o empeoramiento de la eficiencia del mercado laboral. Un corrimiento hacia afuera de la curva implicaría un empeoramiento del match, mientras un corrimiento hacia dentro una mejora.

Finalmente, el componente estructural, la escasez (exceso) global de puestos de trabajo con respecto a los candidatos disponibles, o sea, un exceso (insuficiencia) de oferta laboral. Esto último debido a la relación inversa entre vacantes y desempleo, cuando el desempleo es elevado las vacantes son escasas y viceversa.

El punto en cual nos encontramos sobre la curva puede estar indicando en que parte del ciclo económico se sitúa la economía. Si la posición es un punto con altas vacantes y bajo desempleo, la economía estaría en una etapa expansiva, parte alta del ciclo económico. Contrariamente, un punto con bajas vacantes y elevado desempleo indicaría que la economía esta en una fase recesiva, parte baja del ciclo económico.

Pese a su trascendencia, su análisis en Uruguay esta desactualizado, ya que, el último trabajo al respecto es de \cite{Urrestarazu1997}. Una explicación puede venir por la ausencia de una serie de vacantes pública, sistemática y regular. Sin embargo, entiendo no puede ser la única explicación por cuatro motivos.
 
Primero, el Ministerio de Trabajo y Seguridad Social (MTSS), recolectó información sobre vacantes laborales entre década de 1970 y 1980. Siendo descontinuado por motivos desconocidos. Recién a partir de 2016 el MTSS en conjunto con diario El País publican un informe sobre el marcado laboral en base a las vacantes publicadas en el diario El País, sección ``Gallito Luis''. A su vez el Instituto Nacional de Estadística (INE) que tiene por objetivo la elaboración, supervisión y coordinación de las estadísticas nacionales nunca ha recolectado información ni ha generado encuestas sobre vacantes laborales. Segundo, la existencia del Índice Ceres de Demanda Laboral (ICDL) de frecuencia mensual para 1998-2014, que si bien no es público, se publicaba regularmente. Tercero, por la facilidad y total disposición del diario El País a que estudiantes, investigadores u organismos públicos utilicen su información con fines académicos. Esto queda de manifiesto dado que Urrestarazu accede a dicha información en 1996, yo accedo a la misma en 2018 y el MTSS desde 2016. Cuarto, todas las publicaciones de El País, sección laboral, ``El Gallito'' se encuentran disponibles en la biblioteca Nacional\footnote{En este último punto vale remarcar el enorme trabajo realizado por \cite{Alma2011}, quienes recolectan datos para distintos meses entre 2000 a 2010 permitiendo analizar potenciales sesgos del ``Gallito'' en cuanto puestos de baja calificación, tanto a nivel agregado como por industrias}.

%. De hecho, el Instituto Nacional de Estadística (INE) en Uruguay "que tiene por objetivo la elaboración, supervisión y coordinación de las estadísticas nacional"\footnote{link} nunca ha relevado datos de avisos laborales ni ha realizado encuestas al respecto. El único organismo público que ha realizado un relevamiento al respecto fue el Ministerio de Trabajo y Seguridad Social (MTSS) sobre finales de 1970 y mediados de 1980 para posteriormente retomarlo a partir del año 2016 utilizando como única fuente de información los avisos laborales publicados en el diario El País, sección Gallito. Si bien han existido iniciativas desde el ámbito académico\cite{Alma2011} y privado\cite{Ceres2012} las mismas han quedado descontinuadas y han sido insuficientes, en la medida que no han realizado un análisis del comportamiento de la CB.

Por ello, este trabajo tiene como uno de sus objetivos sistematizar los datos existentes respecto a vacantes laborales recabados en trabajos previos cuya metodología permite una armonización de las series de vacantes. A continuación extender el periodo de análisis en base a información recabada mayoritariamente a partir del diario El País, clasificados laborales, ``Gallito'' con el fin de construir un índice de vacantes laborales, en adelante ``Índice de Vacantes Laborales'' (IVL). De esta forma se da un paso importante en la generación de un índice de vacantes laborales de carácter público, prolongado y que permita ser perfeccionado posteriormente dotando de una herramienta de análisis macroeconómico de la cual Uruguay actualmente adolece.

%Finalizada la creación del IVL, se procede a su análisis estadístico tanto de forma individual como en conjunto con la tasa de desempleo. Esto es relevante, ya que, al no existir previamente para nuestro período de análisis no conocemos sus propiedades estadísticas ni si existe una relación conjunta entre el IVL y la tasa de desempleo, se espera que las mismas estén cointegradas.

La pregunta de investigación, se fundamenta por 3 aspectos: 1) los trabajos previos para Uruguay, en donde se gráfica y estima la curva de Beveridge por \cite{Rama1988, Urrestarazu1997, DECON1993} en los cuales no se rechaza la hipótesis de quiebres estructurales (tanto de nivel como pendiente). 2) el análisis de las series de vacantes existentes y su elevada correlación con el PIB. 3) La literatura, marco DMP, siguiendo \cite{Rodenburg2007} y \cite{Elsby2015} que plantea que shocks o cambios estructurales en la economía generan corrimientos de la curva (hacia afuera o adentro, debido a un aumento o disminución del 'mismatch'), al igual que políticas públicas que busquen aumentar la efectividad del match en el mercado laboral volviéndolo más eficiente (movimiento hacia adentro) y que los movimientos sobre la curva se asocian al ciclo económico. 

Siguiendo a \cite{Antia2001} Uruguay en la década de los 90 transito una apertura de su cuenta corriente y de capitales que genero grandes transformaciones en el mercado laboral, posteriormente tuvo una grave crisis económica entre 2001-2002 y luego un crecimiento promedio anual del PIB entre 2003 y 2009 mayor al 5\% junto a una tasa de desempleo en torno a 5-6\%. A continuación, \cite{Bergara2017} muestra que se generaron un conjunto de reformas estructurales a partir de 2005, en el sector salud, tributario, financiero y social. Finalmente la economía transita desde 2010-2011 un enlentecimiento de la actividad, agravado a partir de 2014-2015, situándose actual y técnicamente en recesión, en conjunto con una tasa de desempleo en torno al 8-9\% y una transformación del mercado laboral en el cual aumenta la sustitución de mano de obra por capital y nuevos procedimientos basados en innovaciones tecnológicas. Por ello, es razonable plantear la hipótesis de que la curva de Beveridge tuvo al menos un corrimiento de nivel, pendiente o un movimiento sobre si misma entre 1980 y 2018. 

%Por lo tanto, surge la siguiente interrogante ¿Existe algún corrimiento y/o cambio de pendiente en la Curva de Beveridge para Uruguay entre los años 1980 y 2018? Dicha pregunta es el objetivo principal de este trabajo de investigación y una de sus motivaciones principales. Como se adelantó, la hipótesis del trabajo es que debe existir al menos un quiebre o cambio de pendiente en la CB para el periodo de análisis. 

Posteriormente se plantea la estimación de la curva de Beveridge mediante una estrategia empírica de TVP-SVAR para analizar cambios de la relación entre vacantes y desempleo, identificado bajo un modelo básico de búsqueda y emparejamiento. Sin embargo, para poder llegar a esta especificación primero debe ponerse a prueba la hipótesis de parámetros variables a lo largo del tiempo y volatilidad estocástica, los cuales siguen un proceso de markov de orden uno. Para ello, se sigue a \cite{Benati2013} quienes utilizan los test desarrollados por \cite{Stock1996, Stock1998} para testear la presencia de un camino aleatorio entre vacantes y desempleo.

El trabajo es organizado de la siguiente forma. La sección siguiente revisa la literatura de la curva de Beveridge en Uruguay y otros países del mundo. Sección 3 define las variables y fuentes de datos a utilizar, a la vez que detalla la metodología utilizada para armonizar las series de vacantes. Sección 4 especifica el marco teórico utilizado para obtener las restricciones de identificación. Sección 5 define la estrategia empírica elegida, discutiendo . Sección 6 especifica los resultados del trabajo. Sección 7 discute al respecto de estos últimos. Sección 8 plantea algunas conclusiones.