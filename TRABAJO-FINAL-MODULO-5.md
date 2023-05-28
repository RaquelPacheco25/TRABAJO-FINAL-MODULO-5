# TRABAJO FINAL MODULO 5
#### Raquel Pacheco
#### 2023-05-28
## **CLÚSTER JERÁRQUICO**
Del archivo BOL\_BP\_MAY\_2017 seleccione los siguientes indicadores, para los bancos, sin considerar las columnas de TOTALES. Activos productivos/total activos Morosidad cartera total Gastos de Operación/Margen Financiero Rentabilidad del ejercicio/ activo promedio Fondos disponibles/total depositos corto plazo Construya un cluster jerarquico. Grafique y comente su composición, usando al menos 2 distancias y dos métodos de clasificación Construya un cluster no jerarquico. Determine el número óptimo de clusters, grafique y comente.
### Carga de las librerias
library(openxlsx)

library(cluster)

library(usethis)

library(devtools)

library(ggplot2)

library(factoextra)

library(fpc)

library(dplyr)

library(NbClust)
### Carga y manipulacion de la base de datos
data<-read.xlsx("C:\\Users\\CompuStore\\Desktop\\CURSOS\\CIENCIA DE DATOS\\MODULO 5\\parte 2\\BOL\_BP\_MAY\_ 2017.xlsx",

`                `sheet = "INDICADORES")

\# Eliminamos filas

data<-slice(data, -c(1:3, 5))

#Eliminamos las columnas de bancos de totales

data<-data[,-1]

data<-data[,-6]

data<-data[,-15]

data<-data[,-c(25:30)]


#Seleccionamos las variables a utilizar 

data<-data %>%

`  `filter(data$INDICADORES.FINANCIEROS=="NOMBRE DEL INDICADOR"|

`    `data$INDICADORES.FINANCIEROS=="ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS"|

`           `data$INDICADORES.FINANCIEROS=="MOROSIDAD DE LA CARTERA TOTAL"|

`           `data$INDICADORES.FINANCIEROS=="GASTOS DE OPERACION  / MARGEN FINANCIERO"|

`           `data$INDICADORES.FINANCIEROS=="RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO"|

`           `data$INDICADORES.FINANCIEROS=="FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO")

\# Trasponemos nuestra base

data<- data.frame(t(data))

\# Convertimos la fila en el nombre de la columna

colnames(data) <- data[1, ]

data <- data[-1, ]

#Guardamos en una variable el nombre de los bancos

nombres<-data$`NOMBRE DEL INDICADOR`


#Ponemos en formato numerico

base<-as.data.frame(lapply(data[,-1], as.numeric))

\# Estandarizamos 

base<-data.frame(scale(base))

#Convertimos como indice a los nombres

row.names(base)<-nombres
### Clúster Jerárquico - Distancia Euclidean y Método WAR.D
cluster<-hclust(dist(base, method = "euclidean"),

`                `method = "ward.D")

plot(cluster, hang = 0.01, cex=0.8)

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.001.png) El dendograma sirve para clasifcar a un grupo dentro de un cluster, en este podemos observar que una primera instancia podrian haber 4 grupos formados, de los cuales en un primer nivel tenemos a los bancos BP GUAYAQUIL, BP PACIFICO, BP PICHINCHA, BP BOLIVARIANO, BP LOJA, BP MACHALA, BP AUSTRO, BP PRODUBANCO, BP INTERNACIONAL, BP CITIBANK, BP COOPNACIONAL, BP GENERAL RUMIÑAHUI y BP SOLIDARIO, los cuales están agrupados es decir comparten similitudes o características comunes según la medida de similitud; la distancia euclideana en este caso utilizada. Por otra parte, tenemos a los bancos BP DELBANK, BP D-MIRO S.A. BP AMAZONAS,BP PROCREDIT, BP FINCA, BP BANCODEDESARROLLO, BP COMERCIAL DE MANABI, BP LITORAL, que podrían ser otro grupo, sin embargo, vemos que después estos 2 grupos se unen y forman uno solo, por lo que podriamos decir que practicamente sus distancias no son tan lejanas y a la final podriamos tener un dendograma con 3 grupos. Esto lo iremos viendo a medida que hacemos otros calculos. No obstante, podemos visualizar que el banco BP VISONFUND ECUADOR, pertenece a un solo grupo y de la misma manera el banco BP CAPITAL pertenece a otro grupo. Todos estos grupos estaban basados en la distancia euclideana y el metodo ward.
### Clúster Jerárquico - Distancia Manhattan y Método Average
\# Cambiamos el método 

cluster2<-hclust(dist(base, method = "manhattan"),

`                 `method = "average")

plot(cluster2, hang = 0.01, cex=0.8)

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.002.png) A diferencia del dendograma anterior utilizado otra distancia y otro método, en este caso, vemos que los grupos se han unido similarmente, por ejemplo tenemos que BP CAPITAL Y BP VISONFUND ECUADOR siguen siendo grupos separados, no obstante, visualizamos que la distancia manhattan y el metodo de averange en BP LITORAL a diferencia del anterior, forma un grupo aparte.
### Ambos dendogramas
\# En una sola ventana los 2 graficos 

par(mfrow=c(1,2))

cluster<-hclust(dist(base, method = "euclidean"),

`                `method = "ward.D")

plot(cluster, hang = 0.01, cex=0.8)

cluster2<-hclust(dist(base, method = "manhattan"),

`                 `method = "average")

plot(cluster2, hang = 0.01, cex=0.8)

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.003.png)
### ¿A qué distancia se encuentran los elementos?
\## Distancia euclideana 

distancia<- dist(base, method = "euclidean")

distancia

`                       `BP GUAYAQUIL BP PACIFICO BP PICHINCHA BP PRODUBANCO

BP PACIFICO               0.4715003                                       

BP PICHINCHA              0.8785435   1.0935979                           

BP PRODUBANCO             0.7379580   0.9393174    0.4767129              

BP AUSTRO                 1.0573832   0.9794326    1.0449852     0.8369484

BP BOLIVARIANO            0.5040926   0.5567835    1.1858166     0.8454229

BP CITIBANK               1.1245269   1.5118524    1.2806900     1.3777129

BP GENERAL RUMIÑAHUI      1.6718961   2.1150689    1.3777317     1.5593343

BP INTERNACIONAL          1.0160265   1.3889851    0.7255328     0.8169216

BP LOJA                   0.5963260   0.7719030    1.2048287     1.2322989

BP MACHALA                0.9227118   0.8085230    1.1300202     0.8020579

BP SOLIDARIO              1.9186287   2.3113456    1.5014414     1.7334983

BP PROCREDIT              1.7367900   1.7639686    1.3948263     1.2570655

BP AMAZONAS               1.3925151   1.4940282    0.9179306     0.9433115

BP COMERCIAL DE MANABI    2.1206653   1.7193069    2.3800884     2.2139159

BP LITORAL                3.3535528   3.2759740    3.0778102     2.9393290

BP COOPNACIONAL           2.0955541   2.5216203    1.6647480     1.7867697

BP CAPITAL                7.1733455   6.9640148    6.9757352     6.9645023

BP FINCA                  1.8865786   2.1532919    1.1487547     1.4025217

BP DELBANK                1.9381527   1.9192333    2.2675743     2.2427764

BP D-MIRO S.A.            2.7991454   2.9763989    2.5624730     2.7045470

BP BANCODESARROLLO        1.8879969   2.1006924    1.4466337     1.4828618

BP VISIONFUND ECUADOR     5.0692811   5.3945478    5.4155376     5.4523256

`                       `BP AUSTRO BP BOLIVARIANO BP CITIBANK

BP PACIFICO                                                

BP PICHINCHA                                               

BP PRODUBANCO                                              

BP AUSTRO                                                  

BP BOLIVARIANO         1.0421944                           

BP CITIBANK            2.0527156      1.4546069            

BP GENERAL RUMIÑAHUI   2.1724215      2.0004076   1.0994098

BP INTERNACIONAL       1.5575170      1.3080080   0.8742536

BP LOJA                1.3387701      0.9966959   1.2507270

BP MACHALA             0.3648149      0.7558290   1.9677780

BP SOLIDARIO           2.1105068      2.2622996   1.6919220

BP PROCREDIT           1.0884036      1.6932444   2.4090684

BP AMAZONAS            0.7318459      1.5411921   2.0725232

BP COMERCIAL DE MANABI 1.5378834      1.9838548   3.2127823

BP LITORAL             2.3201313      3.2562004   4.2258036

BP COOPNACIONAL        2.4546939      2.3388504   1.5477392

BP CAPITAL             6.2747720      7.1304752   8.0463006

BP FINCA               1.8186976      2.1432716   1.8679750

BP DELBANK             1.7205710      2.0569057   2.7277939

BP D-MIRO S.A.         2.4155604      3.0384629   3.1867198

BP BANCODESARROLLO     1.4010931      2.0326533   2.3310634

BP VISIONFUND ECUADOR  5.6503790      5.2429400   4.6606611

`                       `BP GENERAL RUMIÑAHUI BP INTERNACIONAL   BP LOJA

BP PACIFICO                                                           

BP PICHINCHA                                                          

BP PRODUBANCO                                                         

BP AUSTRO                                                             

BP BOLIVARIANO                                                        

BP CITIBANK                                                           

BP GENERAL RUMIÑAHUI                                                  

BP INTERNACIONAL                  1.0033109                           

BP LOJA                           1.7633879        1.3266755          

BP MACHALA                        2.2212541        1.5509941 1.2990630

BP SOLIDARIO                      0.7370542        1.4031468 1.9147331

BP PROCREDIT                      2.2791351        1.9473321 2.0897272

BP AMAZONAS                       1.8420301        1.4618389 1.6074964

BP COMERCIAL DE MANABI            3.6085494        2.8758632 2.2078356

BP LITORAL                        3.9051371        3.5729115 3.5472492

BP COOPNACIONAL                   0.6787227        1.2237068 2.3067579

BP CAPITAL                        7.8331018        7.6487385 7.1852880

BP FINCA                          1.2973849        1.4239447 2.1159284

BP DELBANK                        2.7657958        2.6361031 1.6357587

BP D-MIRO S.A.                    2.5713558        2.8931243 2.6206475

BP BANCODESARROLLO                1.7664573        1.8141643 2.0377751

BP VISIONFUND ECUADOR             4.4830889        5.1960486 4.7808163

`                       `BP MACHALA BP SOLIDARIO BP PROCREDIT BP AMAZONAS

BP PACIFICO                                                            

BP PICHINCHA                                                           

BP PRODUBANCO                                                          

BP AUSTRO                                                              

BP BOLIVARIANO                                                         

BP CITIBANK                                                            

BP GENERAL RUMIÑAHUI                                                   

BP INTERNACIONAL                                                       

BP LOJA                                                                

BP MACHALA                                                             

BP SOLIDARIO            2.2644100                                      

BP PROCREDIT            1.1872556    2.1638225                         

BP AMAZONAS             1.0314359    1.6079098    0.9493248            

BP COMERCIAL DE MANABI  1.5315991    3.5522452    2.2692609   2.1409682

BP LITORAL              2.5305287    3.5081534    2.1400127   2.2108245

BP COOPNACIONAL         2.5033298    1.0578531    2.4361339   2.0611488

BP CAPITAL              6.4630068    7.3504019    5.9146220   6.2240209

BP FINCA                1.9785862    1.1551671    1.4448797   1.2494996

BP DELBANK              1.8276770    2.4995378    2.2423261   1.9022470

BP D-MIRO S.A.          2.6805755    1.9221662    2.3847467   1.9685996

BP BANCODESARROLLO      1.6318565    1.3423980    1.1245699   0.8022059

BP VISIONFUND ECUADOR   5.6108843    4.5255473    5.7380596   5.5633736

`                       `BP COMERCIAL DE MANABI BP LITORAL BP COOPNACIONAL

BP PACIFICO                                                             

BP PICHINCHA                                                            

BP PRODUBANCO                                                           

BP AUSTRO                                                               

BP BOLIVARIANO                                                          

BP CITIBANK                                                             

BP GENERAL RUMIÑAHUI                                                    

BP INTERNACIONAL                                                        

BP LOJA                                                                 

BP MACHALA                                                              

BP SOLIDARIO                                                            

BP PROCREDIT                                                            

BP AMAZONAS                                                             

BP COMERCIAL DE MANABI                                                  

BP LITORAL                          2.6301851                           

BP COOPNACIONAL                     3.9392936  3.9625349                

BP CAPITAL                          5.8422719  4.6711784       8.0610600

BP FINCA                            3.2462594  3.1085032       1.3767559

BP DELBANK                          2.1428333  2.8970033       3.2851594

BP D-MIRO S.A.                      3.3972735  2.7393011       2.9099497

BP BANCODESARROLLO                  2.7946513  2.2412391       1.9366266

BP VISIONFUND ECUADOR               6.5963712  6.9680243       4.8890136

`                       `BP CAPITAL  BP FINCA BP DELBANK BP D-MIRO S.A.

BP PACIFICO                                                          

BP PICHINCHA                                                         

BP PRODUBANCO                                                        

BP AUSTRO                                                            

BP BOLIVARIANO                                                       

BP CITIBANK                                                          

BP GENERAL RUMIÑAHUI                                                 

BP INTERNACIONAL                                                     

BP LOJA                                                              

BP MACHALA                                                           

BP SOLIDARIO                                                         

BP PROCREDIT                                                         

BP AMAZONAS                                                          

BP COMERCIAL DE MANABI                                               

BP LITORAL                                                           

BP COOPNACIONAL                                                      

BP CAPITAL                                                           

BP FINCA                6.8386232                                    

BP DELBANK              5.9872550 2.7403383                          

BP D-MIRO S.A.          5.8352307 2.2690711  1.8178752               

BP BANCODESARROLLO      6.1694351 1.1397215  2.0156099      1.4916221

BP VISIONFUND ECUADOR   9.6725204 5.3244384  4.7035316      4.7293102

`                       `BP BANCODESARROLLO

BP PACIFICO                              

BP PICHINCHA                             

BP PRODUBANCO                            

BP AUSTRO                                

BP BOLIVARIANO                           

BP CITIBANK                              

BP GENERAL RUMIÑAHUI                     

BP INTERNACIONAL                         

BP LOJA                                  

BP MACHALA                               

BP SOLIDARIO                             

BP PROCREDIT                             

BP AMAZONAS                              

BP COMERCIAL DE MANABI                   

BP LITORAL                               

BP COOPNACIONAL                          

BP CAPITAL                               

BP FINCA                                 

BP DELBANK                               

BP D-MIRO S.A.                           

BP BANCODESARROLLO                       

BP VISIONFUND ECUADOR           5.1791109

\# ¿A qué grupo pertenecen los clúster?

cluster$merge

`      `[,1] [,2]

` `[1,]   -5  -11

` `[2,]   -1   -2

` `[3,]   -3   -4

` `[4,]   -6    2

` `[5,]   -8  -17

` `[6,]  -14  -22

` `[7,]   -9    3

` `[8,]  -10    4

` `[9,]  -12    5

[10,]  -13    6

[11,]   -7    7

[12,]  -19   10

[13,]  -20  -21

[14,]    1    8

[15,]   11   14

[16,]  -15  -16

[17,]   12   13

[18,]   16   17

[19,]    9   15

[20,]   18   19

[21,]  -23   20

[22,]  -18   21

\## ¿A qué distancia se encuentran los elementos?

\## Distancia manhattan 

distancia2<- dist(base, method = "manhattan")

distancia2

`                       `BP GUAYAQUIL BP PACIFICO BP PICHINCHA BP PRODUBANCO

BP PACIFICO               0.7997011                                       

BP PICHINCHA              1.6895871   1.7431547                           

BP PRODUBANCO             1.5144362   1.7806311    0.9744719              

BP AUSTRO                 2.0404339   1.9292834    2.0531594     1.6838460

BP BOLIVARIANO            0.9753776   1.1214854    2.3857374     1.5899290

BP CITIBANK               2.0737810   2.8634502    2.6703036     2.7204109

BP GENERAL RUMIÑAHUI      2.0449117   2.8446128    2.6514662     2.7015734

BP INTERNACIONAL          1.7850684   2.3081968    1.4386835     1.5561624

BP LOJA                   1.1289129   1.6948723    2.4987944     2.6433492

BP MACHALA                1.7254383   1.2748909    1.9738901     1.6045767

BP SOLIDARIO              2.5917604   2.9240905    2.5080030     3.0534702

BP PROCREDIT              3.2349321   3.2884996    2.3547668     2.0619704

BP AMAZONAS               3.0571331   3.1107006    1.6632721     1.7183511

BP COMERCIAL DE MANABI    4.2477758   3.4480747    3.9370355     3.5911141

BP LITORAL                6.0759900   6.1295576    5.2368809     5.0916561

BP COOPNACIONAL           2.9394744   3.3910237    2.7721467     2.8222539

BP CAPITAL               13.6996950  12.8999939   13.7012531    13.3319398

BP FINCA                  3.4697001   3.5232676    1.7801129     2.5477237

BP DELBANK                3.6074265   3.5897835    4.2056459     3.8363325

BP D-MIRO S.A.            5.7574037   6.0875440    5.2725905     5.3041894

BP BANCODESARROLLO        3.7564108   3.9909146    3.1759611     3.2075600

BP VISIONFUND ECUADOR     8.3462576   8.7851314    8.4849064     9.0303735

`                        `BP AUSTRO BP BOLIVARIANO BP CITIBANK

BP PACIFICO                                                 

BP PICHINCHA                                                

BP PRODUBANCO                                               

BP AUSTRO                                                   

BP BOLIVARIANO          2.1268628                           

BP CITIBANK             4.1041830      2.5079950            

BP GENERAL RUMIÑAHUI    4.0853456      2.7885955   1.9046048

BP INTERNACIONAL        3.2400085      2.3279730   1.5623558

BP LOJA                 2.2922130      1.9089903   2.3374127

BP MACHALA              0.6543924      1.4724704   3.7891875

BP SOLIDARIO            3.3016532      3.3400722   3.2149646

BP PROCREDIT            1.9524041      3.4860791   4.5400440

BP AMAZONAS             1.1969482      3.3082801   3.9535293

BP COMERCIAL DE MANABI  2.2723309      3.9877476   6.3115249

BP LITORAL              4.2002742      6.3271370   7.8120669

BP COOPNACIONAL         4.2496452      3.2289191   2.4977696

BP CAPITAL             11.6592611     13.4396669  15.7634442

BP FINCA                3.3375037      3.9589891   4.0828725

BP DELBANK              2.8862298      3.3366044   5.6812076

BP D-MIRO S.A.          4.5987765      5.8343649   6.3806080

BP BANCODESARROLLO      2.3705523      4.0075578   4.3695832

BP VISIONFUND ECUADOR   9.8473001      9.0766104   7.5605479

`                       `BP GENERAL RUMIÑAHUI BP INTERNACIONAL    BP LOJA

BP PACIFICO                                                            

BP PICHINCHA                                                           

BP PRODUBANCO                                                          

BP AUSTRO                                                              

BP BOLIVARIANO                                                         

BP CITIBANK                                                            

BP GENERAL RUMIÑAHUI                                                   

BP INTERNACIONAL                  1.8531376                            

BP LOJA                           2.5866203        2.5978945           

BP MACHALA                        3.7703500        3.1607392  2.4654491

BP SOLIDARIO                      1.3103599        2.6991407  2.7778164

BP PROCREDIT                      4.5212065        3.5416159  3.9970980

BP AMAZONAS                       3.9346919        2.7658009  3.3300267

BP COMERCIAL DE MANABI            6.2926875        5.1238846  4.4995549

BP LITORAL                        7.7932295        6.5804468  6.3488836

BP COOPNACIONAL                   1.4269026        1.7646115  3.8477302

BP CAPITAL                       15.7446067       14.8881022 13.9514742

BP FINCA                          2.5513464        2.6241409  4.1815824

BP DELBANK                        5.6246448        5.3924950  3.3437949

BP D-MIRO S.A.                    4.8496072        5.8647840  5.5148866

BP BANCODESARROLLO                3.4219127        3.7681546  4.0293044

BP VISIONFUND ECUADOR             6.5602107        8.4133483  7.9790843

`                       `BP MACHALA BP SOLIDARIO BP PROCREDIT BP AMAZONAS

BP PACIFICO                                                            

BP PICHINCHA                                                           

BP PRODUBANCO                                                          

BP AUSTRO                                                              

BP BOLIVARIANO                                                         

BP CITIBANK                                                            

BP GENERAL RUMIÑAHUI                                                   

BP INTERNACIONAL                                                       

BP LOJA                                                                

BP MACHALA                                                             

BP SOLIDARIO            3.5788048                                      

BP PROCREDIT            2.0146493    4.3307020                         

BP AMAZONAS             1.8358097    3.0619650    1.7609949            

BP COMERCIAL DE MANABI  2.5223375    5.4199606    3.8054171   2.7538213

BP LITORAL              4.8546666    6.9205026    3.4756958   3.8585376

BP COOPNACIONAL         4.1703759    1.7847379    4.6418871   4.0553724

BP CAPITAL             11.9742567   14.8718799   11.3464863  12.1223013

BP FINCA                3.4000415    2.3105583    2.5829145   2.5325475

BP DELBANK              3.0728803    4.6927738    4.6836462   3.5206673

BP D-MIRO S.A.          4.9684924    3.9177363    4.5894622   3.7253822

BP BANCODESARROLLO      2.7892185    2.5491858    2.1769155   1.5126890

BP VISIONFUND ECUADOR   9.5557082    6.6346814   10.3076054   9.6966464

`                       `BP COMERCIAL DE MANABI BP LITORAL BP COOPNACIONAL

BP PACIFICO                                                             

BP PICHINCHA                                                            

BP PRODUBANCO                                                           

BP AUSTRO                                                               

BP BOLIVARIANO                                                          

BP CITIBANK                                                             

BP GENERAL RUMIÑAHUI                                                    

BP INTERNACIONAL                                                        

BP LOJA                                                                 

BP MACHALA                                                              

BP SOLIDARIO                                                            

BP PROCREDIT                                                            

BP AMAZONAS                                                             

BP COMERCIAL DE MANABI                                                  

BP LITORAL                          4.8712001                           

BP COOPNACIONAL                     6.4133680  7.9139100                

BP CAPITAL                         10.1792291  8.4643722      15.8977389

BP FINCA                            5.2863688  5.4147450       2.6720269

BP DELBANK                          3.9826204  4.9946579       6.4021317

BP D-MIRO S.A.                      5.8782325  5.3314389       5.6270942

BP BANCODESARROLLO                  3.8733852  4.3713168       4.0760697

BP VISIONFUND ECUADOR              12.0546420 13.5551840       7.2355426

`                       `BP CAPITAL   BP FINCA BP DELBANK BP D-MIRO S.A.

BP PACIFICO                                                           

BP PICHINCHA                                                          

BP PRODUBANCO                                                         

BP AUSTRO                                                             

BP BOLIVARIANO                                                        

BP CITIBANK                                                           

BP GENERAL RUMIÑAHUI                                                  

BP INTERNACIONAL                                                      

BP LOJA                                                               

BP MACHALA                                                            

BP SOLIDARIO                                                          

BP PROCREDIT                                                          

BP AMAZONAS                                                           

BP COMERCIAL DE MANABI                                                

BP LITORAL                                                            

BP COOPNACIONAL                                                       

BP CAPITAL                                                            

BP FINCA               13.8791172                                     

BP DELBANK             12.1712934  5.3350025                          

BP D-MIRO S.A.         12.2474931  3.6765895  3.1965984               

BP BANCODESARROLLO     12.3226940  2.2719217  4.1275033      2.5589034

BP VISIONFUND ECUADOR  21.5065612  8.3133009  9.3352679      9.2590681

`                       `BP BANCODESARROLLO

BP PACIFICO                              

BP PICHINCHA                             

BP PRODUBANCO                            

BP AUSTRO                                

BP BOLIVARIANO                           

BP CITIBANK                              

BP GENERAL RUMIÑAHUI                     

BP INTERNACIONAL                         

BP LOJA                                  

BP MACHALA                               

BP SOLIDARIO                             

BP PROCREDIT                             

BP AMAZONAS                              

BP COMERCIAL DE MANABI                   

BP LITORAL                               

BP COOPNACIONAL                          

BP CAPITAL                               

BP FINCA                                 

BP DELBANK                               

BP D-MIRO S.A.                           

BP BANCODESARROLLO                       

BP VISIONFUND ECUADOR           9.1838672

\# ¿A qué grupo pertenecen los clúster?

cluster2$merge

`      `[,1] [,2]

` `[1,]   -5  -11

` `[2,]   -1   -2

` `[3,]   -3   -4

` `[4,]   -6    2

` `[5,]   -8  -12

` `[6,]   -9    3

` `[7,]  -14  -22

` `[8,]  -10    4

` `[9,]  -17    5

[10,]    1    8

[11,]  -13    7

[12,]    6   10

[13,]  -19   11

[14,]   -7    9

[15,]   12   14

[16,]  -20  -21

[17,]   13   15

[18,]  -15   17

[19,]   16   18

[20,]  -16   19

[21,]  -23   20

[22,]  -18   21
### Cortes
\# Realizando cortes

cutree(cluster, k=3)

`          `BP GUAYAQUIL            BP PACIFICO           BP PICHINCHA 

`                     `1                      1                      1 

`         `BP PRODUBANCO              BP AUSTRO         BP BOLIVARIANO 

`                     `1                      1                      1 

`           `BP CITIBANK   BP GENERAL RUMIÑAHUI       BP INTERNACIONAL 

`                     `1                      1                      1 

`               `BP LOJA             BP MACHALA           BP SOLIDARIO 

`                     `1                      1                      1 

`          `BP PROCREDIT            BP AMAZONAS BP COMERCIAL DE MANABI 

`                     `1                      1                      1 

`            `BP LITORAL        BP COOPNACIONAL             BP CAPITAL 

`                     `1                      1                      2 

`              `BP FINCA             BP DELBANK         BP D-MIRO S.A. 

`                     `1                      1                      1 

`    `BP BANCODESARROLLO  BP VISIONFUND ECUADOR 

`                     `1                      3 

plot(cluster, hang = 0.01, cex=0.8)

rect.hclust(cluster, k=3, border = "red")

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.004.png)

\# ¿A que grupo pertenecen?

grupos<-as.data.frame(cutree(cluster, k=3))

grupos

`                       `cutree(cluster, k = 3)

BP GUAYAQUIL                                1

BP PACIFICO                                 1

BP PICHINCHA                                1

BP PRODUBANCO                               1

BP AUSTRO                                   1

BP BOLIVARIANO                              1

BP CITIBANK                                 1

BP GENERAL RUMIÑAHUI                        1

BP INTERNACIONAL                            1

BP LOJA                                     1

BP MACHALA                                  1

BP SOLIDARIO                                1

BP PROCREDIT                                1

BP AMAZONAS                                 1

BP COMERCIAL DE MANABI                      1

BP LITORAL                                  1

BP COOPNACIONAL                             1

BP CAPITAL                                  2

BP FINCA                                    1

BP DELBANK                                  1

BP D-MIRO S.A.                              1

BP BANCODESARROLLO                          1

BP VISIONFUND ECUADOR                       3
### Coeficiente de disividalidad
\## cluster 

ncluster<-diana(base, metric="euclidean")

par(mfrow=c(1,2))

El coeficiente de disividalidad nos dice que mientras mas cercano a 1, los modelos estan bien en hechos, en este caso nosotros obtenemos 0,82 por lo que podriamos decir que los cluster estan bien elaborados y agrupados de manera correcta.
### Dendograma Final
cluster1<- hcut(base, k=3, stand=TRUE,

`                `hc\_metric = "euclidean", 

`                `hc\_method = "ward.D")

fviz\_dend(cluster1, rect=1, cex= 0.5,

`          `k\_colors = c("blue", "red", "orange")

)

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.005.png)
## **CLÚSTER NO JERÁRQUICO - CLÚSTER K-MEDIAS** 
### Cluster
\## cluster

cnj<-kmeans(base,3)

cnj

K-means clustering with 3 clusters of sizes 1, 5, 17

Cluster means:

`  `ACTIVOS.PRODUCTIVOS...TOTAL.ACTIVOS MOROSIDAD.DE.LA.CARTERA.TOTAL

1                          -1.8127067                    3.59686390

2                           1.2062846                   -0.58778421

3                          -0.2481598                   -0.03870252

`  `GASTOS.DE.OPERACION....MARGEN.FINANCIERO

1                               4.13445549

2                              -0.54098996

3                              -0.08408857

`  `RESULTADOS.DEL.EJERCICIO...ACTIVO.PROMEDIO

1                                 -2.8225866

2                                  0.9531444

3                                 -0.1143021

`  `FONDOS.DISPONIBLES...TOTAL.DEPOSITOS.A.CORTO.PLAZO

1                                         -0.3471037

2                                          0.7484362

3                                         -0.1997104

Clustering vector:

`          `BP GUAYAQUIL            BP PACIFICO           BP PICHINCHA 

`                     `3                      3                      3 

`         `BP PRODUBANCO              BP AUSTRO         BP BOLIVARIANO 

`                     `3                      3                      3 

`           `BP CITIBANK   BP GENERAL RUMIÑAHUI       BP INTERNACIONAL 

`                     `2                      2                      3 

`               `BP LOJA             BP MACHALA           BP SOLIDARIO 

`                     `3                      3                      2 

`          `BP PROCREDIT            BP AMAZONAS BP COMERCIAL DE MANABI 

`                     `3                      3                      3 

`            `BP LITORAL        BP COOPNACIONAL             BP CAPITAL 

`                     `3                      2                      1 

`              `BP FINCA             BP DELBANK         BP D-MIRO S.A. 

`                     `3                      3                      3 

`    `BP BANCODESARROLLO  BP VISIONFUND ECUADOR 

`                     `3                      2 

Within cluster sum of squares by cluster:

[1]  0.00000 18.95853 29.73459

` `(between\_SS / total\_SS =  55.7 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"

[6] "betweenss"    "size"         "iter"         "ifault"      

#Media de cada cluster 

cnj$centers

`  `ACTIVOS.PRODUCTIVOS...TOTAL.ACTIVOS MOROSIDAD.DE.LA.CARTERA.TOTAL

1                          -1.8127067                    3.59686390

2                           1.2062846                   -0.58778421

3                          -0.2481598                   -0.03870252

`  `GASTOS.DE.OPERACION....MARGEN.FINANCIERO

1                               4.13445549

2                              -0.54098996

3                              -0.08408857

`  `RESULTADOS.DEL.EJERCICIO...ACTIVO.PROMEDIO

1                                 -2.8225866

2                                  0.9531444

3                                 -0.1143021

`  `FONDOS.DISPONIBLES...TOTAL.DEPOSITOS.A.CORTO.PLAZO

1                                         -0.3471037

2                                          0.7484362

3                                         -0.1997104

\# Usando agregate 

aggregate(base, by= list(cnj$cluster), FUN = mean)

`  `Group.1 ACTIVOS.PRODUCTIVOS...TOTAL.ACTIVOS MOROSIDAD.DE.LA.CARTERA.TOTAL

1       1                          -1.8127067                    3.59686390

2       2                           1.2062846                   -0.58778421

3       3                          -0.2481598                   -0.03870252

`  `GASTOS.DE.OPERACION....MARGEN.FINANCIERO

1                               4.13445549

2                              -0.54098996

3                              -0.08408857

`  `RESULTADOS.DEL.EJERCICIO...ACTIVO.PROMEDIO

1                                 -2.8225866

2                                  0.9531444

3                                 -0.1143021

`  `FONDOS.DISPONIBLES...TOTAL.DEPOSITOS.A.CORTO.PLAZO

1                                         -0.3471037

2                                          0.7484362

3                                         -0.1997104
## Como se ven los grupos
fviz\_cluster(cnj, data=base)

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.006.png)

require(cluster)

clusplot(base, cnj$cluster,

`         `color=T, shade = T,

`         `labels = 2, lines = 2)

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.007.png)

Como podemos observar nosotros hemos puestos 3 grupos dado que en el dendograma dividmos los grupos en 3. Similar a los graficos anteriores, la agrupacion de los bancos es la misma, vemos que BP capital está lejos de los demas bancos, sin embargo en el segundo grupo vemos que el Banco Visionfund Ecuador se agrupa con Banco Solidario, Banco Rumiñahui, Citibank y Coopnacional, sin embargo los demas grupos pertenecen a un mismo grupo. Los dos componentes recogen el 83, 96% de variabilidad, por lo que podemos decir que un valor de viabilidad del 83,96% implica que esos dos componentes capturan y explican el 83,96% de la variabilidad total presente en los datos.
### ¿Cuantos cluster son los optimos?
#Numero de cluster optimos 

clustoptim<-NbClust(base, distance = "euclidea",

`                    `min.nc = 2, max.nc = 6,

`                    `method = "ward.D", index = "all")

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.008.png)

\*\*\* : The Hubert index is a graphical method of determining the number of clusters.

`                `In the plot of Hubert index, we seek a significant knee that corresponds to a 

`                `significant increase of the value of the measure i.e the significant peak in Hubert

`                `index second differences plot. 



![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.009.png)

\*\*\* : The D index is a graphical method of determining the number of clusters. 

`                `In the plot of D index, we seek a significant knee (the significant peak in Dindex

`                `second differences plot) that corresponds to a significant increase of the value of

`                `the measure. 



\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\* 

\* Among all indices:                                                

\* 6 proposed 2 as the best number of clusters 

\* 13 proposed 3 as the best number of clusters 

\* 2 proposed 4 as the best number of clusters 

\* 1 proposed 5 as the best number of clusters 

\* 2 proposed 6 as the best number of clusters 

`                   `\*\*\*\*\* Conclusion \*\*\*\*\*                            



\* According to the majority rule, the best number of clusters is  3 





\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\* 

fviz\_nbclust(clustoptim)

Among all indices: 

\===================

\* 2 proposed  0 as the best number of clusters

\* 6 proposed  2 as the best number of clusters

\* 13 proposed  3 as the best number of clusters

\* 2 proposed  4 as the best number of clusters

\* 1 proposed  5 as the best number of clusters

\* 2 proposed  6 as the best number of clusters

Conclusion

\=========================

\* According to the majority rule, the best number of clusters is  3 .

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.010.png)

cnj2<-kmeans(base,3)

Podemos ver que 13 metodos propusieron que sean 3 cluster, 6 metodos que sean 2 cluster, 2 metodos que 0 y 4 y 6 y solo un metodo que sean 5. Por esta razón vemos que si era optimo los 3 cluster que habiamos impuestos al principio.
### Evaluamos el cluster
cnj2<-kmeans(base,3)

silueta<- silhouette(cnj2$cluster, dist(base, method = "euclidean"))

fviz\_silhouette(silueta)

`  `cluster size ave.sil.width

1       1   13          0.25

2       2    9          0.04

3       3    1          0.00

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.011.png)

Sin embargo al momento de evaluar al cluster observamos que existen dos observaciones negativas, lo que quiere decir que probablemente no se esten agrupando en el cluster correcto, por lo que debería replantearse el numero de clusters optimos.

cnj2<-kmeans(base,2)

silueta<- silhouette(cnj2$cluster, dist(base, method = "euclidean"))

fviz\_silhouette(silueta)

`  `cluster size ave.sil.width

1       1    1          0.00

2       2   22          0.68

![](Aspose.Words.7ecd7d80-84b6-402e-9008-76fc1fb76454.012.png)

Finalmente decidimos agruparlos con 2 clusters y vemos que de esta forma no existen observaciones negativas e incluso podemos decir que dado la linea roja que es el promedio, las demas barras estan por encima de ella lo que indica que estan bien clasificados.
