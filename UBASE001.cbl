       IDENTIFICATION  DIVISION.
       PROGRAM-ID. UBASE001.
      ******************************************************************
      *****    FUNZIONI DEL PROGRAMMA  :                            ****
      *****                                                         ****
      *****        - CONTROLLO                                      ****
231024* ubase nuova scad vep
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-DATE-SCAD                PIC 9(8) COMP-3 VALUE 0.
110210 77  IND                         PIC S999 COMP-3 VALUE 0.
040308 COPY "F0S.REC".
       LINKAGE SECTION.
       COPY "LLSMCARE.WRK".
       PROCEDURE   DIVISION        USING  LLCOMAR.
       INIZIO.
               MOVE    0               TO  LSTAT-CORR
      *  Open menu'
               MOVE    -3              TO  DSTF-IOSR
             CALL  "URHXTB20"          USING F0S LLCOMAR

               MOVE    3               TO  DSTF-IOSR
               MOVE    0               TO  LREAD-FILE
            CALL  "URHXTB20"          USING F0S LLCOMAR
           IF DSTF-IOSR NOT EQUAL 1
               MOVE    LDATE-ACTL      TO  F0S-DATA-LIMI
               MOVE    " "             TO  F0S-LIMI-SCAD
               MOVE    5               TO  DSTF-IOSR
             CALL  "URHXTB20"          USING F0S LLCOMAR
           END-IF

040308* ALL
             MOVE      99999999        TO  WS-DATE-SCAD
      *
040308* DEMO
260309*       MOVE      20100101        TO  WS-DATE-SCAD

100713* DEMO OLIO
100713*       MOVE      20131101        TO  WS-DATE-SCAD

240113* DEMO gatti
240113*       MOVE      20130331        TO  WS-DATE-SCAD

    *
      *demo merc 100
090911*       MOVE      20111231        TO  WS-DATE-SCAD

211112* DEMO  FOOD 300
211112*       MOVE      20130731        TO  WS-DATE-SCAD

261112* DEMO SIGESTA POLICORO
261112*       MOVE      20130731        TO  WS-DATE-SCAD

261112* DEMO  FOOD 300 POLICORO
261112*       MOVE      20130731        TO  WS-DATE-SCAD
      *
270612* DEMO FOOD fasano march ( a march invece  20130606 1anno)
270612*       MOVE      20120730         TO  WS-DATE-SCAD
      *
170611* DEMO GESCAT ELECTROMED
170611*       MOVE      20110731        TO  WS-DATE-SCAD

090714* DEMO marcheg
090714*       MOVE      20150106         TO  WS-DATE-SCAD

150519* DEMO AGRIGENTO
090719*       MOVE      20190731         TO  WS-DATE-SCAD

************************************************************************
*******COMMERCIALISTI  -------------------------------------------------
      *
071114* Commerc.Armenise per Ditta Piscotti 
111115*       MOVE      20160525        TO  WS-DATE-SCAD

*******FOOD  ----------------------------------------------------------
      *
031214************ ANTICA OSTERIA sas - ROCCHETTA SANT'ANTONIO (FG)
031214*       MOVE      20151203        TO  WS-DATE-SCAD
      *
300712*"Distributore Mi.Pa. sas"-ORDONA (FG) bar tabacchi     totem ====
070422*         MOVE    20230406        TO  WS-DATE-SCAD
      *
120312************ GATTI BITETTO  - FOOD
120318*       MOVE      20190305        TO  WS-DATE-SCAD
      *
020414***********L'ORSO POETA  DI MASSIMO VANTAGGIATO - BRINDISI 
130415*       MOVE      20170216        TO  WS-DATE-SCAD
      *
130315* NITTOSO srl - STATTE (TA) bar tabacchi                totem ====
111021*         MOVE    20221122        TO  WS-DATE-SCAD
      *
180713************ ROSSI PASQUALE - ANZANO DI PUGLIA (FG)  bar tabacchi
040919*       MOVE      20200915        TO  WS-DATE-SCAD
      *
*******GESCAT ----------------------------------------------------------
      *
161204************  TECNOGAS       / DISDETTO
230317*         MOVE    20180329        TO  WS-DATE-SCAD

040112* GURRADO CD
040112*       MOVE      20160113        TO  WS-DATE-SCAD
040112* GURRADO 
200922*       MOVE      20230928        TO  WS-DATE-SCAD

171110************  KLIMA IMPIANTI
141210*disabilitata
171110*       MOVE      20101214        TO  WS-DATE-SCAD
      *
*******DRESS -----------------------------------------------------------
      *
061216************ ABENA FRANCESCA  - MARTINA FRANCA (TA)
110717*       MOVE      20180220         TO  WS-DATE-SCAD
      *
170610************ BALDACCI SECONDO veryboutyque -BRINDISI
140618*         MOVE    20190207       TO  WS-DATE-SCAD

280910************ SECONDO MODA (di Baldacci Secondo) -BRINDISI
130115*         MOVE    20150728        TO  WS-DATE-SCAD
      *
180516************ DOUBLE POKER/FREESTYLE - RUTIGLIANO
050319*        MOVE      20191008        TO  WS-DATE-SCAD
      *
190911********  INSIGHT CONCEPT STORE DI IVAN DI TURI (BARI)   /** DISDETTO
070417*       MOVE      20180613        TO  WS-DATE-SCAD
      *
280212*LUONGO -BITETTO [STOCK HOUSEdisatt]COUNTRY_STORE + StockShock(+OSCAR)
081019*       MOVE      20191210        TO  WS-DATE-SCAD
301120*       MOVE      20210316        TO  WS-DATE-SCAD
171117*  LUONGO -BITETTO COUNTRY_STORE(disatt.)+GRILLO (+STELLA)
151119*       MOVE      20191212        TO  WS-DATE-SCAD
010620*       MOVE      20201229        TO  WS-DATE-SCAD

210417*  LUONGO SCIPIONE - BITETTO  desert Gallipoli (figlio)  
190320* DISATTIVATA e sostituita da countrystore Riattivata al 
190320*... posto di STockhouse Disattivata
120919*        MOVE      20191121        TO  WS-DATE-SCAD
160620*        MOVE      20201229        TO  WS-DATE-SCAD

040920*        MOVE      20210615        TO  WS-DATE-SCAD
      *
250612************   PAOLA E ROSA  - BRINDISI
110917*        MOVE      20180927        TO  WS-DATE-SCAD
      *
*******MERCATO  --------------------------------------------------------


010217*demo DATTOMA mercato Conversano
010217*       MOVE      20170331        TO  WS-DATE-SCAD

      *demo Martina Franca (CORVINO 100)
140111*       MOVE      20110222        TO  WS-DATE-SCAD

170613*demo MERC.TARANTO (COTRAF 100)
050813*       MOVE      20130916        TO  WS-DATE-SCAD

031115*         demo collaboratore DI ZETA
031115*         MOVE    20151231        TO  WS-DATE-SCAD

120713*demo ZOTTI SRL  (Collab.)
160913*       MOVE      20131028        TO  WS-DATE-SCAD
230114*       MOVE      20140401        TO  WS-DATE-SCAD
160913*portatile ZOTTI SRL  (SANTE CALDAROLA) *** 200918 disatt
270917*         MOVE    20180917        TO  WS-DATE-SCAD

280318* ZOTTI SRL  (SANTE CALDAROLA)MERC
030223*         MOVE    20230403        TO  WS-DATE-SCAD
      *-----------------------------------------------------
      *
250121* AGRISALIANI srl .bari mercato
040324*         MOVE    20250408        TO  WS-DATE-SCAD

191020* Barifrutta srl   .bari
130224*         MOVE    20250304        TO  WS-DATE-SCAD

211011* caldarulo/DE CARO  .bari
200323*         MOVE    20250320        TO  WS-DATE-SCAD

081015*             demo collaboratore DE CARO
081015*         MOVE    20151129        TO  WS-DATE-SCAD
      *
070317* CAMPOFRUTTA - MESSINA
040324*         MOVE    20250415        TO  WS-DATE-SCAD
      *
131118* CAROFIGLIO FRAMA - MOI BARI
040324*         MOVE    20250403        TO  WS-DATE-SCAD
      *
140915* coop.ort.Baresi srl.bari pACE 1
100320*         MOVE    20200519        TO  WS-DATE-SCAD

221018* coop.ort.Baresi srl.bari pACE 2
030924*         MOVE    20250925        TO  WS-DATE-SCAD

120411* DE CHIRICO . TERLIZZI
      *     IF L-NUMR-INST EQUAL 74
191216*         MOVE    20170510        TO  WS-DATE-SCAD   
      *                                                 ELSE
      **        controllo mazzone
100614*         MOVE    20140923        TO  WS-DATE-SCAD
      *     END-IF
      *
210720* Diceglie - COM FASANO
040324*          MOVE    20250225        TO  WS-DATE-SCAD
      *
280715* DIDONNA MICHELE - RUTIGLIANO (BA)   
280715*         MOVE    20151118        TO  WS-DATE-SCAD

220920* DONNALOIA srl  - COM FASANO
050324*         MOVE    20250325        TO  WS-DATE-SCAD

121012* EREDI LOIACONO - BARI   
020217*         MOVE    20170906        TO  WS-DATE-SCAD

050508* FIORE FRUTTA Bari                             totem ====
060224*         MOVE    20250211        TO  WS-DATE-SCAD
      *
061011* GRANDOLFO srl
050324*          MOVE    20250306        TO  WS-DATE-SCAD

311213* Happy nature FRANCO LUIGI Bari   *******disattivata 120716
110716*         MOVE    20161027        TO  WS-DATE-SCAD
      *
291021* HELEN FRUITS srls - Molfetta
291021*         MOVE    20220127        TO  WS-DATE-SCAD
      *
050918* ILFRES srl - Barletta
011018*         MOVE    20181204        TO  WS-DATE-SCAD
      *
160317* LA PALMARA - MESAGNE
290719*         MOVE    20191217        TO  WS-DATE-SCAD
      *
231018* (LA PALMARA) 2a.ditta > ORTOFR.SOLE - MESAGNE
271223*         MOVE    20240116        TO  WS-DATE-SCAD
      *
160614* LAVOPA GAETANO Bari
190724*         MOVE    20250715        TO  WS-DATE-SCAD
      *
121217* LOCONTE SRL - FASANO
171219*         MOVE    20200623        TO  WS-DATE-SCAD
      *
230517* LOPRETE - FASANO
120224*         MOVE    20250225        TO  WS-DATE-SCAD
      *
151116* MANIELLO srl FASANO   
250320*         MOVE    20200908        TO  WS-DATE-SCAD
      *
120411* MAZZONE  .RUVO        
      *     IF L-NUMR-INST EQUAL 75
090615*         MOVE    20160607        TO  WS-DATE-SCAD
      *                                                 ELSE
      **        controllo mazzone/de chirico
100614*         MOVE    20140923        TO  WS-DATE-SCAD
      *     END-IF
      *
070219* MarzulliBari - MOI BARI   **** 08/03/21 disattivata
080321*         MOVE    20210313        TO  WS-DATE-SCAD
      *
070219* Mazzone Bari - MOI BARI
130224*         MOVE    20250220        TO  WS-DATE-SCAD
      *
170720* Medfrutta - COM FASANO
050324*         MOVE    20250313        TO  WS-DATE-SCAD
      *
151216* MIRIAM FRUTTA conversano       disattivata
090217*         MOVE    20170621        TO  WS-DATE-SCAD
      *
230709* NARICI  az. import-export RUTIGLIANO
171209*    CD
171209*         MOVE     20110718       TO  WS-DATE-SCAD        
111114*****          MOVE    20141127        TO  WS-DATE-SCAD
091018*          MOVE    20190206        TO  WS-DATE-SCAD
      *
270309* NICOL FRUTTA
120417*         MOVE    20171010        TO  WS-DATE-SCAD
      *
190421* Nuova Ortofrutta D'Ambruoso srl
030924*         MOVE    20250715        TO  WS-DATE-SCAD
      *
200306* Ortofrutta2000
161023*         MOVE    20241119        TO  WS-DATE-SCAD
      *
210920*Ortofrutta LA RINASCITA srl  - BARI
120124*         MOVE    20250128        TO  WS-DATE-SCAD
      *
021219*Ortofrutta PAZIENZA SRL   operativa fino al 30/04/21
270421*         MOVE    20210531        TO  WS-DATE-SCAD
270421*ex Ortofrutta PAZIENZA SRL-ora RAFFAELE srls operativa dal 01/05/21
170122*         MOVE    20220628        TO  WS-DATE-SCAD                  
      *
091215* Ortofrutticola Alda snc - BARI
091215*       MOVE      20160128        TO  WS-DATE-SCAD
      *
231221* QUADRIFOGLIO /SICOM - MOLFETTA
231221*         MOVE    20220208        TO  WS-DATE-SCAD
      *
091120* RAINIERI snc l   .bari
130224*         MOVE    20250211        TO  WS-DATE-SCAD
      *
220905* ORTARI 
310714*       MOVE      20141117        TO  WS-DATE-SCAD
      *
271020* SACS srl  - COM FASANO
050324*         MOVE    20250318        TO  WS-DATE-SCAD
      *
270409* SCARDINO F.LLI srl      disattivata
100412*       MOVE      20120406        TO  WS-DATE-SCAD
      *
291207* SUD FRUTTA               disattivata
270910*       MOVE      20110322        TO  WS-DATE-SCAD
      *
131114* SOLFRUTTA-  MAGNI
050324*       MOVE      20250328        TO  WS-DATE-SCAD
      *
080609* TERRAFINO SRL     import-export      
160913*         MOVE    20140428        TO  WS-DATE-SCAD

011208* VINCI DANIELE SRL Fasano                          
030924*         MOVE    20250306        TO  WS-DATE-SCAD

101210* VINCI DANIELE SRL Fasano Magazzino
100413*         MOVE    20131015        TO  WS-DATE-SCAD

231024* vep
231024*       move   20251124  to ws-date-scad
      *
*******STANDARD --------------------------------------------------------
      *
181108************  ABBONDANZA GRAZIA (PALESE)                             
150915*          MOVE    20151126        TO  WS-DATE-SCAD
      *
031207************  AF MODA
021111*       MOVE      20141025        TO  WS-DATE-SCAD  CD.....
110310*       MOVE      20110329        TO  WS-DATE-SCAD
      *
070115************  ALMA SHOP di MARCO BARBIERI - BRINDISI  ha chiuso ad Agosto2018
070818* ora LUXURY da MAssafra
201022*        MOVE      20230405        TO  WS-DATE-SCAD  
      *
160605* APPICE  MINIMARKET  ************NON + NS CLIENTE
130224*       MOVE      20240924        TO  WS-DATE-SCAD  
      *
160605************  ASG ZHOFRA -BITETTO     ***CHIUSO
290317*       MOVE      20180308        TO  WS-DATE-SCAD
      *
071016************  BANKALI - BRINDISI     ***CHIUSO
070918*       MOVE      20190115       TO  WS-DATE-SCAD  
      *
031014* JOLLY MARKET snc  DI BARBERIO GIOVANNI & C. - SIMEONE
050324*         MOVE    20250320        TO  WS-DATE-SCAD
      *
211211************  CARDINALE MARIA santeramo   
190220*       MOVE      20210127        TO  WS-DATE-SCAD  
      *
270515************  CASALINDA di Appice N. - BITETTO     CHIUSO
091018*       MOVE      20181204        TO  WS-DATE-SCAD  

220213************  CUORI E STELLINE BITETTO *** chiuso
250314*       MOVE      20150216        TO  WS-DATE-SCAD  
      *
      *
041206************  PUNTO BIANCO di SASSONE PALMA (ex BELLINI SONIA)
180319*       MOVE      20200225        TO  WS-DATE-SCAD
      *
080212* CARAMIA FERRAMENTA -MARTINA FRANCA
030924*       MOVE      20250918        TO  WS-DATE-SCAD

080212* CARAMIA FERRAMENTA -MARTINA FRANCA  old ditta snc
030518*       MOVE      20190519      TO  WS-DATE-SCAD
      *
291110************  CASEIFICIO SAN MARCO *** disabilitata 150216
250215*       MOVE      20160216        TO  WS-DATE-SCAD
      *
230108************  CLEAN HOUSE di DEMARCO MARIANNA  era 21febb09 ************
300109*       MOVE      20100217        TO  WS-DATE-SCAD  CD
060613*       MOVE      20140616        TO  WS-DATE-SCAD
      *
170708* COMAS di DE FEUDIS LUCIA(exPROFESSIONALLINEdiLEO ANGELA)TERLIZZI 
220708*    CD
220708*         MOVE    20101110        TO  WS-DATE-SCAD        
      *....................................
141123*         MOVE    20241105        TO  WS-DATE-SCAD
      *
230108************  DETER LINE di Savino Berardi
230108*    IF LDATE-ACTL GREATER 20100911  CD
140319*       MOVE      20200324        TO  WS-DATE-SCAD

141114************  DIGITAL SYSTEM srl GRUMO APPULA
271015*       MOVE      20160304        TO  WS-DATE-SCAD  
      *
051208************  DOMUSLINDA.IT di AMORESE GIANLUCA 13012010   via dominzoni 
230915*         MOVE    20160309        TO  WS-DATE-SCAD  

060410************  DOMUSLINDA SNC di AMORESE GIANLUCA  VIA trani
021019*       MOVE      20200908      TO  WS-DATE-SCAD
      *
190711**************** DOMUSLINDA SNC di AMORESE GIANLUCA  via Gozzi
200313*       MOVE      20140318      TO  WS-DATE-SCAD
      *
161107* EDDYCART
030924*       MOVE      20250923        TO  WS-DATE-SCAD                  
181116* EDDYCART TABLET antonello   -- disattivata190321
170920*       MOVE      20210330        TO  WS-DATE-SCAD
      *
240311* EDDYCARTportatile antonello -- disattivata
241114*       MOVE      20150325        TO  WS-DATE-SCAD  
      *
230217************  EMPORIO DEL RISPARMIO cina - BARLETTA 
100518*       MOVE      20181113       TO  WS-DATE-SCAD  
      *
140714************ .I FEEL FOOD  (MESAGNE-BR) chiuso 300615
110615*       MOVE      20150728        TO  WS-DATE-SCAD
      *
240210************  I SOGNI DI EMMA (NOICATTARO)
290313*       MOVE      20151020        TO  WS-DATE-SCAD
      *
061014* I PIACERI DEL GUSTO di SARRO VINCENZO (BITONTO)
201021*       MOVE      20220915        TO  WS-DATE-SCAD
      *
110311* L'EMPORIO DEL PARRUCCHIERE sas di Paparella F.sco &C.
050324*       MOVE      20250327        TO  WS-DATE-SCAD
      *
051119*LA BOTTEGA DA UGO FLO SRLS  - CAZZATO MARCO                
130624*         MOVE    20250627        TO  WS-DATE-SCAD
      *
221110************ LA FATINA DEL PULITO DI PACCIONE ANGELA
130415*       MOVE      20160203        TO  WS-DATE-SCAD
      *
151107************  LIMITONE
111213*       MOVE      20141202        TO  WS-DATE-SCAD

101111************  LIRIDE SRL LECCE
190412*       MOVE      20131021        TO  WS-DATE-SCAD
      *
181113* L'ANGOLO DELLE BONTA' - LUIGI SILECCHIA
080424*         MOVE    20250327        TO  WS-DATE-SCAD
      *
100211************  Macchia Margherita - LA BUSSOLA DEI SOGNI - GRUMO APPULA
310113*       MOVE      20140312        TO  WS-DATE-SCAD
      *
110712* Margherita Casalinghi di Florio Angela - MODUGNO(BA)
130224*         MOVE    20250221        TO  WS-DATE-SCAD
      *
170108************  marchegiano  
140618*       MOVE      20190611        TO  WS-DATE-SCAD
      *
020212************  MINI MARKET DI LIUTI MAURIZIO - BRINDISI
110213*       MOVE      20140318       TO  WS-DATE-SCAD
      *
070109* New Garden Service srl - Bari                      
130224*       MOVE      20250225        TO  WS-DATE-SCAD

251114************  Non solo pane minimarket - CARBONARA  / *** DISDETTO
031117*       MOVE      20180522         TO  WS-DATE-SCAD
      *
251007************  OFF srl sede
170720*       MOVE      20210216        TO  WS-DATE-SCAD  

130217************  OFFgallipoli   su vend.dettaglio
130217*       MOVE      20170504        TO  WS-DATE-SCAD  
      *
280312* OTTICA più  (exBICA srl)  MONOPOLI (BA)  (ex 40com Monopoli)
120224*         MOVE    20250227        TO  WS-DATE-SCAD  
      *
251017************  PANDAMARKET srlsu cina - ACQUAVIVA
081118*       MOVE      20191017       TO  WS-DATE-SCAD  
      *
020211* PANIFICIO 2000  N.INST 71 ult.scad.20120710 ha richiamà il 22.01.2015                 
220115*       MOVE      20150520        TO  WS-DATE-SCAD
      *
101007************  PATELLA sas C.SO ITALIA,36 - TORREMAGGIORE (FG)  LUI
290518*       MOVE      20190523        TO  WS-DATE-SCAD     
      *
101007************  PATELLA sas Via LUIGI RISSI,93 - TORREMAGGIORE (FG)  LEI
120417*       MOVE      20180405        TO  WS-DATE-SCAD          
      *
300712************  "PUGLIA BILANCE" - CAPURSO (BA)
260613*       MOVE      20140429        TO  WS-DATE-SCAD
      *
060309* QUARANTAOTTICA di VANESSA CORDELLA (ex QUARANTA COM srl)
251016*          MOVE    20170216        TO  WS-DATE-SCAD
291216*  nuova gestione dal 29/12/16 Vanessa ha venduto ai dipendenti
      *  OTTICA DE NUZZO 
130624*          MOVE    20250627        TO  WS-DATE-SCAD
      *
020904************  RIZZI PACIFICO
150515*       MOVE      20160217        TO  WS-DATE-SCAD
      *
211107************  SHAMPOO
060313*       MOVE      20140312        TO  WS-DATE-SCAD   
211107************  SHAMPOO--IMEX   -npv.. ITALY  ***n.b.mettere scad a un anno***
210217*       MOVE      20180306        TO  WS-DATE-SCAD   
      *
240314************  TOP QUALITY DI MARTUCCI DOMENICO &C. SAS -GIOIA DEL COLLE
240314*       MOVE      20141020        TO  WS-DATE-SCAD
      *
280217************  TORRE GUACETO MULTISERVIZI srl - NOICATTARO
280217*       MOVE      20180320        TO  WS-DATE-SCAD
      *
      *--...OLD CLIE....----------------------------------------------
160108* SECONDO LAVAGGIO *************************
160108*     IF LDATE-ACTL GREATER 20100912 CD
250209*           DISABILITATA
240908*       MOVE      20090225        TO  WS-DATE-SCAD
      *  DE FLORIO LAURA - MOLA
051112*        MOVE      20130212        TO  WS-DATE-SCAD
250111* MANUELA Store di Panaro Emanuela - BRINDISI   ----CHIUSO *******
190911*         MOVE    20120125        TO  WS-DATE-SCAD
      *...............................................................
230310* PUGLIA EXPORT FRUITS srl  Bisceglie ****************************
080212*       MOVE      20120710        TO  WS-DATE-SCAD
221209*  ora AZ.AGRIC.RUBINO WALTER  PUGLIA TRADE srl (export) *******
151111*       MOVE      20120306        TO  WS-DATE-SCAD
221209* PUGLIA TRADE srl (export) ora AZ.AGRIC.RUBINO WALTER *******
151111*       MOVE      20120221        TO  WS-DATE-SCAD
020508* RISO SRL az.agricola Monopoli   import-export *****************
160909*         MOVE    20091116        TO  WS-DATE-SCAD
      *...............................................................
080113* AGRITECNICA DINAMICA SRL (PASTORE GIUSEPPE)
080113*       MOVE      20130918        TO  WS-DATE-SCAD
290708* BUONO MASSIMO - supermercato BARI  (LA BOTTEGA VERDE)
290708*   CD     MOVE                    TO  WS-DATE-SCAD        
040210*         MOVE    20120124        TO  WS-DATE-SCAD    
280308* CAM SERVIZI COMM. srl ******************************************
280308*       MOVE      20090210        TO  WS-DATE-SCAD
041007* Copygrafica MR *************************************************
130509*       MOVE      20090529        TO  WS-DATE-SCAD
300310* COSMO ANTONELLA 
300310*       MOVE      20100429        TO  WS-DATE-SCAD
220904* Favia*************************************************
251105*       MOVE      20060326        TO  WS-DATE-SCAD
251110* GIOIE DI CASA DI PERTA COSIMA - POLIGNANO- moglie PINUCCIO
310111*         MOVE    20110419        TO  WS-DATE-SCAD
020707* HUMUS   ********************************************************
250110*       MOVE      20100628        TO  WS-DATE-SCAD
071111* "IL MAPPAMONDO" cartolibreria di DE MARCO F.SCO (BITETTO)
130913*       MOVE      20141014        TO  WS-DATE-SCAD

301106* LABIANCA non abilitato *****************************************
110308*       MOVE      20081022        TO  WS-DATE-SCAD        
271008*ex LOGISTICOMsrl[Fm40 sede + ditta RV] xVINCENZO OFF oldditta****
030309*       MOVE      20090928        TO  WS-DATE-SCAD
260407* Luna Park di Colella Giuseppe Noicattaro
011210*       MOVE      20110628        TO  WS-DATE-SCAD   
020807* Magest srl  di Mauro Mancino Casamassima PUNTO VENDITA(BA)
200312*       MOVE      20130924        TO  WS-DATE-SCAD
290404* S.A.V. (ex MAITè..ex VR RICAMBI)********************************
281010*       MOVE    20110202        TO  WS-DATE-SCAD
030910* (ex VR RICAMBI)MIMMO  (farà nuova ditta?)********************
030910*       MOVE    20101231        TO  WS-DATE-SCAD
300309* Major srl Casamassima (BA)
300309*       MOVE    20090921        TO  WS-DATE-SCAD
050609* MASELLIS  
050609*       MOVE      20090714        TO  WS-DATE-SCAD
301106* metaldent
210709*       MOVE      20101010        TO  WS-DATE-SCAD
120907* Nonsolocarta di Spina Annamria (ex Montedoro Angela) Torremaggiore(FG)
190911*       MOVE      20120227        TO  WS-DATE-SCAD
170108* OFF srl via marconi       ????????????????????????
110308*       MOVE      20080918        TO  WS-DATE-SCAD  
170108* OFF srl via panareo non ancora attivo ????????????????????????
170108*       MOVE      20080428        TO  WS-DATE-SCAD  SET
310713* OFF srl taranto dal 01/09 NON USERANNO + SIGESTA 
310713*       MOVE      20130909        TO  WS-DATE-SCAD  
181011* OFFICINE SCHENA SRL    ***sbloccato il 150114       
291013*       MOVE      20131231        TO  WS-DATE-SCAD  
121010* OSTUNI FG srl - OSTUNI (BR)
041013*       MOVE      20140226        TO  WS-DATE-SCAD 
121010* PACE FRANCESCO "Il market spesa si" - BITETTO (BA)
130212*       MOVE      20120507        TO  WS-DATE-SCAD 
151107* PESCE
140312*         MOVE    20121009        TO  WS-DATE-SCAD
020311* RAG.SPILOTRO commercialista NARICI (97)
230611*       MOVE      20111219        TO  WS-DATE-SCAD
131112* SAPORI E TRADIZIONI DI CHIATANTE A.M.&C. SAS -BRINDISI
160113*       MOVE      20131113        TO  WS-DATE-SCAD
200306* STYLMODA DI CENTRONE VITINA*********************************
220208*       MOVE      20090210        TO  WS-DATE-SCAD
270213* VALENTE NICOLA Dott.Commerc.(Bisceglie)
270213*       MOVE      20130626        TO  WS-DATE-SCAD   
      *---------------------------------------------------------------
      *
      *
041209     IF L-IDENUTEN EQUAL "SYSADM  "
041209         MOVE    WS-DATE-SCAD    TO  F0S-DATA-LIMI
041209         MOVE    " "             TO  F0S-LIMI-SCAD
041209         MOVE    6               TO  DSTF-IOSR
041209       CALL  "URHXTB20"          USING F0S LLCOMAR
041209                                             ELSE
040308       IF LDATE-ACTL LESS WS-DATE-SCAD 
               IF WS-DATE-SCAD GREATER F0S-DATA-LIMI OR
                  F0S-DATA-LIMI EQUAL  99999999
                 MOVE  WS-DATE-SCAD    TO  F0S-DATA-LIMI
                 MOVE  " "             TO  F0S-LIMI-SCAD
                 MOVE  6               TO  DSTF-IOSR
                 CALL  "URHXTB20"      USING F0S LLCOMAR
               END-IF

               IF  F0S-LIMI-SCAD EQUAL "1"
041209           PERFORM RV01-DISP-MESG

               END-IF
             END-IF

  "          IF LDATE-ACTL NOT LESS WS-DATE-SCAD 
               MOVE    "1"             TO  F0S-LIMI-SCAD
               MOVE    6               TO  DSTF-IOSR
               CALL  "URHXTB20"        USING F0S LLCOMAR

041209         PERFORM RV01-DISP-MESG

             END-IF
      *      Close menu'
               MOVE    -9              TO  DSTF-IOSR
             CALL  "URHXTB20"          USING F0S LLCOMAR
           END-IF
           
           EXIT PROGRAM.
      *
      *----> Display messaggi errore
041209 RV01-DISP-MESG.
110823*          DISPLAY MESSAGE BOX "Errore runtime -992:H1XT",
040308*                          TITLE "ATTENZIONE" 
  "   *                          TYPE 1
  "   *                          ICON 3
  "   *                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
140709*          DISPLAY MESSAGE BOX "ERRORE FILE TABELLE",
  "   *                          TITLE "Errore" 
  "   *                          TYPE 1
  "   *                          ICON 3
140709*                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
011209*          DISPLAY MESSAGE BOX "ERRORE  MENU'",
  "   *                          TITLE "Errore" 
  "   *                          TYPE 1
  "   *                          ICON 3
011209*                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
      *demo mario
090310*     DISPLAY MESSAGE BOX "DEMO SCADUTA ",
  "   *                          TITLE "Errore" 
  "   *                          TYPE 1
  "   *                          ICON 3
090310*                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
271223          DISPLAY MESSAGE BOX "Program missing or inaccessible",
151111                          TITLE "Error of application" 
  "                             TYPE 1
  "                             ICON 3
  "                             RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
190522*          DISPLAY MESSAGE BOX    
100513*     "L'istruzione a '0x7c920ed4' ha fatto riferimento alla memori  
  "   *-    "a a '0xfffffff8'. La memoria non poteva essere 'read'.",
  "   *      h"0a" "Fare clic su OK per terminare l'applicazione",
  "   *                          TITLE "Error of application" 
  "   *                          TYPE 1
  "   *                          ICON 3
  "   *                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
191213*          DISPLAY MESSAGE BOX    
191213*           "The type of file system is NTFS " ,
  "   *-    h"0a" "                                ",
  "   *     h"0a" "One of your disks needs to be checked for consistency
  "   *-          ".You", 
  "   *-    h"0a" "may cancel the disk check, but it is strongly recommen
  "   *-          "ded", 
  "   *-    h"0a" "that you continue.",
  "   *                          TITLE "Error of application" 
  "   *                          TYPE 1
  "   *                          ICON 3
  "   *                          RETURNING L-RETN-VALO
      **-----------------------------------------------------------------
090721*          DISPLAY MESSAGE BOX    
310314*          "Errore runtime -992:HXMNUC",
  "   *                          TITLE "ATTENZIONE" 
  "   *                          TYPE 1
  "   *                          ICON 3
  "   *                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
191121*          DISPLAY MESSAGE BOX    
  "   *          "Errore durante l'avvio di ",
  "   *-    h"0a" "\\progra~1\COMMON~1\System\SysMenu.dll",
  "   *-    h"0a" "   ", 
  "   *-    h"0a" "Impossibile trovare il modulo specificato.", 
  "   *                          TITLE "RunDLL"     
  "   *                          TYPE 1
  "   *                          ICON 3
  "   *                          RETURNING L-RETN-VALO
      *-----------------------------------------------------------------
                IF L-RETN-VALO EQUAL 1
                  MOVE   1             TO  LSTAT-CORR
                  EXIT PARAGRAPH            
                END-IF

010210*            MOVE   1             TO  IND
  "   *     PERFORM VARYING IND FROM 1 BY 1 UNTIL IND < 1
  "   *            ADD   1             TO  IND
  "   *     END-PERFORM
              .
      *(END)

