modificato da branch caramia
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      GBASE001.
       DATE-WRITTEN.                    22/11/2002.
       DATE-COMPILED.                   22/11/2002.
      ****************************************************************
      *     MAIN  MENU'  - GBASE001 - VERSIONE NO TOUCH
      *  N.B.  UGUALE A  - GBASET01 - SE SI MODIFICA AGGIORNARE L'ALTRO        
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
      *{Bench}activex-def
      *{Bench}end
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *{Bench}file-control
      *{Bench}end
       DATA DIVISION.
       FILE SECTION.
      *
      *{Bench}file
      *{Bench}end
       WORKING-STORAGE SECTION.
      *{Bench}acu-def
      *{Bench}end
       77  WS-NOME-PROG                PIC X(8)  VALUE "GBASE001".
021107 77  WS-CTRL-PACK                PIC S9    COMP-3 VALUE 0.
   "   77  CTR                         PIC S999  COMP-3 VALUE 0.
071009 77  WS-READ-FILE                PIC S999   COMP-3 VALUE 0.
071009 77  WS-DSTF-IOSR                PIC S999   COMP-3 VALUE 0.
071009 77  WS-CODE-PROG                PIC S9(5)  comp-4 value 0.
      * indica la presenza del touch screen 1=no/ 2=si
       77  WS-TUCH-SCRN    PIC 9     VALUE 1.
      *
       COPY "ACUGUI.DEF".
       COPY "CRTVARS.DEF".
       COPY "FONTS.DEF".
       copy "TOTEM.DEF".
       COPY "ACUCOBOL.DEF".
       COPY "SHOWMSG.DEF".
      *
       COPY "GBASE001.WRK".
      *
       COPY "LWSMCARE.WRK".
       COPY "LLCOM201.WRK".
       COPY "LLCARENU.WRK".
021107
       COPY "F09.REC".
071009
       COPY "RMN.REC".
       COPY "FAN.REC".
      *
      *{Bench}copy-working
      *{Bench}end
       LINKAGE SECTION.
      *
      *{Bench}linkage
      *{Bench}end
       SCREEN SECTION.
      *
       COPY "GBASE001.SCR". 
      *
      *{Bench}copy-screen
      *{Bench}end
       PROCEDURE DIVISION              CHAINING LWCOMAR.
      * main
       Main Section.
130904      ACCEPT SYSTEM-INFORMATION FROM SYSTEM-INFO
130904         MOVE   OPERATING-SYSTEM TO  L-IDEN-OS
200505      MOVE  FUNCTION CURRENT-DATE   TO  LDATEHORA
      *
      *---- call utente
      *
            IF  WS-TUCH-SCRN EQUAL 2
021107       CALL  "GSUTUC10"            USING LWCOMAR F09 ELSE
   "         CALL  "GSUTEN10"            USING LWCOMAR F09
           END-IF
           IF  LKEY-STATUS EQUAL 1
             PERFORM R100-LAV
           END-IF


           Exit Program
           Stop Run.

      *-------------------------------------------
      *   Inizio 
       R100-LAV.
               MOVE    WS-NOME-PROG      TO  LNOME-PRO2
               MOVE    "00"              TO  LPACK-ID 
           PERFORM ACU-INIT-FONT.
           PERFORM ACU-INIT-BMP.

071009         MOVE    SPACES          TO  W-NOME-UTEN W-INDR-UTEN
071009                                     W-CITT-UTEN
071009*lettura parte anagrafica HX00MNUC e display dati in screen
071009       PERFORM RF30-READ-MENU   

             PERFORM ACU-HBASE001-Scrn       

071009       DISPLAY V-NOME-UTEN V-INDR-UTEN V-CITT-UTEN

             MOVE  0                   TO  L-MAST-CLID L-MAST-FORD
      *
      *       MOVE      0               TO  LSTAT-CORR
      *     CALL  "UBASE001"            USING LWCOMAR
110906*    IF  LSTAT-CORR GREATER 0
  "   *        EXIT PARAGRAPH            
  "   *        MOVE   0             TO  KEY-STATUS
  "   *    END-IF
      
      *---- call  ditta
               MOVE    1               TO  LCODE-DITN
           CALL "GSDITT00"             USING LWCOMAR
            IF  LKEY-STATUS NOT EQUAL 1
              PERFORM R900-Exit
            END-IF

071009     IF F09-IDEN-UTEN NOT EQUAL "SYSADM" AND
  "              F09-PASS-UTEN NOT EQUAL "MAN"
  "          EVALUATE TRUE
  "            WHEN FAN-CODE-CLIE NOT EQUAL L-NUMR-INST
  "              DISPLAY MESSAGE BOX "UTENTE NON ABILITATO",
  "                     H"0A" "M " , FAN-CODE-CLIE
  "                     H"0A" "D " , L-NUMR-INST
  "                             TITLE "ATTENZIONE"
  "                             TYPE 1
  "                             ICON 2
  "              EXIT PARAGRAPH            
  "              MOVE  0               TO  KEY-STATUS
  "          END-EVALUATE
071009     END-IF

260710     EVALUATE L-NUMR-INST
   "  * -----   VERSIONE food  (300 vers. DEMO)
             WHEN 300 THRU 400
260710         MOVE    8                 TO  LVERS-GEST
             WHEN OTHER
040205         MOVE    0                 TO  LVERS-GEST
           END-EVALUATE
      *---
300710*spostata per lettura ditta per n.ro inst.
             MOVE      0               TO  LSTAT-CORR
           CALL  "UBASE001"            USING LWCOMAR
110906     IF  LSTAT-CORR GREATER 0
  "           EXIT PARAGRAPH            
  "           MOVE   0             TO  KEY-STATUS
  "        END-IF
      *-----
050505     DESTROY HBASE001-HANDLE

050505       MOVE    TV-RAGE-DITT        TO  L-TITL-FUNC
050505       MOVE    LTITL-FORM          TO  TV-RAGE-DITT

           PERFORM ACU-HBASE001-Scrn  
      *
051009       PERFORM RG01-DISA-RILA
121009       PERFORM RF30-CLOS-MENU
      *
             MOVE FUNCTION  CURRENT-DATE TO  LDATEHORA
      *       
      * accetta dati
           PERFORM UNTIL Exit-Pushed
              ACCEPT OMITTED LINE 1 COL 1
                 ON EXCEPTION
                    PERFORM R150-LAV
              END-ACCEPT
           END-PERFORM
      *
      *     DESTROY Eras-Medium-ITC9B
      *     DESTROY Arial9
      *     CALL "w$bitmap" USING WBITMAP-DESTROY, sigesta1-jpg
      *     CALL "W$MENU" USING WMENU-DESTROY-DELAYED 
      *     HBASE001-Mn-1-Handle

           DESTROY HBASE001-HANDLE
           INITIALIZE Key-Status
           .
      *
      * routines
      *accetta dati
       R150-LAV.
250505     IF LUTEN-BASE 
   "         DISPLAY MESSAGE BOX " Utente non Autorizzato ",
   "                             TITLE "Attenzione !!!"
   "                             TYPE 1
   "                             ICON 2
   "             PERFORM R900-Exit
   "       END-IF
      *
           EVALUATE TRUE
             WHEN Exit-Pushed
               PERFORM R900-Exit
             WHEN Event-Occurred
               IF Event-Type = Cmd-Close
                 PERFORM R900-Exit
               END-IF
           END-EVALUATE

            EVALUATE KEY-STATUS
      *-- annulla
             WHEN 2
               PERFORM R900-EXIT
      *--- File
              WHEN 4001 THRU 4050
               IF  KEY-STATUS NOT EQUAL 4030
                PERFORM RM01-CALL-FILE  ELSE
      *
      *....     call cambio ditta
                CALL "GSDITT00"             USING LWCOMAR
                DESTROY HBASE001-HANDLE
050505            MOVE    TV-RAGE-DITT        TO  L-TITL-FUNC
050505            MOVE    LTITL-FORM          TO  TV-RAGE-DITT
121009           PERFORM RF30-READ-MENU   
                 PERFORM ACU-HBASE001-Scrn       
071009           PERFORM RG01-DISA-RILA
121009           PERFORM RF30-CLOS-MENU
               END-IF
180407        WHEN 4093 THRU 4094
180407          PERFORM RM01-CALL-FILE  
      *

      *--- Tabelle
              WHEN 1000 THRU 1004
                PERFORM RM02-CALL-TABE
211008        WHEN 1099
   "            PERFORM RM02-CALL-TABE
              WHEN 2000 THRU 2005
                PERFORM RM02-CALL-TABE
      *--- Anagrafiche
              WHEN 1101 THRU 1108
                PERFORM RM03-CALL-ANAG
              WHEN 2101 THRU 2110
                PERFORM RM03-CALL-ANAG
      *--- Contabilità Generale
              WHEN 1010 THRU 1015
                PERFORM RM20-CALL-COGE
              WHEN 1043 THRU 1045
310320        WHEN 1050
                PERFORM RM20-CALL-COGE
090316        WHEN 1121 THRU 1138
                PERFORM RM20-CALL-COGE
              WHEN 2010 THRU 2015
                PERFORM RM20-CALL-COGE
              WHEN 2300 THRU 2315
                PERFORM RM20-CALL-COGE
      *--- Partite scoperte 
090206        WHEN 1901 THRU 1951
090206          PERFORM RM21-CALL-PASC
              WHEN 2901 THRU 2951
                PERFORM RM21-CALL-PASC
      *--- Magazzino
              WHEN 1020 THRU 1023
                PERFORM RM25-CALL-MAGA
              WHEN 1141 THRU 1146
                PERFORM RM25-CALL-MAGA
              WHEN 1181 THRU 1185
                PERFORM RM25-CALL-MAGA
              WHEN 1390 THRU 1420
                PERFORM RM25-CALL-MAGA
150210        WHEN 1426
                PERFORM RM25-CALL-MAGA
141015        WHEN 1429 THRU 1430                                        120912
120912          PERFORM RM25-CALL-MAGA
              WHEN 2020 THRU 2025
                PERFORM RM25-CALL-MAGA
              WHEN 2141 THRU 2175
                PERFORM RM25-CALL-MAGA
              WHEN 2177 THRU 2197
                PERFORM RM25-CALL-MAGA
100222        WHEN 3359
100222          PERFORM RM25-CALL-MAGA
      *--- Vendite
              WHEN 1030 THRU 1041
                PERFORM RM26-CALL-GEVE
              WHEN 1360
                PERFORM RM26-CALL-GEVE
              WHEN 1191 THRU 1287
                PERFORM RM26-CALL-GEVE
              WHEN 1421 THRU 1425
                PERFORM RM26-CALL-GEVE
120912        WHEN 1427 THRU 1428
                PERFORM RM26-CALL-GEVE
120912        WHEN 1430
                PERFORM RM26-CALL-GEVE
              WHEN 1470
                PERFORM RM26-CALL-GEVE
260718        WHEN 1480 THRU 1482
                PERFORM RM26-CALL-GEVE
              WHEN 2030 THRU 2045
                PERFORM RM26-CALL-GEVE
              WHEN 2176
                PERFORM RM26-CALL-GEVE
              WHEN 2200 THRU 2260
                PERFORM RM26-CALL-GEVE
      *--- Statistiche
              WHEN 1110 THRU 1119
                PERFORM RM27-CALL-STAT
051107*--- Gestione Ordini
              WHEN 1291 THRU 1295
160922        WHEN 1297
                PERFORM RM29-CALL-GEOR
              WHEN 1299
                PERFORM RM29-CALL-GEOR
              WHEN 2291 THRU 2297
                PERFORM RM29-CALL-GEOR
      *--- Help
              WHEN 4051
                PERFORM RMH2-CALL-HELP

      *--- Conversioni
              WHEN 3020  THRU 3049
                PERFORM RME5-CALL-CONV
              WHEN 3050
181018        WHEN 3058
170214        WHEN 3052 THRU 3055                                        
                PERFORM RME5-CALL-CONV
              WHEN 3070 THRU 3072
                PERFORM RME5-CALL-CONV
              WHEN 3111
                PERFORM RME5-CALL-CONV
060212        WHEN 3220 THRU 3223
                PERFORM RME5-CALL-CONV
100214        WHEN 3225
100214          PERFORM RME5-CALL-CONV

            END-EVALUATE


              PERFORM R200-LAV
                IF LSELE-BASE = 4050
                  PERFORM R900-Exit
                END-IF
      *
250507*         Passaggio anno/cambio ditta
290507          EVALUATE LSELE-BASE
290507            WHEN 1131 THRU 1134
250507              CALL "GSDITT00"    USING LWCOMAR
250507              DESTROY HBASE001-HANDLE
250507              MOVE TV-RAGE-DITT  TO  L-TITL-FUNC
250507              MOVE LTITL-FORM    TO  TV-RAGE-DITT
121009              PERFORM RF30-READ-MENU   
250507              PERFORM ACU-HBASE001-Scrn  
071009              PERFORM RG01-DISA-RILA
121009              PERFORM RF30-CLOS-MENU
250507          END-EVALUATE
      *
040509       DESTROY HBASE001-HANDLE
040509         MOVE    TV-RAGE-DITT    TO  L-TITL-FUNC
040509         MOVE    LTITL-FORM      TO  TV-RAGE-DITT
121009       PERFORM RF30-READ-MENU   
040509       PERFORM ACU-HBASE001-Scrn  
071009       PERFORM RG01-DISA-RILA
121009       PERFORM RF30-CLOS-MENU
      * avoid changing focus
               MOVE    4               TO  Accept-Control

241006      CANCEL ALL
               .         
      *
      * chiusura maschera
       R900-Exit.
           SET Exit-Pushed TO TRUE.
      * 

      *
      * call programmi base selezionati da menù
       R200-LAV.
               MOVE    KEY-STATUS      TO  LSELE-BASE

021107       PERFORM  R600-CTRL-PACK
  "        IF WS-CTRL-PACK EQUAL 1
             EVALUATE LPACK-ID
               WHEN  "01"
                 CALL  "MBASE001"        USING LWCOMAR

               WHEN  "02"
                 CALL  "MBASE002"        USING LWCOMAR
               
               WHEN  "03"
                 CALL  "MBASE003"        USING LWCOMAR

               WHEN  "20"
                 CALL  "MBASE020"        USING LWCOMAR

               WHEN  "21"
                 CALL  "MBASE021"        USING LWCOMAR

               WHEN  "25"
                 CALL  "MBASE025"        USING LWCOMAR

               WHEN  "26"
                 CALL  "MBASE026"        USING LWCOMAR

               WHEN  "27"
                 CALL  "MBASE027"        USING LWCOMAR
                
               WHEN  "29"
                 CALL  "MBASE029"        USING LWCOMAR

               WHEN  "E0"
                 CALL  "MBASE0E0"        USING LWCOMAR

               WHEN  "E5"
                 CALL  "MBASE0E5"        USING LWCOMAR

               WHEN  "H2"
                 CALL  "MBASE0H2"        USING LWCOMAR

             END-EVALUATE
           END-IF
               MOVE    WS-NOME-PROG    TO  LNOME-PRO2
      *
           .
      *
      *    -----------  CARICA TIPO PACKAGE --------
       RM01-CALL-FILE.
               MOVE    "01"            TO  LPACK-ID
               .
       RM02-CALL-TABE.
               MOVE    "02"            TO  LPACK-ID
               .
       RM03-CALL-ANAG.
               MOVE    "03"            TO  LPACK-ID
               .       
       RM20-CALL-COGE.
               MOVE    "20"            TO  LPACK-ID
               .       
       RM21-CALL-PASC.
               MOVE    "21"            TO  LPACK-ID
               .       
       RM25-CALL-MAGA.
               MOVE    "25"            TO  LPACK-ID
               .
       RM26-CALL-GEVE.
               MOVE    "26"            TO  LPACK-ID
               .       
       RM27-CALL-STAT.
               MOVE    "27"            TO  LPACK-ID
               .       
       RM29-CALL-GEOR.
               MOVE    "29"            TO  LPACK-ID
               .       
       RME0-CALL-VARI.
               MOVE    "E0"            TO  LPACK-ID
               .       
       RMH2-CALL-HELP.
               MOVE    "H2"            TO  LPACK-ID
               .       
       RME5-CALL-CONV.
               MOVE    "E5"            TO  LPACK-ID
               .       
021107*
      **------->  Controllo menu'
       R600-CTRL-PACK.
             MOVE      1               TO  WS-CTRL-PACK
021107       PERFORM     VARYING CTR FROM 1 BY 1 UNTIL CTR > 15
  "           IF F09-PACK-UTEN-X(CTR) NOT EQUAL SPACES 
  "             MOVE   0               TO  WS-CTRL-PACK
  "           END-IF
  "          END-PERFORM
  "          IF  WS-CTRL-PACK EQUAL 0
021107        PERFORM VARYING CTR FROM 1 BY 1 UNTIL CTR > 15
  "            IF F09-PACK-UTEN-X(CTR) EQUAL LPACK-ID 
  "             MOVE   1               TO  WS-CTRL-PACK
  "            END-IF
  "           END-PERFORM
             END-IF
             .
      *
      *----------------- G E S T I O N E    M E N U ' ------------------
      *
      *
      *----> Lettura parte anagrafica Menù
       RF30-READ-MENU.
           IF F09-IDEN-UTEN EQUAL "SYSADM" AND F09-PASS-UTEN EQUAL "MAN"
             EXIT PARAGRAPH
           END-IF

      *....    OPEN FILE MENù cliente
               MOVE    -3              TO  DSTF-IOSR
              CALL    "URMNUC30"      USING RMN FAN LWCOMAR
      *
      *....    lettura anagrafica cliente (HX00MNUC)
               MOVE    LOW-VALUE       TO  FAN-KEI0
               MOVE    "A"             TO  FAN-TIPO-RECO DSTF-TRKR
               MOVE    19              TO  LREAD-FILE
               MOVE    3               TO  DSTF-IOSR
              CALL    "URMNUC30"      USING RMN FAN LWCOMAR
           IF DSTF-IOSR EQUAL 1
               MOVE    0               TO  L-MULT-AZIE
             IF FAN-NUMR-DITT GREATER 1
               MOVE    1               TO  L-MULT-AZIE
             END-IF
               MOVE    FAN-RAGE-CLIE   TO  W-NOME-UTEN
               MOVE    FAN-INDR-CLIE   TO  W-INDR-UTEN
               MOVE    FAN-CITT-CLIE   TO  W-CITT-UTEN
           END-IF
            .
      *
      *----> Lettura menù Cliente per disabilitare rilancio
       RG01-DISA-RILA.
           IF F09-IDEN-UTEN EQUAL "SYSADM" AND F09-PASS-UTEN EQUAL "MAN"
             EXIT PARAGRAPH
           END-IF
               MOVE    FAN-CODE-CLIE   TO  RMN-CODE-CLIE
               MOVE    FAN-VERS-GEST   TO  RMN-VERS-GEST
               MOVE    LOW-VALUE       TO  RMN-POSZMENU
               MOVE    "M"             TO  RMN-TIPO-RECO DSTF-TRKR
               MOVE    1               TO  WS-DSTF-IOSR
               MOVE    19              TO  WS-READ-FILE
             PERFORM UNTIL WS-DSTF-IOSR NOT EQUAL 1
               MOVE    WS-READ-FILE    TO  LREAD-FILE
               MOVE    3               TO  DSTF-IOSR
               CALL    "URMNUC30"      USING RMN FAN LWCOMAR
               IF DSTF-IOSR NOT EQUAL 1
                 AND DSTATUS-F NOT EQUAL "10"
                 STOP RUN
               END-IF
               MOVE    DSTF-IOSR       TO  WS-DSTF-IOSR 
               IF WS-DSTF-IOSR EQUAL 1
                 IF FAN-CODE-CLIE NOT EQUAL RMN-CODE-CLIE
                   MOVE  0             TO  WS-DSTF-IOSR  ELSE
                   IF FAN-VERS-GEST NOT EQUAL RMN-VERS-GEST  
                     MOVE  0           TO  WS-DSTF-IOSR  ELSE
                     PERFORM RG01-DELE-MNUC
                   END-IF
                 END-IF
               END-IF
               MOVE    9               TO  WS-READ-FILE               
             END-PERFORM
      *display menù
           MOVE Menu-Handle TO HBASE001-Mn-1-Handle
           CALL "W$MENU" USING Wmenu-Show, HBASE001-Mn-1-Handle
             .
      
      *----> Disabilita rilancio menù
       RG01-DELE-MNUC.
           IF RMN-INDC-MENU EQUAL "D"
               MOVE    RMN-CODE-PROG   TO  WS-CODE-PROG 
            CALL "W$MENU" USING WMENU-DISABLE, MENU-HANDLE, WS-CODE-PROG
           END-IF
           .
      *
      *----> Close File Menù
       RF30-CLOS-MENU.
           IF F09-IDEN-UTEN EQUAL "SYSADM" AND F09-PASS-UTEN EQUAL "MAN"
             EXIT PARAGRAPH
           END-IF
               MOVE    -9              TO  DSTF-IOSR
             CALL    "URMNUC30"      USING RMN FAN LWCOMAR
             .
      *-----------------------------------------------------------------
      *
      * menù          
       COPY "GBASE001.MNU".
       COPY "GBASE001.PRD".
       HBASE001-EVENT-PROC.
       SHOW-MSG-ROUTINE.
       MESSAGE-BOX-ROUTINE.
       HBASE001-AFTERPROCEDURE.
       HBASE001-LINKTO.
       HBASE011-AfterProcedure.
       HBASE011-EVENT-PROC.
      *(END)

      *{Bench}copy-procedure
      *{Bench}end
