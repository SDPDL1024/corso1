       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPCAP010.
      ************************************************************
      *****    FUNZIONE DEL PROGRAMMA :                       ****
      *****                                                   ****
      *****    - RICERCA PROVINCIA DA C.A.P.                  ****
      *----------------------------------------------------------*
      * LINDC-PROV ! FUNZIONE                      ! INPUT       *
      *      1     ! RICERCA PROVINCIA DA CAP      ! LCAP        *
      *      2     !    "       "      DA SIGLA PRV! LSIGL-PROV  *
      *      3     !    "       "      DA DESC. PRV! LDESC-PROV  *
      ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT   IS  COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IND                 PIC S999        COMP-3 VALUE 0.
       01  DCAMP-P5            PIC S9(5)       COMP-3 VALUE 0.
       01  C-TABEPROV.
           03  FILLER          PIC X(17) VALUE "RMROMA           ".
           03  FILLER          PIC X(17) VALUE "VTVITERBO        ".
           03  FILLER          PIC X(17) VALUE "RIRIETI          ".
           03  FILLER          PIC X(17) VALUE "FRFROSINONE      ".
           03  FILLER          PIC X(17) VALUE "LTLATINA         ".
           03  FILLER          PIC X(17) VALUE "TRTERNI          ".
           03  FILLER          PIC X(17) VALUE "PGPERUGIA        ".
           03  FILLER          PIC X(17) VALUE "SSSASSARI        ".
           03  FILLER          PIC X(17) VALUE "NUNUORO          ".
           03  FILLER          PIC X(17) VALUE "CACAGLIARI       ".
           03  FILLER          PIC X(17) VALUE "TOTORINO         ".
           03  FILLER          PIC X(17) VALUE "AOAOSTA          ".
           03  FILLER          PIC X(17) VALUE "CUCUNEO          ".
           03  FILLER          PIC X(17) VALUE "VCVERCELLI       ".
           03  FILLER          PIC X(17) VALUE "ATASTI           ".
           03  FILLER          PIC X(17) VALUE "ALALESSANDRIA    ".
           03  FILLER          PIC X(17) VALUE "GEGENOVA         ".
           03  FILLER          PIC X(17) VALUE "SVSAVONA         ".
           03  FILLER          PIC X(17) VALUE "IMIMPERIA        ".
           03  FILLER          PIC X(17) VALUE "SPLA SPEZIA      ".
           03  FILLER          PIC X(17) VALUE "MIMILANO         ".
           03  FILLER          PIC X(17) VALUE "VAVARESE         ".
           03  FILLER          PIC X(17) VALUE "COCOMO           ".
           03  FILLER          PIC X(17) VALUE "SOSONDRIO        ".
           03  FILLER          PIC X(17) VALUE "BGBERGAMO        ".
           03  FILLER          PIC X(17) VALUE "BSBRESCIA        ".
           03  FILLER          PIC X(17) VALUE "CRCREMONA        ".
           03  FILLER          PIC X(17) VALUE "PVPAVIA          ".
           03  FILLER          PIC X(17) VALUE "NONOVARA         ".
           03  FILLER          PIC X(17) VALUE "PCPIACENZA       ".
           03  FILLER          PIC X(17) VALUE "VEVENEZIA        ".  
           03  FILLER          PIC X(17) VALUE "TVTREVISO        ".
           03  FILLER          PIC X(17) VALUE "BLBELLUNO        ".
           03  FILLER          PIC X(17) VALUE "UDUDINE          ".
           03  FILLER          PIC X(17) VALUE "TSTRIESTE        ".
           03  FILLER          PIC X(17) VALUE "PDPADOVA         ".
           03  FILLER          PIC X(17) VALUE "VCVICENZA        ".
           03  FILLER          PIC X(17) VALUE "VRVERONA         ".
           03  FILLER          PIC X(17) VALUE "TNTRENTO         ".
           03  FILLER          PIC X(17) VALUE "BZBOLZANO        ".
           03  FILLER          PIC X(17) VALUE "BOBOLOGNA        ".
           03  FILLER          PIC X(17) VALUE "MOMODENA         ".
           03  FILLER          PIC X(17) VALUE "REREGGIO EMILIA  ".
           03  FILLER          PIC X(17) VALUE "PRPARMA          ".
           03  FILLER          PIC X(17) VALUE "FEFERRARA        ".
           03  FILLER          PIC X(17) VALUE "ROROVIGO         ".
           03  FILLER          PIC X(17) VALUE "MNMANTOVA        ".
           03  FILLER          PIC X(17) VALUE "FOFORLI'         ".
           03  FILLER          PIC X(17) VALUE "RARAVENNA        ".
           03  FILLER          PIC X(17) VALUE "                 ".
           03  FILLER          PIC X(17) VALUE "FIFIRENZE        ".
           03  FILLER          PIC X(17) VALUE "PTPISTOIA        ".
           03  FILLER          PIC X(17) VALUE "ARAREZZO         ".
           03  FILLER          PIC X(17) VALUE "SISIENA          ".
           03  FILLER          PIC X(17) VALUE "MSMASSA CARRARA  ".
           03  FILLER          PIC X(17) VALUE "LULUCCA          ".
           03  FILLER          PIC X(17) VALUE "PIPISA           ".
           03  FILLER          PIC X(17) VALUE "LILIVORNO        ".
           03  FILLER          PIC X(17) VALUE "GRGROSSETO       ".
           03  FILLER          PIC X(17) VALUE "POPRATO          ".
           03  FILLER          PIC X(17) VALUE "ANANCONA         ".
           03  FILLER          PIC X(17) VALUE "PSPESARO E URBINO".
           03  FILLER          PIC X(17) VALUE "MCMACERATA       ".
           03  FILLER          PIC X(17) VALUE "APASCOLI PICENO  ".
           03  FILLER          PIC X(17) VALUE "TETERAMO         ".
           03  FILLER          PIC X(17) VALUE "PEPESCARA        ".
           03  FILLER          PIC X(17) VALUE "CHCHIETI         ".
           03  FILLER          PIC X(17) VALUE "AQL'AQUILA       ".
           03  FILLER          PIC X(17) VALUE "                 ".
           03  FILLER          PIC X(17) VALUE "BT BAT           ".
           03  FILLER          PIC X(17) VALUE "BABARI           ".
           03  FILLER          PIC X(17) VALUE "FGFOGGIA         ".
           03  FILLER          PIC X(17) VALUE "BRBRINDISI       ".
           03  FILLER          PIC X(17) VALUE "LELECCE          ".
           03  FILLER          PIC X(17) VALUE "TATARANTO        ".
           03  FILLER          PIC X(17) VALUE "MTMATERA         ".
           03  FILLER          PIC X(17) VALUE "                 ".
           03  FILLER          PIC X(17) VALUE "                 ".
           03  FILLER          PIC X(17) VALUE "                 ".
           03  FILLER          PIC X(17) VALUE "                 ".
           03  FILLER          PIC X(17) VALUE "NANAPOLI         ".
           03  FILLER          PIC X(17) VALUE "CECASERTA        ".
           03  FILLER          PIC X(17) VALUE "BNBENEVENTO      ".
           03  FILLER          PIC X(17) VALUE "AVAVELLINO       ".
           03  FILLER          PIC X(17) VALUE "SASALERNO        ".
           03  FILLER          PIC X(17) VALUE "PZPOTENZA        ".
           03  FILLER          PIC X(17) VALUE "CBCAMPOBASSO     ".
           03  FILLER          PIC X(17) VALUE "CSCOSENZA        ".
           03  FILLER          PIC X(17) VALUE "CZCATANZARO      ".
           03  FILLER          PIC X(17) VALUE "RCREGGIO CALABRIA".
           03  FILLER          PIC X(17) VALUE "PAPALERMO        ".
           03  FILLER          PIC X(17) VALUE "TPTRAPANI        ".
           03  FILLER          PIC X(17) VALUE "AGAGRIGENTO      ".
           03  FILLER          PIC X(17) VALUE "CLCALTANISSETTA  ".
           03  FILLER          PIC X(17) VALUE "ENENNA           ".
           03  FILLER          PIC X(17) VALUE "CTCATANIA        ".
           03  FILLER          PIC X(17) VALUE "SRSIRACUSA       ".
           03  FILLER          PIC X(17) VALUE "RGRAGUSA         ".
           03  FILLER          PIC X(17) VALUE "MEMESSINA        ".
           03  FILLER          PIC X(17) VALUE "                 ".
       01  C-TABEPROV-R  REDEFINES C-TABEPROV.
         02  C-TABE-PROV-X                 OCCURS 100.
           03  C-TBSG-PROV-X           PIC XX.
           03  C-TBDS-PROV-X           PIC X(15).
       01  C-TABEPROV1.
           03  FILLER          PIC X(19) VALUE "ORORISTANO       09".
           03  FILLER          PIC X(19) VALUE "PNPORDENONE      33".
           03  FILLER          PIC X(19) VALUE "GOGORIZIA        34".
           03  FILLER          PIC X(19) VALUE "ISISERNIA        86".
       01  CTABEPROV1-R   REDEFINES C-TABEPROV1.
         02  C-TABE-PROV1-X                OCCURS 4.
           03  C-TBSG-PROV1-X          PIC XX.
           03  C-TBDS-PROV1-X          PIC X(15).
           03  C-TBNR-PROV1-X          PIC 99.
      
       01  C-TABEPROV2.
           03  FILLER          PIC X(19) VALUE "BIBIELLA         13".
           03  FILLER          PIC X(19) VALUE "LCLECCO          23".
           03  FILLER          PIC X(19) VALUE "LOLODI           26".
           03  FILLER          PIC X(19) VALUE "VBVERBANIA       28".
           03  FILLER          PIC X(19) VALUE "RNRIMINI         47".
           03  FILLER          PIC X(19) VALUE "KRCROTONE        88".
           03  FILLER          PIC X(19) VALUE "VVVIBO VALENTIA  89".
       01  CTABEPROV2-R   REDEFINES C-TABEPROV2.
         02  C-TABE-PROV2-X                OCCURS 7.
           03  C-TBSG-PROV2-X          PIC XX.
           03  C-TBDS-PROV2-X          PIC X(15).
           03  C-TBNR-PROV2-X          PIC 99. 
      *
       LINKAGE SECTION.
       01  LCTRPROV.
         02  LCTRPROV1.
           03  LINDC-PROV              PIC X.
           03  LCAP                    PIC 9(5).
           03  LDESCPROV.
               05  LSIGL-PROV          PIC XX.
               05  LDESC-PROV          PIC X(15).
         02  LCTRPROV1-R  REDEFINES LCTRPROV1.
           03  FILLER  PIC X.
           03  LCAP--PROV              PIC 99.
           03  LCAP--3                 PIC 9.
           03  LCAP--4                 PIC 9.
           03  LCAP--5                 PIC 9.
           03  FILLER                  PIC X(17).
       PROCEDURE DIVISION              USING LCTRPROV.
       PAR-000.
           IF  LINDC-PROV EQUAL "1"
                   GO  TO  R100-LAV.
           IF  LINDC-PROV EQUAL "2"
                   GO  TO  R200-LAV.
           IF  LINDC-PROV EQUAL "3"
                   GO  TO  R300-LAV.
      *
       EXIT-ERR.
               MOVE    SPACES          TO  LINDC-PROV
               MOVE    0               TO  LCAP
               MOVE    "0"             TO  LINDC-PROV.
      *
       EXIT-PRO.
             EXIT PROGRAM.
      *---------------------------
      *
      *-------> RICERCA PROVINCIA DA CAP
       R100-LAV.
               MOVE    "1"             TO  LINDC-PROV.
           IF  LCAP--4 EQUAL 7
                   GO  TO  R100-100.
           IF  LCAP--3 EQUAL 9
                   GO  TO  R100-120.
               ADD     1       LCAP--PROV   GIVING DCAMP-P5
               MOVE C-TABE-PROV-X(DCAMP-P5) TO LDESCPROV
           IF  LDESC-PROV EQUAL SPACES
                   GO  TO  EXIT-ERR.
                   GO  TO  EXIT-PRO.
      *
       R100-100.
               MOVE    1               TO  IND.
       R100L110.
           IF  LCAP--PROV EQUAL C-TBNR-PROV1-X(IND)
               MOVE C-TABE-PROV1-X(IND)   TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 4
                   GO  TO  R100L110.
                   GO  TO  EXIT-ERR.
      *
       R100-120.
               MOVE    1               TO  IND.
       R100L130.
           IF  LCAP--PROV EQUAL C-TBNR-PROV2-X(IND)
               MOVE C-TABE-PROV2-X(IND)   TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 7
                   GO  TO  R100L130.
                   GO  TO  EXIT-ERR.
      *----------------------------------
      *
      *-------> RICERCA PROVINCIA DA SIGLA PROV.
       R200-LAV.
           IF  LSIGL-PROV NOT GREATER SPACES
                   GO  TO  EXIT-ERR.
               MOVE    "1"             TO  LINDC-PROV
               MOVE    0               TO  LCAP
               MOVE    1               TO  IND.
       R200L010.
           IF  C-TBSG-PROV-X(IND) EQUAL LSIGL-PROV
             SUBTRACT  1   FROM  IND  GIVING LCAP--PROV
               MOVE C-TABE-PROV-X(IND) TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 99
                   GO  TO  R200L010.
      *
               MOVE    7               TO  LCAP--4
               MOVE    1               TO  LCAP--3 IND.
       R200L050.
           IF  C-TBSG-PROV1-X(IND) EQUAL LSIGL-PROV
               MOVE C-TBNR-PROV1-X(IND) TO LCAP--PROV
               MOVE C-TABE-PROV1-X(IND) TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 4
                   GO  TO  R200L050.
       
               MOVE    9               TO  LCAP--3.
               MOVE    0               TO  LCAP--4.
               MOVE    1               TO  IND.
       R200L080.
           IF  C-TBSG-PROV2-X(IND) EQUAL LSIGL-PROV
               MOVE C-TBNR-PROV2-X(IND) TO LCAP--PROV
               MOVE C-TABE-PROV2-X(IND) TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 7
                   GO  TO  R200L080.
   
                   GO  TO  EXIT-ERR.
      *
      *
      *-------> RICERCA PROVINCIA DA DESCRIZIONE PROVINCIA
       R300-LAV.
           IF  LDESC-PROV NOT GREATER SPACES
                   GO  TO  EXIT-ERR.
               MOVE    "1"             TO  LINDC-PROV
               MOVE    0               TO  LCAP
               MOVE    1               TO  IND.
       R300L010.
           IF  C-TBDS-PROV-X(IND) EQUAL LDESC-PROV
             SUBTRACT  1   FROM  IND  GIVING LCAP--PROV
               MOVE C-TABE-PROV-X(IND) TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 99
                   GO  TO  R300L010.
      *
               MOVE    7               TO  LCAP--4
               MOVE    1               TO  LCAP--3 IND.
       R300L050.
           IF  C-TBDS-PROV1-X(IND) EQUAL LDESC-PROV
               MOVE C-TBNR-PROV1-X(IND) TO LCAP--PROV
               MOVE C-TABE-PROV1-X(IND) TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 4
                   GO  TO  R300L050.
 
               MOVE    9               TO  LCAP--3.
               MOVE    0               TO  LCAP--4.
               MOVE    1               TO  IND.
       R300L080.
           IF  C-TBDS-PROV2-X(IND) EQUAL LDESC-PROV
               MOVE C-TBNR-PROV2-X(IND) TO LCAP--PROV
               MOVE C-TABE-PROV2-X(IND) TO LDESCPROV
                   GO  TO  EXIT-PRO.
               ADD     1               TO  IND.
           IF  IND NOT GREATER 7
                   GO  TO  R300L080.
      *
                    GO  TO  EXIT-ERR.
