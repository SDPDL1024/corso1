       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPROV010.
      ******************************************************************
      *****    FUNZIONI DEL PROGRAMMA:                              ****
      *****                                                         ****
      *****   - RICERCA PROVINCIA PARTENDO DAL CAP                  ****
      *****                                                         ****
      ******************************************************************
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WLCAP1.
           03  WLCAP1-4.
               05  WLCAP-PRV       PIC 99      VALUE ZERO.
               05  FILLER          PIC XX      VALUE SPACES.
           03  FILLER              PIC X       VALUE SPACES.
       01  WLCAP2     REDEFINES  WLCAP1.
           03  WLCAP1-3            PIC 999.
           03  FILLER              PIC XX.
       01  WLCAP      REDEFINES  WLCAP1      PIC 9(5).
      *
      *----------  TABELLA PROVINCE            
       01  CDESPRV.
           03  CDESPRV0 PIC X(20)  VALUE "VTRIFRLTTRPGSSNUCATO".
           03  CDESPRV1 PIC X(20)  VALUE "AOCNVCATALGESVIMSPMI".
           03  CDESPRV2 PIC X(20)  VALUE "VACOSOBGBSCRPVNOPCVE".
           03  CDESPRV3 PIC X(20)  VALUE "TVBLUDTSPDVIVRTNBZBO".
           03  CDESPRV4 PIC X(20)  VALUE "MOREPRFEROMNFORA  FI".
           03  CDESPRV5 PIC X(20)  VALUE "PTARSIMSLUPILIGRPOAN".
           03  CDESPRV6 PIC X(20)  VALUE "PSMCAPTEPECHAQ    BA".
           03  CDESPRV7 PIC X(20)  VALUE "FGBRLETAMT        NA".
           03  CDESPRV8 PIC X(20)  VALUE "CEBNAVSAPZCBCSCZRCPA".
           03  CDESPRV9 PIC X(20)  VALUE "TPAGCLENCTSRRGME    ".
       01  CTBDESPRV   REDEFINES CDESPRV.
           03  TBDES-PRV   PIC XX      OCCURS 100  INDEXED PRV.
      *
       LINKAGE SECTION.
       01  LCAP.
           03  LC-CAP              PIC 9(5)    COMP-3.
           03  LDPRV.
               05  LD-PRV          PIC XX.
               05  FILLER          PIC XX.
       PROCEDURE DIVISION          USING   LCAP.
       ENTRYS.
       PAR-0.
               MOVE    LC-CAP          TO  WLCAP.
               MOVE    SPACES          TO  LDPRV.
           IF  WLCAP EQUAL ZEROES
                   GO TO EXIT-PRO.
           IF  WLCAP-PRV EQUAL ZEROES
               MOVE    "RM"            TO  LD-PRV
                   GO TO EXIT-PRO.
               SET     PRV             TO  WLCAP-PRV
               MOVE    TBDES-PRV(PRV)  TO LD-PRV.
           IF  WLCAP1-4 EQUAL "0917"
               MOVE    "OR"            TO  LD-PRV.
           IF  WLCAP1-4 EQUAL "3317"
               MOVE    "PN"            TO  LD-PRV.
           IF  WLCAP1-4 EQUAL "3417"
               MOVE    "GO"            TO  LD-PRV.
           IF  WLCAP1-4 EQUAL "8617"
               MOVE    "IS"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "139"
               MOVE    "BI"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "239"
               MOVE    "LC"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "269"
               MOVE    "LO"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "289"
               MOVE    "VB"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "479"
               MOVE    "RN"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "889"
               MOVE    "KR"            TO  LD-PRV.
           IF  WLCAP1-3 EQUAL "899"
               MOVE    "VV"            TO  LD-PRV.
160219     IF  WLCAP1-3 EQUAL "761"
  "            MOVE    "BT"            TO  LD-PRV.
160219     IF  WLCAP1-3 EQUAL "760"
  "            MOVE    "BT"            TO  LD-PRV.
      *
       EXIT-PRO.
             EXIT PROGRAM.
