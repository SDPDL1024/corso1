       IDENTIFICATION  DIVISION.
       PROGRAM-ID. UCFIS010.
      ******************************************************************
      *****    FUNZIONI DEL PROGRAMMA  :                            ****
      *****                                                         ****
      *****        - CONTROLLO COD.FISC./PART.IVA                   ****
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABCONV.
           03  FILLER              PIC X(5)    VALUE "00001".
           03  FILLER              PIC X(5)    VALUE "10100".
           03  FILLER              PIC X(5)    VALUE "20205".
           03  FILLER              PIC X(5)    VALUE "30307".
           03  FILLER              PIC X(5)    VALUE "40409".
           03  FILLER              PIC X(5)    VALUE "50513".
           03  FILLER              PIC X(5)    VALUE "60615".
           03  FILLER              PIC X(5)    VALUE "70717".
           03  FILLER              PIC X(5)    VALUE "80819".
           03  FILLER              PIC X(5)    VALUE "90921".
           03  FILLER              PIC X(5)    VALUE "A0001".
           03  FILLER              PIC X(5)    VALUE "B0100".
           03  FILLER              PIC X(5)    VALUE "C0205".
           03  FILLER              PIC X(5)    VALUE "D0307".
           03  FILLER              PIC X(5)    VALUE "E0409".
           03  FILLER              PIC X(5)    VALUE "F0513".
           03  FILLER              PIC X(5)    VALUE "G0615".
           03  FILLER              PIC X(5)    VALUE "H0717".
           03  FILLER              PIC X(5)    VALUE "I0819".
           03  FILLER              PIC X(5)    VALUE "J0921".
           03  FILLER              PIC X(5)    VALUE "K1002".
           03  FILLER              PIC X(5)    VALUE "L1104".
           03  FILLER              PIC X(5)    VALUE "M1218".
           03  FILLER              PIC X(5)    VALUE "N1320".
           03  FILLER              PIC X(5)    VALUE "O1411".
           03  FILLER              PIC X(5)    VALUE "P1503".
           03  FILLER              PIC X(5)    VALUE "Q1606".
           03  FILLER              PIC X(5)    VALUE "R1708".
           03  FILLER              PIC X(5)    VALUE "S1812".
           03  FILLER              PIC X(5)    VALUE "T1914".
           03  FILLER              PIC X(5)    VALUE "U2016".
           03  FILLER              PIC X(5)    VALUE "V2110".
           03  FILLER              PIC X(5)    VALUE "W2222".
           03  FILLER              PIC X(5)    VALUE "X2325".
           03  FILLER              PIC X(5)    VALUE "Y2424".
           03  FILLER              PIC X(5)    VALUE "Z2523".
       01  TAB-CHAR   REDEFINES       WS-TABCONV.
           03  T-CHARELEM                      OCCURS 36 INDEXED IXT.
               05  T-CHAR          PIC X.
               05  T-PARI          PIC 99.
               05  T-DISP          PIC 99.
       01  TCHAREL.
           03  TCHAR               PIC X.
           03  TPARI               PIC 99.
           03  TDISP               PIC 99.
       01  SOMMA                   PIC 999     COMP-3.
       01  CAMPO2                  PIC 99.
       01  CAMPO-2                 REDEFINES CAMPO2.
           03  CAMPO-21            PIC 9.
           03  CAMPO-22            PIC 9.
       01  WS-CODICE.
           03  WS-SWCOD            PIC 9.
           03  WS-CODFIS.
               05  WS-SPAZ         PIC X(5).
               05  WPART-IVA       PIC X(11).
           03  WS-CODFIS-R   REDEFINES WS-CODFIS.
               05  WS-CODN         PIC X(11).
               05  WS-ZERO         PIC X(5).
           03  WS-CODTABA    REDEFINES WS-CODFIS.
               05  WS-CODCAR              OCCURS 16 INDEXED IXC.
                   07  WS-CODNUM   PIC 9.
      *
       LINKAGE SECTION.
       01  CODICE.
           03  SW-COD              PIC 9.
           03  COD-FIS.
               05  COD-FISN        PIC 9(16).
       PROCEDURE   DIVISION        USING  CODICE.
       INIZIO.
               MOVE    CODICE      TO  WS-CODICE.
               MOVE    ZERO        TO  WS-SWCOD.
           IF  COD-FIS EQUAL SPACES
                   GO  TO  STOP-PROG.
               MOVE    ZERO        TO  SOMMA.
           IF  WS-CODN NUMERIC
                   GO TO  ELAB-NUM.
           IF  WPART-IVA NOT NUMERIC
                   GO TO  ELAB-ALFA.
       
       ELAB-NUM.
           IF  WS-ZERO EQUAL SPACES
               MOVE    COD-FIS     TO  WPART-IVA.
               MOVE    SPACES      TO  WS-SPAZ.
               SET     IXC         TO  6.
      *
       LOOP-NUM.
               ADD     WS-CODNUM(IXC)  TO  SOMMA.
               SET     IXC         UP  BY  1.
               MULTIPLY WS-CODNUM(IXC) BY  2   GIVING  CAMPO2.
               ADD     CAMPO-21    TO  SOMMA.
               ADD     CAMPO-22    TO  SOMMA.
               SET     IXC         UP  BY  1.
           IF  IXC NOT GREATER 15
                   GO  TO  LOOP-NUM.
      *
       VALIDA-NUM.
               MOVE    SOMMA       TO  CAMPO2.
               SUBTRACT CAMPO-22   FROM    10  GIVING  CAMPO-22.
           IF  CAMPO-22  NOT EQUAL WS-CODNUM(16)
                   GO TO   STOP-ERR.
                   GO  TO  STOP-PROG.
      *
       ELAB-ALFA.
               SET     IXC         TO  1.
      *
       LOOP-ALFA.
             PERFORM   SCORRI-TAB  THRU    SCORRI-TAB-EX.
               ADD     TDISP       TO  SOMMA.
           IF  IXC NOT GREATER 14
             PERFORM   SCORRI-TAB  THRU    SCORRI-TAB-EX
               ADD     TPARI       TO  SOMMA
                   GO  TO  LOOP-ALFA.
      *
       DOPO-LOOP.
               DIVIDE  SOMMA   BY  26  GIVING  SOMMA  REMAINDER  CAMPO2.
               SET     IXT         TO  11.
      *
       LOOP-CONV.
           IF  IXT     GREATER     36
                   GO  TO  STOP-ERR.
      *
           IF  T-PARI(IXT) NOT EQUAL   CAMPO2
               SET     IXT         UP  BY  1
                   GO  TO  LOOP-CONV.
      *
       VALIDA-ALFA.
           IF  T-CHAR(IXT)  EQUAL   WS-CODCAR(16)
                   GO  TO  STOP-PROG.
       STOP-ERR.
               MOVE    1           TO  WS-SWCOD.
       STOP-PROG.
               MOVE    WS-CODICE   TO  CODICE.
           EXIT PROGRAM.
      *
      *****************    RICERCA ELEM. CARATTERE    ******************
       SCORRI-TAB.
               SET     IXT         TO  1.
       LOOP-TAB.
           IF  IXT     GREATER     36
                   GO  TO  STOP-ERR.
           IF  T-CHAR(IXT) NOT EQUAL   WS-CODCAR(IXC)
               SET     IXT         UP  BY  1
                   GO  TO  LOOP-TAB.
               MOVE T-CHARELEM(IXT) TO TCHAREL
               SET     IXC         UP  BY 1.
       SCORRI-TAB-EX.
               EXIT.
