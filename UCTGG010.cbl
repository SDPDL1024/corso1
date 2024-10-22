       IDENTIFICATION DIVISION.
       PROGRAM-ID. UCTGG010.
       DATE-WRITTEN.  15/04/2003.    
      ******************************************************************
      *****       FUNZIONE DEL PROGRAMMA:                           ****
      *****          - CALCOLA NUMERO GIORNI TRA DUE DATE           ****
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-NUMR-GG                   PIC S9(5)  COMP-3 VALUE 0.
       77  WS-SERV                      PIC S999   COMP-3 VALUE 0.
       77  WS-REST                      PIC S9     COMP-3 VALUE 0.
       01  WS-DATE                                 VALUE "20030415".
           03  WS-AAAA                  PIC 9999.
           03  WS-MM                    PIC 99.
           03  WS-GG                    PIC 99.
      *
       01  C-TABLGG.
           03  C-MM2                   PIC 999  COMP-3 VALUE  31.
           03  C-MM3                   PIC 999  COMP-3 VALUE  59.
           03  C-MM4                   PIC 999  COMP-3 VALUE  90.
           03  C-MM5                   PIC 999  COMP-3 VALUE 120.
           03  C-MM6                   PIC 999  COMP-3 VALUE 151.
           03  C-MM7                   PIC 999  COMP-3 VALUE 181.
           03  C-MM8                   PIC 999  COMP-3 VALUE 212.
           03  C-MM9                   PIC 999  COMP-3 VALUE 243.
           03  C-MM10                  PIC 999  COMP-3 VALUE 273.
           03  C-MM11                  PIC 999  COMP-3 VALUE 304.
           03  C-MM12                  PIC 999  COMP-3 VALUE 334.
       01  C-TABLGG-R REDEFINES C-TABLGG.
           03  C-TABL-GG-X             PIC 999  COMP-3 OCCURS 11.
      *
       LINKAGE SECTION.
       01  L-AREADATE.
           03  L-DATE-DA               PIC X(8).
           03  L-DATE-AL               PIC X(8).
           03  L-NUMR-GG               PIC S9(5)  COMP-3.
       PROCEDURE DIVISION              USING L-AREADATE.
       ENTRYS.
               MOVE    L-DATE-AL       TO  WS-DATE.
             PERFORM   R100-LAV        THRU R100-EX.
               MOVE    WS-NUMR-GG       TO  L-NUMR-GG
               MOVE    L-DATE-DA       TO  WS-DATE
             PERFORM   R100-LAV        THRU R100-EX.
             SUBTRACT  WS-NUMR-GG       FROM L-NUMR-GG.
       EXIT-PRO.
           EXIT PROGRAM.
      ******************************************************************
      *****                CALCOLA  N. GIORNI                       ****
       R100-LAV.
           IF  WS-MM NOT GREATER 0
               MOVE    99999           TO  WS-NUMR-GG
                   GO  TO  R100-EX.
           IF  WS-MM GREATER 12
               MOVE    99999           TO  WS-NUMR-GG
                   GO  TO  R100-EX.
             MULTIPLY  365 BY WS-AAAA   GIVING WS-NUMR-GG
               ADD     WS-GG            TO  WS-NUMR-GG.
           IF  WS-MM LESS 3
                   GO TO   R100EBISEST.
      *
       R100-BISEST.
             DIVIDE 4 INTO WS-AAAA GIVING WS-SERV REMAINDER WS-REST.
           IF  WS-REST EQUAL 0
               ADD     1               TO  WS-NUMR-GG.
      *
       R100EBISEST.
           IF  WS-MM GREATER 1
               ADD C-TABL-GG-X(WS-MM - 1)   TO  WS-NUMR-GG.
       R100-EX. EXIT.
