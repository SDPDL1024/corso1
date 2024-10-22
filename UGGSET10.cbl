       IDENTIFICATION DIVISION.
       PROGRAM-ID. UGGSET10.
       DATE-WRITTEN.  20/10/2015.    
      ******************************************************************
      *****       FUNZIONE DEL PROGRAMMA:                           ****
      *****          - CALCOLA GIORNO DELLA SETTIMANA DA UNA DATA   ****
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  IND                    PIC S999      COMP-3 VALUE 0.
       77  W-SERVI                PIC S9(7)     COMP-3 VALUE 0.
       77  W-SERVI-1              PIC S9(7)     COMP-3 VALUE 0.

       01  C-TABLGIOR.
           03  FILLER                  PIC X(9)   VALUE "Lunedì   ".
           03  FILLER                  PIC X(9)   VALUE "Martedì  ".
           03  FILLER                  PIC X(9)   VALUE "Mercoledì".
           03  FILLER                  PIC X(9)   VALUE "Giovedì  ".
           03  FILLER                  PIC X(9)   VALUE "Venerdì  ".
           03  FILLER                  PIC X(9)   VALUE "Sabato   ".
           03  FILLER                  PIC X(9)   VALUE "Domenica ".
       01  C-TABLGIOR-R REDEFINES C-TABLGIOR.
           03  C-TABL-GIOR-X           PIC X(9)   OCCURS 7.
      *
       LINKAGE SECTION.
       01  L-AREAGSET.
           03  L-DATE-GSET             PIC 9(9)   COMP-3.
           03  L-GIOR-SETT             PIC X(10).
       PROCEDURE DIVISION              USING L-AREAGSET.
       ENTRYS.
               MOVE    SPACES          TO  L-GIOR-SETT
           IF  L-DATE-GSET GREATER 0
             PERFORM   R100-LAV 
           END-IF

           Exit Program
           Stop Run.
      ******************************************************************
      *****                CALCOLA  N. GIORNI                       ****
       R100-LAV.
             COMPUTE W-SERVI-1 = FUNCTION INTEGER-OF-DATE(L-DATE-GSET)         
               MOVE    0                TO  IND
           IF  W-SERVI-1 NOT EQUAL 0   
             DIVIDE  W-SERVI-1           BY 7 GIVING  W-SERVI
                                        REMAINDER IND
           END-IF
           IF  IND EQUAL  0
               MOVE    7               TO  IND
           END-IF

               MOVE    C-TABL-GIOR-X(IND)  TO L-GIOR-SETT
           .
