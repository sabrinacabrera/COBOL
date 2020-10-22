      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-NUM PIC 9(2) VALUE 0.
           01 FILLER.
           03 WS-NUMAUX PIC 9(2) OCCURS 5 VALUE 0.
           03 WS-IVAAUX PIC 9(2) OCCURS 5 VALUE 0.
           01 WS-IVA PIC 9(2) VALUE 21.


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            PERFORM 100000-CARGA.
            PERFORM 200000-CALCULO.
            PERFORM 300000-RESULTADO.
            STOP RUN.
       100000-CARGA.
           PERFORM UNTIL WS-NUM> 4
           ACCEPT WS-NUM
           MOVE WS-NUM TO WS-NUM1 (WS-NUM)
           END-PERFORM.
       200000-CALCULO.
           PERFORM UNTIL WS-NUM>4
           ACCEPT WS-IVAAUX(WS-NUM)= WS-NUMAUX(WS-NUM)* WS-IVA *100.
           END-PERFORM.

       300000-RESULTADO.
           PERFORM UNTIL WS-NUM >5
           COMPUTE  WS-IVAAUX(WS-NUM)= WS-NUMAUX(WS-NUM)* WS-IVA *100.
       END PROGRAM YOUR-PROGRAM-NAME.
