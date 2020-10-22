      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ02-SERIE1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
         01 WSG-FECHA.
             05 WSD-DIA    PIC 99      VALUE 0.
             05 FILLER     PIC X       VALUE '/'.
             05 WSD-MES    PIC 99      VALUE 0.
             05 FILLER     PIC X       VALUE '/'.
             05 WSD-AÑO    PIC 9(4)    VALUE 0.

       PROCEDURE DIVISION.

           DISPLAY 'INGRESE DIA: '
           ACCEPT WSD-DIA
           DISPLAY 'INGRESE MES: '
           ACCEPT WSD-MES
           DISPLAY 'INGRESE AÑO: '
           ACCEPT WSD-AÑO.

           DISPLAY WSG-FECHA .



       END PROGRAM EJ02-SERIE1.
