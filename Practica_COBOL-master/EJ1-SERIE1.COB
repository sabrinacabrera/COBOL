      ******************************************************************
      * Author:SABRINA CABRERA
      * Date:
      * Purpose:ALMACENAR FECHAS-EJ1-SERIE1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJ01-S01.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
         01 WSG-FECHA.
             05 WSD-DIA    PIC 99      VALUE 0.
             05 WSD-MES    PIC 99      VALUE 0.
             05 WSD-AÑO    PIC 9(4)    VALUE 0.

       PROCEDURE DIVISION.
           ACCEPT WSG-FECHA FROM DATE.



       END PROGRAM EJ01-S01.
