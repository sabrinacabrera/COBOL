      *****************************************************************
      * Author:      FABRICA DE SOFTWARE - SISTEMAS ACTIVOS S.R.L.
      * Date:        01 DE JUNIO 2019.
      * Purpose:     EJERCICIO 1 - Estructuras Básicas
      * Tectonics:   cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
      *****************************************************************
      *****El nombre de programa se conforma de la sig. manera      ***
      *****PROG01 donde 01 corresponde al nro de ejerc de 2 digitos ***
      *****       si el ejercicio fuera 1-b se agrega la letra      ***
      *****       a continuación ej. PROG01B                        ***
      *****-00-   indica el cod. de usuario s/lista curso           ***
      *****-SA    indica las iniciales del usuario                  ***
      *****************************************************************
       PROGRAM-ID.   PROG01-00-SA.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

           77 MASCARA             PIC ZZZZ.99,99.

           77 WSV-IVA             PIC 9(5).

  f        77 WSV-IVA-CON         PIC 9(5).

           77 PROMEDIO-CUOTA      PIC 9(5).

       01 VARIABLES.
          05 WSV-IMP-CUOTA        PIC 9(04)V9(02).
          05 WSV-NRO-CUOTA        PIC 9(01).

       01 CONSTANTES.
          05 WSC-0                PIC 9(01)       VALUE 0.
          05 WSC-1                PIC 9(01)       VALUE 1.
          05 WSC-5                PIC 9(01)       VALUE 5.

       01 ACUMULADORES.
          05 WSA-ACUM-IMPORTE     PIC 9(04)V9(02).
          05 WSA-CONT             PIC 9.


       01 NOMBRE-COMPLETO.
          05 WSV-NOMBRE          PIC X(20).
          05 WSV-APELLIDO        PIC X(20).


       PROCEDURE DIVISION.

       000000-CONTROL.
           PERFORM 100000-INICIO
           PERFORM 200000-PROCESO
           PERFORM 210000-CARGA
           PERFORM 220000-CALCULO
           PERFORM 230000-RESULTADO
           STOP RUN.

       100000-INICIO.

           INITIALIZE VARIABLES
                      ACUMULADORES.

       200000-PROCESO.

              DISPLAY 'INGRESE SU NOMBRE: '
              ACCEPT WSV-NOMBRE
              DISPLAY 'INGRESE SU APPELIDO: '
              ACCEPT WSV-APELLIDO
              PERFORM UNTIL WSA-CONT= WSC-5
              ADD 1 TO WSV-NRO-CUOTA
              DISPLAY '**************************************'
              DISPLAY 'INGRESE IMPORTE CUOTA ' WSA-CONT
              DISPLAY '**************************************'
              ACCEPT WSV-IMP-CUOTA
              COMPUTE WSA-ACUM-IMPORTE=WSA-ACUM-IMPORTE + WSV-IMP-CUOTA
              ADD 1 TO WSA-CONT
              END-PERFORM.

       210000-CARGA.
           DISPLAY WSA-ACUM-IMPORTE.

       220000-CALCULO.
           COMPUTE WSV-IVA = (WSA-ACUM-IMPORTE * 21) / 100
           COMPUTE WSV-IVA-CON = WSV-IVA + WSA-ACUM-IMPORTE.

           COMPUTE PROMEDIO-CUOTA = WSA-ACUM-IMPORTE / 5.

       230000-RESULTADO.

           DISPLAY '**************************************'
           DISPLAY 'Author:   SISTEMAS ACTIVOS S.R.L.'
           DISPLAY 'Purpose:  EJERCICIO 1 - Estr.Basicas'
           DISPLAY 'Programme:PROG01-00-SA'.
           DISPLAY '**************************************'.

           MOVE WSA-ACUM-IMPORTE TO MASCARA.

           DISPLAY '**************************************'
           DISPLAY NOMBRE-COMPLETO.
           DISPLAY '**************************************'
           DISPLAY 'PAGOS TOTALES ULTIMO SEMESTRE $' MASCARA.
           DISPLAY '**************************************'.
           DISPLAY 'PAGOS TOTALES SIN IVA: ' MASCARA

           MOVE WSV-IVA TO MASCARA.

           DISPLAY 'MONTO IVA: ' MASCARA

           MOVE WSV-IVA-CON TO MASCARA.

           DISPLAY 'PAGOS TOTALES CON IVA: ' MASCARA
           DISPLAY '**************************************'.

           MOVE PROMEDIO-CUOTA TO MASCARA.

           DISPLAY 'LA CUOTA PROMEDIO ES: ' MASCARA.
           DISPLAY '**************************************'.

       END-RUN.
