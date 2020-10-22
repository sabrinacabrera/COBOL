      ******************************************************************
      * Author: Sebastian Anderson
      * Date: 14/06/2019
      * Purpose: Ejercicio 1 - Evaluacion Integradora
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG01-01-SA.
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT TRANSAC              ASSIGN TO DISK
           'C:\Users\POSA-116\Desktop\Archivos\Transac.TXT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       ACCESS MODE IS SEQUENTIAL
                                       FILE STATUS IS WSS-FS-TRAN.

           SELECT CLIENTES             ASSIGN TO DISK
           'C:\Users\POSA-116\Desktop\Archivos\Clientes.TXT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       ACCESS MODE IS SEQUENTIAL
                                       FILE STATUS IS WSS-FS-CTE.

           SELECT RESULTADO                  ASSIGN TO DISK
           'C:\Users\POSA-116\Desktop\Archivos\Resultados.TXT'
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       ACCESS MODE IS SEQUENTIAL
                                       FILE STATUS IS WSS-FS-RES.

       DATA DIVISION.

       FILE SECTION.

       FD TRANSAC.

       01 FD-TRANSAC.
         05 WSF-TRAN-CTE         PIC 9(07).
         05 WSF-TRAN-CODSER      PIC X(03).
         05 WSF-TRAN-IMPORT-T    PIC 9(07)V99.

       FD CLIENTES.

       01 FD-CLIENTES.
         05 WSF-CTE-CTE          PIC 9(07).
         05 WSF-CTE-NYA          PIC X(60).
         05 WSF-CTE-IMPORT-C     PIC 9(07)V99.

       FD RESULTADO.

       01 FD-RESULTADO.
         05 WSF-RES-CTE          PIC 9(07).
         05 WSF-RES-NYA          PIC X(60).
         05 WSF-RES-IMP-TOTALTR  PIC 9(07)V99.
         05 WSF-RES-CANT-TRAN    PIC 9(03).


       WORKING-STORAGE SECTION.

       01 SWITCHES.

         05 WSS-FS-TRAN                          PIC X(02).
           88 WSS-FS-TRAN-OK                     VALUE '00'.
           88 WSS-FS-TRAN-EOF                    VALUE '10'.

         05 WSS-FS-CTE                           PIC X(02).
           88 WSS-FS-CTE-OK                      VALUE '00'.
           88 WSS-FS-CTE-EOF                     VALUE '10'.

         05 WSS-FS-RES                           PIC X(02).
           88 WSS-FS-RES-OK                      VALUE '00'.
           88 WSS-FS-RES-EOF                     VALUE '10'.


       01 CONSTANTES.
           05 WSC-1                 PIC 9 VALUE 1.


       01 CONTADORES.
           05 WSCC-CONT-LEIDOS              PIC 9(09).
           05 WSCC-CONT-CONTRAN             PIC 9(09).
           05 WSCC-CONT-SINTRAN             PIC 9(09).
           05 WSCC-CONT-TRANCTE             PIC 9(09).

       01 REG-TRANSAC.
         05 WSR-TRAN-CTE         PIC 9(07).
         05 WSR-TRAN-CODSER      PIC X(03).
         05 WSR-TRAN-IMPORT-T    PIC 9(07)V99.

       01 REG-CLIENTES.
         05 WSR-CTE-CTE          PIC 9(07).
         05 WSR-CTE-NYA          PIC X(60).
         05 WVR-CTE-IMPORT-C     PIC 9(07)V99.

       01 REG-RESULTADO.
         05 WSR-RES-CTE          PIC 9(07).
         05 WSR-RES-NYA          PIC X(60).
         05 WSR-RES-IMP-TOTALTR  PIC 9(07)V99.
         05 WSR-RES-CAT-TRAN     PIC 9(03).


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

       000000-CONTROL.

           PERFORM 100000-INICIO.
           PERFORM 200000-PROCESO UNTIL WSS-FS-TRAN-EOF.
           PERFORM 300000-FINAL.

       100000-INICIO.

           INITIALIZE CONTADORES.

           OPEN INPUT TRANSAC.

           IF NOT WSS-FS-TRAN-OK
             DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
             DISPLAY 'FILE STATUS' WSS-FS-TRAN
             PERFORM 300000-FINAL

           END-IF.

           OPEN INPUT CLIENTES.
               IF NOT WSS-FS-CTE-OK
             DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
             DISPLAY 'FILE STATUS' WSS-FS-CTE
             PERFORM 300000-FINAL
           END-IF.

           OPEN OUTPUT RESULTADO.
           IF NOT WSS-FS-RES-OK
             DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
             DISPLAY 'FILE STATUS' WSS-FS-RES
             PERFORM 300000-FINAL
           END-IF.

           READ TRANSAC
              EVALUATE TRUE
                 WHEN WSS-FS-TRAN-OK
                      MOVE FD-TRANSAC         TO REG-TRANSAC
                      CONTINUE
                 WHEN WSS-FS-TRAN-EOF
                      CONTINUE
                 WHEN OTHER
                      DISPLAY 'FILE STATUS' WSS-FS-TRAN
                      PERFORM 300000-FINAL.
           IF WSS-FS-TRAN-EOF
             DISPLAY 'ARCHIVO VACIO'
             PERFORM 300000-FINAL
           END-IF.

           READ CLIENTES
           ADD 1 TO WSCC-CONT-LEIDOS
              EVALUATE TRUE
                 WHEN WSS-FS-CTE-OK
                      MOVE FD-CLIENTES         TO REG-CLIENTES
                      CONTINUE
                 WHEN WSS-FS-CTE-EOF
                      CONTINUE
                 WHEN OTHER
                      DISPLAY 'FILE STATUS' WSS-FS-CTE
                      PERFORM 300000-FINAL.

           IF WSS-FS-CTE-EOF
             DISPLAY 'ARCHIVO VACIO'
             PERFORM 300000-FINAL
           END-IF.

       200000-PROCESO.

           INITIALIZE WSCC-CONT-TRANCTE

           PERFORM UNTIL WSR-TRAN-CTE = WSR-CTE-CTE OR WSS-FS-CTE-EOF

           READ CLIENTES
           ADD 1 TO WSCC-CONT-LEIDOS
              EVALUATE TRUE
                 WHEN WSS-FS-CTE-OK
                      MOVE FD-CLIENTES         TO REG-CLIENTES
                      CONTINUE
                 WHEN WSS-FS-CTE-EOF
                      CONTINUE
                 WHEN OTHER
                      DISPLAY 'FILE STATUS' WSS-FS-CTE
                      PERFORM 300000-FINAL
           END-PERFORM.

           PERFORM 210000-PROCESAR-CTE.

       210000-PROCESAR-CTE.

           MOVE WSR-TRAN-CTE TO WSR-RES-CTE
           MOVE WSR-CTE-NYA TO WSR-RES-NYA

           PERFORM UNTIL WSR-TRAN-CTE<>WSR-CTE-CTE OR WSS-FS-TRAN-EOF

           ADD 1 TO WSCC-CONT-TRANCTE

           COMPUTE WSR-RES-IMP-TOTALTR =
                   WSR-RES-IMP-TOTALTR + WSR-TRAN-IMPORT-T

           READ TRANSAC
              EVALUATE TRUE
                 WHEN WSS-FS-TRAN-OK
                      MOVE FD-TRANSAC         TO REG-TRANSAC
                      CONTINUE
                 WHEN WSS-FS-TRAN-EOF
                      CONTINUE
                 WHEN OTHER
                      DISPLAY 'FILE STATUS' WSS-FS-TRAN
                      PERFORM 300000-FINAL

           END-PERFORM.

       220000-GRABAR-SALIDA.

           MOVE WSR-TRAN-CTE TO WSF-RES-CTE
           MOVE WSR-CTE-NYA TO WSF-RES-NYA
           MOVE WSR-TRAN-IMPORT-T TO  WSF-RES-IMP-TOTALTR
           MOVE WSCC-CONT-TRANCTE TO WSF-RES-CANT-TRAN .

           WRITE FD-RESULTADO.

       300000-FINAL.
           COMPUTE WSCC-CONT-SINTRAN=
                                   WSCC-CONT-LEIDOS - WSCC-CONT-CONTRAN
           PERFORM 310000-TOTALES-DE-CONTROL
           PERFORM 320000-CERRAR-SALIDA
           STOP RUN.

       310000-TOTALES-DE-CONTROL.

           DISPLAY '********************************************'
           DISPLAY 'TOTAL LEIDOS =' WSCC-CONT-LEIDOS
           DISPLAY 'TOTAL SIN MOVIMIENTOS =' WSCC-CONT-SINTRAN
           DISPLAY '********************************************'.

       320000-CERRAR-SALIDA.

           CLOSE CLIENTES
           IF NOT WSS-FS-CTE-OK
              DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
            DISPLAY 'FILE STATUS' WSS-FS-CTE
           END-IF.


           CLOSE TRANSAC
           IF NOT WSS-FS-TRAN-OK
              DISPLAY 'ERROR DE ARCHIVO DE ENTRADA'
            DISPLAY 'FILE STATUS' WSS-FS-TRAN
           END-IF.

           CLOSE RESULTADO
           IF NOT WSS-FS-RES-OK
              DISPLAY 'ERROR DE ARCHIVO DE SALIDA'
            DISPLAY 'FILE STATUS' WSS-FS-RES
           END-IF.

       END PROGRAM PROG01-01-SA.
