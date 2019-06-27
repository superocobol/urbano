      *--------------------------
      * VALIDACAO DE CPF E CNPJ
      *---------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Cnpjcpf.
      *AUTHOR.      URBANO.
      *
      *  CNPJCGC - Verifica a validade do CPF ou do CNPJ
      *
      *           Parametros para o programa
      *           1 = Pessoa      -> F ou J                ->  1 byte
      *           2 = CPF/CNPJ    -> CPF/CNPJ a validar    -> 18 bytes
      *           3 = Status      -> V=Valido  I=Invalido  ->  1 byte
      *                                                       --
      *                                                       20 bytes
      *
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WSM                   PIC X(01) VALUE SPACES.
       01  MC-MASC-CGC.
           02 MC-MASC-CGC1       PIC 9(02) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '.'.
           02 MC-MASC-CGC2       PIC 9(03) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '.'.
           02 MC-MASC-CGC3       PIC 9(03) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '/'.
           02 MC-MASC-CGC4       PIC 9(04) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '-'.
           02 MC-MASC-CGC5       PIC 9(02) VALUE ZEROS.
       01  MC-MASC-CPF.
           02 MC-MASC-CPF1       PIC 9(03) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '.'.
           02 MC-MASC-CPF2       PIC 9(03) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '.'.
           02 MC-MASC-CPF3       PIC 9(03) VALUE ZEROS.
           02 FILLER             PIC X(01) VALUE '-'.
           02 MC-MASC-CPF4       PIC 9(02) VALUE ZEROS.

       01  WX                     PIC 9(02) VALUE ZEROS.
       01  WK                     PIC 9(02) VALUE ZEROS.
       01  IX                     PIC 9(02) VALUE ZEROS.
       01  CAMPO-CGC              PIC 9(14) VALUE ZEROS.
       01  CAMPO-CPF              PIC 9(11) VALUE ZEROS.

       01  WS-DIGITO              PIC 9(01) VALUE ZEROS.
       01  WWS-CGC                PIC 9(14) VALUE ZEROS.
       01  WWWS-CGC REDEFINES WWS-CGC.
           02 WS-CGC1             PIC 9(02).
           02 WS-CGC2             PIC 9(03).
           02 WS-CGC3             PIC 9(03).
           02 WS-CGC4             PIC 9(04).
           02 WS-CGC5             PIC 9(02).
       01  RCGC REDEFINES WWS-CGC PIC 9(12).
       01  WS-CALCULO-DIGITO-1.
           03 WS-CGC.
              05 WS-NRO-CGC     PIC 9(12)       VALUE 0.
              05 WS-CGC-DIG-1   PIC 9(01)       VALUE 0.
              05 WS-CGC-DIG-2   PIC 9(01)       VALUE 0.
           03 RWS-CGC REDEFINES WS-CGC.
              05 F              PIC X(12).
              05 WS-DIGITO-C    PIC 9(02).
           03 CAMPO1            PIC 9(18)       VALUE 0.
           03 RCAMPO1 REDEFINES CAMPO1.
              05 CAMP-1 PIC 9(01)       OCCURS 18.
           03 CAMPO2            PIC 9(18)       VALUE  6543298765432.
           03 RCAMPO2 REDEFINES CAMPO2.
              05 CAMP-2                PIC 9(01)  OCCURS 18.
           03 CAMPO3           PIC 9(06)               VALUE 0.
           03 CAMPO4           PIC 9(06)               VALUE 0.
           03 DIG-CAL          PIC 9(11)  VALUE 00987654321.
           03 RDIG-CAL REDEFINES DIG-CAL.
              05 DIGCAL        PIC 9(01)  OCCURS 11.
           03 DX               PIC 9(02)               VALUE 0.
           03 DD               PIC 9(02)               VALUE 0.
           03 DW               PIC 9(02)               VALUE 0.

       01  WWS-CPF               PIC 9(11) VALUE ZEROS.
       01  WWWS-CPF REDEFINES WWS-CPF.
           02 WS-CPF1            PIC 9(03).
           02 WS-CPF2            PIC 9(03).
           02 WS-CPF3            PIC 9(03).
           02 WS-CPF4            PIC 9(02).
       01  RCPF REDEFINES WWS-CPF PIC 9(09).
       01  WS-CALCULO-DIGITO-2.
           03 WS-CPF.
              05 WS-NRO-CPF     PIC 9(09)       VALUE 0.
              05 WS-CPF-DIG-1   PIC 9(01)       VALUE 0.
              05 WS-CPF-DIG-2   PIC 9(01)       VALUE 0.
           03 RWS-CPF REDEFINES WS-CPF.
              05 F              PIC X(09).
              05 WS-DIGITO-CPF  PIC 9(02).
           03 CPF-CAMPO1        PIC 9(11)       VALUE 0.
           03 R-CPF-CAMPO1 REDEFINES CPF-CAMPO1.
              05 CPF-CAMP-1     PIC 9(01)       OCCURS 11.
           03 CPF-CAMPO2 PIC X(22) VALUE '0011100908070605040302'.
           03 R-CPF-CAMPO2 REDEFINES CPF-CAMPO2.
              05 CPF-CAMP-2     PIC 9(02)  OCCURS 11.

       01  WMESSAGE-SP2.
           02 WMESSAGE-TITLE      PIC X(40) VALUE SPACES.
           02 WMESSAGE-ICON       PIC X(01) VALUE SPACES.
           02 WMESSAGE-BUTTON     PIC X(01) VALUE SPACES.
           02 WMESSAGE-TEXT       PIC X(80) VALUE SPACES.

           COPY "L-LINK.CPY".

       PROCEDURE DIVISION USING LINK-CPFCGC.
       MAINLINE.
           MOVE LINK-CPFCGC(20:1) TO WSM.
           MOVE ZEROS TO WWS-CGC.
           MOVE 'I' TO LINK-CPFCGC(20:1).
           IF LINK-CPFCGC(1:1) = 'F'
              PERFORM PROCESSA-CPF
           END-IF.

       PROCESSA-CGC.
           MOVE 14 TO WX.
           MOVE 19 TO WK.
           PERFORM UNTIL WK < 2
                   IF LINK-CPFCGC(WK:1) IS NUMERIC
                      IF WX NOT = ZEROS
                         MOVE LINK-CPFCGC(WK:1) TO WWS-CGC(WX:1)
                         ADD -1 TO WX
                      END-IF
                   END-IF
                   ADD -1 TO WK
           END-PERFORM
           IF WX NOT = ZEROS
              PERFORM CPFCGC-INVALIDO
              EXIT PROGRAM
           END-IF
           IF RCGC NOT = ZEROS
              MOVE RCGC TO CAMPO1
              PERFORM CALCULA-DIGITO-CGC
              COMPUTE CAMPO1 = CAMPO1 * 10
              MOVE WS-DIGITO TO WS-CGC-DIG-1 CAMP-1(18)
              PERFORM CALCULA-DIGITO-CGC
              MOVE WS-DIGITO TO WS-CGC-DIG-2
              IF WS-DIGITO-C = WS-CGC5
                 MOVE 'V' TO LINK-CPFCGC(20:1)
                 MOVE WS-CGC1 TO MC-MASC-CGC1
                 MOVE WS-CGC2 TO MC-MASC-CGC2
                 MOVE WS-CGC3 TO MC-MASC-CGC3
                 MOVE WS-CGC4 TO MC-MASC-CGC4
                 MOVE WS-CGC5 TO MC-MASC-CGC5
                 MOVE SPACES  TO LINK-CPFCGC(2:18)
                 MOVE MC-MASC-CGC TO LINK-CPFCGC(2:18)
              END-IF
              PERFORM CPFCGC-INVALIDO
           END-IF
           EXIT PROGRAM.
       PROCESSA-CPF.
           MOVE 11 TO WX.
           MOVE 19 TO WK.
           PERFORM UNTIL WK < 2
                   IF LINK-CPFCGC(WK:1) IS NUMERIC
                      IF WX NOT = ZEROS
                         MOVE LINK-CPFCGC(WK:1) TO WWS-CPF(WX:1)
                         ADD -1 TO WX
                      END-IF
                   END-IF
                   ADD -1 TO WK
           END-PERFORM
           IF WX NOT = ZEROS
              PERFORM CPFCGC-INVALIDO
              EXIT PROGRAM
           END-IF
           IF RCPF NOT = ZEROS
              MOVE RCPF TO CPF-CAMPO1
              PERFORM CALCULA-DIGITO-CPF
              COMPUTE CPF-CAMPO1 = CPF-CAMPO1 * 10
              MOVE WS-DIGITO TO WS-CPF-DIG-1 CPF-CAMP-1(11)
              PERFORM CALCULA-DIGITO-CPF
              MOVE WS-DIGITO TO WS-CPF-DIG-2
              IF WS-DIGITO-CPF = WS-CPF4
                 MOVE 'V' TO LINK-CPFCGC(20:1)
                 MOVE WS-CPF1 TO MC-MASC-CPF1
                 MOVE WS-CPF2 TO MC-MASC-CPF2
                 MOVE WS-CPF3 TO MC-MASC-CPF3
                 MOVE WS-CPF4 TO MC-MASC-CPF4
                 MOVE SPACES  TO LINK-CPFCGC(2:18)
                 MOVE MC-MASC-CPF TO LINK-CPFCGC(2:14)
              END-IF
              PERFORM CPFCGC-INVALIDO
           END-IF
           EXIT PROGRAM.
       CPFCGC-INVALIDO.
           IF WSM = 'S'
              IF LINK-CPFCGC(20:1) = 'I'
                 EXIT
              END-IF
           END-IF.

       CALCULA-DIGITO-CGC.
           ADD 1 TO IX.
           MOVE ZEROS TO CAMPO3 CAMPO4.
           MOVE 18 TO DX.
           PERFORM UNTIL DX = ZEROS
                   COMPUTE CAMPO4 = CAMPO4 + (CAMP-1(DX) * CAMP-2(DX))
                   ADD -1 TO DX
           END-PERFORM
           COMPUTE DD = CAMPO4 / 11.
           COMPUTE DW = CAMPO4 - (11 * DD) + 1.
           MOVE DIGCAL(DW) TO WS-DIGITO.
       CALCULA-DIGITO-CPF.
           ADD 1 TO IX.
           MOVE ZEROS TO CAMPO3 CAMPO4.
           MOVE 11 TO DX.
           PERFORM UNTIL DX = ZEROS
                   COMPUTE CAMPO4 = CAMPO4 +
                   (CPF-CAMP-1(DX) * CPF-CAMP-2(DX))
                   ADD -1 TO DX
           END-PERFORM
           COMPUTE DD = CAMPO4 / 11.
           COMPUTE DW = CAMPO4 - (11 * DD) + 1.
           MOVE DIGCAL(DW) TO WS-DIGITO.

       FINALIZA.
           EXIT PROGRAM.
