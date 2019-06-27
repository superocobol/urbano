      *--------------------------
      *  EXECUCAO DE DISTRIBUICAO
      *---------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ExeDistrCli.
      * AUTHOR.      URBANO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT VENDEDORES ASSIGN TO  DISK WID-ARQ-VENDEDOR
              ORGANIZATION      IS INDEXED
              ACCESS MODE       IS DYNAMIC
              RECORD KEY        IS CHAVE-VEN
              ALTERNATE RECORD KEY IS CPF-VEN WITH DUPLICATES
              ALTERNATE RECORD KEY IS CODIGO-VEN WITH DUPLICATES
              ALTERNATE RECORD KEY IS NOME-VEN  WITH DUPLICATES
      *        ALTERNATE RECORD KEY IS CH01-VEN-1 =
      *                                    CODIGO-VEN
      *                                    NOME-VEN WITH DUPLICATES
              LOCK MODE         IS MANUAL
              FILE STATUS       IS FS-VEN.

       SELECT CLIENTES ASSIGN TO  DISK WID-ARQ-CLIENTE
              ORGANIZATION      IS INDEXED
              ACCESS MODE       IS DYNAMIC
              RECORD KEY        IS CHAVE-CLI
              ALTERNATE RECORD KEY IS CNPJ   WITH DUPLICATES
              ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES
              ALTERNATE RECORD KEY IS RAZAO  WITH DUPLICATES
      *        ALTERNATE RECORD KEY IS CH01-CLI-1 =
      *                                    CODIGO
      *                                    RAZAO WITH DUPLICATES
              LOCK MODE         IS MANUAL
              FILE STATUS       IS FS-CLI.

       SELECT DISTRIBUICAO ASSIGN TO  DISK WID-ARQ-DISTRIBUICAO
              ORGANIZATION      IS INDEXED
              ACCESS MODE       IS DYNAMIC
              RECORD KEY        IS CHAVE-DIS
              ALTERNATE RECORD KEY IS CODIGO-CLI-DIS   WITH DUPLICATES
              ALTERNATE RECORD KEY IS CODIGO-VEN-DIS   WITH DUPLICATES
              ALTERNATE RECORD KEY IS LATITUDE-DIS     WITH DUPLICATES
              ALTERNATE RECORD KEY IS LONGITUDE-DIS    WITH DUPLICATES
      *        ALTERNATE RECORD KEY IS CH01-DIS-1 =
      *                                CODIGO-CLI-DIS
      *                                CODIGO-VEN-DIS
      *                                LONGITUDE-DIS
              LOCK MODE         IS MANUAL
              FILE STATUS       IS FS-DIS.

       SELECT ARQREL  ASSIGN TO DISK WID-ARQ-REL
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMPORT.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REGISTRO-CLIENTES.
          02 CHAVE-CLI.
             03 CODIGO             PIC  9(007).
             03 CNPJ               PIC  9(014).
          02 RAZAO                 PIC  X(040).
          02 LATITUDE              PIC S9(003)v9(008).
          02 LONGITUDE             PIC S9(003)v9(008).

       FD VENDEDORES.
       01  REGISTRO-VENDEDORES.
           02 CHAVE-VEN.
              03 CODIGO-VEN             PIC  9(003).
              03 CPF-VEN                PIC  9(011).
           02 NOME-VEN                  PIC  X(040).
           02 LATITUDE-VEN              PIC S9(003)v9(008).
           02 LONGITUDE-VEN             PIC S9(003)v9(008).

       FD DISTRIBUICAO.
       01  REGISTRO-DISTRIBUICAO.
           02 CHAVE-DIS.
              03 CODIGO-VEN-DIS         PIC  9(003).
              03 CODIGO-CLI-DIS         PIC  9(007).
           02 LATITUDE-DIS              PIC S9(003)v9(008).
           02 LONGITUDE-DIS             PIC S9(003)v9(008).

       FD  ARQREL.
       01  ARQREL-REC.
           05  ARQREL-DATA-01   PIC X(00512).

       WORKING-STORAGE SECTION.
       01  WARQIMPORT-REC               PIC X(512).
       01  WARQIMPORT-2 REDEFINES WARQIMPORT-REC.
           02 WCHAVE-CLI-IMP.
             03 WCODIGO-IMP             PIC  9(007).
             03 WCNPJ-IMP               PIC  9(014).
           02 WRAZAO-IMP                 PIC  X(040).
           02 WLATITUDE-IMP              PIC S9(003)v9(008).
           02 WLONGITUDE-IMP             PIC S9(003)v9(008).
           02 FILLER                     PIC X(429).

       01  WREGISTRO-CLIENTES.
           02 WCHAVE-CLI.
             03 WCODIGO             PIC  9(007) VALUE ZEROES.
             03 WCNPJ               PIC  9(014) VALUE ZEROES.
           02 WRAZAO                 PIC  X(040) VALUE ZEROES.
           02 WLATITUDE              PIC S9(003)v9(008) VALUE ZEROES.
           02 WLONGITUDE             PIC S9(003)v9(008) VALUE ZEROES.

       01  FS-VEN.
           02 FS-VEN-1                PIC 9.
           02 FS-VEN-2                PIC 9.
           02 FS-VEN-R REDEFINES FS-VEN-2 PIC 99 COMP-X.

       01 FS-CLI.
           02 FS-CLIENTES-1                PIC 9.
           02 FS-CLIENTES-2                PIC 9.
           02 FS-CLIENTE-R REDEFINES FS-CLIENTES-2 PIC 99 COMP-X.

       01  FS-DIS.
           02 FS-CLIENTES-1                PIC 9.
           02 FS-CLIENTES-2                PIC 9.
           02 FS-CLIENTE-R REDEFINES FS-CLIENTES-2 PIC 99 COMP-X.

       01  FS-IMPORT.
           02 FS-IMPORT-1                       PIC 9.
           02 FS-IMPORT-2                       PIC 9.
           02 FS-IMPORT-R REDEFINES FS-IMPORT-2 PIC 99 COMP-X.

       01 WID-ARQ-CLIENTE.
           02 WID-ARQ-CLIENTE-1            PIC X(50).
       01 WID-ARQ-VENDEDOR.
           02 WID-ARQ-VENDEDOR-1           PIC X(50).
       01 WID-ARQ-DISTRIBUICAO.
           02 WID-ARQ-DISTRIBUICAO-1       PIC X(50).
       01 WID-ARQ-REL.
           02 WID-ARQ-REL-1                PIC X(50).

       77 WX              PIC X VALUE SPACES.
       77 WSAIDA          PIC 9 VALUE ZEROES.
      *                  (0=OK, 1=SAIDA)
       77 WCONT           PIC 9(04) VALUE ZEROES.
       77 WCODIGO-CLI-ANT PIC 9(07) VALUE ZEROES.

       01 WMENSAGEM.
          02 WTXT         PIC X(50) VALUE SPACES.
          02 WST          PIC 99 VALUE ZEROES.

      *-------------CALCULO DLO - DLA -------------------------
       01 WDLA-VEN        PIC S9(003)v9(008) VALUE ZEROS.
       01 WDLA-CLI        PIC S9(003)v9(008) VALUE ZEROS.
       01 WDLO-VEN        PIC S9(003)v9(008) VALUE ZEROS.
       01 WDLO-CLI        PIC S9(003)v9(008) VALUE ZEROS.
      *------ (DLA OU DLO-VEN - OU DLA DLO-CLI) = RESULT1
       01 WRESULT1        PIC S9(003)v9(008) VALUE ZEROS.
      *------ (RESULT1 / 60) RESULT2
       01 WRESULT2        PIC S9(003)v9(008) VALUE ZEROS.
      *------ (RESULT2 * 1852) RESULT-FINAL
       01 WRESULT-FINAL   PIC S9(003)v9(008) VALUE ZEROS.
      *--------------------------------------------------

       COPY "LAY-REL-DIS.CPY".
       COPY "FUNCAO.CPY".
       COPY "L-LINK.CPY".
       COPY "REL-TELAS-DIS.CPY".

       PROCEDURE DIVISION USING LINK-DADOS.
       ABERTURA.
           INITIALIZE WSAIDA.
           MOVE LID-ARQ-CLIENTE-1      TO WID-ARQ-CLIENTE-1
           MOVE LID-ARQ-VENDEDOR-1     TO WID-ARQ-VENDEDOR-1
           MOVE LID-ARQ-DISTRIBUICAO-1 TO WID-ARQ-DISTRIBUICAO-1
           PERFORM ABRE-ARQUIVOS.
           PERFORM S-PROCESSA-DADOS.

       ABRE-ARQUIVOS.
           OPEN I-O VENDEDORES.
           IF FS-VEN NOT = '00'
              STRING
                "ERRO NA ABERTURA DO ARQUIVO DE VENDEDORES ST "
001880                      FS-VEN DELIMITED BY SIZE
001900                      INTO WMENSAGEM
001910        END-STRING
001920        DISPLAY TELA-MENSAGEM
              ACCEPT WX
              EXIT PROGRAM
           END-IF.

           OPEN I-O CLIENTES.
           IF FS-CLI NOT = '00'
              MOVE
              'ERRO NA ABERTURA DO ARQUIVO DE CLIENTES ST '
                 TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              STOP RUN
           END-IF.

           OPEN OUTPUT DISTRIBUICAO.
           CLOSE DISTRIBUICAO.
           OPEN I-O DISTRIBUICAO.
           IF FS-DIS NOT = '00'
              MOVE
              'ERRO NA ABERTURA DO ARQUIVO DE DISTRIBUICAO ST '
                TO WTXT
              MOVE FS-DIS TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              STOP RUN
           END-IF.
       ABRE-ARQUIVOS-EXIT.
           EXIT.

           COPY R-PROCESSA-DADOS.CPY.
           COPY R-IMP-DIS.CPY.
