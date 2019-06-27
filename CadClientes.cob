      *--------------------------
      * CADASTRO DE CLIENTES
      *---------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CadClientes.
      *AUTHOR.      URBANO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CLIENTES ASSIGN TO  DISK WID-ARQ-CLIENTE
              ORGANIZATION      IS INDEXED
              ACCESS MODE       IS DYNAMIC
              RECORD KEY        IS CHAVE-CLI
              ALTERNATE RECORD KEY IS CNPJ WITH DUPLICATES
              LOCK MODE         IS MANUAL
              FILE STATUS       IS FS-CLI.
       SELECT ARQIMPORT  ASSIGN TO DISK WID-ARQ-IMPORT
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

       FD  ARQIMPORT.
       01  ARQIMPORT-REC.
           05  ARQIMPORT-DATA-01   PIC X(00512).

       WORKING-STORAGE SECTION.
       01 WARQIMPORT-REC               PIC X(512) VALUE SPACES.
       01 WARQIMPORT-2 REDEFINES WARQIMPORT-REC.
          02 WCODIGO-IMP                PIC  9(007).
          02 WCNPJ-IMP                  PIC  9(014).
          02 WRAZAO-IMP                 PIC  X(040).
          02 WLATITUDE-IMP              PIC S9(003)v9(008).
          02 WLONGITUDE-IMP             PIC S9(003)v9(008).
          02 FILLER                     PIC X(429).

       01 WREGISTRO-CLIENTES.
          02 WCHAVE-CLI.
             03 WCODIGO             PIC  9(007) VALUE ZEROES.
             03 WCNPJ               PIC  9(014) VALUE ZEROES.
          02 WRAZAO                 PIC  X(040) VALUE ZEROES.
          02 WLATITUDE              PIC S9(003)v9(008) VALUE ZEROES.
          02 WLONGITUDE             PIC S9(003)v9(008) VALUE ZEROES.

       01 FS-CLI.
           02 FS-CLIENTES-1                PIC 9.
           02 FS-CLIENTES-2                PIC 9.
           02 FS-CLIENTE-R REDEFINES FS-CLIENTES-2 PIC 99 COMP-X.
       01 FS-IMPORT.
           02 FS-IMPORT-1                PIC 9.
           02 FS-IMPORT-2                PIC 9.
           02 FS-IMPORT-R REDEFINES FS-IMPORT-2 PIC 99 COMP-X.

       01 WID-ARQ-CLIENTE.
           02 WID-ARQ-CLIENTE-1            PIC X(50).

       01 WID-ARQ-IMPORT.
           02 WID-ARQ-IMPORT-1             PIC X(50).

       01 WCGCCPF.
           02 WCGCCPF-1                    PIC X(01).
           02 WCGCCPF-2                    PIC X(01).

       77 WOPCAO-C        PIC 9 VALUE ZEROES.
       77 WX              PIC X VALUE SPACES.
       77 WSAIDA          PIC 9 VALUE ZEROES.
      *                  (0=OK, 1=SAIDA)
       77 WCONT           PIC 9(04) VALUE ZEROES.

       01 WMENSAGEM.
          02 WTXT         PIC X(50) VALUE SPACES.
          02 WST          PIC 99 VALUE ZEROES.

       COPY "FUNCAO.CPY".
       COPY "L-LINK.CPY".
       COPY "TELAS-CLI.CPY".


       PROCEDURE DIVISION USING LINK-DADOS.
       ABERTURA.
           INITIALIZE WSAIDA WOPCAO-C.
           MOVE LID-ARQ-CLIENTE-1 TO WID-ARQ-CLIENTE-1
           PERFORM ABRE-CLIENTE.

       ACCEPTA-OPCAO.
           PERFORM UNTIL WOPCAO-C = 5
                DISPLAY TELA-OPCAO
                ACCEPT TELA-OPCAO
           EVALUATE WOPCAO-C
             WHEN 1
               PERFORM S-INCLUIR
             WHEN 2
               PERFORM S-ALTERAR
             WHEN 3
               PERFORM S-EXCLUIR
             WHEN 4
               PERFORM S-IMPORTAR
             WHEN 5
               CLOSE CLIENTES
               EXIT PROGRAM
           END-EVALUATE
           END-PERFORM.
       ACCEPTA-OPCAO-EXIT.
           EXIT.

       COPY CRUD-CLI.CPY.

       S-IMPORTAR SECTION.
       IMPORTAR-1.
           MOVE 'INFORME O NOME DO ARQUIVO PARA IMPORTAÇÃO:' TO WTXT
           DISPLAY TELA-MENSAGEM
           ACCEPT TELA-NOME-ARIMPORT
           IF NOT ESC
              OPEN INPUT ARQIMPORT
              IF FS-IMPORT NOT = '00'
                 MOVE
                 'ERRO NA ABERTURA DO ARQUIVO DE IMPORTAÇÃO ST '
                  TO WTXT
                 MOVE FS-IMPORT TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
              ELSE
                 INITIALIZE WCONT
                 PERFORM LER-IMPORT UNTIL FS-IMPORT = '10'
                 MOVE FS-IMPORT TO WST
                 STRING
                  "IMPORTAÇÃO CONCLUÍDA ST " WST
                  " - REG. IMPORTADOS - "
                  WCONT DELIMITED BY SIZE INTO WTXT
                 MOVE FS-IMPORT TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 CLOSE ARQIMPORT
              END-IF
           END-IF.
           INITIALIZE TELA-NOME-ARIMPORT.
       IMP-FIM.
           EXIT.

       LER-IMPORT.
           READ ARQIMPORT NEXT.
           IF FS-IMPORT = '00'
              MOVE ARQIMPORT-REC TO WARQIMPORT-REC
              MOVE WCODIGO-IMP    TO  WCODIGO
              MOVE WCNPJ-IMP       TO WCNPJ
              MOVE WRAZAO-IMP      TO WRAZAO
              MOVE WLATITUDE-IMP   TO WLATITUDE
              MOVE WLONGITUDE-IMP  TO WLONGITUDE
              PERFORM LER-CLIENTE
              IF FS-CLI = '23'
                 MOVE WCNPJ       TO CNPJ
                 MOVE WRAZAO      TO RAZAO
                 MOVE WLATITUDE   TO LATITUDE
                 MOVE WLONGITUDE  TO LONGITUDE
                 PERFORM GRAVACAO-1 THRU GRAVACAO-EXIT
                 IF FS-CLI = '00'
                    ADD 1 TO WCONT
                 ELSE
                    MOVE 'ERRO NA GRAVAÇÃO DO ARQUIVO DE CLIENTES ST '
                    TO WTXT
                    MOVE FS-CLI TO WST
                    DISPLAY TELA-MENSAGEM
                    MOVE '10' TO FS-IMPORT
                    ACCEPT WX
                 END-IF
              END-IF.

       ABRE-CLIENTE.
           OPEN I-O CLIENTES.
           IF FS-CLI = '35'
              CLOSE CLIENTES OPEN OUTPUT CLIENTES
              CLOSE CLIENTES OPEN I-O CLIENTES
           END-IF
           IF FS-CLI = '00' OR '05'
              CONTINUE
           ELSE
              STRING "ERRO NA ABERTURA DO ARQUIVO DE CLIENTES ST "
001880                      FS-CLI DELIMITED BY SIZE
001900                      INTO WMENSAGEM
001910        END-STRING
001920        DISPLAY TELA-MENSAGEM
              ACCEPT WX
              STOP RUN.

       LER-CLIENTE.
           READ CLIENTES.
       LER-CLIENTE-FIM.
           EXIT.

       S-CHECA-CODIGO SECTION.
       CHECA-CODIGO.
          IF WCODIGO = ZEROES
              MOVE 'CODIGO INVALIDO ' TO WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE WMENSAGEM
              DISPLAY TELA-MENSAGEM
      *       SETFOCUS WCODIGO
           END-IF.
       CHECA-CODIGO-EXIT.
           EXIT.

       S-VALIDACPFCNPJ SECTION.
       R-VALIDA-CNPJ.
           MOVE 'J'        TO WCGCCPF-1.
           MOVE WCNPJ      TO WCGCCPF-2.
           MOVE WCGCCPF TO LINK-CPFCGC.

       COPY VALIDACNPJCPF.CPY.
