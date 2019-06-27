      *--------------------------
      *  MENU PRICIPAL
      *---------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MenuPincipal.
      *AUTHOR.     URBANO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WOPCAO          PIC 9 VALUE ZERO.
       77 WX              PIC 9 VALUE ZERO.

       01 LINK-DADOS.
          02 LID-ARQ-CLIENTE-1            PIC X(50).
          02 LID-ARQ-VENDEDOR-1           PIC X(50).
          02 LID-ARQ-DISTRIBUICAO-1       PIC X(50).

       SCREEN SECTION.
       01 MENU-PRINCIPAL.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "SISTEMA EXEMPLO".
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 4 COL 33 VALUE "MENU PRINCIPAL".
          02 LINE 7 COL 10 VALUE "[1] CADASTRO CLIENTES".
          02 LINE 8 COL 10 VALUE "[2] CADASTRO VENDEDORES".
          02 LINE 9 COL 10 VALUE "[3] RELATORIO DE CLIENTES".
          02 LINE 10 COL 10 VALUE "[4] RELATORIO DE VENDEDORES".
          02 LINE 11 COL 10 VALUE
             "[5] EXECUTAR DISTRIBUICAO DE CLIENTE".
          02 LINE 12 COL 10 VALUE "[6] SAIR SISTEMA".
          02 LINE 15 COL 10 VALUE "DIGITE A OPCAO DESEJADA[.]".
          02 OPCAO LINE 15 COL 34 PIC 9 USING WOPCAO AUTO.
       PROCEDURE DIVISION.
       INICIO.
          PERFORM UNTIL WOPCAO = 6
          MOVE 'ARQVEN.DAT' TO LID-ARQ-VENDEDOR-1
          MOVE 'ARQCLI.DAT' TO LID-ARQ-CLIENTE-1
          MOVE 'ARQDIS.DAT' TO LID-ARQ-DISTRIBUICAO-1
          DISPLAY MENU-PRINCIPAL
          ACCEPT MENU-PRINCIPAL
          EVALUATE WOPCAO
             WHEN 1
      *         MOVE 'ARQCLI.DAT' TO LID-ARQ-CLIENTE-1
               CALL "CadClientes" USING LINK-DADOS
               MOVE ZEROES TO WOPCAO
             WHEN 2
      *         MOVE 'ARQVEN.DAT' TO LID-ARQ-VENDEDOR-1
               CALL "CadVend" USING LINK-DADOS
               MOVE ZEROES TO WOPCAO
             WHEN 3
      *         MOVE 'ARQCLI.DAT' TO LID-ARQ-CLIENTE-1
               CALL "RelCli" USING LINK-DADOS
             WHEN 4
                CALL "RelVen" USING LINK-DADOS
             WHEN 5
                CALL "ExeDistrCli" USING LINK-DADOS
             WHEN 6
                STOP RUN
           END-EVALUATE
           END-PERFORM.
