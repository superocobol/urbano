      *-------------------------------------
      * ROTINA DE IMPRESSAO DO RELATORIO DE CLIENTES
      *-------------------------------------

       S-OPCAO1 SECTION.
       R-OPCAO-1.
           IF WOPCAO1 = 'a' OR 'A'
              MOVE ZEROES TO CODIGO
              START CLIENTES KEY IS NOT LESS THAN CHAVE-CLI
           ELSE
              IF WOPCAO1 = 'd' OR 'D'
                 MOVE 9999999 TO CODIGO
                 START CLIENTES KEY IS NOT LESS THAN CHAVE-CLI
              END-IF
           END-IF
           IF FS-CLI NOT = '0'
                 MOVE 'ERRO START CLIENTES ST ' TO WTXT
                 MOVE FS-CLI TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF
           MOVE 'REL1.CSV' TO WID-ARQ-REL
           OPEN OUTPUT ARQREL
           PERFORM S-IMP-CAB
           PERFORM S-IMP-REL-CLI-1 UNTIL FS-CLI NOT = '00'.
       R-OPCAO-1-EXIT.
           CLOSE ARQREL.
           EXIT.

       S-OPCAO2 SECTION.
       R-OPCAO-2.
           IF WOPCAO2 = 'c' OR 'C'
              INITIALIZE RAZAO
              START CLIENTES KEY IS NOT LESS THAN RAZAO
              IF FS-CLI NOT = '0'
                 MOVE 'ERRO START CLIENTES ST ' TO WTXT
                 MOVE FS-CLI TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
              END-IF
              MOVE 'REL2.CSV' TO WID-ARQ-REL
              OPEN OUTPUT ARQREL
              PERFORM S-IMP-CAB
              PERFORM S-IMP-REL-CLI-1 UNTIL FS-CLI NOT = '00'
          END-IF.
       R-OPCAO-2-EXIT.
           CLOSE ARQREL.
           EXIT.

       S-OPCAO3 SECTION.
       R-OPCAO-3.
           IF WOPCAO3 = 'f' OR 'F'
              MOVE ZEROES TO CODIGO
              START CLIENTES KEY IS NOT LESS THAN CODIGO
              IF FS-CLI NOT = '0'
                 MOVE 'ERRO START CLIENTES ST ' TO WTXT
                 MOVE FS-CLI TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
              END-IF
              MOVE 'REL3.CSV' TO WID-ARQ-REL
              OPEN OUTPUT ARQREL
              PERFORM S-IMP-CAB
              PERFORM S-IMP-REL-CLI-3 UNTIL FS-CLI NOT = '00'
           END-IF.
       R-OPCAO-3-EXIT.
           CLOSE ARQREL.
           EXIT.

       S-OPCAO4 SECTION.
       R-OPCAO-4.
           IF WOPCAO4 = 'f' OR 'F'
              MOVE ZEROES TO CODIGO
              START CLIENTES KEY IS NOT LESS THAN RAZAO
              IF FS-CLI NOT = '0'
                 MOVE 'ERRO START CLIENTES ST ' TO WTXT
                 MOVE FS-CLI TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
              END-IF
              MOVE 'REL4.CSV' TO WID-ARQ-REL
              OPEN OUTPUT ARQREL
              PERFORM S-IMP-CAB
              PERFORM S-IMP-REL-CLI-4 UNTIL FS-CLI NOT = '00'
           END-IF.
       R-OPCAO-4-EXIT.
           CLOSE ARQREL.
           EXIT.


       S-IMP-REL-CLI-1 SECTION.
       R-IMP-REL-CLI-1.
           IF WOPCAO1 = 'a' OR 'A'
              READ CLIENTES NEXT
           END-IF
           IF WOPCAO1 = 'd' OR 'D'
              READ CLIENTES PREVIOUS
           END-IF
           PERFORM S-IMP-DET.
       R-IMP-REL-CLI-1-EXIT.
           EXIT.

       S-IMP-REL-CLI-2 SECTION.
       R-IMP-REL-CLI-2.
           READ CLIENTES NEXT RECORD WITH IGNORE LOCK KEY IS RAZAO.
           PERFORM S-IMP-DET.
       R-IMP-REL-CLI-1-EXIT.
           EXIT.

       S-IMP-REL-CLI-3 SECTION.
       R-IMP-REL-CLI-3.
           READ CLIENTES NEXT RECORD WITH IGNORE LOCK KEY IS CODIGO.
           PERFORM S-IMP-DET.
       R-IMP-REL-CLI-3.
           EXIT.

       S-IMP-REL-CLI-4 SECTION.
       R-IMP-REL-CLI-4.
           READ CLIENTES NEXT RECORD WITH IGNORE LOCK KEY IS RAZAO.
           PERFORM S-IMP-DET.
       R-IMP-REL-CLI-4.
           EXIT.

       S-IMP-CAB SECTION.
       R-IMP-CAB.
           MOVE CAB01 TO ARQREL-DATA-01
           WRITE ARQREL-REC
           MOVE CAB02 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
           MOVE CAB03 TO ARQREL-DATA-01
           WRITE ARQREL-REC.

       S-IMP-DET SECTION.
       R-IMP-DET.
           MOVE CODIGO TO RL-CODIGO
           MOVE RAZAO TO RL-RAZAO
           MOVE CNPJ TO RL-CNPJ
           MOVE LATITUDE TO RL-LATITUDE
           MOVE LONGITUDE TO RL-LONGITUDE
           MOVE DET01 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
