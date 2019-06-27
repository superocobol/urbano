      *-------------------------------------
      * ROTINA DE IMPRESSAO DO RELATORIOO DE VENDEDORES
      *-------------------------------------
       S-OPCAO1 SECTION.
       R-OPCAO-1.
           IF WOPCAO1 = 'a' OR 'A'
              MOVE ZEROES TO CODIGO-VEN
              START VENDEDORES KEY IS NOT LESS THAN CHAVE-VEN
           ELSE
              IF WOPCAO1 = 'd' OR 'D'
                 MOVE 999 TO CODIGO-VEN
                 START VENDEDORES KEY IS NOT LESS THAN CHAVE-VEN
              END-IF
           END-IF
           IF FS-VEN NOT = '0'
                 MOVE 'ERRO START VENDEDORES ST ' TO WTXT
                 MOVE FS-VEN TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF
           MOVE 'REL-VEN1.CSV' TO WID-ARQ-REL
           OPEN OUTPUT ARQREL
           PERFORM S-IMP-CAB
           PERFORM S-IMP-REL-VEN-1 UNTIL FS-VEN NOT = '00'.
       R-OPCAO-1-EXIT.
           CLOSE ARQREL.
           EXIT.

       S-OPCAO2 SECTION.
       R-OPCAO-2.
           IF WOPCAO2 = 'c' OR 'C'
              START VENDEDORES KEY IS NOT LESS THAN CODIGO-VEN
           END-IF
           IF WOPCAO2 = 'n' OR 'N'
              START VENDEDORES KEY IS NOT LESS THAN NOME-VEN
           END-IF
           IF FS-VEN NOT = '0'
                 MOVE 'ERRO START VENDEDORES ST ' TO WTXT
                 MOVE FS-VEN TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF
           MOVE 'REL-VEN2.CSV' TO WID-ARQ-REL
           OPEN OUTPUT ARQREL
           PERFORM S-IMP-CAB
           PERFORM S-IMP-REL-VEN-1 UNTIL FS-VEN NOT = '00'.
       R-OPCAO-2-EXIT.
           CLOSE ARQREL.
           EXIT.

       S-OPCAO3 SECTION.
       R-OPCAO-3.
           IF WOPCAO3 = 'f' OR 'F'
              MOVE ZEROES TO CODIGO-VEN
              START VENDEDORES KEY IS NOT LESS THAN CODIGO-VEN
              IF FS-VEN NOT = '0'
                 MOVE 'ERRO START VENDEDORES ST ' TO WTXT
                 MOVE FS-VEN TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
              END-IF
              MOVE 'REL-VEN3.CSV' TO WID-ARQ-REL
              OPEN OUTPUT ARQREL
              PERFORM S-IMP-CAB
              PERFORM S-IMP-REL-VEN-3 UNTIL FS-VEN NOT = '00'
           END-IF.
       R-OPCAO-3-EXIT.
           CLOSE ARQREL.
           EXIT.

       S-OPCAO4 SECTION.
       R-OPCAO-4.
           IF WOPCAO4 = 'g' OR 'G'
              MOVE ZEROES TO CODIGO
              START VENDEDORES KEY IS NOT LESS THAN NOME-VEN
              IF FS-VEN NOT = '0'
                 MOVE 'ERRO START VENDEDORES ST ' TO WTXT
                 MOVE FS-VEN TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE WMENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
              END-IF
              MOVE 'REL4.CSV' TO WID-ARQ-REL
              OPEN OUTPUT ARQREL
              PERFORM S-IMP-CAB
              PERFORM S-IMP-REL-VEN-4 UNTIL FS-VEN NOT = '00'
           END-IF.
       R-OPCAO-4-EXIT.
           CLOSE ARQREL.
           EXIT.


       S-IMP-REL-VEN-1 SECTION.
       R-IMP-REL-VEN-1.
           IF WOPCAO1 = 'a' OR 'A'
              READ VENDEDORES NEXT
           END-IF
           IF WOPCAO1 = 'd' OR 'D'
              READ VENDEDORES PREVIOUS
           END-IF
           PERFORM S-IMP-DET.
       R-IMP-REL-VEN-1-EXIT.
           EXIT.

       S-IMP-REL-VEN-2 SECTION.
       R-IMP-REL-VEN-2.
           IF WOPCAO2 = 'c' OR 'C' OR 'a' OR 'a'
              READ VENDEDORES NEXT
      *                RECORD WITH
      *                IGNORE LOCK KEY IS NOME-VEN.
           END-IF
           PERFORM S-IMP-DET.
       R-IMP-REL-VEN-1-EXIT.
           EXIT.

       S-IMP-REL-VEN-3 SECTION.
       R-IMP-REL-VEN-3.
           READ VENDEDORES NEXT
      *                RECORD WITH
      *                IGNORE LOCK KEY IS CODIGO-VEN.
           PERFORM S-IMP-DET.
       R-IMP-REL-VEN-3.
           EXIT.

       S-IMP-REL-VEN-4 SECTION.
       R-IMP-REL-VEN-4.
           READ VENDEDORES NEXT
      *                RECORD WITH
      *                IGNORE LOCK KEY IS NOME-VEN.
           PERFORM S-IMP-DET.
       R-IMP-REL-VEN-4.
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
           MOVE CODIGO-VEN TO RL-CODIGO-VEN
           MOVE NOME-VEN TO RL-NOME-VEN
           MOVE CPF-VEN TO RL-CPF-VEN
           MOVE LATITUDE-VEN TO RL-LATITUDE-VEN
           MOVE LONGITUDE-VEN TO RL-LONGITUDE-VEN
           MOVE DET01 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
