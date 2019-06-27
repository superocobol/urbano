      *-------------------------------------
      * LAYOUT DO RELATORIOO DE DISTRIBUIÇÃO
      *-------------------------------------
       01  CAB01.
           02 FILLER            PIC X(36) VALUE IS ALL '-'.
           02 FILLER            PIC X(29) VALUE
           '  RELATORIO DE DISTRIBUICAO '.
           02 FILLER            PIC X(59) VALUE IS ALL '-'.

       01  CAB02.
           03 FILLER PIC X(13) VALUE 'COD. CLIENTE'.
           03 FILLER PIC X(01) VALUE SPACES.
           03 FILLER PIC X(13) VALUE 'RAZÃO SOCIAL'.
           03 FILLER PIC X(30) VALUE SPACES.
           03 FILLER PIC X(14) VALUE 'COD. VEND.  '.
           03 FILLER PIC X(01) VALUE SPACES.
           03 FILLER PIC X(11) VALUE 'NOME VEND.'.
           03 FILLER PIC X(32) VALUE SPACES.
           03 FILLER PIC X(15) VALUE 'DISTÂNCIA EM m'.

       01  CAB03.
           03 FILLER PIC X(122) VALUE IS ALL '-'.

       01  DET01.
           03 RL-CODIGO-CLI       PIC 9(07)  VALUE ZEROES.
           03 FILLER                   PIC X(07)  VALUE SPACES.
           03 RL-RAZAO-CLI        PIC X(40) VALUE SPACES.
           03 FILLER                   PIC X(01)  VALUE SPACES.
           03 RL-CODIGO-VEN       PIC 9(03)  VALUE ZEROES.
           03 FILLER                   PIC X(05)  VALUE SPACES.
           03 RL-NOME-VEN         PIC X(40)  VALUE SPACES.
           03 FILLER                   PIC X(01)  VALUE SPACES.
           03 RL-DISTANCIA-VEN    PIC -999,99999999 VALUE ZEROS.
