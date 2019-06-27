      *------------------------------------
      * LAYOUT DO RELATORIOO DE VENDEDORES
      *------------------------------------
       01  CAB01.
           02 FILLER            PIC X(43) VALUE SPACES.
           02 FILLER            PIC X(54) VALUE
           '     REL VENDEDORES      '.

       01  CAB02.
           03 FILLER PIC X(09) VALUE 'COD.    '.
           03 FILLER PIC X(04) VALUE 'NOME'.
           03 FILLER PIC X(38) VALUE SPACES.
           03 FILLER PIC X(03) VALUE 'CPF'.
           03 FILLER PIC X(13) VALUE SPACES.
           03 FILLER PIC X(08) VALUE 'LATITUDE'.
           03 FILLER PIC X(07) VALUE SPACES.
           03 FILLER PIC X(09) VALUE 'LONGITUDE'.

       01  CAB03.
           03 FILLER PIC X(92) VALUE IS ALL '-'.

       01  DET01.
           03 RL-CODIGO-VEN       PIC 9(007) VALUE ZEROES.
           03 FILLER                   PIC X(01)  VALUE SPACES.
           03 RL-NOME-VEN         PIC X(040) VALUE SPACES.
           03 FILLER                   PIC X(01)  VALUE SPACES.
           03 RL-CPF-VEN          PIC 99999999999 VALUE ZEROS.
           03 FILLER                   PIC X(06)  VALUE SPACES.
           03 RL-LATITUDE-VEN     PIC -999,99999999 VALUE ZEROS.
           03 FILLER                   PIC X(01)  VALUE SPACES.
           03 RL-LONGITUDE-VEN    PIC -999,99999999 VALUE ZEROS.
           03 FILLER                   PIC X(01)  VALUE SPACES.
