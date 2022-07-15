      *================================================================J00001001
       IDENTIFICATION                            DIVISION.              00002001
      *================================================================*00003001
                                                                        00004001
       PROGRAM-ID. FR06DB11.                                            00005001
                                                                        00006001
      *================================================================*00007001
      *   AUTOR...........: JOEI LORENTI                               *00008001
      *   ANALISTA........: IVAN SANCHES                               *00008101
      *   DATA ...........: 09/06/2022                                 *00009001
      *----------------------------------------------------------------*00009101
      *   OBJETIVO........: LER TODOS OS REGISTROS DA TABELA DB2 (FUNC)*00010001
      *                     E GRAVAR NO ARQUIVO DE SAIDA RELDB2        *00010101
      *                                                                *
      *----------------------------------------------------------------*
      *  BASE DE DADOS:                                                *
      *      TABELAS DB2                             INCLUDE/BOOK      *
      *      FOUR001.FUNC                              BOOKFUNC        *
      *----------------------------------------------------------------*
      *   MODULOS.........:
      *   #GRVLOG2 - LOG DE ERROS
      *
      *                                                                *00010201
      *================================================================*00011001
                                                                        00012001
      *================================================================*00013001
       ENVIRONMENT                               DIVISION.              00014001
      *================================================================*00015001
                                                                        00015101
      *----------------------------------------------------------------*00015201
       CONFIGURATION                             SECTION.               00015301
      *----------------------------------------------------------------*00015401
                                                                        00015501
       SPECIAL-NAMES.                                                   00015601
           DECIMAL-POINT IS COMMA.                                      00015701
                                                                        00015801
      *----------------------------------------------------------------*00015801
       INPUT-OUTPUT                              SECTION.               00015801
      *----------------------------------------------------------------*00015801
                                                                        00015801
       FILE-CONTROL.                                                    00015801
           SELECT RELDB2 ASSIGN            TO RELDB2                    00015801
           FILE STATUS                     IS WRK-FS-REL.               00015801
                                                                        00015801
                                                                        00015801
      *================================================================*00016001
       DATA                                      DIVISION.              00016101
      *================================================================*00016201
                                                                        00017001
      *----------------------------------------------------------------*00015801
       FILE                                      SECTION.               00015801
      *----------------------------------------------------------------*00015801
                                                                        00015801
      *----------------------------------------------------------------*00015801
      * OUTPUT - DADOS DO ARQUIVO DE SAIDA (RELDB2)                    *00015801
      *                                             - LRECL = 98       *00015801
      *----------------------------------------------------------------*00015801

       FD  RELDB2                                                       00015801
           RECORDING MODE IS F                                          00015801
           LABEL RECORD IS STANDARD                                     00015801
           BLOCK CONTAINS 0 RECORDS.                                    00015801
                                                                        00015801
      *------------------------ LRECL - 98                              00015801
       01 FD-REG-RELDB2.                                                00015801
           05 REG-ID                      PIC 9(04).                    00015801
           05 REG-NOME                    PIC X(30).                    00015801
           05 REG-SETOR                   PIC 9(04).                    00015801
           05 REG-SALARIO                 PIC 9(10).                    00015801
           05 REG-EMAIL                   PIC X(40).
           05 FILLER                      PIC X(10).
                                                                        00015801
      *----------------------------------------------------------------*00018001
       WORKING-STORAGE                           SECTION.               00019001
      *----------------------------------------------------------------*00020001
                                                                        00020101
      *----------------------------------------------------------------*00020201
       01 FILLER                       PIC  X(050)         VALUE        00020301
           '***  FR06DB01 - INICIO DA AREA DE WORKING   ***'.           00020401
      *----------------------------------------------------------------*00020501
                                                                        00020601
      *----------------------------------------------------------------*00020701
       01 FILLER                       PIC  X(050)         VALUE        00020801
           '***  VARIAVEIS DE NULIDADE  ***'.                           00020901
      *----------------------------------------------------------------*00021001
                                                                        00021101
       77 WRK-EMAIL-NULL               PIC S9(4) COMP VALUE ZEROS.      00021201
                                                                        00021301
      *----------------------------------------------------------------*00020701
       01 FILLER                       PIC  X(050)         VALUE        00020801
           '***  ACUMULADORES  ***'.                                    00020901
      *----------------------------------------------------------------*00021001

       77 WRK-ACUM-LIDOS               PIC 9(02) VALUE ZEROS.

      *----------------------------------------------------------------*00020701
       01 FILLER                       PIC  X(050)         VALUE        00020801
           '***  AUXILIARES  ***'.                                      00020901
      *----------------------------------------------------------------*00021001

       77 WRK-SOMA-SAL                 PIC 9(11) VALUE ZEROS.
       77 WRK-MEDIA-SAL                PIC 9(10) VALUE ZEROS.

      *----------------------------------------------------------------*00021501
       01 FILLER                       PIC  X(050)         VALUE        00021601
           '***  AREA DE TRATAMENTO DE ERROS DB2 ***'.                  00021701
      *----------------------------------------------------------------*00021801
                                                                        00021901
           COPY '#GRVLOG2'.                                             00022001
                                                                        00022101
       77 WRK-ID                       PIC 9(04).                       00022301
       77 WRK-FS-REL                   PIC 9(02).                       00022401
      *----------------------------------------------------------------*00022501
       01 FILLER                       PIC  X(050)         VALUE        00022601
           '*** AREA DB2 ***'.                                          00022701
      *----------------------------------------------------------------*00022801
                                                                        00022901
           EXEC SQL                                                     00023401
              INCLUDE BOOKFUNC                                          00023501
           END-EXEC.                                                    00023601
                                                                        00023301
           EXEC SQL                                                     00023001
              INCLUDE SQLCA                                             00023101
           END-EXEC.                                                    00023201
                                                                        00023701
      *----------------------------------------------------------------*00023801
       01 FILLER                      PIC  X(050)         VALUE         00023901
           '*** AREA CURSOR ***'.                                       00024001
      *----------------------------------------------------------------*00024101
                                                                        00024201
           EXEC SQL                                                     00024201
            DECLARE CFUNC CURSOR FOR                                    00024201
             SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL
                    FROM FOUR001.FUNC
           END-EXEC.                                                    00024201
                                                                        00024201
      *----------------------------------------------------------------*00023801
       01 FILLER                      PIC  X(050)         VALUE         00023901
           '*** FIM DA WORKING STORAGE SECTION ***'.                    00024001
      *----------------------------------------------------------------*00024101
                                                                        00024201
      *================================================================*00024301
       PROCEDURE                                 DIVISION.              00024401
      *================================================================*00024501
                                                                        00024601
      ******************************************************************00024701
      *                   PROCESSAMENTO PRINCIPAL                      *00024801
      ******************************************************************00024901
                                                                        00025001
      *----------------------------------------------------------------*00025101
       0000-PRINCIPAL                  SECTION.                         00025201
      *----------------------------------------------------------------*00025301
                                                                        00025401
                PERFORM 1000-INICIAR.                                   00025501
                                                                        00025601
                PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100          00025701

                PERFORM 3000-FINALIZAR.                                 00025901
                                                                        00026001
                STOP RUN.                                               00026201
                                                                        00026301
      *----------------------------------------------------------------*00026401
       0000-99-FIM.                    EXIT.                            00026501
      *----------------------------------------------------------------*00026601
                                                                        00026701
      *----------------------------------------------------------------*00026801
       1000-INICIAR                              SECTION.               00026901
      *----------------------------------------------------------------*00027001
                                                                        00027101
            EXEC SQL                                                    00027201
                OPEN CFUNC                                              00027301
            END-EXEC.                                                   00027301

           IF (SQLCODE                 NOT EQUAL ZEROS AND +100) OR
              (SQLWARN0                EQUAL 'W')
               MOVE SQLCODE            TO WRK-SQLCODE
               DISPLAY 'ERRO ... '     WRK-SQLCODE
               GOBACK
           END-IF.

           OPEN OUTPUT RELDB2.                                          00027301

           PERFORM 1100-TESTAR-FILESTATUS.

           PERFORM 2100-LER-FUNCIONARIO.                                00027301

      *------------------------------------------------------------     00027601
       1000-99-FIM.                    EXIT.                            00027701
      *------------------------------------------------------------     00027801
                                                                        00027901
      *------------------------------------------------------------     00028001
       1100-TESTAR-FILESTATUS                    SECTION.               00028101
      *----------------------------------------------------------------*00028201

            IF WRK-FS-REL              NOT EQUAL ZERO                   00027901
               MOVE 'FR06DB11'         TO WRK-PROGRAMA                  00027901
               MOVE 'ERRO NO OPEN RELDB2 '                              00027901
                                       TO WRK-MSG-ERRO                  00027901
               MOVE '1100'             TO WRK-SECTION                   00027901
               MOVE WRK-FS-REL         TO WRK-STATUS                    00027901
               PERFORM 9000-GRAVAR-ERROS                                00027901
            END-IF.                                                     00027901
                                                                        00027901
      *----------------------------------------------------------------*00027601
       1100-99-FIM.                    EXIT.                            00027701
      *----------------------------------------------------------------*00027801
                                                                        00027901
      *----------------------------------------------------------------*00028001
       2000-PROCESSAR                  SECTION.                         00028101
      *----------------------------------------------------------------*00028201

               IF WRK-EMAIL-NULL = -1                                   00032601
                 MOVE SPACES           TO DB2-EMAIL                     00032701
               END-IF.                                                  00033001

                 MOVE DB2-ID           TO REG-ID
                 MOVE DB2-NOME         TO REG-NOME
                 MOVE DB2-SETOR        TO REG-SETOR
                 MOVE DB2-SALARIO      TO REG-SALARIO
                 MOVE DB2-EMAIL        TO REG-EMAIL

               WRITE   FD-REG-RELDB2.                                   00030001

               PERFORM 2100-LER-FUNCIONARIO.                            00030001
                                                                        00030001
      *----------------------------------------------------------------*00030101
       2000-99-FIM.                    EXIT.                            00030201
      *----------------------------------------------------------------*00030301
                                                                        00030401
      *----------------------------------------------------------------*00035001
       2100-LER-FUNCIONARIO            SECTION.                         00035101
      *----------------------------------------------------------------*00035201
                                                                        00036001
           EXEC SQL                                                     00028601
             FETCH CFUNC                                                00028701
              INTO :DB2-ID,                                             00028801
                   :DB2-NOME,                                           00028901
                   :DB2-SETOR,                                          00029001
                   :DB2-SALARIO,                                        00029101
                   :DB2-DATAADM,                                        00029201
                   :DB2-EMAIL     :WRK-EMAIL-NULL                       00029301
           END-EXEC.                                                    00029601

           EVALUATE SQLCODE                                             00031001

            WHEN 0                                                      00032001
              ADD 1                     TO WRK-ACUM-LIDOS
              CONTINUE

            WHEN 100                                                    00033201
              DISPLAY ' FINAL DA TABELA '                               00033301

            WHEN OTHER                                                  00033401
              MOVE SQLCODE             TO WRK-SQLCODE                   00033501
              DISPLAY 'ERRO ... ' WRK-SQLCODE                           00033601
              MOVE '4000 '             TO WRK-SECTION                   00033701
              MOVE 'NA LEITURA '       TO WRK-MSG-ERRO                  00033801
                                                                        00034001
              PERFORM 9000-GRAVAR-ERROS                                 00034101
                                                                        00034201
           END-EVALUATE.                                                00034301
                                                                        00036901
      *----------------------------------------------------------------*00037001
       2100-99-FIM.                    EXIT.                            00037101
      *----------------------------------------------------------------*00037201
      *----------------------------------------------------------------*00035001
       3000-FINALIZAR                  SECTION.                         00035101
      *----------------------------------------------------------------*00035201
                                                                        00036001
            EXEC SQL                                                    00036001
              CLOSE CFUNC                                               00036801
            END-EXEC.                                                   00036901

           CLOSE RELDB2.                                                00036901
                                                                        00036901
           PERFORM 1100-TESTAR-FILESTATUS.


            DISPLAY ' -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
            DISPLAY ' *               FIM DE PROCESSAMENTO            *'
            DISPLAY ' * TOTAL DE REGISTROS LIDOS..:' WRK-ACUM-LIDOS
            DISPLAY ' *                                               *'
            DISPLAY ' -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'.


      *----------------------------------------------------------------*00037001
       3000-99-FIM.                    EXIT.                            00037101
      *----------------------------------------------------------------*00037201
                                                                        00037301
      *----------------------------------------------------------------*00037401
       9000-GRAVAR-ERROS                         SECTION.               00037501
      *----------------------------------------------------------------*00037601
                                                                        00037701
            CALL 'GRVLOG'             USING WRK-LOG                     00037801
                                                                        00038401
            GOBACK.                                                     00038501
                                                                        00038601
      *----------------------------------------------------------------*00038701
       9000-99-FIM.                              EXIT.                  00038801
      *----------------------------------------------------------------*00038901
