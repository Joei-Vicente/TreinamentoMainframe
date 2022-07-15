      *================================================================J00001001
       IDENTIFICATION                            DIVISION.              00002001
      *================================================================*00003001
                                                                        00004001
       PROGRAM-ID. FR06DB10.                                            00005001
                                                                        00006001
      *================================================================*00007001
      *   AUTOR...........: JOEI LORENTI                               *00008001
      *   ANALISTA........: IVAN SANCHES                               *00008101
      *   DATA ...........: 08/06/2022                                 *00009001
      *----------------------------------------------------------------*00009101
      *   OBJETIVO........: ATUALIZA TABELA FUNC A PARTIR DA           *00010001
      *                     SYSIN                                      *00010101
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
                                                                        00015801
      *================================================================*00016001
       DATA                                      DIVISION.              00016101
      *================================================================*00016201
                                                                        00017001
      *----------------------------------------------------------------*00015801
       FILE                                      SECTION.               00015801
      *----------------------------------------------------------------*00015801
                                                                        00015801
                                                                        00015801
      *----------------------------------------------------------------*00018001
       WORKING-STORAGE                           SECTION.               00019001
      *----------------------------------------------------------------*00020001
                                                                        00020101
      *----------------------------------------------------------------*00020201
       01 FILLER                       PIC  X(050)         VALUE        00020301
           '***  FR06DB10 - INICIO DA AREA DE WORKING   ***'.           00020401
      *----------------------------------------------------------------*00020501
                                                                        00020601
      *----------------------------------------------------------------*00020701
       01 FILLER                       PIC  X(050)         VALUE        00020801
           '***  VARIAVEIS DE NULIDADE  ***'.                           00020901
      *----------------------------------------------------------------*00021001
                                                                        00021101
       77 WRK-EMAIL-NULL               PIC S9(4) COMP VALUE ZEROS.      00021201
       77 WRK-TEL-NULL                 PIC S9(4) COMP VALUE ZEROS.      00021201
                                                                        00021301
      *----------------------------------------------------------------*00021501
       01 FILLER                       PIC  X(050)         VALUE        00021601
           '***  AREA DE TRATAMENTO DE ERROS DB2 ***'.                  00021701
      *----------------------------------------------------------------*00021801
                                                                        00021901
       COPY '#GRVLOG'.                                                  00022001
                                                                        00022101
       77 WRK-SQLCODE                  PIC -999.                        00022201
                                                                        00022401
       01 WRK-ID.
          05 FILLER                    PIC X(10).
          05 WRK-ID-AC                 PIC 9(04).
                                                                        00022401
       01 WRK-NOME.
          05 FILLER                    PIC X(10).
          05 WRK-NOME-AC               PIC X(30).
                                                                        00022401
       01 WRK-SETOR.
          05 FILLER                    PIC X(10).
          05 WRK-SETOR-AC              PIC X(04).
                                                                        00022401
       01 WRK-SALARIO.
          05 FILLER                    PIC X(10).
          05 WRK-SALARIO-AC            PIC 9(10).
                                                                        00022401
       01 WRK-DATAADM.
          05 FILLER                    PIC X(10).
          05 WRK-DATAADM-AC            PIC X(10).
                                                                        00022401
       01 WRK-EMAIL.
          05 FILLER                    PIC X(10).
          05 WRK-EMAIL-AC              PIC X(40).

       01 WRK-TELEFONE.
          05 FILLER                    PIC X(10).
          05 WRK-TEL-AC                PIC X(40).

      *----------------------------------------------------------------*00022501
       01 FILLER                       PIC  X(050)         VALUE        00022601
           '*** AREA DB2 ***'.                                          00022701
      *----------------------------------------------------------------*00022801
                                                                        00022901
           EXEC SQL                                                     00023401
           INCLUDE #BKFUNC2                                             00023501
           END-EXEC.                                                    00023601
                                                                        00023301
           EXEC SQL                                                     00023001
              INCLUDE SQLCA                                             00023101
           END-EXEC.                                                    00023201
                                                                        00023701
      *----------------------------------------------------------------*00023801
       01 FILLER                       PIC  X(050)         VALUE        00023901
           '***AUXILIARES ***'.                                         00024001
      *----------------------------------------------------------------*00024101
                                                                        00024201
       77 WRK-EMAIL-LEN                PIC S9(4).

       77 WRK-POSICAO                  PIC S9(4).
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
       0000-PRINCIPAL                            SECTION.               00025201
      *----------------------------------------------------------------*00025301
                                                                        00025401
                PERFORM 1000-INICIAR                                    00025501
                                                                        00025601
                PERFORM 2000-PROCESSAR                                  00025701
                                                                        00025801
                PERFORM 3000-FINALIZAR.                                 00025901
                                                                        00026001
                STOP RUN.                                               00026201
                                                                        00026301
      *----------------------------------------------------------------*00026401
       0000-99-FIM.                              EXIT.                  00026501
      *----------------------------------------------------------------*00026601
                                                                        00026701
      *----------------------------------------------------------------*00026801
       1000-INICIAR                              SECTION.               00026901
      *----------------------------------------------------------------*00027001
                                                                        00027101
           ACCEPT WRK-ID               FROM SYSIN.                      00027201
           ACCEPT WRK-NOME             FROM SYSIN.                      00027201
           ACCEPT WRK-SETOR            FROM SYSIN.                      00027201
           ACCEPT WRK-SALARIO          FROM SYSIN.                      00027201
           ACCEPT WRK-DATAADM          FROM SYSIN.                      00027201
           ACCEPT WRK-EMAIL            FROM SYSIN.                      00027201
           ACCEPT WRK-TELEFONE         FROM SYSIN.                      00027201
                                                                        00027301
      *----------------------------------------------------------------*00027601
       1000-99-FIM.                              EXIT.                  00027701
      *----------------------------------------------------------------*00027801
                                                                        00027901
      *----------------------------------------------------------------*00028001
       2000-PROCESSAR                            SECTION.               00028101
      *----------------------------------------------------------------*00028201
           MOVE WRK-ID-AC              TO DB2-ID.                       00028301

           EXEC SQL
            SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL,TELEFONE         00028301
             INTO :DB2-ID,
                  :DB2-NOME,                                            00028301
                  :DB2-SETOR,
                  :DB2-SALARIO,                                         00028301
                  :DB2-DATAADM,
                  :DB2-EMAIL    :WRK-EMAIL-NULL,                        00028301
                  :DB2-TELEFONE :WRK-TEL-NULL                           00028301
               FROM FOUR001.FUNC2
               WHERE ID = :DB2-ID                                       00028301
           END-EXEC.
                                                                        00028301
           EVALUATE TRUE
             WHEN DB2-NOME             NOT EQUAL
                  WRK-NOME-AC          AND                              00028301
                  WRK-NOME-AC          NOT EQUAL SPACES                 00028301
           MOVE WRK-NOME-AC            TO DB2-NOME                      00027201

             WHEN DB2-SETOR            NOT EQUAL
                  WRK-SETOR-AC         AND                              00028301
                  WRK-SETOR-AC         NOT EQUAL SPACES
                  MOVE WRK-SETOR-AC    TO DB2-SETOR                     00027201

             WHEN DB2-SALARIO          NOT EQUAL
                  WRK-SALARIO-AC       AND                              00028301
                  WRK-SALARIO-AC       NOT EQUAL ZEROS
                  MOVE WRK-SALARIO-AC  TO DB2-SALARIO                   00027201

             WHEN DB2-DATAADM          NOT EQUAL
                  WRK-DATAADM-AC       AND                              00028301
                  WRK-DATAADM-AC       NOT EQUAL SPACES
                  MOVE WRK-DATAADM-AC  TO DB2-DATAADM                   00027201

             WHEN DB2-EMAIL            NOT EQUAL
                  WRK-EMAIL-AC         AND                              00028301
                  WRK-EMAIL-AC         NOT EQUAL SPACES
                  MOVE WRK-EMAIL-AC    TO DB2-EMAIL                     00027201
                                                                        00028301
             WHEN DB2-TELEFONE         NOT EQUAL
                  WRK-TEL-AC           AND                              00028301
                  WRK-TEL-AC           NOT EQUAL SPACES
                  MOVE WRK-TEL-AC      TO DB2-TELEFONE                  00027201
                                                                        00028301
           END-EVALUATE.                                                00028301


           MOVE WRK-ID-AC              TO DB2-ID.
           MOVE WRK-NOME-AC            TO DB2-NOME.
           MOVE WRK-SETOR-AC           TO DB2-SETOR.
           MOVE WRK-SALARIO-AC         TO DB2-SALARIO.
           MOVE WRK-DATAADM-AC         TO DB2-DATAADM.
           MOVE WRK-EMAIL-AC           TO DB2-EMAIL.

           PERFORM VARYING WRK-EMAIL-LEN        FROM 40
            BY -1 UNTIL WRK-EMAIL-LEN           EQUAL 0


              IF WRK-EMAIL-AC(WRK-EMAIL-LEN:1)   EQUAL SPACES
                 MOVE  WRK-EMAIL-LEN   TO WRK-POSICAO
                    CONTINUE
               END-IF
           END-PERFORM.

           ADD -1                      TO WRK-POSICAO.



           MOVE WRK-POSICAO            TO DB2-EMAIL-LEN.
           MOVE WRK-EMAIL-AC           TO DB2-EMAIL-TEXT.
           MOVE WRK-TEL-AC             TO DB2-TELEFONE.

           EXEC SQL                                                     00028601
             UPDATE FOUR001.FUNC2                                       00028701
             SET NOME    =:DB2-NOME,                                    00028701
                 SETOR   =:DB2-SETOR,                                   00028801
                 SALARIO =:DB2-SALARIO,                                 00028901
                 DATAADM =:DB2-DATAADM,                                 00029001
                 EMAIL   =:DB2-EMAIL,                                   00029101
                 TELEFONE=:DB2-TELEFONE                                 00029101
             WHERE ID = :DB2-ID                                         00029201
           END-EXEC.                                                    00029601
                                                                        00029701
           PERFORM 2100-DISPLAY-DADOS.                                  00029901
                                                                        00030001
      *----------------------------------------------------------------*00030101
       2000-99-FIM.                              EXIT.                  00030201
      *----------------------------------------------------------------*00030301
                                                                        00030401
      *----------------------------------------------------------------*00030501
       2100-DISPLAY-DADOS                        SECTION.               00030601
      *----------------------------------------------------------------*00030701
                                                                        00030801
                                                                        00030901
           EVALUATE SQLCODE                                             00031001
            WHEN 0                                                      00032001
              DISPLAY 'DADOS ALTERADOS'
              DISPLAY 'ID..... ' DB2-ID                                 00032101
              DISPLAY 'NOME... ' DB2-NOME                               00032201
              DISPLAY 'SETOR.. ' DB2-SETOR                              00032301
              DISPLAY 'SALARIO ' DB2-SALARIO                            00032401
              DISPLAY 'DATAADM ' DB2-DATAADM                            00032501
              DISPLAY 'EMAIL.. ' DB2-EMAIL                              00032701
                                                                        00033101
            WHEN -181                                                   00033401
              DISPLAY 'DATA NO FORMATO ERRADO...:' WRK-DATAADM-AC

            WHEN OTHER                                                  00033401
              MOVE SQLCODE             TO WRK-SQLCODE                   00033501
              DISPLAY 'ERRO ... ' WRK-SQLCODE                           00033601
              MOVE '2000 '             TO WRK-SECTION                   00033701
              MOVE 'NA LEITURA '       TO WRK-MSG-ERRO                  00033801
              MOVE WRK-SQLCODE         TO WRK-STATUS                    00033901
                                                                        00034001
              PERFORM 9000-GRAVAR-ERROS                                 00034101
                                                                        00034201
           END-EVALUATE.                                                00034301
                                                                        00034401
                                                                        00034501
      *----------------------------------------------------------------*00034601
       2100-99-FIM.                              EXIT.                  00034701
      *----------------------------------------------------------------*00034801
                                                                        00034901
      *----------------------------------------------------------------*00035001
       3000-FINALIZAR                            SECTION.               00035101
      *----------------------------------------------------------------*00035201
                                                                        00036001
            DISPLAY ' FIM DE PROCESSAMENTO'.                            00036801
                                                                        00036901
      *----------------------------------------------------------------*00037001
       3000-99-FIM.                              EXIT.                  00037101
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