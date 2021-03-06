      *================================================================J00001001
       IDENTIFICATION                            DIVISION.              00002001
      *================================================================*00003001
                                                                        00004001
       PROGRAM-ID. FR06DB03.                                            00005001
                                                                        00006001
      *================================================================*00007001
      *   AUTOR...........: JOEI LORENTI                               *00008001
      *   ANALISTA........: IVAN SANCHES                               *00008101
      *   DATA ...........: 03/06/2022                                 *00009001
      *----------------------------------------------------------------*00009101
      *   OBJETIVO........: INSERE DADOS NA TABELA (CFUNC)A PARTIR DA  *00010001
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
       FILE-CONTROL.                                                    00015801
           SELECT LOGERROS ASSIGN          TO LOGERROS                  00015801
           FILE STATUS                     IS WRK-FS-LOG.               00015801
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
      * INPUT - DADOS DO ARQUIVO DE GRAVACAO DE ERROS (LOGERROS)       *00015801
      *                                               LRECL = 58       *00015801
      *----------------------------------------------------------------*00015801

       FD  LOGERROS                                                     00015801
           RECORDING MODE IS F                                          00015801
           LABEL RECORD IS STANDARD                                     00015801
           BLOCK CONTAINS 0 RECORDS.                                    00015801
                                                                        00015801
                                                                        00015801
       01 FD-LOG                        PIC X(58).                      00015801
                                                                        00015801
      *----------------------------------------------------------------*00018001
       WORKING-STORAGE                           SECTION.               00019001
      *----------------------------------------------------------------*00020001
                                                                        00020101
      *----------------------------------------------------------------*00020201
       01 FILLER                      PIC  X(050)         VALUE         00020301
           '***  FR06DB01 - INICIO DA AREA DE WORKING   ***'.           00020401
      *----------------------------------------------------------------*00020501
                                                                        00020601
      *----------------------------------------------------------------*00020701
       01 FILLER                       PIC  X(050)         VALUE        00020801
           '***  VARIAVEIS DE NULIDADE  ***'.                           00020901
      *----------------------------------------------------------------*00021001
                                                                        00021101
       77 WRK-INDICATOR                PIC S9(4) COMP VALUE ZEROS.      00021201
                                                                        00021301
      *----------------------------------------------------------------*00021501
       01 FILLER                       PIC  X(050)         VALUE        00021601
           '***  AREA DE TRATAMENTO DE ERROS DB2 ***'.                  00021701
      *----------------------------------------------------------------*00021801
                                                                        00021901
       COPY '#GRVLOG'.                                                  00022001
                                                                        00022101
       77 WRK-GRV                      PIC X(06) VALUE 'GRVLOG'.        00022101
                                                                        00022101
                                                                        00022101
       77 WRK-SQLCODE                  PIC -999.                        00022201
       77 WRK-FS-LOG                   PIC 9(02).                       00022301
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
          05 WRK-SALARIO-AC            PIC 9(08)V99.
                                                                        00022401
       01 WRK-DATAADM.
          05 FILLER                    PIC X(10).
          05 WRK-DATAADM-AC            PIC X(10).
                                                                        00022401
       01 WRK-EMAIL.
          05 FILLER                    PIC X(10).
          05 WRK-EMAIL-AC              PIC X(40).

      *----------------------------------------------------------------*00022501
       01 FILLER                       PIC  X(050)         VALUE        00022601
           '*** AREA DB2 ***'.                                          00022701
      *----------------------------------------------------------------*00022801
                                                                        00022901
           EXEC SQL                                                     00023401
           INCLUDE BOOKFUNC                                             00023501
           END-EXEC.                                                    00023601
                                                                        00023301
           EXEC SQL                                                     00023001
              INCLUDE SQLCA                                             00023101
           END-EXEC.                                                    00023201
                                                                        00023701
      *----------------------------------------------------------------*00023801
       01 FILLER                      PIC  X(050)         VALUE         00023901
           '*** AREA DB2 ***'.                                          00024001
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
                PERFORM 1000-INICIAR                                    00025501
                                                                        00025601
                PERFORM 2000-PROCESSAR                                  00025701
                                                                        00025801
                PERFORM 3000-FINALIZAR.                                 00025901
                                                                        00026001
                STOP RUN.                                               00026201
                                                                        00026301
      *----------------------------------------------------------------*00026401
       0000-99-FIM.                    EXIT.                            00026501
      *----------------------------------------------------------------*00026601
                                                                        00026701
      *----------------------------------------------------------------*00026801
       1000-INICIAR                    SECTION.                         00026901
      *----------------------------------------------------------------*00027001
                                                                        00027101
           ACCEPT WRK-ID               FROM SYSIN.                      00027201
           ACCEPT WRK-NOME             FROM SYSIN.                      00027201
           ACCEPT WRK-SETOR            FROM SYSIN.                      00027201
           ACCEPT WRK-SALARIO          FROM SYSIN.                      00027201
           ACCEPT WRK-DATAADM          FROM SYSIN.                      00027201
           ACCEPT WRK-EMAIL            FROM SYSIN.                      00027201
                                                                        00027301
      *----------------------------------------------------------------*00027601
       1000-99-FIM.                    EXIT.                            00027701
      *----------------------------------------------------------------*00027801
                                                                        00027901
      *----------------------------------------------------------------*00028001
       2000-PROCESSAR                  SECTION.                         00028101
      *----------------------------------------------------------------*00028201
                                                                        00028301
           MOVE WRK-ID-AC              TO DB2-ID.                       00027201
           MOVE WRK-NOME-AC            TO DB2-NOME.                     00027201
           MOVE WRK-SETOR-AC           TO DB2-SETOR.                    00027201
           MOVE WRK-SALARIO-AC         TO DB2-SALARIO.                  00027201
           MOVE WRK-DATAADM-AC         TO DB2-DATAADM.                  00027201
           MOVE WRK-EMAIL-AC           TO DB2-EMAIL.                    00027201
                                                                        00027301
                                                                        00028501
           EXEC SQL                                                     00028601
             INSERT INTO                                                00028701
             FOUR001.FUNC (ID,NOME,SETOR,SALARIO,DATAADM,EMAIL)         00028701
              VALUES( :DB2-ID,                                          00028801
                      :DB2-NOME,                                        00028901
                      :DB2-SETOR,                                       00029001
                      :DB2-SALARIO,                                     00029101
                      :DB2-DATAADM,                                     00029201
                      :DB2-EMAIL)                                       00029301
           END-EXEC.                                                    00029601
                                                                        00029701
           PERFORM 2100-DISPLAY-DADOS.                                  00029901
                                                                        00030001
      *----------------------------------------------------------------*00030101
       2000-99-FIM.                    EXIT.                            00030201
      *----------------------------------------------------------------*00030301
                                                                        00030401
      *----------------------------------------------------------------*00030501
       2100-DISPLAY-DADOS              SECTION.                         00030601
      *----------------------------------------------------------------*00030701
                                                                        00030801
                                                                        00030901
           EVALUATE SQLCODE                                             00031001
            WHEN 0                                                      00032001
              DISPLAY 'DADOS GRAVADOS'
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
              MOVE '2000 '             TO WRK-MSG-ERRO                  00033701
              MOVE 'NA LEITURA '       TO WRK-MSG-ERRO                  00033801
              MOVE WRK-SQLCODE         TO WRK-STATUS                    00033901
                                                                        00034001
              PERFORM 9000-GRAVAR-ERROS                                 00034101
                                                                        00034201
           END-EVALUATE.                                                00034301
                                                                        00034401
                                                                        00034501
      *----------------------------------------------------------------*00034601
       2100-99-FIM.                    EXIT.                            00034701
      *----------------------------------------------------------------*00034801
                                                                        00034901
      *----------------------------------------------------------------*00035001
       3000-FINALIZAR                  SECTION.                         00035101
      *----------------------------------------------------------------*00035201
                                                                        00036001
            DISPLAY ' FIM DE PROCESSAMENTO'.                            00036801
                                                                        00036901
      *----------------------------------------------------------------*00037001
       3000-99-FIM.                    EXIT.                            00037101
      *----------------------------------------------------------------*00037201
                                                                        00037301
      *----------------------------------------------------------------*00037401
       9000-GRAVAR-ERROS               SECTION.                         00037501
      *----------------------------------------------------------------*00037601
                                                                        00037701
            CALL WRK-GRV             USING WRK-LOG                      00037801
                                                                        00038401
            GOBACK.                                                     00038501
                                                                        00038601
      *----------------------------------------------------------------*00038701
       9000-99-FIM.                    EXIT.                            00038801
      *----------------------------------------------------------------*00038901
                                                                        00039001
                                                                        00039101
                                                                        00039201
                                                                        00039301
                                                                        00039401
                                                                        00039501
                                                                        00039601
                                                                        00039701
                                                                        00039801
                                                                        00039901
                                                                        00040001
                                                                        00041001
                                                                        00050001
