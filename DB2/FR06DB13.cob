      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR06DB13.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR...........: JOEI LORENTI                            *   00008000
      *   ANALISTA........: IVAN SANCHES                            *   00008100
      *   DATA ...........: 13/06/2022                              *   00009000
      *-------------------------------------------------------------*   00009100
      *   OBJETIVO........: SYNPOINT - RESTART                      *   00010000
      *                     SELECT DA TABELA FUNC COM SYNCPOINT EM  *   00010100
      *                     REGISTRO COM CAMPO SALARIO ZERADO    *      00010200
      *=============================================================*   00011000
                                                                        00012000
      *=============================================================*   00013000
       ENVIRONMENT                               DIVISION.              00014000
      *=============================================================*   00015000
                                                                        00015100
      *-------------------------------------------------------------*   00015200
       CONFIGURATION                               SECTION.             00015300
      *-------------------------------------------------------------*   00015400
       SPECIAL-NAMES.                                                   00015500
           DECIMAL-POINT IS COMMA.                                      00015600
                                                                        00015700
      *=============================================================*   00015800
       DATA                                      DIVISION.              00015900
      *=============================================================*   00016000
                                                                        00017000
      *-------------------------------------------------------------*   00018000
       WORKING-STORAGE                             SECTION.             00019000
      *-------------------------------------------------------------*   00020000
           EXEC SQL                                                     00020100
             INCLUDE #BKFUNC2                                           00020200
           END-EXEC.                                                    00020300
                                                                        00020400
                                                                        00020500
           EXEC SQL                                                     00020600
             INCLUDE #BKCHECK                                           00020700
           END-EXEC.                                                    00020800
                                                                        00020900
           EXEC SQL                                                     00021000
              INCLUDE SQLCA                                             00021100
           END-EXEC.                                                    00021200
                                                                        00021300
           EXEC SQL                                                     00021400
              DECLARE CFUNC CURSOR FOR                                  00021500
               SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL               00021600
                FROM FOUR001.FUNC2  WHERE ID >=                         00021700
                  (SELECT REGISTRO FROM FOUR001.CHECK                   00021800
                      WHERE IDCHECK = 'FOUR006' )                       00021900
               ORDER BY ID                                              00022000
                                                                        00022100
           END-EXEC.                                                    00022200
                                                                        00022300
                                                                        00023100
      *----------------------------------------------------------------*00023201
       01 FILLER                   PIC X(50)              VALUE         00023301
             '**** VARIAVEIS AUXILIARES ****'.                          00023401
      *----------------------------------------------------------------*00023501
       77 WRK-ID         PIC 9(04).                                     00023701
       77 WRK-SQLCODE    PIC -999.                                      00023801
       77 WRK-INDICATOR  PIC S9(4) COMP VALUE ZEROS.                    00023901
       77 WRK-CHECKPOINT PIC 9(2)  VALUE ZEROS.                         00024001
       77 WRK-CONTAREG   PIC 9(3)  VALUE ZEROS.                         00024101
       77 WRK-REGATUAL   PIC 9(3)  VALUE ZEROS.                         00024201
                                                                        00024301
      *=============================================================*   00024401
       PROCEDURE DIVISION.                                              00024501
      *=============================================================*   00024601
       0000-PRINCIPAL                                        SECTION.   00024701
           PERFORM 1000-INICIAR.                                        00024801
           PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100.              00024901
                                                                        00025001
                                                                        00025101
           PERFORM 3000-FINALIZAR.                                      00025201
           STOP RUN.                                                    00025301
       0000-99-FIM.          EXIT.                                      00025401
                                                                        00025501
       1000-INICIAR                                          SECTION.   00025601
            EXEC SQL                                                    00025701
               OPEN CFUNC                                               00025801
            END-EXEC.                                                   00025901
             EVALUATE SQLCODE                                           00026001
              WHEN 0                                                    00026101
                PERFORM 4000-LER-FUNCIONARIO                            00026201
                                                                        00026301
                                                                        00026401
                                                                        00026501
              WHEN 100                                                  00026601
                DISPLAY 'SEM FUNCIONARIOS'                              00026701
              WHEN OTHER                                                00026801
                MOVE SQLCODE TO WRK-SQLCODE                             00026901
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN CURSOR'           00027001
      *          MOVE 200 TO RETURN-CODE                                00027101
                 GOBACK                                                 00027201
              END-EVALUATE.                                             00027301
       1000-99-FIM.          EXIT.                                      00027401
                                                                        00027501
       2000-PROCESSAR                                        SECTION.   00027601
              DISPLAY '--------------------'.                           00027701
              DISPLAY 'ID..... ' DB2-ID                                 00027801
              DISPLAY 'NOME... ' DB2-NOME                               00027901
              DISPLAY 'SETOR.. ' DB2-SETOR                              00028001
              DISPLAY 'SALARIO ' DB2-SALARIO                            00028101
              DISPLAY 'DATAADM ' DB2-DATAADM                            00028201
               IF WRK-INDICATOR = 0                                     00028301
                 DISPLAY 'EMAIL.. ' DB2-EMAIL                           00028401
               ELSE                                                     00028501
                DISPLAY '-- SEM EMAIL '                                 00028601
               END-IF                                                   00028701
                                                                        00028801
      *         IF WRK-CONTAREG > 5                                     00028901
      *           EXEC SQL                                              00029001
      *              COMMIT                                             00029101
      *           END-EXEC                                              00029201
      *            MOVE 0 TO WRK-CONTAREG                               00029301
      *         END-IF                                                  00029401
                                                                        00029501
      *       IF DB2-SALARIO IS NOT NUMERIC OR DB2-SALARIO EQUAL ZEROS  00030000
                                                                        00030100
              IF DB2-SALARIO IS NOT NUMERIC                             00030201
                         OR DB2-SALARIO EQUAL 66666,00                  00030301
                  PERFORM 2100-SET-CHECK-ID                             00030401
              ELSE                                                      00031101
                  PERFORM 2200-SET-ZERO-CHECK                           00031201
              END-IF.                                                   00031301
                                                                        00031401
               PERFORM 4000-LER-FUNCIONARIO.                            00031501
                                                                        00031601
       2000-99-FIM.          EXIT.                                      00031701
                                                                        00031801
       2100-SET-CHECK-ID  SECTION.                                      00031901
                                                                        00032001
                                                                        00032101
                 EXEC SQL                                               00032201
                    UPDATE FOUR001.CHECK SET REGISTRO = :DB2-ID         00032301
                     WHERE IDCHECK = 'FOUR006'                          00032401
                 END-EXEC                                               00032501
                   PERFORM 3000-FINALIZAR                               00032601
                    GOBACK                                              00032701
                                                                        00033301
       2100-99-FIM.                EXIT.                                00033401
                                                                        00033501
       2200-SET-ZERO-CHECK                                              00033601
                                                                        00033701
                 EXEC SQL                                               00033801
                    UPDATE FOUR001.CHECK SET REGISTRO = 0               00033901
                     WHERE IDCHECK = 'FOUR006'                          00034001
                 END-EXEC                                               00034101
                                                                        00034201
                   PERFORM 3000-FINALIZAR                               00034301
                    GOBACK                                              00034501
                                                                        00034601
                                                                        00035101
                                                                        00035201
       2200-99-FIM.                EXIT.                                00035301
       3000-FINALIZAR                                        SECTION.   00035401
                                                                        00035501
           EXEC SQL                                                     00035601
             CLOSE CFUNC                                                00035701
           END-EXEC.                                                    00035801
                                                                        00035901
                                                                        00036001
                                                                        00036101
                                                                        00036201
       3000-99-FIM.          EXIT.                                      00036301
                                                                        00036401
                                                                        00036501
                                                                        00036601
       4000-LER-FUNCIONARIO                                  SECTION.   00036701
           EXEC SQL                                                     00036801
             FETCH CFUNC                                                00036901
              INTO :DB2-ID,                                             00037001
                   :DB2-NOME,                                           00037101
                   :DB2-SETOR,                                          00037201
                   :DB2-SALARIO,                                        00037301
                   :DB2-DATAADM,                                        00037401
                   :DB2-EMAIL     :WRK-INDICATOR                        00037501
           END-EXEC.                                                    00037601
                                                                        00037701
                                                                        00037801
                                                                        00037901
           EVALUATE SQLCODE                                             00038001
            WHEN 0                                                      00038101
                ADD 1 TO WRK-CONTAREG                                   00038201
                ADD 1 TO WRK-REGATUAL                                   00038301
                CONTINUE                                                00038401
            WHEN 100                                                    00038501
              DISPLAY ' FINAL DA TABELA '                               00038601
                                                                        00038701
            WHEN OTHER                                                  00038801
              MOVE SQLCODE TO WRK-SQLCODE                               00038901
              DISPLAY 'ERRO ... ' WRK-SQLCODE                           00039001
           END-EVALUATE.                                                00039101
                                                                        00039201
       4000-99-FIM.          EXIT.                                      00040001
