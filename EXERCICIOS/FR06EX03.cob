      *================================================================J00001000
       IDENTIFICATION                  DIVISION.                        00002000
      *================================================================*00003000
                                                                        00004000
       PROGRAM-ID. FR06EX03                                             00005001
                                                                        00006000
      *================================================================*00007000
      *                                                                *00008000
      *     AUTOR.......: JOEI LORENTI                                 *00009000
      *     ANALISTA....: IVAN SANCHES                                 *00009100
      *     EMPRESA.....: FOURSYS                                      *00009200
      *     DATA........: 26/05/2022                                   *00009300
      *----------------------------------------------------------------*00009400
      *     OBJETIVO....: RECEBER DADOS  DOS ARQUIVOS DE ENTRADA       *00009500
      *                   CLI2505 E MOV2505, FAZER A RELACAO (BALANCO) *00009600
      *                   ENTRE AS CHAVES E GRAVAR NOS ARQUIVOS DE     *00009700
      *                   SAIDA MOV2505A, MOV2505C.                    *00009803
      *                                                                *00009900
      *================================================================*00010000
       ENVIRONMENT                     DIVISION.                        00011002
      *================================================================*00012000
                                                                        00013000
      *----------------------------------------------------------------*00014000
       CONFIGURATION                   SECTION.                         00015002
      *----------------------------------------------------------------*00016000
                                                                        00017000
       SPECIAL-NAMES.                                                   00018000
           DECIMAL-POINT IS COMMA.                                      00019000
                                                                        00020000
      *----------------------------------------------------------------*00030000
       INPUT-OUTPUT                    SECTION.                         00040002
      *----------------------------------------------------------------*00050000
                                                                        00060000
       FILE-CONTROL.                                                    00070000
            SELECT CLI2505  ASSIGN     TO CLI2505                       00071002
            FILE STATUS                IS WRK-FS-CLI2505.               00071102
                                                                        00071200
            SELECT MOV2505  ASSIGN     TO MOV2505                       00071302
            FILE STATUS                IS WRK-FS-MOV2505.               00071402
                                                                        00071500
            SELECT MOV2505A  ASSIGN    TO MOV2505A                      00071602
            FILE STATUS                IS WRK-FS-MOV2505A.              00071702
                                                                        00071800
            SELECT MOV2505C  ASSIGN    TO MOV2505C                      00071902
            FILE STATUS                IS WRK-FS-MOV2505C.              00072002
                                                                        00072100
      *================================================================*00072200
       DATA                            DIVISION.                        00072302
      *================================================================*00072400
                                                                        00072500
      *----------------------------------------------------------------*00072600
       FILE                            SECTION.                         00072702
      *----------------------------------------------------------------*00072800
                                                                        00072902
      *----------------------------------------------------------------*00073002
      *   INPUT - DADOS DO ARQUIVO DE ENTRADA (CLI2505)                *00073102
      *                                    LRECL = 46                  *00073202
      *----------------------------------------------------------------*00073302
                                                                        00073402
       FD   CLI2505                                                     00073502
            RECORDING MODE IS F                                         00073602
            LABEL RECORD IS STANDARD                                    00073702
            BLOCK CONTAINS 0 RECORDS.                                   00073802
                                                                        00073902
                                                                        00074102
       01 FD-CLI2505.                                                   00074202
          05 FD-CLI2505-CHAVE.                                          00074302
             10 FD-CLI2505-AGENCIA     PIC X(04).                       00074402
             10 FD-CLI2505-CONTA       PIC X(04).                       00074502
          05 FD-CLI2505-NOME           PIC X(30).                       00074602
          05 FD-CLI2505-SALDO          PIC 9(08).                       00074702
                                                                        00074802
      *----------------------------------------------------------------*00074902
      *   INPUT - DADOS DO ARQUIVO DE ENTRADA (MOV2505)                *00075002
      *                                    LRECL = 47                  *00075102
      *----------------------------------------------------------------*00075202
                                                                        00075302
                                                                        00075402
       FD   MOV2505                                                     00075502
            RECORDING MODE IS F                                         00075602
            LABEL RECORD IS STANDARD                                    00075702
            BLOCK CONTAINS 0 RECORDS.                                   00075802
                                                                        00076102
       01 FD-MOV2505.                                                   00076202
          05 FD-MOV2505-CHAVE.                                          00076302
             10 FD-MOV2505-AGENCIA     PIC X(04).                       00076402
             10 FD-MOV2505-CONTA       PIC X(04).                       00076502
          05 FD-MOVIMENTO              PIC X(30).                       00076602
          05 FD-VALORMOV               PIC 9(08).                       00076702
          05 FD-TIPOMOV                PIC X(01).                       00076802
                                                                        00076902
      *----------------------------------------------------------------*00077002
      *   INPUT - DADOS DO ARQUIVO DE SAIDA (MOV2505A)                 *00077102
      *                                    LRECL = 46                  *00077202
      *----------------------------------------------------------------*00077302
                                                                        00077402
       FD   MOV2505A                                                    00077502
            RECORDING MODE IS F                                         00077602
            LABEL RECORD IS STANDARD                                    00077702
            BLOCK CONTAINS 0 RECORDS.                                   00077802
                                                                        00077902
                                                                        00078102
       01 FD-MOV2505A                  PIC X(46).                       00078202
                                                                        00078300
      *----------------------------------------------------------------*00078402
      *   INPUT - DADOS DO ARQUIVO DE SAIDA (MOV2505C)                 *00078502
      *                                    LRECL = 46                  *00078602
      *----------------------------------------------------------------*00078702
                                                                        00078802
       FD  MOV2505C                                                     00078900
           RECORDING MODE IS F                                          00079000
           LABEL RECORD IS STANDARD                                     00079100
           BLOCK CONTAINS 0 RECORDS.                                    00079200
                                                                        00079300
                                                                        00079500
       01 FD-MOV2505C                  PIC X(46).                       00079602
                                                                        00079700
      *----------------------------------------------------------------J00079800
       WORKING-STORAGE                 SECTION.                         00079902
      *----------------------------------------------------------------*00080000
                                                                        00080100
      *----------------------------------------------------------------*00080202
       01 FILLER                       PIC X(050)  VALUE                00080302
           '*** INICIO DA WORKING FR06EX03 ***'.                        00080402
      *----------------------------------------------------------------*00080502
                                                                        00080602
      *----------------------------------------------------------------*00080702
       01 FILLER                       PIC X(050)  VALUE                00080802
           '*** AREA DO ARQUIVO LOGERROS ***'.                          00080902
      *----------------------------------------------------------------*00081002
                                                                        00081102
                                                                        00081302
       COPY '#GRVLOG'.                                                  00081400
                                                                        00081500
                                                                        00081600
       77 WRK-GRV                      PIC X(06) VALUE 'GRVLOG'.        00081702
                                                                        00081800
                                                                        00081902
      *----------------------------------------------------------------*00082002
       01 FILLER                       PIC X(050)  VALUE                00082102
           '*** AREA DE VARIAVEIS DE FILE-STATUS ***'.                  00082202
      *----------------------------------------------------------------*00082302
                                                                        00082700
       77 WRK-FS-CLI2505               PIC 9(02) VALUE ZEROS.           00082802
       77 WRK-FS-MOV2505               PIC 9(02) VALUE ZEROS.           00082902
       77 WRK-FS-MOV2505A              PIC 9(02) VALUE ZEROS.           00083002
       77 WRK-FS-MOV2505C              PIC 9(02) VALUE ZEROS.           00083102
                                                                        00083200
      *----------------------------------------------------------------*00083302
       01 FILLER                       PIC X(050)  VALUE                00083402
           '*** AREA DE ACUMULADORES ***'.                              00083502
      *----------------------------------------------------------------*00083602
                                                                        00084000
       77 WRK-ACU-LIDOS-CLI            PIC 9(02) VALUE ZEROS.           00084102
       77 WRK-ACU-LIDOS-MOV            PIC 9(02) VALUE ZEROS.           00084202
       77 WRK-ACU-GRAV-MOV-A           PIC 9(02) VALUE ZEROS.           00084302
       77 WRK-ACU-GRAV-MOV-C           PIC 9(02) VALUE ZEROS.           00084402
                                                                        00084500
      *================================================================*00084600
       PROCEDURE                       DIVISION.                        00084702
      *================================================================*00084800
                                                                        00084902
      ******************************************************************00085002
      *                    ROTINA PRINCIPAL                            *00085102
      ******************************************************************00085202
                                                                        00085302
      *----------------------------------------------------------------*00085400
       0000-PRINCIPAL                  SECTION.                         00085502
      *----------------------------------------------------------------*00085600
                                                                        00085700
            PERFORM 1000-INICIAR.                                       00085802
                                                                        00085900
            PERFORM 1100-VERIFICAR-VAZIO.                               00086002
                                                                        00086100
            PERFORM 2000-PROCESSAR     UNTIL WRK-FS-CLI2505 EQUAL 10    00086202
                                       AND WRK-FS-MOV2505   EQUAL 10.   00086302
                                                                        00086400
                                                                        00086500
            PERFORM 3000-FINALIZAR.                                     00086602
                                                                        00086700
            STOP RUN.                                                   00086802
                                                                        00086900
      *----------------------------------------------------------------*00087000
       0000-99-FIM.                    EXIT.                            00087100
      *----------------------------------------------------------------*00087200
                                                                        00087300
      ******************************************************************00087402
      *                    PROCEDIMENTOS INICIAIS                      *00087502
      ******************************************************************00087602
                                                                        00087702
      *----------------------------------------------------------------*00087800
       1000-INICIAR                    SECTION.                         00087900
      *----------------------------------------------------------------*00088000
                                                                        00088102
            OPEN INPUT  CLI2505                                         00088202
                        MOV2505                                         00088302
            OPEN OUTPUT MOV2505A                                        00088402
                        MOV2505C.                                       00088502
                                                                        00088602
            PERFORM 4000-TESTE-FS.                                      00088702
                                                                        00088800
      *----------------------------------------------------------------*00088900
       1000-99-FIM.                    EXIT.                            00089000
      *----------------------------------------------------------------*00089100
                                                                        00089200
      ******************************************************************00089302
      *                    VERIFICAR VAZIO                             *00089402
      ******************************************************************00089502
                                                                        00089602
      *----------------------------------------------------------------*00089700
       1100-VERIFICAR-VAZIO            SECTION.                         00089800
      *----------------------------------------------------------------*00089900
                                                                        00090000
            READ CLI2505.                                               00090102
            READ MOV2505.                                               00090202
                                                                        00090302
            ADD 1                      TO WRK-ACU-LIDOS-CLI.            00090402
            ADD 1                      TO WRK-ACU-LIDOS-MOV.            00090502
                                                                        00090602
      *----------------------------------------------------------------*00090702
       1100-99-FIM.                    EXIT.                            00090802
      *----------------------------------------------------------------*00090902
                                                                        00091002
      ******************************************************************00091102
      *                    PROCESSAMENTO PRINCIPAL                     *00091202
      ******************************************************************00091302
                                                                        00091402
      *----------------------------------------------------------------*00091502
       2000-PROCESSAR                  SECTION.                         00091602
      *----------------------------------------------------------------*00091702
                                                                        00091802
            EVALUATE TRUE                                               00091902
                                                                        00092002
                WHEN FD-CLI2505-CHAVE  LESS FD-MOV2505-CHAVE            00092102
                    IF FD-CLI2505-SALDO                                 00092202
                                       GREATER THAN OR                  00092302
                                       EQUAL 1000000                    00092402
                        PERFORM 2300-GRAVAR-MOV2505A                    00092502
                      ELSE                                              00092602
                        PERFORM 2400-GRAVAR-MOV2505C                    00092702
                    END-IF                                              00092802
                                                                        00092902
                    PERFORM 2200-LER-CLI2505                            00093002
                                                                        00093102
                WHEN FD-CLI2505-CHAVE EQUAL FD-MOV2505-CHAVE            00093202
                    PERFORM 2500-ATUALIZA-SALDO                         00093302
                    PERFORM 2100-LER-MOV2505                            00093402
                                                                        00093502
                WHEN OTHER                                              00093602
                    DISPLAY 'CHAVE ERRADA'                              00093702
                    PERFORM 2100-LER-MOV2505                            00093802
                                                                        00093902
                END-EVALUATE.                                           00094002
                                                                        00094102
      *----------------------------------------------------------------*00094202
       2000-99-FIM.                    EXIT.                            00094302
      *----------------------------------------------------------------*00094402
                                                                        00094502
      ******************************************************************00094602
      *      LEITURA DO ARQUIVO DE ENTRADA - MOV2505                   *00094702
      ******************************************************************00094802
                                                                        00094902
      *----------------------------------------------------------------*00095002
       2100-LER-MOV2505                SECTION.                         00095102
      *----------------------------------------------------------------*00095202
                 READ MOV2505                                           00095302
                  IF WRK-FS-MOV2505 EQUAL 10                            00095402
                   MOVE HIGH-VALUES TO FD-MOV2505-CHAVE                 00095502
                  END-IF.                                               00095602
            ADD 1                        TO WRK-ACU-LIDOS-MOV.          00095702
      *----------------------------------------------------------------*00095802
       2100-99-FIM.                    EXIT.                            00095902
      *----------------------------------------------------------------*00096002
                                                                        00096102
      ******************************************************************00096202
      *      LEITURA DO ARQUIVO DE ENTRADA - CLI2505                   *00096302
      ******************************************************************00096402
                                                                        00096502
      *----------------------------------------------------------------*00096602
       2200-LER-CLI2505                SECTION.                         00096702
      *----------------------------------------------------------------*00096802
                 READ CLI2505.                                          00096902
            ADD 1                        TO WRK-ACU-LIDOS-CLI.          00097002
                                                                        00097102
      *----------------------------------------------------------------*00097202
       2200-99-FIM.                    EXIT.                            00097302
      *----------------------------------------------------------------*00097402
      *----------------------------------------------------------------*00097502
       2300-GRAVAR-MOV2505A            SECTION.                         00097602
      *----------------------------------------------------------------*00097702
                                                                        00097802
                WRITE FD-MOV2505A     FROM FD-CLI2505.                  00097902
            ADD 1                        TO WRK-ACU-GRAV-MOV-A.         00098002
                                                                        00098102
      *----------------------------------------------------------------*00098202
       2300-99-FIM.                    EXIT.                            00098302
      *----------------------------------------------------------------*00098402
                                                                        00098502
      *----------------------------------------------------------------*00098602
       2400-GRAVAR-MOV2505C            SECTION.                         00098702
      *----------------------------------------------------------------*00098802
                                                                        00098902
                WRITE FD-MOV2505C FROM FD-CLI2505.                      00099002
            ADD 1                        TO WRK-ACU-GRAV-MOV-C.         00099102
                                                                        00099202
      *----------------------------------------------------------------*00099302
       2400-99-FIM.                    EXIT.                            00099402
      *----------------------------------------------------------------*00099502
                                                                        00099602
      *----------------------------------------------------------------*00099702
       2500-ATUALIZA-SALDO             SECTION.                         00099802
      *----------------------------------------------------------------*00099902
                                                                        00100002
            IF FD-TIPOMOV EQUAL 'C'                                     00100102
               ADD FD-VALORMOV TO FD-CLI2505-SALDO                      00100202
            ELSE                                                        00100302
              IF FD-TIPOMOV EQUAL 'D'                                   00100402
               SUBTRACT FD-VALORMOV FROM FD-CLI2505-SALDO               00100502
              ELSE                                                      00100602
               DISPLAY FD-CLI2505-CHAVE 'SEM SALDO'                     00100702
              END-IF                                                    00100802
            END-IF.                                                     00100902
                                                                        00101002
      *----------------------------------------------------------------*00101102
       2500-99-FIM.                    EXIT.                            00101202
      *----------------------------------------------------------------*00101302
                                                                        00101402
      *----------------------------------------------------------------*00101502
       3000-FINALIZAR                  SECTION.                         00101602
      *----------------------------------------------------------------*00101702
                                                                        00101802
            IF                                                          00101902
              WRK-ACU-LIDOS-CLI GREATER ZEROS                           00102002
              AND  WRK-ACU-LIDOS-MOV GREATER ZEROS                      00102102
                PERFORM 5000-TOTAIS                                     00102202
            ELSE                                                        00102302
                DISPLAY 'ERRO NO PROCESSAMENTO'                         00102402
            END-IF                                                      00102502
                                                                        00102602
            CLOSE CLI2505                                               00102702
                  MOV2505                                               00102802
                  MOV2505A                                              00102902
                  MOV2505C.                                             00103002
                                                                        00103102
      *----------------------------------------------------------------*00103202
       3000-99-FIM.                    EXIT.                            00103302
      *----------------------------------------------------------------*00103402
                                                                        00103502
      *----------------------------------------------------------------*00103602
       4000-TESTE-FS                   SECTION.                         00103702
      *----------------------------------------------------------------*00103802
                                                                        00103902
           PERFORM 4100-TESTE-FS-CLI2505.                               00104002
                                                                        00104102
           PERFORM 4200-TESTE-FS-MOV2505.                               00104202
                                                                        00104302
           PERFORM 4300-TESTE-FS-MOV2505A.                              00104402
                                                                        00104502
           PERFORM 4400-TESTE-FS-MOV2505C.                              00104602
                                                                        00104702
      *----------------------------------------------------------------*00104802
       4000-99-FIM.                    EXIT.                            00104902
      *----------------------------------------------------------------*00105002
                                                                        00105102
      *----------------------------------------------------------------*00105202
       4100-TESTE-FS-CLI2505           SECTION.                         00105302
      *----------------------------------------------------------------*00105402
                                                                        00105502
           IF WRK-FS-CLI2505           NOT EQUAL ZERO                   00105602
               MOVE 'FR06EX03'         TO WRK-PROGRAMA                  00105702
               MOVE 'ERRO NO OPEN CLI2505 '                             00105802
                                       TO WRK-MSG-ERRO                  00105902
               MOVE '4100'             TO WRK-SECTION                   00106003
               MOVE WRK-FS-CLI2505     TO WRK-STATUS                    00106102
                 PERFORM 9000-TRATAR-ERRO                               00106202
           END-IF.                                                      00106302
                                                                        00106402
      *----------------------------------------------------------------*00106502
       4100-99-FIM.                    EXIT.                            00106602
      *----------------------------------------------------------------*00106702
      *----------------------------------------------------------------*00106802
       4200-TESTE-FS-MOV2505           SECTION.                         00106902
      *----------------------------------------------------------------*00107002
                                                                        00107102
           IF WRK-FS-MOV2505           NOT EQUAL ZEROS                  00107202
               MOVE 'FR06EX03'         TO WRK-PROGRAMA                  00107302
               MOVE 'ERRO NO OPEN MOV2505 '                             00107402
                                       TO WRK-MSG-ERRO                  00107502
               MOVE '4200'             TO WRK-SECTION                   00107603
               MOVE WRK-FS-MOV2505     TO WRK-STATUS                    00107702
                 PERFORM 9000-TRATAR-ERRO                               00107802
           END-IF.                                                      00107902
                                                                        00108002
      *----------------------------------------------------------------*00108102
       4200-99-FIM.                    EXIT.                            00108202
      *----------------------------------------------------------------*00108302
      *----------------------------------------------------------------*00108402
       4300-TESTE-FS-MOV2505A          SECTION.                         00108502
      *----------------------------------------------------------------*00108602
                                                                        00108702
           IF WRK-FS-MOV2505A          NOT EQUAL ZERO                   00108802
               MOVE 'FR06EX03'         TO WRK-PROGRAMA                  00108902
               MOVE 'ERRO NO OPEN MOV2505A'                             00109002
                                       TO WRK-MSG-ERRO                  00109102
               MOVE '4300'             TO WRK-SECTION                   00109203
               MOVE WRK-FS-MOV2505A    TO WRK-STATUS                    00109302
                 PERFORM 9000-TRATAR-ERRO                               00109402
           END-IF.                                                      00109502
                                                                        00109602
      *----------------------------------------------------------------*00109702
       4300-99-FIM.                    EXIT.                            00109802
      *----------------------------------------------------------------*00109902
      *----------------------------------------------------------------*00110002
       4400-TESTE-FS-MOV2505C          SECTION.                         00110102
      *----------------------------------------------------------------*00110202
                                                                        00110302
           IF WRK-FS-MOV2505C          NOT EQUAL ZERO                   00110402
               MOVE 'FR06EX03'         TO WRK-PROGRAMA                  00110502
               MOVE 'ERRO NO OPEN MOV2505'                              00110602
                                       TO WRK-MSG-ERRO                  00110702
               MOVE '4400'             TO WRK-SECTION                   00110803
               MOVE WRK-FS-MOV2505C    TO WRK-STATUS                    00110902
                 PERFORM 9000-TRATAR-ERRO                               00111002
           END-IF.                                                      00111102
                                                                        00111202
      *----------------------------------------------------------------*00111302
       4400-99-FIM.                    EXIT.                            00111402
      *----------------------------------------------------------------*00111502
      *----------------------------------------------------------------*00111602
       5000-TOTAIS                     SECTION.                         00111702
      *----------------------------------------------------------------*00111802
                                                                        00111902
            DISPLAY 'TOTAL LIDOS CLI2505---------->' WRK-ACU-LIDOS-CLI  00112002
            DISPLAY 'TOTAL LIDOS MOV2505---------->' WRK-ACU-LIDOS-MOV  00112102
            DISPLAY 'TOTAL GRAVADOS MOV2505A------>' WRK-ACU-GRAV-MOV-A 00112202
            DISPLAY 'TOTAL GRAVADOS MOV2505C------>' WRK-ACU-GRAV-MOV-C.00112302
                                                                        00112502
      *----------------------------------------------------------------*00112602
       5000-99-FIM.                    EXIT.                            00112702
      *----------------------------------------------------------------*00112802
      *----------------------------------------------------------------*00112902
       9000-TRATAR-ERRO                SECTION.                         00113002
      *----------------------------------------------------------------*00113102
                                                                        00113202
           CALL WRK-GRV                USING WRK-LOG                    00113302
           GOBACK.                                                      00113402
                                                                        00113503
      *----------------------------------------------------------------*00113602
       9000-99-FIM.                    EXIT.                            00114000
      *----------------------------------------------------------------*00120000
