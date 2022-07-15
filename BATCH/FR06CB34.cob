      *================================================================*00001000
       IDENTIFICATION                  DIVISION.                        00002000
      *================================================================J00003045
                                                                        00004000
       PROGRAM-ID.  FR06CB34.                                           00005000
                                                                        00006000
      *================================================================*00007000
      *                                                                 00007100
      *     AUTOR.......: JOEI LORENTI                                  00007200
      *     ANALISTA....: IVAN SANCHES                                  00007300
      *     EMPRESA.....: FOURSYS                                       00007400
      *     DATA........: 09/05/2022                                    00007500
      *                                                                 00007600
      *----------------------------------------------------------------*00007700
      *                                                                 00007800
      *     OBJETIVO: 1-LER TODOS OS REGISTROS DO ARQUIVO SEQUENCIAL    00007901
      *                 (ARQCLI).                                       00008001
      *               2-GERAR UM ARQUIVO RELATORIO COM TODOS OS         00008101
      *                 REGISTROS (RELATCLI).                           00008201
      *               2-SOMAR TODOS OS SALDOS E MOSTRAR RESUMO NA       00008301
      *                 SYSOUT.                                         00008401
      *               3-A CADA 05 REGISTROS PULAR 1 PAGINA.             00008501
      *               5-MOSTRAR LIDOS E GRAVADOS NA SYSOUT.             00008601
      *                                                                 00008701
      *----------------------------------------------------------------*00008800
      *                                                                 00008900
      *     INPUT:                                                      00009000
      *     DDNAME           I/O                                        00009100
      *     ARQCLI            I                                         00009201
      *     RELATCLI          O                                         00009301
      *                                                                 00009400
      *================================================================*00009500
                                                                        00009600
      *================================================================*00009700
       ENVIRONMENT                     DIVISION.                        00009800
      *================================================================J00009945
                                                                        00010000
      *----------------------------------------------------------------*00010100
       CONFIGURATION                   SECTION.                         00010200
      *----------------------------------------------------------------*00010300
                                                                        00010400
       SPECIAL-NAMES.                                                   00010500
           DECIMAL-POINT IS COMMA.                                      00010600
                                                                        00010700
      *----------------------------------------------------------------*00010800
       INPUT-OUTPUT                    SECTION.                         00010900
      *----------------------------------------------------------------*00011000
                                                                        00011100
       FILE-CONTROL.                                                    00011200
             SELECT ARQCLI             ASSIGN TO JCLCLI                 00011301
                FILE STATUS            IS WRK-FS-ARQCLI.                00011401
                                                                        00011500
             SELECT RELATCLI           ASSIGN TO JCLREL                 00011601
                FILE STATUS            IS WRK-FS-RELATCLI.              00011701
                                                                        00011800
      *================================================================*00011900
       DATA                            DIVISION.                        00012000
      *================================================================*00012100
                                                                        00013000
      *----------------------------------------------------------------*00014000
       FILE                            SECTION.                         00015000
      *----------------------------------------------------------------*00016000
                                                                        00017000
      *----------------------------------------------------------------*00018000
      *                                                                *00019000
      *    INPUT - DADOS DO ARQUIVO DE ENTRADA (ARQCLI) - LRECL = 037  *00020001
      *                                                                *00021000
      *----------------------------------------------------------------*00022000
                                                                        00022100
       FD  ARQCLI                                                       00022201
            RECORDING MODE IS F                                         00022300
            BLOCK CONTAINS 0 RECORDS.                                   00022400
                                                                        00022500
       01  FD-ARQCLI  PIC X(37).                                        00022601
                                                                        00022700
      *----------------------------------------------------------------*00022800
      *                                                                *00022900
      *    OUTPUT - DADOS DO ARQUIVO DE SAIDA (RELATCLI) - LRECL = 070 *00023001
      *                                                                *00024000
      *----------------------------------------------------------------*00025000
                                                                        00026000
       FD  RELATCLI                                                     00027001
            RECORDING MODE IS F                                         00027100
            BLOCK CONTAINS 0 RECORDS.                                   00027200
                                                                        00027300
       01  FD-RELATCLI PIC X(70).                                       00027401
                                                                        00027500
                                                                        00027600
      *----------------------------------------------------------------*00027700
       WORKING-STORAGE                 SECTION.                         00027800
      *----------------------------------------------------------------*00027900
                                                                        00028000
      *----------------------------------------------------------------*00028100
       01  FILLER                      PIC X(50)    VALUE               00028200
           '***AREA DO AQUIVO ARQCLI***'.                               00028301
      *----------------------------------------------------------------*00028400
                                                                        00028500
         COPY 'B#CLI'.                                                  00028626
                                                                        00028726
      *----------------------------------------------------------------*00028826
                                                                        00029407
       77 WRK-SOMA-SALD             PIC 9(06)V99    VALUE ZEROS.        00029524
                                                                        00029601
      *----------------------------------------------------------------*00029700
       01  FILLER                      PIC X(50)    VALUE               00029800
           '***AREA DO AQUIVO RELATCLI***'.                             00029901
      *----------------------------------------------------------------*00030000
                                                                        00030100
         COPY 'B#RELCLI'.                                               00030227
                                                                        00031301
      *----------------------------------------------------------------*00031400
       01  FILLER                      PIC X(50)               VALUE    00031500
           '**** AREA DE VARIAVEIS DE FILE-STATUS ****'.                00031600
      *----------------------------------------------------------------*00031700
                                                                        00031800
       77 WRK-FS-ARQCLI                PIC X(02)    VALUE SPACES.       00031901
                                                                        00032000
       77 WRK-FS-RELATCLI              PIC X(02)    VALUE SPACES.       00032101
                                                                        00032200
      *----------------------------------------------------------------*00032300
       01  FILLER                      PIC X(50)               VALUE    00032400
           '**** AREA DE AUXILIARES ****'.                              00032500
      *----------------------------------------------------------------*00032600
                                                                        00032700
       77 WRK-MSG                      PIC X(50)    VALUE SPACES.       00033000
                                                                        00033100
       77 WRK-ACU-GRAVADOS             PIC 9(002)   VALUE ZEROS.        00033200
                                                                        00033300
       77 WRK-ACU-LIDOS                PIC 9(002)   VALUE ZEROS.        00033400
                                                                        00033500
       77 WRK-PAGINA                   PIC X(20)    VALUE SPACES.       00033629
                                                                        00033732
       77 WRK-LINHAS                   PIC 9(02)    VALUE ZEROS.        00033832
                                                                        00033945
       01 WRK-TOTAL-CHAVE.                                              00034045
           05 FILLER                       PIC X(35)    VALUE           00034145
              '------------ TOTAL CHAVE ---------'.                     00034245
           05 FILLER                       PIC X(06)    VALUE SPACES.   00034345
           05 WRK-SALDO-CHAVE              PIC 9(08)    VALUE ZEROS.    00034445
                                                                        00034545
       77 WRK-CHAVE-QUEBRA             PIC 9(09)    VALUE ZEROS.        00034645
                                                                        00034745
                                                                        00034845
                                                                        00034945
                                                                        00035032
      *----------------------------------------------------------------*00035100
       01  FILLER                      PIC X(50)               VALUE    00035200
           '****  AREA DE REDEFINES ****'.                              00035300
      *----------------------------------------------------------------*00035400
                                                                        00035500
      *----------------------------------------------------------------*00035631
       01  FILLER                      PIC X(50)               VALUE    00035731
           '****  AREA CABECALHO ****'.                                 00035833
      *----------------------------------------------------------------*00035931
                                                                        00036035
                                                                        00036135
       01 WRK-CABEC1.                                                   00036233
          05 FILLER                    PIC X(66) VALUE                  00036336
           '-------------- RELATORIO DE CLIENTES --------------- PAG: '.00036444
          05 WRK-NUM-PAG               PIC 9(02)    VALUE ZEROS.        00036733
          05 FILLER                    PIC X(02).                       00036836
                                                                        00036933
       01 LINHA.                                                        00037044
          05 FILLER                    PIC X(70) VALUE ALL '-'.         00037144
                                                                        00037244
       01 WRK-CABEC2.                                                   00037333
          05 FILLER                    PIC X(07) VALUE 'AGENCIA'.       00037433
          05 FILLER                    PIC X(02) VALUE SPACES.          00037533
          05 FILLER                    PIC X(05) VALUE 'CONTA'.         00037633
          05 FILLER                    PIC X(05) VALUE SPACES.          00037733
          05 FILLER                    PIC X(12) VALUE 'NOME CLIENTE'.  00037833
          05 FILLER                    PIC X(13) VALUE SPACES.          00037933
          05 FILLER                    PIC X(05) VALUE 'SALDO'.         00038033
          05 FILLER                    PIC X(03) VALUE SPACES.          00038133
                                                                        00038233
                                                                        00038333
      *================================================================*00038400
       PROCEDURE DIVISION.                                              00038500
      *================================================================*00038600
                                                                        00038700
      *----------------------------------------------------------------*00038800
       0000-PRINCIPAL                  SECTION.                         00038900
      *----------------------------------------------------------------*00039000
                                                                        00039100
           PERFORM 1000-INICIAR.                                        00039200
                                                                        00039300
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-ARQCLI EQUAL '10'.       00039401
                                                                        00039500
                 PERFORM 2100-SOMAR-SALDO.                              00039645
                                                                        00040200
           PERFORM 3000-FINALIZAR.                                      00040345
                                                                        00040445
           STOP RUN.                                                    00040500
                                                                        00040600
      *----------------------------------------------------------------*00040700
       0000-99-FIM.                    EXIT.                            00040800
      *----------------------------------------------------------------*00040900
                                                                        00041000
      *----------------------------------------------------------------*00041100
       1000-INICIAR                    SECTION.                         00041200
      *----------------------------------------------------------------*00041300
                                                                        00041400
           OPEN INPUT  ARQCLI                                           00041501
                OUTPUT RELATCLI                                         00041601
                                                                        00041700
           PERFORM 1100-TESTAR-FILE-STATUS.                             00041800
                READ ARQCLI  INTO REG-CLIENTES.                         00041903
                                                                        00042000
      *----------------------------------------------------------------*00042100
       1000-99-FIM.            EXIT.                                    00042200
      *----------------------------------------------------------------*00042300
      *----------------------------------------------------------------*00042400
       1100-TESTAR-FILE-STATUS         SECTION.                         00042500
      *----------------------------------------------------------------*00042600
                                                                        00042700
                                                                        00042800
            PERFORM 1110-TESTAR-FILE-STATUS-ARQCLI.                     00042901
                                                                        00043000
                                                                        00043100
            PERFORM 1120-TESTAR-FILE-STATUS-REL.                        00043204
                                                                        00043300
      *----------------------------------------------------------------*00043400
       1100-99-FIM.                    EXIT.                            00043500
      *----------------------------------------------------------------*00043600
                                                                        00043731
      *----------------------------------------------------------------*00043800
       1110-TESTAR-FILE-STATUS-ARQCLI  SECTION.                         00043901
      *----------------------------------------------------------------*00044000
                                                                        00044100
            IF WRK-FS-ARQCLI           NOT EQUAL ZEROS                  00044201
                MOVE ' ERRO ABERTURA ARQCLI '                           00044331
                                       TO WRK-MSG                       00044400
                PERFORM 9000-TRATAR-ERROS                               00044531
            ELSE                                                        00044600
                PERFORM 2200-GRAVAR-CABEC                               00044745
            END-IF.                                                     00045200
                                                                        00045300
      *----------------------------------------------------------------*00045400
       1110-99-FIM.                    EXIT.                            00045500
      *----------------------------------------------------------------*00045600
                                                                        00045700
      *----------------------------------------------------------------*00045800
       1120-TESTAR-FILE-STATUS-REL     SECTION.                         00045903
      *----------------------------------------------------------------*00046000
                                                                        00046100
            IF WRK-FS-RELATCLI         NOT EQUAL ZEROS                  00046203
                 MOVE ' ERRO ABERTURA RELATCLI '                        00046301
                                       TO WRK-MSG                       00046400
                 PERFORM 9000-TRATAR-ERROS                              00046500
            END-IF.                                                     00046600
                                                                        00046700
      *----------------------------------------------------------------*00046800
       1120-99-FIM.                    EXIT.                            00046900
      *----------------------------------------------------------------*00047000
      *----------------------------------------------------------------*00047100
       2000-PROCESSAR                  SECTION.                         00047200
      *----------------------------------------------------------------*00047300
            ADD 1 TO WRK-ACU-LIDOS                                      00047431
            ADD 1 TO WRK-LINHAS                                         00047531
      *------TESTAR CHAVE ATUAL IGUAL ANTERIOR                          00047645
            IF FD-CHAVE-CLIENTES       EQUAL WRK-CHAVE-QUEBRA           00047745
               ADD FD-SALDO-CLIENTES   TO WRK-SALDO-CHAVE               00047845
            ELSE                                                        00047945
      *---SE NAO FOR GRAVA O SALDO DA ULTIMA CHAVE E ATUALIZA CH ATUAL  00048045
                                                                        00048145
            WRITE FD-RELATCLI          FROM WRK-TOTAL-CHAVE             00048245
            WRITE FD-RELATCLI          FROM WRK-LINHAS                  00048345
            MOVE  FD-CHAVE-CLIENTES    TO   WRK-CHAVE-QUEBRA            00048445
                COMPUTE WRK-SALDO-CHAVE = FD-SALDO-CLIENTES             00048545
            END-IF.                                                     00048645
                                                                        00048745
            IF WRK-LINHAS              GREATER 5                        00048845
              ADD 1                    TO WRK-NUM-PAG                   00048945
              WRITE FD-RELATCLI        FROM LINHA                       00049045
              WRITE FD-RELATCLI        FROM WRK-CABEC1 AFTER PAGE       00049145
              WRITE FD-RELATCLI        FROM WRK-CABEC2 AFTER 2 LINES    00049245
              WRITE FD-RELATCLI        FROM LINHA                       00049345
              MOVE 1                   TO WRK-LINHAS                    00049445
                                                                        00049539
            END-IF.                                                     00049632
                                                                        00049731
            MOVE SPACES                TO FD-RELATCLI                   00049845
                                                                        00049945
            MOVE FD-AGENCIA-CLIENTES                                    00050032
                                       TO WRK-SAIAGENCIA                00050131
            MOVE FD-CONTA-CLIENTES                                      00050232
                                       TO WRK-CONTA                     00050331
            MOVE FD-NOME-CLIENTES                                       00050432
                                       TO WRK-NOME                      00050531
            MOVE FD-SALDO-CLIENTES                                      00050632
                                       TO WRK-SALDO                     00050731
              ADD WRK-SALDO                                             00050842
                                       TO WRK-SOMA-SALD                 00050942
                                                                        00051031
            WRITE FD-RELATCLI          FROM WRK-RELATCLI                00051145
                                                                        00051231
              ADD 1                    TO WRK-ACU-GRAVADOS              00051331
                                                                        00051431
                                                                        00051541
      *-----------------LER PROXIMO REGISTRO - SAIR DO LOOP------------*00051645
                                                                        00051745
            READ ARQCLI                INTO REG-CLIENTES.               00051845
                                                                        00051941
            IF WRK-FS-ARQCLI           EQUAL '10'                       00052045
              MOVE 'FINAL DE ARQUIVO ' TO WRK-MSG                       00052131
            END-IF.                                                     00052204
                                                                        00052345
            ADD 1                      TO WRK-ACU-LIDOS.                00052445
                                                                        00052500
                                                                        00052600
                                                                        00052711
      *----------------------------------------------------------------*00052800
       2000-99-FIM.                    EXIT.                            00052900
      *----------------------------------------------------------------*00053000
                                                                        00053145
      *----------------------------------------------------------------*00053245
       2100-SOMAR-SALDO                SECTION.                         00053345
      *----------------------------------------------------------------*00053445
                                                                        00053545
            MOVE WRK-SOMA-SALD         TO SALDO                         00053645
            WRITE FD-RELATCLI          FROM RODAPE.                     00053745
                                                                        00053845
                                                                        00053945
      *----------------------------------------------------------------*00054045
       2100-99-FIM.                    EXIT.                            00054145
      *----------------------------------------------------------------*00054245
                                                                        00054345
      *----------------------------------------------------------------*00054445
       2200-GRAVAR-CABEC               SECTION.                         00054545
      *----------------------------------------------------------------*00054645
                                                                        00054745
            MOVE 1 TO WRK-NUM-PAG.                                      00054845
                WRITE FD-RELATCLI      FROM WRK-CABEC1 AFTER PAGE.      00054945
                WRITE FD-RELATCLI      FROM LINHA.                      00055045
                WRITE FD-RELATCLI      FROM WRK-CABEC2 AFTER 2 LINES.   00055145
                                                                        00055245
                                                                        00055345
      *----------------------------------------------------------------*00055445
       2200-99-FIM.                    EXIT.                            00055545
      *----------------------------------------------------------------*00055645
                                                                        00055745
      *----------------------------------------------------------------*00055800
       3000-FINALIZAR                  SECTION.                         00055900
      *----------------------------------------------------------------*00056000
                                                                        00056100
            DISPLAY 'TOTAL LIDOS       : '     WRK-ACU-LIDOS            00056223
            DISPLAY 'TOTAL GRAVADOS    : '     WRK-ACU-GRAVADOS         00056323
            DISPLAY '--------------------------------------------------'00056428
            DISPLAY 'SALDO TOTAL       : '     WRK-SOMA-SALD.           00056523
                                                                        00056600
            CLOSE ARQCLI                                                00056701
            CLOSE RELATCLI                                              00056801
              IF WRK-FS-ARQCLI         NOT EQUAL ZEROS                  00056901
                MOVE ' STATUS NO CLOSE '                                00057000
                                       TO WRK-MSG                       00057100
                PERFORM 9000-TRATAR-ERROS                               00057200
              END-IF.                                                   00057300
                                                                        00057400
      *----------------------------------------------------------------*00057500
       3000-99-FIM.                    EXIT.                            00057600
      *----------------------------------------------------------------*00057700
                                                                        00057800
      *----------------------------------------------------------------*00057900
       9000-TRATAR-ERROS               SECTION.                         00058000
      *----------------------------------------------------------------*00058100
                                                                        00058245
            DISPLAY '-------------------STATUS-----------------------'. 00058300
            DISPLAY '  MENSAGEM        '  WRK-MSG.                      00058400
            DISPLAY '  FILE STATUS     '  WRK-FS-ARQCLI.                00058501
            DISPLAY '  FILE STATUS     '  WRK-FS-RELATCLI.              00058601
            DISPLAY '------------------------------------------------'. 00058700
                                                                        00058800
            STOP RUN.                                                   00058918
                                                                        00059045
      *---------------------------------------------------------------* 00059100
       9000-99-FIM.                    EXIT.                            00059200
      *----------------------------------------------------------------*00059300
                                                                        00060000
