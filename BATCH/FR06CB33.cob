      *================================================================*00001000
       IDENTIFICATION                            DIVISION.              00002000
      *================================================================J00003000
                                                                        00004000
       PROGRAM-ID.  FR06CB33.                                           00005003
                                                                        00006000
      *================================================================*00007000
      *                                                                 00007100
      *     AUTOR.......: JOEI LORENTI                                  00008000
      *     ANALISTA....: IVAN SANCHES                                  00008100
      *     EMPRESA.....: FOURSYS                                       00008200
      *     DATA........: 05/05/2022                                    00008300
      *                                                                 00008400
      *----------------------------------------------------------------*00008500
      *                                                                 00008600
      *     OBJETIVO: LER ARQUIVO  DE ENTRADA (ROTINA) E GRAVAR ARQUIVO 00008700
      *               SAIDA (RESUMO).                                   00008800
      *               REVISAO.                                          00008900
      *                                                                 00009000
      *----------------------------------------------------------------*00009100
      *                                                                 00009200
      *     INPUT:                                                      00009300
      *     DDNAME           I/O                                        00009400
      *     ROTINA            I                                         00009500
      *     RESUMO            O                                         00009600
      *                                                                 00009700
      *================================================================*00009800
                                                                        00009900
      *================================================================*00010000
       ENVIRONMENT                               DIVISION.              00010100
      *================================================================*00010200
                                                                        00010300
      *----------------------------------------------------------------*00010400
       CONFIGURATION                             SECTION.               00010500
      *----------------------------------------------------------------*00010600
                                                                        00010700
       SPECIAL-NAMES.                                                   00010800
           DECIMAL-POINT IS COMMA.                                      00010900
                                                                        00011000
      *----------------------------------------------------------------*00011100
       INPUT-OUTPUT                              SECTION.               00011200
      *----------------------------------------------------------------*00011300
                                                                        00012000
       FILE-CONTROL.                                                    00013000
             SELECT ROTINA             ASSIGN TO JCLROT                 00014000
                FILE STATUS            IS WRK-FS-ROTINA.                00015000
       FILE-CONTROL.                                                    00015100
             SELECT RESUMO             ASSIGN TO JCLRES                 00015200
                FILE STATUS            IS WRK-FS-RESUMO.                00015300
                                                                        00016000
      *================================================================*00017000
       DATA                                      DIVISION.              00018000
      *================================================================*00019000
                                                                        00020000
      *----------------------------------------------------------------*00021000
       FILE                                      SECTION.               00022000
      *----------------------------------------------------------------*00023000
                                                                        00024000
       FD ROTINA                                                        00025000
           RECORDING MODE IS F                                          00026000
           BLOCK CONTAINS 0 RECORDS.                                    00027000
       01 FD-ROTINA PIC X(44).                                          00027100
                                                                        00027200
       FD RESUMO                                                        00027302
           RECORDING MODE IS F                                          00027400
           BLOCK CONTAINS 0 RECORDS.                                    00027500
       01 FD-RESUMO PIC X(40).                                          00027600
                                                                        00027700
                                                                        00027800
      *----------------------------------------------------------------*00027900
       WORKING-STORAGE                           SECTION.               00028000
      *----------------------------------------------------------------*00028100
                                                                        00028200
      *----------------------------------------------------------------*00028300
       01 FILLER                       PIC X(50)    VALUE               00028400
          '***AREA DO AQUIVO ROTINA***'                                 00028500
      *----------------------------------------------------------------*00028600
                                                                        00028700
        COPY 'B#ROT'.                                                   00028801
                                                                        00028900
      * 01 WRK-ROTINA.                                                  00029001
      *   05 WRK-DIA                    PIC X(15)    VALUE SPACES.      00029101
      *   05 WRK-EVENTO                 PIC X(25)    VALUE SPACES.      00029201
      *   05 WRK-HORARIO                PIC 9(04)    VALUE ZEROS.       00029301
                                                                        00029400
      *----------------------------------------------------------------*00029500
       01 FILLER                       PIC X(50)    VALUE               00029600
          '***AREA DO AQUIVO RESUMO***'                                 00029700
      *----------------------------------------------------------------*00029800
                                                                        00029900
       01 WRK-RESUMO                   PIC X(40)    VALUE SPACES.       00030000
                                                                        00030100
      *----------------------------------------------------------------*00030700
       01 FILLER                       PIC X(50)               VALUE    00030800
            '**** AREA DE VARIAVEIS DA FILE SEC ****'.                  00030900
      *----------------------------------------------------------------*00031000
                                                                        00031100
       77 WRK-FS-ROTINA                PIC X(02)    VALUE SPACES.       00031200
                                                                        00031300
       77 WRK-FS-RESUMO                PIC X(02)    VALUE SPACES.       00031400
                                                                        00031500
      *----------------------------------------------------------------*00031800
       01 FILLER                       PIC X(50)               VALUE    00031900
            '**** AREA DE AUXILIARES ****'.                             00032000
      *----------------------------------------------------------------*00032100
                                                                        00032200
       77 WRK-MSG                      PIC X(50) VALUE SPACES.          00032300
                                                                        00032400
      *----------------------------------------------------------------*00032500
       01 FILLER                       PIC X(50)               VALUE    00032600
            '****  AREA DE REDEFINES ****'.                             00032700
      *----------------------------------------------------------------*00032800
                                                                        00032900
      *================================================================*00033000
       PROCEDURE DIVISION.                                              00033100
      *================================================================*00033200
                                                                        00033300
      *----------------------------------------------------------------*00033400
       0000-PRINCIPAL                     SECTION.                      00033500
      *----------------------------------------------------------------*00033600
                                                                        00033700
           PERFORM 1000-INICIAR.                                        00033800
                                                                        00033900
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-ROTINA EQUAL '10'.       00034000
                                                                        00034100
           PERFORM 3000-FINALIZAR.                                      00034200
                                                                        00034300
                                                                        00034400
           STOP RUN.                                                    00035000
                                                                        00036000
      *----------------------------------------------------------------*00037000
       0000-99-FIM.            EXIT.                                    00037100
      *----------------------------------------------------------------*00037200
                                                                        00037300
      *----------------------------------------------------------------*00037400
       1000-INICIAR                       SECTION.                      00037500
      *----------------------------------------------------------------*00037600
            OPEN INPUT ROTINA                                           00037700
                OUTPUT RESUMO                                           00037802
            PERFORM 1100-TESTAR-FILE-STATUS.                            00037900
                READ ROTINA INTO WRK-ROTINA.                            00038000
                                                                        00038100
      *----------------------------------------------------------------*00039000
       1000-99-FIM.            EXIT.                                    00040000
      *----------------------------------------------------------------*00040100
      *----------------------------------------------------------------*00040200
       1100-TESTAR-FILE-STATUS            SECTION.                      00040300
      *----------------------------------------------------------------*00040400
                                                                        00040500
            PERFORM 1110-TESTAR-FILE-STATUS-ROTINA.                     00040602
                                                                        00040702
                                                                        00040802
            PERFORM 1120-TESTAR-FILE-STATUS-RESUMO.                     00040902
                                                                        00041700
      *----------------------------------------------------------------*00041802
       1100-99-FIM.                    EXIT.                            00041902
      *----------------------------------------------------------------*00042002
      *----------------------------------------------------------------*00042102
       1110-TESTAR-FILE-STATUS-ROTINA  SECTION.                         00042202
      *----------------------------------------------------------------*00042302
                                                                        00042402
            IF WRK-FS-ROTINA NOT EQUAL ZEROS                            00042502
                 MOVE ' ERRO ABERTURA ROTINA '                          00042602
                                       TO WRK-MSG                       00042702
                 PERFORM 9000-TRATAR-ERROS                              00042802
            ELSE                                                        00042902
                READ ROTINA INTO WRK-ROTINA.                            00043002
                IF WRK-FS-ROTINA NOT EQUAL ZEROS                        00043102
                     DISPLAY ' FIM DE ARQUIVO '                         00043202
                END-IF                                                  00043302
            END-IF.                                                     00043402
                                                                        00043500
      *----------------------------------------------------------------*00043600
       1110-99-FIM.                    EXIT.                            00043702
      *----------------------------------------------------------------*00043800
                                                                        00043900
      *----------------------------------------------------------------*00044002
       1120-TESTAR-FILE-STATUS-RESUMO  SECTION.                         00044102
      *----------------------------------------------------------------*00044202
                                                                        00044302
            IF WRK-FS-RESUMO NOT EQUAL ZEROS                            00044402
                 MOVE ' ERRO ABERTURA ROTINA '                          00044502
                                       TO WRK-MSG                       00044602
                 PERFORM 9000-TRATAR-ERROS                              00044702
            ELSE                                                        00044802
                READ ROTINA INTO WRK-RESUMO.                            00044902
                IF WRK-FS-RESUMO NOT EQUAL ZEROS                        00045002
                     DISPLAY ' FIM DE ARQUIVO '                         00045102
                END-IF                                                  00045202
            END-IF.                                                     00045302
                                                                        00045402
      *----------------------------------------------------------------*00045502
       1120-99-FIM.                    EXIT.                            00045602
      *----------------------------------------------------------------*00045702
      *----------------------------------------------------------------*00045800
       2000-PROCESSAR                  SECTION.                         00045900
      *----------------------------------------------------------------*00046000
                                                                        00046100
            IF WRK-FS-ROTINA           EQUAL ZEROS                      00046200
                 MOVE FD-ROTINA        TO WRK-ROTINA                    00046300
                                                                        00046400
                 DISPLAY '----------------------------'                 00046500
                 DISPLAY ' ROTINA SEMANAL ' WRK-ROTINA                  00046600
                 DISPLAY '----------------------------'                 00046700
            ELSE                                                        00046800
                MOVE ' FIM DE ARQUIVO '                                 00046900
                                       TO WRK-MSG                       00047000
            END-IF.                                                     00047100
                                                                        00047200
                READ ROTINA            INTO WRK-ROTINA.                 00047300
                                                                        00047400
      *----------------------------------------------------------------*00047500
       2000-99-FIM.                    EXIT.                            00047600
      *----------------------------------------------------------------*00047700
      *----------------------------------------------------------------*00047800
       3000-FINALIZAR                  SECTION.                         00047900
      *----------------------------------------------------------------*00048000
                                                                        00048100
            CLOSE ROTINA.                                               00048200
               IF WRK-FS-ROTINA NOT EQUAL ZEROS                         00048300
                 MOVE ' STATUS NO CLOSE '                               00048400
                                       TO WRK-MSG                       00048500
                 PERFORM 9000-TRATAR-ERROS                              00048600
               END-IF.                                                  00048700
                                                                        00048800
      *----------------------------------------------------------------*00048900
       3000-99-FIM.                    EXIT.                            00049000
      *----------------------------------------------------------------*00049100
                                                                        00049200
      *----------------------------------------------------------------*00049300
       9000-TRATAR-ERROS               SECTION.                         00049400
      *----------------------------------------------------------------*00049500
                                                                        00049600
             DISPLAY '-------------------STATUS-----------------------'.00049700
             DISPLAY '  MENSAGEM        '  WRK-MSG.                     00049800
             DISPLAY '  FILE STATUS     '  WRK-FS-ROTINA.               00049900
             DISPLAY '------------------------------------------------'.00050000
                                                                        00050100
      *----------------------------------------------------------------*00050200
       9000-99-FIM.                    EXIT.                            00050300
      *----------------------------------------------------------------*00051000
                                                                        00060000
