*$*$ -------------------------------------------------------------- *$*$
*$*$ AUTOR      : ANDRÉ LUIZ GUILHERMINI JUNIOR                     *$*$
*$*$ DATA       : 02/06/2024                                        *$*$
*$*$ -------------------------------------------------------------- *$*$
REPORT z_algj_46.

*$*$ -------------------------------------------------------------- *$*$
*$*$                         DECLARAÇÕES                            *$*$
*$*$ -------------------------------------------------------------- *$*$

TABLES: mard,
        vbbe,
        mara.

TYPE-POOLS: slis.

TYPES:

BEGIN OF type_mard,
         matnr TYPE mard-matnr,
         werks TYPE mard-werks,
         lgort TYPE mard-lgort,
         labst TYPE mard-labst,
         umlme TYPE mard-umlme,
         insme TYPE mard-insme,
         einme TYPE mard-einme,
         speme TYPE mard-speme,
END OF type_mard,

BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         spras TYPE makt-spras,
         maktx TYPE makt-maktx,
END OF type_makt,

BEGIN OF type_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
END OF type_t001w,

BEGIN OF type_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         meins TYPE mara-meins,
END OF type_mara,

BEGIN OF type_mara_aux,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
END OF type_mara_aux,

BEGIN OF type_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         clabs TYPE mchb-clabs,
         cumlm TYPE mchb-cumlm,
         cinsm TYPE mchb-cinsm,
         ceinm TYPE mchb-ceinm,
         cspem TYPE mchb-cspem,
         cretm TYPE mchb-cretm,
END OF type_mchb,

BEGIN OF type_t001l,
         werks TYPE t001l-werks,
         lgort TYPE t001l-lgort,
         lgobe TYPE t001l-lgobe,
END OF type_t001l,

BEGIN OF type_saida_lote_false,
         matnr      TYPE mard-matnr,
         maktx      TYPE makt-maktx,
         werks      TYPE mard-werks,
         name1      TYPE t001w-name1,
         lgort      TYPE mchb-lgort,
         lgobe      TYPE t001l-lgobe,
         labst      TYPE mard-labst,
         umlme      TYPE mard-umlme,
         insme      TYPE mard-insme,
         einme      TYPE mard-einme,
         speme      TYPE mard-speme,
         mabga      TYPE s094-mabga,
         mzuga      TYPE s094-mzuga,
         estoq_disp TYPE mard-labst,
         meins      TYPE mara-meins,
END OF type_saida_lote_false,

BEGIN OF type_saida_lote_true,
         matnr TYPE mchb-matnr,
         maktx TYPE makt-maktx,
         werks TYPE mchb-werks,
         name1 TYPE t001w-name1,
         lgort TYPE mchb-lgort,
         lgobe TYPE t001l-lgobe,
         charg TYPE mchb-charg,
         clabs TYPE mchb-clabs,
         cumlm TYPE mchb-cumlm,
         cinsm TYPE mchb-cinsm,
         ceinm TYPE mchb-ceinm,
         cspem TYPE mchb-cspem,
         cretm TYPE mchb-cretm,
         meins TYPE mara-meins,
END OF type_saida_lote_true,

BEGIN OF type_saida_aux,
           werks    TYPE mard-werks,
           matnr    TYPE mard-matnr,
           labst    TYPE mard-labst,
END OF type_saida_aux.

DATA:
      ti_mard             TYPE TABLE OF type_mard,
      ti_makt             TYPE TABLE OF type_makt,
      ti_t001w            TYPE TABLE OF type_t001w,
      ti_mara             TYPE TABLE OF type_mara,
      ti_mchb             TYPE TABLE OF type_mchb,
      ti_t001l            TYPE TABLE OF type_t001l,
      ti_saida_lote_false TYPE TABLE OF type_saida_lote_false,
      ti_saida_lote_true  TYPE TABLE OF type_saida_lote_true,
      ti_s094             TYPE TABLE OF s094,
      ti_fieldcat         TYPE TABLE OF slis_fieldcat_alv.

DATA:
  rc_mtart TYPE RANGE OF mtart.

CONSTANTS:
  cc_e                       TYPE c          VALUE 'E',
  cc_x                       TYPE c          VALUE 'X',
  cc_a                       TYPE c          VALUE 'A',
  cc_l                       TYPE c          VALUE 'L',
  cc_bis_datum               TYPE sy-datum   VALUE '29991231',
  cc_bukrs                   TYPE bukrs      VALUE '*',
  cc_werks                   TYPE werks_d    VALUE '*',
  cc_ti_saida_lote_false(19) TYPE c          VALUE 'TI_SAIDA_LOTE_FALSE',
  cc_ti_saida_lote_true(18)  TYPE c          VALUE 'TI_SAIDA_LOTE_TRUE',
  cc_s094(4)                 TYPE c          VALUE 'S094',
  cc_mard(4)                 TYPE c          VALUE 'MARD',
  cc_makt(4)                 TYPE c          VALUE 'MAKT',
  cc_mchb(4)                 TYPE c          VALUE 'MCHB',
  cc_mara(4)                 TYPE c          VALUE 'MARA',
  cc_t001w(5)                TYPE c          VALUE 'T001W',
  cc_t001l(5)                TYPE c          VALUE 'T001L',
  cc_matnr(5)                TYPE c          VALUE 'MATNR',
  cc_maktx(5)                TYPE c          VALUE 'MAKTX',
  cc_werks2(5)               TYPE c          VALUE 'WERKS',
  cc_name1(5)                TYPE c          VALUE 'NAME1',
  cc_lgort(5)                TYPE c          VALUE 'LGORT',
  cc_lgobe(5)                TYPE c          VALUE 'LGOBE',
  cc_labst(5)                TYPE c          VALUE 'LABST',
  cc_umlme(5)                TYPE c          VALUE 'UMLME',
  cc_insme(5)                TYPE c          VALUE 'INSME',
  cc_einme(5)                TYPE c          VALUE 'EINME',
  cc_speme(5)                TYPE c          VALUE 'SPEME',
  cc_mabga(5)                TYPE c          VALUE 'MABGA',
  cc_mzuga(5)                TYPE c          VALUE 'MZUGA',
  cc_meins(5)                TYPE c          VALUE 'MEINS',
  cc_charg(5)                TYPE c          VALUE 'CHARG',
  cc_clabs(5)                TYPE c          VALUE 'CLABS',
  cc_cumlm(5)                TYPE c          VALUE 'CUMLM',
  cc_cinsm(5)                TYPE c          VALUE 'CINSM',
  cc_ceinm(5)                TYPE c          VALUE 'CEINM',
  cc_cspem(5)                TYPE c          VALUE 'CSPEM',
  cc_cretm(5)                TYPE c          VALUE 'CRETM',
  cc_constante               TYPE aeobjekt   VALUE 'DELETE_MTART',
  cc_estoq_disp(10)          TYPE c          VALUE 'ESTOQ_DISP',
  cu_unit_new_imp            TYPE t006-msehi VALUE 'L',
  cc_eq(2)                   TYPE c          VALUE 'EQ',
  cc_i                       TYPE c          VALUE 'I',
  cc_data2                   TYPE dats       VALUE '99991231'.


*$*$ -------------------------------------------------------------- *$*$
*$*$                            TELA                                *$*$
*$*$ -------------------------------------------------------------- *$*$
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01. " Tela de seleção

SELECT-OPTIONS: sc_werks FOR mard-werks MODIF ID mf1,
                sc_matnr FOR mard-matnr MODIF ID mf1,
                sc_lgort FOR mard-lgort MODIF ID mf1.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(32) text-b01 MODIF ID mf1.
PARAMETERS: pc_lote AS CHECKBOX MODIF ID mf1. "Lote

SELECTION-SCREEN COMMENT 38(10) text-b02 MODIF ID mf1.
PARAMETERS: pc_snull AS CHECKBOX MODIF ID mf1 DEFAULT 'X'. "Saldo nulo
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(32) text-b03 MODIF ID mf1.
PARAMETERS: pc_edn AS CHECKBOX MODIF ID mf1. "Estoque disponível/necessidades
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(32) text-b04 MODIF ID mf1.
PARAMETERS: pc_estql AS CHECKBOX MODIF ID mf1. "Estoque em Litros
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.


*$*$ -------------------------------------------------------------- *$*$
*$*$                           EVENTOS                              *$*$
*$*$ -------------------------------------------------------------- *$*$
START-OF-SELECTION.

  IF pc_lote IS NOT INITIAL AND
     pc_edn  IS NOT INITIAL.

    MESSAGE s208(00) WITH text-e03 DISPLAY LIKE cc_e. "Escolha Lote ou Estoque disponivel/necessidades!
    LEAVE LIST-PROCESSING.

  ENDIF.

  PERFORM yf_busca_constantes.

  IF pc_lote IS INITIAL.

    PERFORM yf_seleciona_dados_lote_false.

  ELSE.

    PERFORM yf_seleciona_dados_lote_true.

  ENDIF.

END-OF-SELECTION.
  IF pc_lote IS INITIAL.

    PERFORM yf_processa_dados_lote_false.
    PERFORM yf_processa_saida_lote_false.

  ELSE.

    PERFORM yf_processa_dados_lote_true.
    PERFORM yf_processa_saida_lote_true.

  ENDIF.

  PERFORM yf_monta_filedcat.
  PERFORM yf_exibe_alv.
*$*$ -------------------------------------------------------------- *$*$
*$*$                            FORMS                               *$*$
*$*$ -------------------------------------------------------------- *$*$

FORM yf_busca_constantes.

  DATA: s_mtart LIKE LINE OF rc_mtart.

        s_mtart-sign   = cc_i.
        s_mtart-option = cc_eq.
        s_mtart-low    = 'ZRO2'.
        APPEND s_mtart TO rc_mtart.
        s_mtart-low    = 'ZLAG'.
        APPEND s_mtart TO rc_mtart.

  ENDFORM.                    "yf_busca_constantes

  FORM yf_seleciona_dados_lote_false.

    DATA: ti_mard_aux TYPE TABLE OF type_mard.

    IF sc_werks IS INITIAL.
      MESSAGE s208(00) WITH text-e04 DISPLAY LIKE cc_e. "O campo Centro é obrigatório!
      LEAVE LIST-PROCESSING.
    ENDIF.

* Codigo do material
    FREE ti_mard.
    SELECT matnr
           werks
           lgort
           labst
           umlme
           insme
           einme
           speme
      FROM mard
      INTO TABLE ti_mard
     WHERE werks IN sc_werks
       AND matnr IN sc_matnr
       AND lgort IN sc_lgort.

    IF sy-subrc <> 0.
      FREE ti_mard.
      MESSAGE s208(00) WITH text-e01 DISPLAY LIKE cc_e. "Dados não encontrados.
      LEAVE LIST-PROCESSING.
    ENDIF. "IF sy-subrc NE 0.

*  Texto breve do material
    IF ti_mard IS NOT INITIAL.

      ti_mard_aux = ti_mard.
      SORT ti_mard_aux BY matnr.
      DELETE ADJACENT DUPLICATES FROM ti_mard_aux COMPARING matnr.

      FREE ti_mara.
      SELECT matnr
             mtart
             meins
        FROM mara
        INTO TABLE ti_mara
         FOR ALL ENTRIES IN ti_mard_aux
       WHERE matnr = ti_mard_aux-matnr.

      IF sy-subrc = 0.
        IF rc_mtart[] IS NOT INITIAL.
          DELETE ti_mara WHERE mtart IN rc_mtart.
        ENDIF.
      ENDIF. "IF sy-subrc NE 0.

      IF ti_mara IS NOT INITIAL.
        FREE ti_makt.
        SELECT matnr
               spras
               maktx
          FROM makt
          INTO TABLE ti_makt
         FOR ALL ENTRIES IN ti_mara
          WHERE matnr = ti_mara-matnr
            AND spras = sy-langu.

        IF sy-subrc <> 0.
          FREE ti_makt.
        ENDIF. "IF sy-subrc NE 0.
      ENDIF.

      ti_mard_aux = ti_mard.
      SORT ti_mard_aux BY werks.
      DELETE ADJACENT DUPLICATES FROM ti_mard_aux COMPARING werks.

      FREE ti_t001w.
      SELECT werks
             name1
        FROM t001w
        INTO TABLE ti_t001w
        FOR ALL ENTRIES IN ti_mard_aux
        WHERE werks = ti_mard_aux-werks.

      IF sy-subrc <> 0.
        FREE ti_t001w.
      ENDIF. "IF sy-subrc NE 0.

    ENDIF.


    IF pc_edn IS INITIAL.


      IF ti_mard IS NOT INITIAL.
        SELECT matnr
               werks
               lgort
               charg
        FROM mchb
         INTO TABLE ti_mchb
          FOR ALL ENTRIES IN ti_mard
          WHERE matnr = ti_mard-matnr
            AND werks = ti_mard-werks
            AND lgort = ti_mard-lgort.

        IF sy-subrc <> 0.
          FREE ti_mchb.
        ENDIF.

      ENDIF.

      IF ti_mchb IS NOT INITIAL.
        SELECT werks
               lgort
               lgobe
        FROM t001l
          INTO TABLE ti_t001l
          FOR ALL ENTRIES IN ti_mchb
          WHERE werks = ti_mchb-werks
            AND lgort = ti_mchb-lgort.

        IF sy-subrc <> 0.
          FREE ti_t001l.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDFORM.                    "yf_seleciona_dados

  FORM yf_seleciona_dados_lote_true.

    DATA ti_mchb_aux TYPE TABLE OF type_mchb.

    FREE ti_mchb.
    SELECT matnr
           werks
           lgort
           charg
           clabs
           cumlm
           cinsm
           ceinm
           cspem
           cretm
      FROM mchb
      INTO TABLE ti_mchb
     WHERE werks IN sc_werks
       AND matnr IN sc_matnr
       AND lgort IN sc_lgort.

    IF sy-subrc <> 0.
      FREE ti_mard.
      MESSAGE s208(00) WITH text-e01 DISPLAY LIKE cc_e. "Dados não encontrados.
      LEAVE LIST-PROCESSING.
    ENDIF. "IF sy-subrc NE 0.

    IF ti_mchb IS NOT INITIAL.

      ti_mchb_aux = ti_mchb.
      SORT ti_mchb_aux BY matnr.
      DELETE ADJACENT DUPLICATES FROM ti_mchb_aux COMPARING matnr.

      FREE ti_mara.
      SELECT matnr
             meins
        FROM mara
        INTO TABLE ti_mara
         FOR ALL ENTRIES IN ti_mchb_aux
       WHERE matnr = ti_mchb_aux-matnr.

      IF sy-subrc = 0.
        IF rc_mtart[] IS NOT INITIAL.
          DELETE ti_mara WHERE mtart IN rc_mtart.
        ENDIF.
      ENDIF.

      IF ti_mara IS NOT INITIAL.
        FREE ti_makt.
        SELECT matnr
               spras
               maktx
          FROM makt
          INTO TABLE ti_makt
          FOR ALL ENTRIES IN ti_mara
          WHERE matnr = ti_mara-matnr
            AND spras = sy-langu.


        IF sy-subrc <> 0.
          FREE ti_makt.
        ENDIF. "IF sy-subrc NE 0.
      ENDIF.

      ti_mchb_aux = ti_mchb.
      SORT ti_mchb_aux BY werks.
      DELETE ADJACENT DUPLICATES FROM ti_mchb_aux COMPARING werks.

      SELECT werks
             name1
        FROM t001w
        INTO TABLE ti_t001w
        FOR ALL ENTRIES IN ti_mchb_aux
        WHERE werks = ti_mchb_aux-werks.

      IF sy-subrc <> 0.
        FREE ti_t001w.
      ENDIF. "IF sy-subrc NE 0.

      ti_mchb_aux = ti_mchb.
      SORT ti_mchb_aux BY lgort
                          werks.
      DELETE ADJACENT DUPLICATES FROM ti_mchb_aux COMPARING lgort
                                                            werks.

      SELECT werks
             lgort
             lgobe
        FROM t001l
        INTO TABLE ti_t001l
        FOR ALL ENTRIES IN ti_mchb
       WHERE lgort = ti_mchb-lgort
         AND werks = ti_mchb-werks.

      IF sy-subrc <> 0.
        FREE ti_t001l.
      ENDIF. "IF sy-subrc NE 0.

    ENDIF.


  ENDFORM.                    "yf_seleciona_dados_lote_x

  FORM yf_processa_dados_lote_false.

    DATA: s_mard  TYPE type_mard,
          s_makt  TYPE type_makt,
          s_t001w TYPE type_t001w,
          s_mchb  TYPE type_mchb,
          s_mara  TYPE type_mara,
          s_t001l TYPE type_t001l,
          s_saida TYPE type_saida_lote_false.

    SORT: ti_mard BY werks
                     matnr
                     lgort,
          ti_makt  BY matnr,
          ti_t001w BY werks,
          ti_mara  BY matnr,
          ti_mchb  BY matnr,
          ti_t001l BY werks.

    CLEAR s_mard.
    LOOP AT ti_mard INTO s_mard.

      CLEAR s_makt.
      READ TABLE ti_makt INTO s_makt WITH KEY
                                     matnr = s_mard-matnr BINARY SEARCH.

      IF sy-subrc <> 0.
        CLEAR s_makt.
      ENDIF.

      CLEAR s_t001w.
      READ TABLE ti_t001w INTO s_t001w WITH KEY
                                       werks = s_mard-werks BINARY SEARCH.

      IF sy-subrc <> 0.
        CLEAR s_t001w.
      ENDIF.

      CLEAR s_mara.
      READ TABLE ti_mara INTO s_mara WITH KEY
                                     matnr = s_mard-matnr BINARY SEARCH.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF pc_edn IS INITIAL.

        CLEAR s_mchb.
        READ TABLE ti_mchb INTO s_mchb WITH KEY
                                       matnr = s_mard-matnr BINARY SEARCH.

        IF sy-subrc = 0.

          CLEAR s_t001l.
          READ TABLE ti_t001l INTO s_t001l WITH KEY
                                           werks = s_mchb-werks BINARY SEARCH.
          IF sy-subrc <> 0.
            CLEAR s_t001l.
          ENDIF.

        ENDIF. "SY-SUBRC READ TABLE TI_T001L

      ENDIF. "pc_EDN IS NOT INITIAL


      s_saida-matnr = s_mard-matnr.
      s_saida-maktx = s_makt-maktx.
      s_saida-werks = s_mard-werks.
      s_saida-name1 = s_t001w-name1.

      IF pc_edn IS INITIAL.

        s_saida-lgort = s_mchb-lgort.
        s_saida-lgobe = s_t001l-lgobe.
      ELSE.
        s_saida-labst = s_mard-labst.
        s_saida-umlme = s_mard-umlme.

      ENDIF.

      s_saida-insme = s_mard-insme.
      s_saida-einme = s_mard-einme.
      s_saida-speme = s_mard-speme.
      s_saida-meins = s_mara-meins.

      COLLECT s_saida INTO ti_saida_lote_false.

    ENDLOOP.

  ENDFORM.                    "yf_processa_dados

  FORM yf_processa_dados_lote_true.

    DATA: s_saida TYPE type_saida_lote_true,
          s_mchb  TYPE type_mchb,
          s_makt  TYPE type_makt,
          s_t001w TYPE type_t001w,
          s_t001l TYPE type_t001l,
          s_mara  TYPE type_mara.

    SORT: ti_mchb  BY werks
                      matnr
                      lgort,
          ti_makt  BY matnr,
          ti_t001w BY werks,
          ti_t001l BY lgort
                      werks,
          ti_mara  BY matnr.

    CLEAR s_mchb.
    LOOP AT ti_mchb INTO s_mchb.

      CLEAR s_makt.
      READ TABLE ti_makt INTO s_makt WITH KEY
                                     matnr = s_mchb-matnr BINARY SEARCH.

      IF sy-subrc <> 0.
        CLEAR s_makt.
      ENDIF.

      CLEAR s_t001w.
      READ TABLE ti_t001w INTO s_t001w WITH KEY
                                       werks = s_mchb-werks BINARY SEARCH.

      IF sy-subrc <> 0.
        CLEAR s_t001w.
      ENDIF.

      CLEAR s_t001l.
      READ TABLE ti_t001l INTO s_t001l WITH KEY
                                       lgort = s_mchb-lgort
                                       werks = s_mchb-werks BINARY SEARCH.

      IF sy-subrc <> 0.
        CLEAR s_t001l.
      ENDIF.

      CLEAR s_mara.
      READ TABLE ti_mara INTO s_mara WITH KEY
                                     matnr = s_mchb-matnr BINARY SEARCH.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      s_saida-matnr = s_mchb-matnr.
      s_saida-maktx = s_makt-maktx.
      s_saida-werks = s_mchb-werks.
      s_saida-name1 = s_t001w-name1.
      s_saida-lgort = s_mchb-lgort.
      s_saida-lgobe = s_t001l-lgobe.
      s_saida-charg = s_mchb-charg.
      s_saida-clabs = s_mchb-clabs.
      s_saida-cumlm = s_mchb-cumlm.
      s_saida-cinsm = s_mchb-cinsm.
      s_saida-ceinm = s_mchb-ceinm.
      s_saida-cspem = s_mchb-cspem.
      s_saida-cretm = s_mchb-cretm.
      s_saida-meins = s_mara-meins.
      s_saida-meins = s_mara-meins.

      APPEND s_saida TO ti_saida_lote_true.


    ENDLOOP.

  ENDFORM.                    "yf_processa_dados_lote_x

  FORM yf_processa_saida_lote_false.

    DATA:
          ti_saida_aux  TYPE TABLE OF type_saida_aux,
          s_saida_aux   TYPE type_saida_aux,
          s_saida       TYPE type_saida_lote_false,
          ld_data       TYPE sy-datum,
          li_tabix      TYPE sy-tabix,
          s_s094        TYPE s094,
          s_plscn       TYPE plsc,
          lc_matnr      TYPE mara-matnr,                      "#EC NEEDED
          lc_werks      TYPE t001w-werks,                     "#EC NEEDED
          ld_ano        TYPE sy-datum.

    SORT ti_saida_lote_false BY matnr werks.

    LOOP AT ti_saida_lote_false INTO s_saida.


      li_tabix = sy-tabix.

      IF pc_edn IS NOT INITIAL.

        ld_ano = sy-datum(4).

        CLEAR ld_data.
        CONCATENATE ld_ano '0101' INTO ld_data.

        lc_matnr = s_saida-matnr.
        lc_werks = s_saida-werks.

        FREE ti_s094[].
        CALL FUNCTION 'MCB_STOCK_FUTURE'
          EXPORTING
            matnr     = s_saida-matnr
            werks     = s_saida-werks
            von_datum = ld_data
            bis_datum = cc_bis_datum
            perkz     = cc_a
            plscn     = s_plscn
            auswbs    = cc_x
          TABLES
            t_s094    = ti_s094.


        READ TABLE ti_s094 INTO s_s094 INDEX 1.
        s_saida-mabga = s_s094-mabga.
        s_saida-mzuga = s_s094-mzuga.
*     cálculo ESTOQ_DISP
        s_saida-estoq_disp = s_saida-labst + s_saida-mzuga - s_saida-mabga.
        MODIFY ti_saida_lote_false FROM s_saida INDEX li_tabix.
        CLEAR s_s094.
        FREE ti_s094.

      ENDIF.

      IF pc_snull IS INITIAL.

*   Tratando Saldo nulo
        IF s_saida-labst = 0 AND
           s_saida-umlme = 0 AND
           s_saida-insme = 0 AND
           s_saida-einme = 0 AND
           s_saida-speme = 0 AND
           s_saida-mabga = 0 AND
           s_saida-mzuga = 0 AND
           s_saida-estoq_disp = 0.
          DELETE ti_saida_lote_false INDEX li_tabix.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF pc_estql IS NOT INITIAL.

*   Tratando o estoque em litros
        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-labst
                                          s_saida-meins
                                 CHANGING s_saida-labst.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-umlme
                                          s_saida-meins
                                 CHANGING s_saida-umlme.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-insme
                                          s_saida-meins
                                 CHANGING s_saida-insme.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-einme
                                          s_saida-meins
                                 CHANGING s_saida-einme.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-speme
                                          s_saida-meins
                                CHANGING  s_saida-speme.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-mabga
                                          s_saida-meins
                                CHANGING  s_saida-mabga .

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-mzuga
                                          s_saida-meins
                                 CHANGING s_saida-mzuga.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-estoq_disp
                                          s_saida-meins
                                 CHANGING s_saida-estoq_disp.

        s_saida-meins = cu_unit_new_imp.

        MODIFY ti_saida_lote_false FROM s_saida INDEX li_tabix.

      ENDIF.

      IF pc_lote IS INITIAL AND pc_edn IS INITIAL.

        CONTINUE.
      ELSE.

* somatória do valor de LABST localizados na tabela MARD por material de todos os depósitos
        s_saida_aux-matnr = s_saida-matnr.
        s_saida_aux-werks = s_saida-werks.
        s_saida_aux-labst = s_saida-labst.
        COLLECT s_saida_aux INTO ti_saida_aux.
        CLEAR s_saida.
      ENDIF.

    ENDLOOP.

  sort ti_saida_aux by werks matnr.

    LOOP AT ti_saida_lote_false INTO s_saida.

      READ TABLE ti_saida_aux INTO s_saida_aux WITH KEY
                                               werks = s_saida-werks
                                               matnr = s_saida-matnr BINARY SEARCH.

      IF sy-subrc = 0.
        s_saida-labst = s_saida_aux-labst.
        MODIFY ti_saida_lote_false FROM s_saida.
      ENDIF.

    ENDLOOP.

  ENDFORM.                    "yf_processa_saida

  FORM yf_convert_quant USING pc_matnr TYPE any
                              pc_value TYPE any
                              pc_meins TYPE any
                     CHANGING pq_value_new.

    DATA: lc_matnr     TYPE mara-matnr,
          lu_in_me     TYPE mara-meins,
          lu_out_me    TYPE mara-meins,
          lp_menge_in  TYPE ekpo-menge,
          lp_menge_out TYPE ekpo-menge.

    lc_matnr    = pc_matnr.
    lu_in_me    = pc_meins.
    lu_out_me   = cu_unit_new_imp.
    lp_menge_in = pc_value.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = lc_matnr
        i_in_me              = lu_in_me
        i_out_me             = lu_out_me
        i_menge              = lp_menge_in
      IMPORTING
        e_menge              = lp_menge_out
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      CLEAR lp_menge_out.
    ENDIF.

    pq_value_new = lp_menge_out.

  ENDFORM.                    "yf_processa_saida

  FORM yf_processa_saida_lote_true.

    DATA: s_saida TYPE type_saida_lote_true,
          li_tabix TYPE sy-tabix.

    CLEAR s_saida.
    LOOP AT ti_saida_lote_true INTO s_saida.

      li_tabix = sy-tabix.

*   Formatando o valor do material
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = s_saida-matnr
        IMPORTING
          output = s_saida-matnr.

*   Tratando Saldo nulo
      IF pc_snull IS INITIAL.
        IF s_saida-clabs = 0 AND
           s_saida-cumlm = 0 AND
           s_saida-cinsm = 0 AND
           s_saida-ceinm = 0 AND
           s_saida-cspem = 0 AND
           s_saida-cretm = 0.
          DELETE ti_saida_lote_true INDEX li_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

*   Tratando o estoque em litros
      IF pc_estql IS NOT INITIAL.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-meins
                                          s_saida-clabs
                                 CHANGING s_saida-clabs.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-meins
                                          s_saida-cumlm
                                 CHANGING s_saida-cumlm.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-meins
                                          s_saida-cinsm
                                 CHANGING s_saida-cinsm.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-meins
                                          s_saida-ceinm
                                 CHANGING s_saida-ceinm.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-meins
                                          s_saida-cspem
                                 CHANGING s_saida-cspem.

        PERFORM yf_convert_quant USING    s_saida-matnr
                                          s_saida-meins
                                          s_saida-cretm
                                 CHANGING s_saida-cretm.

        s_saida-meins = cu_unit_new_imp.

        MODIFY ti_saida_lote_true FROM s_saida INDEX li_tabix.
      ENDIF.

    ENDLOOP.

  ENDFORM.                    "yf_processa_saida_lote_true

  FORM yf_monta_filedcat.

    DATA: s_fieldcat TYPE slis_fieldcat_alv.

    IF pc_lote IS INITIAL.

* Relatorio com o flag de lote desligado
      PERFORM yf_monta_ti_filedcat USING:

*   FIELDNAME      TABNAME                  REF_FIELDNAME     REF_TABNAME  CHECK
      cc_matnr       cc_ti_saida_lote_false   cc_matnr          cc_mard       space,
      cc_maktx       cc_ti_saida_lote_false   cc_maktx          cc_makt       space,
      cc_werks2      cc_ti_saida_lote_false   cc_werks2         cc_mard       space,
      cc_name1       cc_ti_saida_lote_false   cc_name1          cc_t001w      space,

      cc_lgort       cc_ti_saida_lote_false   cc_lgort          cc_mchb       space,
      cc_lgobe       cc_ti_saida_lote_false   cc_lgobe          cc_t001l      space,

      cc_labst       cc_ti_saida_lote_false   cc_labst          cc_mard       space,
      cc_umlme       cc_ti_saida_lote_false   cc_umlme          cc_mard       space,

      cc_insme       cc_ti_saida_lote_false   cc_insme          cc_mard       space,
      cc_einme       cc_ti_saida_lote_false   cc_einme          cc_mard       space,
      cc_speme       cc_ti_saida_lote_false   cc_speme          cc_mard       space,
      cc_mabga       cc_ti_saida_lote_false   cc_mabga          cc_s094       cc_x,
      cc_mzuga       cc_ti_saida_lote_false   cc_mzuga          cc_s094       cc_x.

      IF pc_edn IS NOT INITIAL.
        CLEAR s_fieldcat.
        s_fieldcat-fieldname        = cc_estoq_disp.
        s_fieldcat-tabname          = cc_ti_saida_lote_false.
        s_fieldcat-seltext_l        = text-001.
        s_fieldcat-seltext_m        = text-001. "Estoque disponível
        s_fieldcat-seltext_s        = text-002. "Est. disp
        APPEND s_fieldcat TO ti_fieldcat.
      ENDIF.

      PERFORM yf_monta_ti_filedcat USING:
      cc_meins        cc_ti_saida_lote_false   cc_meins         cc_mara       cc_x.

    ELSE.

* Relatorio com o flag de lote ligado
    PERFORM yf_monta_ti_filedcat USING:
*   fieldname    tabname                 ref_fieldname    ref_tabname   check
      cc_matnr     cc_ti_saida_lote_true   cc_matnr         cc_mchb       space,
      cc_maktx     cc_ti_saida_lote_true   cc_maktx         cc_makt       space,
      cc_werks2    cc_ti_saida_lote_true   cc_werks2        cc_mchb       space,
      cc_name1     cc_ti_saida_lote_true   cc_name1         cc_t001w      space,
      cc_lgort     cc_ti_saida_lote_true   cc_lgort         cc_mchb       space,
      cc_lgobe     cc_ti_saida_lote_true   cc_lgobe         cc_t001l      space,
      cc_charg     cc_ti_saida_lote_true   cc_charg         cc_mchb       space,
      cc_clabs     cc_ti_saida_lote_true   cc_clabs         cc_mchb       space,
      cc_cumlm     cc_ti_saida_lote_true   cc_cumlm         cc_mchb       space,
      cc_cinsm     cc_ti_saida_lote_true   cc_cinsm         cc_mchb       space,
      cc_ceinm     cc_ti_saida_lote_true   cc_ceinm         cc_mchb       space,
      cc_cspem     cc_ti_saida_lote_true   cc_cspem         cc_mchb       space,
      cc_cretm     cc_ti_saida_lote_true   cc_cretm         cc_mchb       space,
      cc_meins     cc_ti_saida_lote_true   cc_meins         cc_mara       space.

    ENDIF.



  ENDFORM.                    "yf_monta_filedcat

  FORM yf_monta_ti_filedcat USING fieldname     TYPE any "slis_fieldcat_alv-fieldname
                                  tabname       TYPE any "slis_fieldcat_alv-tabname
                                  ref_fieldname TYPE any "slis_fieldcat_alv-ref_fieldname
                                  ref_tabname   TYPE any "slis_fieldcat_alv-ref_tabname
                                  p_check       TYPE flag.

    DATA: s_fieldcat TYPE slis_fieldcat_alv.

    IF  p_check IS NOT INITIAL.
      IF pc_edn IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    IF pc_edn IS INITIAL.
      IF fieldname = cc_labst OR fieldname = cc_umlme.
        RETURN.
      ENDIF.
    ELSE.
      IF fieldname = cc_lgort OR fieldname = cc_lgobe.
        RETURN.
      ENDIF.
    ENDIF.

    CLEAR s_fieldcat.
    s_fieldcat-fieldname        = fieldname.
    s_fieldcat-tabname          = tabname.
    s_fieldcat-ref_fieldname    = ref_fieldname.
    s_fieldcat-ref_tabname      = ref_tabname.
    APPEND s_fieldcat TO ti_fieldcat.

  ENDFORM.                    "yf_monta_ti_filedcat

  FORM yf_exibe_alv.

    IF pc_lote IS INITIAL.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
          it_fieldcat        = ti_fieldcat
        TABLES
          t_outtab           = ti_saida_lote_false
        EXCEPTIONS
          program_error      = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        MESSAGE s208(00) WITH text-e02 DISPLAY LIKE cc_e. "Erro ao gerar o relatorio!
        LEAVE LIST-PROCESSING.
      ENDIF.

    ELSE.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
          it_fieldcat        = ti_fieldcat
        TABLES
          t_outtab           = ti_saida_lote_true
        EXCEPTIONS
          program_error      = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
        MESSAGE s208(00) WITH text-e02 DISPLAY LIKE cc_e. "Erro ao gerar o relatorio!
        LEAVE LIST-PROCESSING.
      ENDIF.

    ENDIF.

  ENDFORM.                    "yf_exibe_alv
  