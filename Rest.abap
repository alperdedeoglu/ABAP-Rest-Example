CLASS zcl_commerce_ws_handler DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension
      FINAL METHODS handle_request .

    TYPES:
      BEGIN OF flow_history_s,
        script_name          TYPE string,
        script_name_expanded TYPE string,
        flow_rc              TYPE i,
      END OF flow_history_s .
    TYPES:
      flow_history_t TYPE STANDARD TABLE OF flow_history_s WITH DEFAULT KEY .

    CLASS-METHODS address_parse
      IMPORTING
        !iv_address    TYPE char140
      EXPORTING
        !ev_street     TYPE ad_street
        !ev_str_suppl1 TYPE ad_strspp1
        !ev_str_suppl2 TYPE ad_strspp2 .
  PROTECTED SECTION.

    DATA mo_context TYPE REF TO if_rest_context .
    DATA mo_server TYPE REF TO if_http_server .
    DATA mt_head_fields TYPE tihttpnvp .
    DATA mt_form_fields TYPE tihttpnvp .
  PRIVATE SECTION.

    DATA mv_json_data TYPE xstring .
    DATA mv_log_handler TYPE balloghndl .
    DATA c_create TYPE crmt_db_mode VALUE 'A' ##NO_TEXT.
    DATA c_update TYPE crmt_db_mode VALUE 'B' ##NO_TEXT.

    METHODS create_customer
      IMPORTING
        VALUE(is_request)  TYPE zcrms_comm_maint_cust_req
      RETURNING
        VALUE(rs_response) TYPE zcrms_comm_maint_cust_resp .
    METHODS update_customer
      IMPORTING
        VALUE(is_request)  TYPE zcrms_comm_maint_cust_req
      RETURNING
        VALUE(rs_response) TYPE zcrms_comm_maint_cust_resp .
    METHODS maintain_customer .
    METHODS maintain_address .
    METHODS create_address
      IMPORTING
        VALUE(is_request)  TYPE zcrms_comm_maint_addr_req
      RETURNING
        VALUE(rs_response) TYPE zcrms_comm_maint_addr_resp .
    METHODS update_address
      IMPORTING
        VALUE(is_request)  TYPE zcrms_comm_maint_addr_req
      RETURNING
        VALUE(rs_response) TYPE zcrms_comm_maint_addr_resp .
    METHODS add_log
      IMPORTING
        VALUE(it_return) TYPE bapiret2_t OPTIONAL
        VALUE(is_return) TYPE bapiret2 OPTIONAL
          PREFERRED PARAMETER it_return .
    METHODS create_log
      IMPORTING
        VALUE(iv_object) TYPE balobj_d
        VALUE(iv_subobj) TYPE balsubobj
        VALUE(iv_extnum) TYPE balnrext .
ENDCLASS.



CLASS ZCL_COMMERCE_WS_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMERCE_WS_HANDLER=>ADDRESS_PARSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ADDRESS                     TYPE        CHAR140
* | [<---] EV_STREET                      TYPE        AD_STREET
* | [<---] EV_STR_SUPPL1                  TYPE        AD_STRSPP1
* | [<---] EV_STR_SUPPL2                  TYPE        AD_STRSPP2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD address_parse.

    DATA: lv_string TYPE string,
          lv_str1   TYPE string,
          lv_str2   TYPE string,
          lv_str3   TYPE string,
          lt_string TYPE stringtab.

    lv_string = iv_address.

    SPLIT lv_string AT space INTO TABLE lt_string.

    LOOP AT lt_string INTO lv_string .
      IF lv_str1 IS INITIAL.
        lv_str1 = lv_string.
      ELSE.
        CONCATENATE lv_str1 ' ' lv_string INTO lv_str1 RESPECTING BLANKS.
        IF strlen( lv_str1 ) LE 60.
          ev_street = lv_str1.
        ELSE.
          IF lv_str2 IS INITIAL.
            lv_str2 = lv_string.
          ELSE.
            CONCATENATE lv_str2 ' ' lv_string INTO lv_str2 RESPECTING BLANKS.
            IF strlen( lv_str2 ) LE 40.
              ev_str_suppl1 = lv_str2.
            ELSE.
              IF lv_str2 IS INITIAL.
                lv_str2 = lv_string.
              ELSE.
                CONCATENATE lv_str3 ' ' lv_string INTO lv_str3 RESPECTING BLANKS.
                IF strlen( lv_str3 ) LE 40.
                  ev_str_suppl2 = lv_str3.
                ELSE.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ev_street IS NOT INITIAL.
      DO.
        IF ev_street(1) EQ space.
          ev_street = ev_street+1(59).
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    IF ev_str_suppl1 IS NOT INITIAL.
      DO.
        IF ev_str_suppl1(1) EQ space.
          ev_str_suppl1 = ev_str_suppl1+1(39).
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    IF ev_str_suppl2 IS NOT INITIAL.
      DO.
        IF ev_str_suppl2(1) EQ space.
          ev_str_suppl2 = ev_str_suppl2+1(39).
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->ADD_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_RETURN                      TYPE        BAPIRET2_T(optional)
* | [--->] IS_RETURN                      TYPE        BAPIRET2(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_log.

    DATA: lt_log_handle TYPE bal_t_logh,
          ls_msg        TYPE bal_s_msg.

    CHECK it_return IS NOT INITIAL OR
          is_return IS NOT INITIAL.

    LOOP AT it_return INTO DATA(ls_return).

      CLEAR ls_msg.
      ls_msg-msgid     = ls_return-id.
      ls_msg-msgty     = ls_return-type.
      ls_msg-msgno     = ls_return-number.
      ls_msg-msgv1     = ls_return-message_v1.
      ls_msg-msgv2     = ls_return-message_v2.
      ls_msg-msgv3     = ls_return-message_v3.
      ls_msg-msgv4     = ls_return-message_v4.
      ls_msg-probclass = '2'.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = mv_log_handler
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

    ENDLOOP.

    IF is_return IS NOT INITIAL.

      CLEAR ls_msg.
      ls_msg-msgid     = is_return-id.
      ls_msg-msgty     = is_return-type.
      ls_msg-msgno     = is_return-number.
      ls_msg-msgv1     = is_return-message_v1.
      ls_msg-msgv2     = is_return-message_v2.
      ls_msg-msgv3     = is_return-message_v3.
      ls_msg-msgv4     = is_return-message_v4.
      ls_msg-probclass = '2'.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = mv_log_handler
          i_s_msg          = ls_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

    ENDIF.

    APPEND mv_log_handler TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle = lt_log_handle
      EXCEPTIONS
        OTHERS         = 1.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->CREATE_ADDRESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REQUEST                     TYPE        ZCRMS_COMM_MAINT_ADDR_REQ
* | [<-()] RS_RESPONSE                    TYPE        ZCRMS_COMM_MAINT_ADDR_RESP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_address.

    DATA: ls_address      TYPE bapibus1006_address,
          ls_adtel        TYPE bapiadtel,
          lt_adtel        TYPE bapiadtel_t,
          ls_adsmtp       TYPE bapiadsmtp,
          lt_adsmtp       TYPE bapiadsmtp_t,
          ls_usage        TYPE bapibus1006_addressusage,
          lt_usage        TYPE bapibus1006_addressusage_t,
          lt_return       TYPE bapiret2_t,
          lv_message      TYPE bapiret2-message,
          lv_address_guid TYPE but020-guid.

    IF is_request-partner IS INITIAL.
      rs_response-return-success     = abap_false.
      rs_response-return-description = 'Muhatap numarası boş olamaz!'.
      RETURN.
    ENDIF.

    "Address
    ls_address-country        = 'TR'.
    ls_address-region         = is_request-region.
    ls_address-city_no        = is_request-city_no .
    ls_address-distrct_no     = is_request-district_no .

    IF strlen( ls_address-region ) EQ 1.
      ls_address-region = '0' && ls_address-region.
    ENDIF.

    CALL METHOD address_parse
      EXPORTING
        iv_address    = is_request-address
      IMPORTING
        ev_street     = ls_address-street
        ev_str_suppl1 = ls_address-str_suppl1
        ev_str_suppl2 = ls_address-str_suppl2.

    UNPACK ls_address-city_no    TO ls_address-city_no.
    SELECT SINGLE city_name
      INTO ls_address-city
      FROM adrcityt
     WHERE langu      EQ 'T'
       AND country    EQ ls_address-country
       AND city_code  EQ ls_address-city_no .

    UNPACK ls_address-distrct_no TO ls_address-distrct_no.
    SELECT SINGLE city_part
      INTO ls_address-district
      FROM adrcityprt
     WHERE country    EQ ls_address-country
       AND city_code  EQ ls_address-city_no
       AND cityp_code EQ ls_address-distrct_no .

    "Phone
    ls_adtel-telephone    = is_request-phone.
    ls_adtel-r_3_user     = '3'.
    APPEND ls_adtel TO lt_adtel.
    CLEAR: ls_adtel.

    "Email
    ls_adsmtp-e_mail      = is_request-email.
    APPEND ls_adsmtp TO lt_adsmtp.

    "Usage
    ls_usage-addresstype  = 'ZCOMMSHIP'.
    APPEND ls_usage TO lt_usage.
    CLEAR: ls_usage.

    CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
      EXPORTING
        businesspartner = is_request-partner
        addressdata     = ls_address
        accept_error    = abap_true
      IMPORTING
        addressguid     = lv_address_guid
      TABLES
        bapiadtel       = lt_adtel
        bapiadsmtp      = lt_adsmtp
        addressusage    = lt_usage
        return          = lt_return.

    LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'AEX'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          id         = ls_return-id
          number     = ls_return-number
          message_v1 = ls_return-message_v1
          message_v2 = ls_return-message_v2
          message_v3 = ls_return-message_v3
          message_v4 = ls_return-message_v4
        IMPORTING
          message    = lv_message.
      rs_response-return-success     = abap_false.
      rs_response-return-description = lv_message.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GET_NUMBERS'
        EXPORTING
          businesspartner = is_request-partner
          addressguid     = lv_address_guid
        IMPORTING
          addr_no_out     = rs_response-addrnumber.
      rs_response-partner            = is_request-partner.
      rs_response-return-success     = abap_true.
      rs_response-return-description = 'Başarılı'.
*--> Adres Bazında Lokasyon Bilgisi Alınacak Geçici Olarak Eklendi.

      SELECT SINGLE * FROM geoloc INTO @DATA(ls_geoloc)
                                       WHERE addrnumber = @rs_response-addrnumber.

      rs_response-latitude  = ls_geoloc-latitude.
      rs_response-longitude = ls_geoloc-longitude.
*<--
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->CREATE_CUSTOMER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REQUEST                     TYPE        ZCRMS_COMM_MAINT_CUST_REQ
* | [<-()] RS_RESPONSE                    TYPE        ZCRMS_COMM_MAINT_CUST_RESP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_customer.

    DATA: lt_searchresult TYPE crm_isa_bus1006_bp_addr,
          ls_cperson      TYPE bapibus1006_central_person,
          ls_central      TYPE bapibus1006_central,
          ls_address      TYPE bapibus1006_address,
          ls_adtel        TYPE bapiadtel,
          lt_adtel        TYPE bapiadtel_t,
          ls_adsmtp       TYPE bapiadsmtp,
          lt_adsmtp       TYPE bapiadsmtp_t,
          ls_frg0040      TYPE crmt_bus_frg0040,
          lt_return       TYPE bapiret2_t,
          lv_partner_guid TYPE bu_partner_guid,
          lv_message      TYPE bapiret2-message.

    IF is_request-phone IS INITIAL.
      rs_response-return-success     = abap_false.
      rs_response-return-description = 'Telefon numarası boş olamaz!'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_BUPA_SEARCH_2'
      EXPORTING
        telephone    = is_request-phone
      TABLES
        searchresult = lt_searchresult
        return       = lt_return.

    IF lt_searchresult IS NOT INITIAL.
      SELECT partner
        INTO TABLE @DATA(lt_partner)
        FROM but000
         FOR ALL ENTRIES IN @lt_searchresult
       WHERE partner     EQ @lt_searchresult-partner
         AND bu_group    EQ 'Z002'
         AND xdele       EQ @space
         AND xblck       EQ @space.
    ENDIF.

    IF lt_partner IS NOT INITIAL.

      SELECT partner,rltyp
        INTO TABLE @DATA(lt_but100)
        FROM but100
         FOR ALL ENTRIES IN @lt_searchresult
       WHERE partner     EQ @lt_searchresult-partner.

      "Muhatabı rolü CRM000 ve ZDOKME veya ZELEKT veya CRM002 ve ZDOKME veya ZELEKT ise,
      "gerçek ödeyen veya gerçek malı teslim alan dökme veya elektrik müşterisi olduğu tespit edilir
      READ TABLE lt_but100 INTO DATA(ls_but100) WITH KEY rltyp = 'CRM000'.
      IF sy-subrc NE 0.
        READ TABLE lt_but100 INTO ls_but100 WITH KEY rltyp = 'CRM002'.
      ENDIF.
      IF sy-subrc EQ 0.
        LOOP AT lt_but100 TRANSPORTING NO FIELDS  WHERE partner EQ ls_but100-partner
                                                    AND ( rltyp EQ 'ZDOKME' OR rltyp EQ 'ZELEKT' ).
          rs_response-partner = ls_but100-partner.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Muhatabı rolü ZSTGO ise B2B2C/Bayi müşterisi olduğu tespit edilir
      IF rs_response-partner IS INITIAL.
        READ TABLE lt_but100 INTO ls_but100 WITH KEY rltyp = 'ZSTGO'.
        IF sy-subrc EQ 0.
          rs_response-partner = ls_but100-partner.
        ENDIF.
      ENDIF.

      "Muhatabı rolü BUP002 ve sektör ZELK veya ZDKG olan kayıt varsa potansiyel müşteri olduğu tespit edilir
      IF rs_response-partner IS INITIAL.
        LOOP AT lt_but100 INTO ls_but100 WHERE rltyp EQ 'BUP002'.
          SELECT SINGLE COUNT(*)
            FROM but0is
           WHERE partner EQ ls_but100-partner
             AND istype  IN ('ZELK','ZDKG').
          IF sy-dbcnt    GT 0.
            rs_response-partner = ls_but100-partner.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

    IF rs_response-partner IS NOT INITIAL.

      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner             = rs_response-partner
          businesspartnerrolecategory = 'BUP002'
          businesspartnerrole         = 'ZSTTUP'
        TABLES
          return                      = lt_return.

      add_log( lt_return ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ELSE.

      IF is_request-name_first IS INITIAL OR
         is_request-name_last  IS INITIAL.
        rs_response-return-success     = abap_false.
        rs_response-return-description = 'İsim ve soyisim boş olamaz!'.
        RETURN.
      ENDIF.

      "Central Person
      ls_cperson-firstname      = is_request-name_first.
      ls_cperson-lastname       = is_request-name_last.

      "Central Data
      ls_central-dataorigintype = 'Z015'.

      "Address
      ls_address-country        = 'TR'.

      "Phone
      ls_adtel-telephone        = is_request-phone.
      ls_adtel-r_3_user         = '3'.
      APPEND ls_adtel TO lt_adtel.
      CLEAR: ls_adtel.

      "Email
      ls_adsmtp-e_mail          = is_request-email.
      APPEND ls_adsmtp TO lt_adsmtp.
      CLEAR: ls_adsmtp.

      CALL FUNCTION 'ADDR_ACCEPT_REG_DATA_ERROR'
        EXPORTING
          accept_error   = abap_true
        EXCEPTIONS
          wrong_value    = 1
          dialog_allowed = 2
          OTHERS         = 3.

      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory   = '1'
          partnergroup      = 'Z002'
          addressdata       = ls_address
          centraldata       = ls_central
          centraldataperson = ls_cperson
          accept_error      = abap_true
        IMPORTING
          businesspartner   = rs_response-partner
        TABLES
          e_maildata        = lt_adsmtp
          telefondata       = lt_adtel
          return            = lt_return.

      IF rs_response-partner IS NOT INITIAL.

        CALL FUNCTION 'BUPA_NUMBERS_GET'
          EXPORTING
            iv_partner      = rs_response-partner
          IMPORTING
            ev_partner_guid = lv_partner_guid.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner             = rs_response-partner
            businesspartnerrolecategory = 'BUP002'
            businesspartnerrole         = 'ZSTTUP'
          TABLES
            return                      = lt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner             = rs_response-partner
            businesspartnerrolecategory = 'BUP002'
            businesspartnerrole         = 'ZB2B2C'
          TABLES
            return                      = lt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        ls_frg0040-account_group =  'Z002'.
        ls_frg0040-is_prospect   = abap_true.

        CALL FUNCTION 'CRM_BUPA_FRG0040_CREATE'
          EXPORTING
            iv_partner_guid = lv_partner_guid
            is_data         = ls_frg0040
          IMPORTING
            et_return       = lt_return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        rs_response-return-success     = abap_true.
        rs_response-return-description = 'Başarılı'.

      ELSE.

        add_log( lt_return ).

        LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'AEX'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
            EXPORTING
              id         = ls_return-id
              number     = ls_return-number
              message_v1 = ls_return-message_v1
              message_v2 = ls_return-message_v2
              message_v3 = ls_return-message_v3
              message_v4 = ls_return-message_v4
            IMPORTING
              message    = lv_message.
          rs_response-return-success     = abap_false.
          rs_response-return-description = lv_message.
          EXIT.
        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->CREATE_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUBOBJ                      TYPE        BALSUBOBJ
* | [--->] IV_EXTNUM                      TYPE        BALNREXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_log.

    DATA ls_log TYPE bal_s_log.

    ls_log-extnumber = iv_extnum.
    ls_log-object    = iv_object.
    ls_log-subobject = iv_subobj.
    ls_log-alprog    = sy-repid.
    ls_log-aluser    = sy-uname.
    ls_log-aldate    = sy-datum.
    ls_log-altime    = sy-uzeit.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handler
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMERCE_WS_HANDLER->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.

    DATA: lv_path_info TYPE string.

    mo_server       = server.
    mv_json_data    = server->request->get_data( ).
    DATA(lv_method) = mo_server->request->get_header_field( if_http_header_fields_sap=>path_info ).

    CASE lv_method.
      WHEN '/maintainCustomer'.
        maintain_customer( ).
      WHEN '/maintainAddress'.
        maintain_address( ).
      WHEN OTHERS.
        mo_server->response->set_status( code = cl_rest_status_code=>gc_server_error_internal reason = 'Hata' ).
    ENDCASE.

  ENDMETHOD.                    "if_http_extension~handle_request


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->MAINTAIN_ADDRESS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD maintain_address.

    DATA: ls_request  TYPE zcrms_comm_maint_addr_req,
          ls_response TYPE zcrms_comm_maint_addr_resp,
          lv_response TYPE string.

    /ui2/cl_json=>deserialize( EXPORTING jsonx = mv_json_data pretty_name = 'X' CHANGING data = ls_request ).

    CALL METHOD create_log
      EXPORTING
        iv_object = 'ZCOMMERCE'
        iv_subobj = 'ZADDR'
        iv_extnum = CONV balnrext( ls_request-partner ).

    CASE ls_request-mode.
      WHEN me->c_create.
        ls_response = create_address( ls_request ).
      WHEN me->c_update.
        ls_response = update_address( ls_request ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_rest_resource_exception
          EXPORTING
            status_code    = cl_rest_status_code=>gc_client_error_bad_request
            request_method = if_rest_request=>gc_method_post.
    ENDCASE.

    IF ls_response-return-success EQ abap_true.
      lv_response = /ui2/cl_json=>serialize( pretty_name = 'X' data = ls_response ).
      mo_server->response->set_header_field( name = 'content-type' value = 'application/json; charset=UTF-8' ).
      mo_server->response->set_cdata( lv_response ).
      mo_server->response->set_status( code = cl_rest_status_code=>gc_success_ok reason = 'OK' ).
    ELSE.
      lv_response = /ui2/cl_json=>serialize( pretty_name = 'X' data = ls_response ).
      mo_server->response->set_header_field( name = 'content-type' value = 'application/json; charset=UTF-8' ).
      mo_server->response->set_cdata( lv_response ).
      mo_server->response->set_status( code = cl_rest_status_code=>gc_server_error_internal reason = 'OK' ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->MAINTAIN_CUSTOMER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD maintain_customer.

    DATA: ls_request  TYPE zcrms_comm_maint_cust_req,
          ls_response TYPE zcrms_comm_maint_cust_resp,
          lv_response TYPE string.

    /ui2/cl_json=>deserialize( EXPORTING jsonx = mv_json_data pretty_name = 'X' CHANGING data = ls_request ).

    CALL METHOD create_log
      EXPORTING
        iv_object = 'ZCOMMERCE'
        iv_subobj = 'ZCUST'
        iv_extnum = CONV balnrext( ls_request-partner ).

    CASE ls_request-mode.
      WHEN me->c_create.
        ls_response = create_customer( ls_request ).
      WHEN me->c_update.
        ls_response = update_customer( ls_request ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_rest_resource_exception
          EXPORTING
            status_code    = cl_rest_status_code=>gc_client_error_bad_request
            request_method = if_rest_request=>gc_method_post.
    ENDCASE.

    IF ls_response-return-success EQ abap_true.
      lv_response = /ui2/cl_json=>serialize( pretty_name = 'X' data = ls_response ).
      mo_server->response->set_header_field( name = 'content-type' value = 'application/json; charset=UTF-8' ).
      mo_server->response->set_cdata( lv_response ).
      mo_server->response->set_status( code = cl_rest_status_code=>gc_success_ok reason = 'OK' ).
    ELSE.
      lv_response = /ui2/cl_json=>serialize( pretty_name = 'X' data = ls_response ).
      mo_server->response->set_header_field( name = 'content-type' value = 'application/json; charset=UTF-8' ).
      mo_server->response->set_cdata( lv_response ).
      mo_server->response->set_status( code = cl_rest_status_code=>gc_server_error_internal reason = 'OK' ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->UPDATE_ADDRESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REQUEST                     TYPE        ZCRMS_COMM_MAINT_ADDR_REQ
* | [<-()] RS_RESPONSE                    TYPE        ZCRMS_COMM_MAINT_ADDR_RESP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_address.

    DATA: ls_address      TYPE bapibus1006_address,
          ls_address_x    TYPE bapibus1006_address_x,
          ls_adtel        TYPE bapiadtel,
          lt_adtel        TYPE bapiadtel_t,
          lt_adtel_db     TYPE bapiadtel_t,
          ls_adtel_x      TYPE bapiadtelx,
          lt_adtel_x      TYPE bapiadtelx_t,
          ls_adsmtp       TYPE bapiadsmtp,
          lt_adsmtp       TYPE bapiadsmtp_t,
          lt_adsmtp_db    TYPE bapiadsmtp_t,
          ls_adsmtp_x     TYPE bapiadsmtx,
          lt_adsmtp_x     TYPE bapiadsmtx_t,
          ls_usage        TYPE bapibus1006_addressusage,
          lt_usage        TYPE bapibus1006_addressusage_t,
          lt_return       TYPE bapiret2_t,
          lv_message      TYPE bapiret2-message,
          lv_address_guid TYPE but020-guid.

    IF is_request-partner    IS INITIAL OR
       is_request-addrnumber IS INITIAL.
      rs_response-return-success     = abap_false.
      rs_response-return-description = 'Muhatap numarası ve adres numarası dolu olmalıdır!'.
      RETURN.
    ENDIF.

    SELECT SINGLE COUNT(*)
      FROM but000
     WHERE partner  EQ is_request-partner
       AND bu_group EQ 'Z002'
       AND source   EQ 'Z015'
       AND xdele    EQ space
       AND xblck    EQ space.
    IF sy-subrc     EQ 0.

      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GET_NUMBERS'
        EXPORTING
          businesspartner = is_request-partner
          addr_no         = is_request-addrnumber
        IMPORTING
          addressguidout  = lv_address_guid.

      CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
        EXPORTING
          businesspartner = is_request-partner
          addressguid     = lv_address_guid
        IMPORTING
          addressdata     = ls_address
        TABLES
          bapiadtel       = lt_adtel_db
          bapiadsmtp      = lt_adsmtp_db.

      IF lv_address_guid IS NOT INITIAL.

        "Address
        ls_address-country        = 'TR'.
        ls_address-region         = is_request-region.
        ls_address-city_no        = is_request-city_no .
        ls_address-distrct_no     = is_request-district_no .

        IF strlen( ls_address-region ) EQ 1.
          ls_address-region = '0' && ls_address-region.
        ENDIF.

        CALL METHOD address_parse
          EXPORTING
            iv_address    = is_request-address
          IMPORTING
            ev_street     = ls_address-street
            ev_str_suppl1 = ls_address-str_suppl1
            ev_str_suppl2 = ls_address-str_suppl2.

        UNPACK ls_address-city_no    TO ls_address-city_no.
        SELECT SINGLE city_name
          INTO ls_address-city
          FROM adrcityt
         WHERE langu      EQ 'T'
           AND country    EQ ls_address-country
           AND city_code  EQ ls_address-city_no .

        UNPACK ls_address-distrct_no TO ls_address-distrct_no.
        SELECT SINGLE city_part
          INTO ls_address-district
          FROM adrcityprt
         WHERE country    EQ ls_address-country
           AND city_code  EQ ls_address-city_no
           AND cityp_code EQ ls_address-distrct_no .

        IF ls_address-street      IS NOT INITIAL.
          ls_address_x-street     = 'X'.
        ENDIF.

        IF ls_address-str_suppl1  IS NOT INITIAL.
          ls_address_x-str_suppl1 = 'X'.
        ENDIF.
        IF ls_address-str_suppl2  IS NOT INITIAL.
          ls_address_x-str_suppl2 = 'X'.
        ENDIF.

        IF ls_address-city_no     IS NOT INITIAL.
          ls_address_x-city       = 'X'.
          ls_address_x-city_no    = 'X'.
        ENDIF.

        IF ls_address-distrct_no  IS NOT INITIAL.
          ls_address_x-district   = 'X'.
          ls_address_x-distrct_no = 'X'.
        ENDIF.

        "Phone
        CLEAR: ls_adtel.
        READ TABLE lt_adtel_db INTO ls_adtel WITH KEY r_3_user = '3'.
        IF sy-subrc EQ 0.
          ls_adtel_x-updateflag = 'U'.
        ELSE.
          ls_adtel_x-updateflag = 'I'.
        ENDIF.

        ls_adtel-r_3_user       = '3'.
        ls_adtel-telephone      = is_request-phone.
        APPEND ls_adtel TO lt_adtel.
        CLEAR: ls_adtel.

        ls_adtel_x-r_3_user     = 'X'.
        ls_adtel_x-telephone    = 'X'.
        APPEND ls_adtel_x TO lt_adtel_x.
        CLEAR: ls_adtel_x.

        "Email
        CLEAR: ls_adsmtp.
        READ TABLE lt_adsmtp_db INTO ls_adsmtp INDEX 1.
        IF sy-subrc EQ 0.
          ls_adsmtp_x-updateflag  = 'U'.
        ELSE.
          ls_adsmtp_x-updateflag  = 'I'.
        ENDIF.

        ls_adsmtp-e_mail      = is_request-email.
        APPEND ls_adsmtp TO lt_adsmtp.
        CLEAR: ls_adsmtp.

        ls_adsmtp_x-e_mail    = 'X'.
        APPEND ls_adsmtp_x TO lt_adsmtp_x.
        CLEAR: ls_adsmtp_x.

        CLEAR: lt_return.
        CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
          EXPORTING
            businesspartner = is_request-partner
            addressguid     = lv_address_guid
            addressdata     = ls_address
            addressdata_x   = ls_address_x
            accept_error    = abap_true
          TABLES
            bapiadtel       = lt_adtel
            bapiadsmtp      = lt_adsmtp
            bapiadtel_x     = lt_adtel_x
            bapiadsmt_x     = lt_adsmtp_x
            return          = lt_return.

        LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'AEX'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
            EXPORTING
              id         = ls_return-id
              number     = ls_return-number
              message_v1 = ls_return-message_v1
              message_v2 = ls_return-message_v2
              message_v3 = ls_return-message_v3
              message_v4 = ls_return-message_v4
            IMPORTING
              message    = lv_message.
          rs_response-return-success     = abap_false.
          rs_response-return-description = lv_message.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
          rs_response-partner            = is_request-partner.
          rs_response-addrnumber         = is_request-addrnumber.
*--> Adres Bazında Lokasyon Bilgisi Alınacak Geçici Olarak Eklendi.

          SELECT SINGLE * FROM geoloc INTO @DATA(ls_geoloc)
                                           WHERE addrnumber = @rs_response-addrnumber.

          rs_response-latitude  = ls_geoloc-latitude.
          rs_response-longitude = ls_geoloc-longitude.
*<--
          rs_response-return-success     = abap_true.
          rs_response-return-description = 'Başarılı'.
        ENDIF.

      ELSE.
        rs_response-return-success     = abap_false.
        rs_response-return-description = 'Adres bulunamadı!'.
      ENDIF.

    ELSE.
      "Değişiklik log kaydı
      rs_response-return-success     = abap_true.
      rs_response-return-description = 'Başarılı'.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMERCE_WS_HANDLER->UPDATE_CUSTOMER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REQUEST                     TYPE        ZCRMS_COMM_MAINT_CUST_REQ
* | [<-()] RS_RESPONSE                    TYPE        ZCRMS_COMM_MAINT_CUST_RESP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_customer.

    DATA: ls_cperson   TYPE bapibus1006_central_person,
          ls_cperson_x TYPE bapibus1006_central_person_x,
          ls_adtel     TYPE bapiadtel,
          lt_adtel     TYPE bapiadtel_t,
          ls_adtel_x   TYPE bapiadtelx,
          lt_adtel_x   TYPE bapiadtelx_t,
          ls_adsmtp    TYPE bapiadsmtp,
          lt_adsmtp    TYPE bapiadsmtp_t,
          ls_adsmtp_x  TYPE bapiadsmtx,
          lt_adsmtp_x  TYPE bapiadsmtx_t,
          lt_return    TYPE bapiret2_t,
          lv_message   TYPE bapiret2-message.

    IF is_request-partner IS INITIAL.
      rs_response-return-success     = abap_false.
      rs_response-return-description = 'Muhatap numarası boş olamaz!'.
      RETURN.
    ENDIF.

    SELECT SINGLE COUNT(*)
      FROM but000
     WHERE partner  EQ is_request-partner
       AND bu_group EQ 'Z002'
       AND source   EQ 'Z015'
       AND xdele    EQ space
       AND xblck    EQ space.
    IF sy-subrc     EQ 0.

      "Central Person
      ls_cperson-firstname  = is_request-name_first.
      ls_cperson-lastname   = is_request-name_last.

      "Phone
      ls_adtel-telephone    = is_request-phone.
      ls_adtel-r_3_user     = '3'.
      APPEND ls_adtel TO lt_adtel.
      CLEAR: ls_adtel.

      "Email
      ls_adsmtp-e_mail      = is_request-email.
      APPEND ls_adsmtp TO lt_adsmtp.

      CLEAR: lt_return.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
        EXPORTING
          businesspartner     = is_request-partner
          centraldataperson   = ls_cperson
          centraldataperson_x = ls_cperson_x
        TABLES
          return              = lt_return.

      add_log( lt_return ).

      LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        CLEAR: lt_return.
        CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
          EXPORTING
            businesspartner = is_request-partner
            accept_error    = abap_true
          TABLES
            bapiadtel       = lt_adtel
            bapiadsmtp      = lt_adsmtp
            bapiadtel_x     = lt_adtel_x
            bapiadsmt_x     = lt_adsmtp_x
            return          = lt_return.

        LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
          EXIT.
        ENDLOOP.
        IF sy-subrc EQ 0.
          CLEAR: lt_return.
          CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
            EXPORTING
              businesspartner = is_request-partner
              accept_error    = abap_true
            TABLES
              bapiadtel       = lt_adtel
              bapiadsmtp      = lt_adsmtp
              return          = lt_return.
        ENDIF.
      ENDIF.

      add_log( lt_return ).

      LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'AEX'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING
            id         = ls_return-id
            number     = ls_return-number
            message_v1 = ls_return-message_v1
            message_v2 = ls_return-message_v2
            message_v3 = ls_return-message_v3
            message_v4 = ls_return-message_v4
          IMPORTING
            message    = lv_message.
        rs_response-return-success     = abap_false.
        rs_response-return-description = lv_message.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
        rs_response-partner            = is_request-partner.
        rs_response-return-success     = abap_true.
        rs_response-return-description = 'Başarılı'.
      ENDIF.

    ELSE.
      rs_response-return-success     = abap_true.
      rs_response-return-description = 'Başarılı'.
      "Değişiklik log kaydı
    ENDIF.

  ENDMETHOD.
ENDCLASS.
