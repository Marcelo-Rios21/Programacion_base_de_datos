-- CASO 1
SET SERVEROUTPUT ON;

-- 1) VARIABLES BIND (PARAMETROS)
VAR p_run          VARCHAR2(15);  

VAR p_tramo1       NUMBER;        
VAR p_tramo2       NUMBER;        

VAR p_peso_normal  NUMBER;        
VAR p_extra1       NUMBER;        
VAR p_extra2       NUMBER;        
VAR p_extra3       NUMBER;        

-- 2) ASIGNACION DE PARAMETROS (VALORES DE PRUEBA)

EXEC :p_tramo1      := 1000000;
EXEC :p_tramo2      := 3000000;

EXEC :p_peso_normal := 1200;
EXEC :p_extra1      := 100;
EXEC :p_extra2      := 300;
EXEC :p_extra3      := 550;

-- 3) DEFINIR EL RUN DEL CLIENTE A PROCESAR (CAMBIAR EN CADA EJECUCION)
--    KAREN   -> 21.242.003-4
--    SILVANA -> 22.176.845-2
--    DENISSE -> 18.858.542-6
--    LUIS    -> 21.300.628-2
--    AMANDA  -> 22.558.061-8
EXEC :p_run := '22.558.061-8';

DECLARE
  -- CONSTANTES / VARIABLES LOCALES
  c_unidad_monto CONSTANT NUMBER := 100000;

  v_ini_anio_ant DATE;  
  v_fin_anio_ant DATE;   

  v_run_clean    VARCHAR2(20);
  v_numrun       NUMBER(10);
  v_dv           VARCHAR2(1);

  v_nro_cliente  CLIENTE.nro_cliente%TYPE;
  v_numrun_db    CLIENTE.numrun%TYPE;
  v_dv_db        CLIENTE.dvrun%TYPE;

  v_nombre_cliente VARCHAR2(50);
  v_tipo_cliente   VARCHAR2(30);

  v_monto_total    NUMBER(10);
  v_unidades_100k  NUMBER;

  v_pesos_normales NUMBER;
  v_pesos_extras   NUMBER;
  v_pesos_total    NUMBER;

  v_run_formateado VARCHAR2(15);

BEGIN
  v_fin_anio_ant := TRUNC(SYSDATE, 'YYYY');
  v_ini_anio_ant := ADD_MONTHS(v_fin_anio_ant, -12);
  v_run_clean := REPLACE(REPLACE(REPLACE(UPPER(:p_run),'.',''),'-',''),' ','');
  v_dv        := SUBSTR(v_run_clean, -1);
  v_numrun    := TO_NUMBER(SUBSTR(v_run_clean, 1, LENGTH(v_run_clean)-1));

  SELECT c.nro_cliente,
         c.numrun,
         c.dvrun,
         UPPER(TRIM(c.pnombre || ' ' || NVL(c.snombre,'') || ' ' || c.appaterno || ' ' || NVL(c.apmaterno,''))),
         tc.nombre_tipo_cliente
    INTO v_nro_cliente,
         v_numrun_db,
         v_dv_db,
         v_nombre_cliente,
         v_tipo_cliente
    FROM cliente c
    JOIN tipo_cliente tc
      ON tc.cod_tipo_cliente = c.cod_tipo_cliente
   WHERE c.numrun = v_numrun
     AND UPPER(c.dvrun) = v_dv;

  DECLARE
    l VARCHAR2(10);
  BEGIN
    l := TO_CHAR(v_numrun_db);

    IF LENGTH(l) = 7 THEN
      v_run_formateado := SUBSTR(l,1,1)||'.'||SUBSTR(l,2,3)||'.'||SUBSTR(l,5,3)||'-'||UPPER(v_dv_db);
    ELSIF LENGTH(l) = 8 THEN
      v_run_formateado := SUBSTR(l,1,2)||'.'||SUBSTR(l,3,3)||'.'||SUBSTR(l,6,3)||'-'||UPPER(v_dv_db);
    ELSIF LENGTH(l) = 9 THEN
      v_run_formateado := SUBSTR(l,1,3)||'.'||SUBSTR(l,4,3)||'.'||SUBSTR(l,7,3)||'-'||UPPER(v_dv_db);
    ELSE
      v_run_formateado := l||'-'||UPPER(v_dv_db);
    END IF;
  END;

  SELECT NVL(SUM(cc.monto_solicitado), 0)
    INTO v_monto_total
    FROM credito_cliente cc
   WHERE cc.nro_cliente = v_nro_cliente
     AND cc.fecha_otorga_cred >= v_ini_anio_ant
     AND cc.fecha_otorga_cred <  v_fin_anio_ant;

  v_unidades_100k := TRUNC(v_monto_total / c_unidad_monto);

  v_pesos_normales := v_unidades_100k * :p_peso_normal;

  IF LOWER(v_tipo_cliente) LIKE '%independ%' THEN

    IF v_monto_total <= :p_tramo1 THEN
      v_pesos_extras := v_unidades_100k * :p_extra1;

    ELSIF v_monto_total <= :p_tramo2 THEN
      v_pesos_extras := v_unidades_100k * :p_extra2;

    ELSE
      v_pesos_extras := v_unidades_100k * :p_extra3;
    END IF;

  ELSE
    v_pesos_extras := 0;
  END IF;

  v_pesos_total := v_pesos_normales + v_pesos_extras;
  INSERT INTO cliente_todosuma
    (nro_cliente, run_cliente, nombre_cliente, tipo_cliente, monto_solic_creditos, monto_pesos_todosuma)
  VALUES
    (v_nro_cliente, v_run_formateado, SUBSTR(v_nombre_cliente,1,50), SUBSTR(v_tipo_cliente,1,30),
     v_monto_total, v_pesos_total);
  COMMIT;

  DBMS_OUTPUT.PUT_LINE('OK -> NRO_CLIENTE='||v_nro_cliente||
                       ' RUN='||v_run_formateado||
                       ' MONTO='||v_monto_total||
                       ' PESOS='||v_pesos_total);

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: No existe CLIENTE para el RUN ingresado: '||:p_run);

  WHEN DUP_VAL_ON_INDEX THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: Ya existe el cliente en CLIENTE_TODOSUMA. '||
                         'Debe eliminarlo previamente para re-ejecutar.');

  WHEN VALUE_ERROR THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: RUN inválido. Valor recibido: '||:p_run);

  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('ERROR inesperado: '||SQLERRM);
    ROLLBACK;
END;
/

-- CONSULTA
SELECT
  nro_cliente   AS "NRO_CLIENTE",
  run_cliente   AS "RUN_CLIENTE",
  nombre_cliente AS "NOMBRE_CLIENTE",
  tipo_cliente  AS "TIPO_CLIENTE",
  monto_solic_creditos AS "MONTO_SOLIC_CREDITOS",
  monto_pesos_todosuma AS "MONTO_PESOS_TODOSUMA"
FROM cliente_todosuma
ORDER BY nro_cliente DESC;

-- CASO 2
SET SERVEROUTPUT ON;

-- 1) VARIABLES BIND (PARAMETROS)

VAR p_nro_cliente       NUMBER;   
VAR p_nro_solic_credito NUMBER;   
VAR p_cuotas_postergar  NUMBER;   

-- 2) ASIGNACIÓN DE PARAMETROS (VALORES DE PRUEBA)

-- Ejemplo 1 (Sebastian) 2001, postergar 2:
-- EXEC :p_nro_cliente := 5;
-- EXEC :p_nro_solic_credito := 2001;
-- EXEC :p_cuotas_postergar := 2;

-- Ejemplo 2 (Karen) 3004, postergar 1:
-- EXEC :p_nro_cliente := 67;
-- EXEC :p_nro_solic_credito := 3004;
-- EXEC :p_cuotas_postergar := 1;

-- Ejemplo 3 (Julian) 2004, postergar 1:
-- EXEC :p_nro_cliente := 13;
-- EXEC :p_nro_solic_credito := 2004;
-- EXEC :p_cuotas_postergar := 1;

-- CAMBIAR CON CADA EJEMPLO ANTES DE EJECUTAR EL BLOQUE DEL CASO 2
    EXEC :p_nro_cliente := 13;
    EXEC :p_nro_solic_credito := 2004;
    EXEC :p_cuotas_postergar := 1;
DECLARE
  -- CONSTANTES DE TASAS (PREDEFINIDAS)
  c_tasa_hipotecario_2 CONSTANT NUMBER := 0.005; -- 0,5%
  c_tasa_consumo      CONSTANT NUMBER := 0.01;  -- 1%
  c_tasa_automotriz   CONSTANT NUMBER := 0.02;  -- 2%

  -- RANGO AÑO ANTERIOR (automático, sin fechas fijas)
  v_ini_anio_ant DATE;
  v_fin_anio_ant DATE;

  -- DATOS DEL CRÉDITO / CLIENTE
  v_nombre_credito   VARCHAR2(50); 
  v_tipo_credito     VARCHAR2(20); -- HIPOTECARIO / CONSUMO / AUTOMOTRIZ
  v_tasa_aplicar     NUMBER;       

  -- ÚLTIMA CUOTA DEL CRÉDITO (ORIGINALES)
  v_ult_nro_cuota     NUMBER;
  v_ult_fecha_venc    DATE;
  v_ult_valor_cuota   NUMBER;

  -- CONDICIÓN: MAS DE UN CRÉDITO EL AÑO ANTERIOR
  v_cant_creditos_ant NUMBER;

  -- VARIABLES PARA INSERTAR NUEVAS CUOTAS
  v_nueva_cuota   NUMBER;
  v_nueva_fecha   DATE;
  v_nuevo_valor   NUMBER;

BEGIN
  v_fin_anio_ant := TRUNC(SYSDATE, 'YYYY');       
  v_ini_anio_ant := ADD_MONTHS(v_fin_anio_ant, -12); 


  SELECT UPPER(cr.nombre_credito)
    INTO v_nombre_credito
    FROM credito_cliente cc
    JOIN credito cr
      ON cr.cod_credito = cc.cod_credito
   WHERE cc.nro_solic_credito = :p_nro_solic_credito
     AND cc.nro_cliente       = :p_nro_cliente;

  IF v_nombre_credito LIKE '%HIPOTEC%' THEN
    v_tipo_credito := 'HIPOTECARIO';
  ELSIF v_nombre_credito LIKE '%CONSUM%' THEN
    v_tipo_credito := 'CONSUMO';
  ELSIF v_nombre_credito LIKE '%AUTOMOT%' THEN
    v_tipo_credito := 'AUTOMOTRIZ';
  ELSE
    RAISE_APPLICATION_ERROR(-20001, 'Tipo de crédito no reconocido: '||v_nombre_credito);
  END IF;

  IF v_tipo_credito = 'HIPOTECARIO' THEN
    IF :p_cuotas_postergar NOT IN (1,2) THEN
      RAISE_APPLICATION_ERROR(-20002, 'Hipotecario permite postergar 1 o 2 cuotas.');
    END IF;
  ELSIF v_tipo_credito IN ('CONSUMO','AUTOMOTRIZ') THEN
    IF :p_cuotas_postergar <> 1 THEN
      RAISE_APPLICATION_ERROR(-20003, 'Consumo y Automotriz permiten postergar solo 1 cuota.');
    END IF;
  END IF;

  SELECT MAX(nro_cuota)
    INTO v_ult_nro_cuota
    FROM cuota_credito_cliente
   WHERE nro_solic_credito = :p_nro_solic_credito;

  SELECT fecha_venc_cuota, valor_cuota
    INTO v_ult_fecha_venc, v_ult_valor_cuota
    FROM cuota_credito_cliente
   WHERE nro_solic_credito = :p_nro_solic_credito
     AND nro_cuota         = v_ult_nro_cuota;

  SELECT COUNT(DISTINCT nro_solic_credito)
    INTO v_cant_creditos_ant
    FROM credito_cliente
   WHERE nro_cliente = :p_nro_cliente
     AND fecha_solic_cred >= v_ini_anio_ant
     AND fecha_solic_cred <  v_fin_anio_ant;

  IF v_cant_creditos_ant > 1 THEN
    UPDATE cuota_credito_cliente
       SET fecha_pago_cuota = v_ult_fecha_venc,
           monto_pagado     = v_ult_valor_cuota
     WHERE nro_solic_credito = :p_nro_solic_credito
       AND nro_cuota         = v_ult_nro_cuota;
  END IF;

  IF v_tipo_credito = 'HIPOTECARIO' THEN
    IF :p_cuotas_postergar = 1 THEN
      v_tasa_aplicar := 0;                 -- 1 CUOTA SIN INTERES
    ELSE
      v_tasa_aplicar := c_tasa_hipotecario_2; -- 2 CUOTAS CON 0,5%
    END IF;

  ELSIF v_tipo_credito = 'CONSUMO' THEN
    v_tasa_aplicar := c_tasa_consumo;      -- 1% SOBRE ULTIMA CUOTA

  ELSE -- AUTOMOTRIZ
    v_tasa_aplicar := c_tasa_automotriz;   -- 2% SOBRE ULTIMA CUOTA
  END IF;

  FOR i IN 1 .. :p_cuotas_postergar LOOP

    v_nueva_cuota := v_ult_nro_cuota + i;

    v_nueva_fecha := ADD_MONTHS(v_ult_fecha_venc, i);

    v_nuevo_valor := ROUND(v_ult_valor_cuota * (1 + v_tasa_aplicar));

    INSERT INTO cuota_credito_cliente
      (nro_solic_credito, nro_cuota, fecha_venc_cuota, valor_cuota,
       fecha_pago_cuota, monto_pagado, saldo_por_pagar, cod_forma_pago)
    VALUES
      (:p_nro_solic_credito, v_nueva_cuota, v_nueva_fecha, v_nuevo_valor,
       NULL, NULL, NULL, NULL);

  END LOOP;

  COMMIT;

  DBMS_OUTPUT.PUT_LINE('OK -> Cliente '||:p_nro_cliente||
                       ', Crédito '||:p_nro_solic_credito||
                       ', Tipo='||v_tipo_credito||
                       ', Postergadas='||:p_cuotas_postergar||
                       ', Tasa='||TO_CHAR(v_tasa_aplicar));

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: Cliente/Crédito no encontrado o no corresponde.');
    ROLLBACK;

  WHEN DUP_VAL_ON_INDEX THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: Se intentó insertar una cuota ya existente (PK duplicada).');
    ROLLBACK;

  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('ERROR inesperado: '||SQLERRM);
    ROLLBACK;
END;
/

-- CONSULTA
SELECT
  q.nro_solic_credito,
  q.nro_cuota,
  q.fecha_venc_cuota,
  q.valor_cuota,
  q.fecha_pago_cuota,
  q.monto_pagado,
  q.saldo_por_pagar,
  q.cod_forma_pago
FROM cuota_credito_cliente q
WHERE q.nro_solic_credito IN (2001, 2004, 3004)
  AND q.nro_cuota >= (
    SELECT MAX(q2.nro_cuota) - 2
    FROM cuota_credito_cliente q2
    WHERE q2.nro_solic_credito = q.nro_solic_credito
  )
ORDER BY
  q.nro_solic_credito,
  q.fecha_venc_cuota;



