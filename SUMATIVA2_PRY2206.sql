-- CASO

SET SERVEROUTPUT ON


VAR b_periodo VARCHAR2(10)
EXEC :b_periodo := TO_CHAR(SYSDATE,'YYYY-MM-DD')
PRINT b_periodo

DECLARE
  TYPE t_varray_tipos IS VARRAY(2) OF VARCHAR2(50);

  v_tipos t_varray_tipos := t_varray_tipos(
    'Avance en Efectivo',
    UNISTR('S\00FAper Avance en Efectivo')  -- "Súper Avance en Efectivo" (referencia)
  );

  v_fecha_proceso  DATE := TRUNC(TO_DATE(:b_periodo,'YYYY-MM-DD'));
  v_ini_anio       DATE := TRUNC(v_fecha_proceso, 'YYYY');
  v_fin_anio_excl  DATE := ADD_MONTHS(TRUNC(v_fecha_proceso,'YYYY'), 12);

  v_total_registros   NUMBER := 0;
  v_iteraciones       NUMBER := 0;

  v_monto_mes_av       NUMBER := 0;
  v_aporte_mes_av      NUMBER := 0;
  v_monto_mes_sav      NUMBER := 0;
  v_aporte_mes_sav     NUMBER := 0;

  v_monto_total_red     NUMBER := 0;
  v_porc_aporte         NUMBER := 0;
  v_aporte_calc         NUMBER := 0;


  v_det DETALLE_APORTE_SBIF%ROWTYPE;


  e_sin_privilegios EXCEPTION;
  PRAGMA EXCEPTION_INIT(e_sin_privilegios, -1031);

  e_tramo_no_encontrado EXCEPTION;

  e_iteraciones_incompletas EXCEPTION;


  CURSOR c_meses IS
    SELECT TO_CHAR(month_start, 'MMYYYY') AS mes_anno
    FROM (
      SELECT DISTINCT TRUNC(t.fecha_transaccion, 'MM') AS month_start
      FROM transaccion_tarjeta_cliente t
      JOIN tipo_transaccion_tarjeta tt
        ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
      WHERE t.fecha_transaccion >= v_ini_anio
        AND t.fecha_transaccion <  v_fin_anio_excl
        AND UPPER(tt.nombre_tptran_tarjeta) LIKE '%AVANCE%'
        AND UPPER(tt.nombre_tptran_tarjeta) NOT LIKE '%COMPRA%'
    )
    ORDER BY month_start;

  CURSOR c_trans_mes(p_mes_anno VARCHAR2) IS
    SELECT
      c.numrun,
      c.dvrun,
      tc.nro_tarjeta,
      t.nro_transaccion,
      t.fecha_transaccion,
      tt.nombre_tptran_tarjeta AS tipo_transaccion,
      t.monto_total_transaccion
    FROM transaccion_tarjeta_cliente t
    JOIN tarjeta_cliente tc
      ON tc.nro_tarjeta = t.nro_tarjeta
    JOIN cliente c
      ON c.numrun = tc.numrun
    JOIN tipo_transaccion_tarjeta tt
      ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
    WHERE t.fecha_transaccion >= v_ini_anio
      AND t.fecha_transaccion <  v_fin_anio_excl
      AND TO_CHAR(t.fecha_transaccion, 'MMYYYY') = p_mes_anno
      -- Solo avances/súper avances
      AND UPPER(tt.nombre_tptran_tarjeta) LIKE '%AVANCE%'
      AND UPPER(tt.nombre_tptran_tarjeta) NOT LIKE '%COMPRA%'
    ORDER BY t.fecha_transaccion ASC, c.numrun ASC;

BEGIN

  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_APORTE_SBIF';
    EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_APORTE_SBIF';
  EXCEPTION
    WHEN e_sin_privilegios THEN
      DBMS_OUTPUT.PUT_LINE('ERROR ORA-01031: sin privilegios para TRUNCATE.');
      RAISE;
  END;


  SELECT COUNT(*)
    INTO v_total_registros
  FROM transaccion_tarjeta_cliente t
  JOIN tipo_transaccion_tarjeta tt
    ON tt.cod_tptran_tarjeta = t.cod_tptran_tarjeta
  WHERE t.fecha_transaccion >= v_ini_anio
    AND t.fecha_transaccion <  v_fin_anio_excl
    AND UPPER(tt.nombre_tptran_tarjeta) LIKE '%AVANCE%'
    AND UPPER(tt.nombre_tptran_tarjeta) NOT LIKE '%COMPRA%';

  IF v_total_registros = 0 THEN
    DBMS_OUTPUT.PUT_LINE('No existen Avances/Súper Avances para el año del periodo ingresado.');
    RETURN;
  END IF;

  FOR m IN c_meses LOOP

    v_monto_mes_av   := 0;
    v_aporte_mes_av  := 0;
    v_monto_mes_sav  := 0;
    v_aporte_mes_sav := 0;

    FOR r IN c_trans_mes(m.mes_anno) LOOP

      v_monto_total_red := ROUND(r.monto_total_transaccion, 0);

      BEGIN
        SELECT tporc.porc_aporte_sbif
          INTO v_porc_aporte
        FROM tramo_aporte_sbif tporc
        WHERE v_monto_total_red BETWEEN tporc.tramo_inf_av_sav AND tporc.tramo_sup_av_sav;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          RAISE e_tramo_no_encontrado;
      END;

      v_aporte_calc := ROUND((v_monto_total_red * v_porc_aporte) / 100, 0);

      v_det.numrun            := r.numrun;
      v_det.dvrun             := r.dvrun;
      v_det.nro_tarjeta       := r.nro_tarjeta;
      v_det.nro_transaccion   := r.nro_transaccion;
      v_det.fecha_transaccion := r.fecha_transaccion;
      v_det.tipo_transaccion  := r.tipo_transaccion;
      v_det.monto_transaccion := v_monto_total_red;
      v_det.aporte_sbif       := v_aporte_calc;

      INSERT INTO detalle_aporte_sbif
      (numrun, dvrun, nro_tarjeta, nro_transaccion, fecha_transaccion,
       tipo_transaccion, monto_transaccion, aporte_sbif)
      VALUES
      (v_det.numrun, v_det.dvrun, v_det.nro_tarjeta, v_det.nro_transaccion, v_det.fecha_transaccion,
       v_det.tipo_transaccion, v_det.monto_transaccion, v_det.aporte_sbif);

      IF UPPER(r.tipo_transaccion) LIKE 'S%PER%' THEN
  v_monto_mes_sav  := v_monto_mes_sav  + v_monto_total_red;
  v_aporte_mes_sav := v_aporte_mes_sav + v_aporte_calc;
ELSE
  v_monto_mes_av   := v_monto_mes_av   + v_monto_total_red;
  v_aporte_mes_av  := v_aporte_mes_av  + v_aporte_calc;
END IF;

      v_iteraciones := v_iteraciones + 1;

    END LOOP;

    IF v_monto_mes_av > 0 THEN
      INSERT INTO resumen_aporte_sbif
      (mes_anno, tipo_transaccion, monto_total_transacciones, aporte_total_abif)
      VALUES
      (m.mes_anno, v_tipos(1), ROUND(v_monto_mes_av,0), ROUND(v_aporte_mes_av,0));
    END IF;

    IF v_monto_mes_sav > 0 THEN
      INSERT INTO resumen_aporte_sbif
      (mes_anno, tipo_transaccion, monto_total_transacciones, aporte_total_abif)
      VALUES
      (m.mes_anno, v_tipos(2), ROUND(v_monto_mes_sav,0), ROUND(v_aporte_mes_sav,0));
    END IF;

  END LOOP;

  IF v_iteraciones = v_total_registros THEN
    COMMIT;
    DBMS_OUTPUT.PUT_LINE('OK: Proceso finalizado correctamente.');
    DBMS_OUTPUT.PUT_LINE('Periodo (bind): ' || :b_periodo);
    DBMS_OUTPUT.PUT_LINE('Año procesado: ' || TO_CHAR(v_fecha_proceso,'YYYY'));
    DBMS_OUTPUT.PUT_LINE('Total registros esperados: ' || v_total_registros);
    DBMS_OUTPUT.PUT_LINE('Total registros procesados: ' || v_iteraciones);
  ELSE
    ROLLBACK;
    RAISE e_iteraciones_incompletas;
  END IF;

EXCEPTION
  WHEN e_tramo_no_encontrado THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR: No existe tramo en TRAMO_APORTE_SBIF para algún monto_total_transaccion.');
    DBMS_OUTPUT.PUT_LINE('Proceso revertido (ROLLBACK).');

  WHEN e_iteraciones_incompletas THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR: No se procesaron todas las transacciones.');
    DBMS_OUTPUT.PUT_LINE('Esperadas: ' || v_total_registros || ' | Procesadas: ' || v_iteraciones);
    DBMS_OUTPUT.PUT_LINE('Proceso revertido (ROLLBACK).');

  WHEN e_sin_privilegios THEN
    ROLLBACK;

  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR GENERAL: ' || SQLERRM);
    DBMS_OUTPUT.PUT_LINE('Proceso revertido (ROLLBACK).');
END;
/

----------- CONSULTAS ------------
SELECT
  numrun,
  dvrun,
  nro_tarjeta,
  nro_transaccion,
  fecha_transaccion,
  tipo_transaccion,
  monto_transaccion        AS monto_total_transaccion,
  aporte_sbif
FROM detalle_aporte_sbif
ORDER BY fecha_transaccion ASC, numrun ASC;


SELECT
  mes_anno,
  tipo_transaccion,
  monto_total_transacciones,
  aporte_total_abif
FROM resumen_aporte_sbif
ORDER BY TO_DATE(mes_anno, 'MMYYYY') ASC, tipo_transaccion ASC;

