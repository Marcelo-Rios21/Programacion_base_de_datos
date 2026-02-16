
-- CONVERSION A FECHA
CREATE OR REPLACE FUNCTION fn_pcgc_yyyymm_to_date(p_anno_mes NUMBER)
RETURN DATE
IS
  v_fecha DATE;
BEGIN
  -- FECHA DINAMICA
  v_fecha := TO_DATE(TO_CHAR(p_anno_mes) || '01', 'YYYYMMDD');
  RETURN v_fecha;
END;
/

-- RETORNO
CREATE OR REPLACE FUNCTION fn_pcgc_periodo_anterior(p_anno_mes NUMBER, p_meses_atras NUMBER)
RETURN NUMBER
IS
  v_fecha DATE;
BEGIN
  -- FECHA BASE
  v_fecha := fn_pcgc_yyyymm_to_date(p_anno_mes);

  -- CALCULO MES O MESES ATRAS
  v_fecha := ADD_MONTHS(v_fecha, -p_meses_atras);

  -- RETORNO CON FORMATO
  RETURN TO_NUMBER(TO_CHAR(v_fecha, 'YYYYMM'));
END;
/

-- FORMATO RUT
CREATE OR REPLACE FUNCTION fn_formatear_run(p_numrun NUMBER, p_dv VARCHAR2)
RETURN VARCHAR2
IS
  v_run VARCHAR2(30);
BEGIN
  -- FORMATO CON SEPARADOR
  v_run := TO_CHAR(p_numrun,
                   'FM999G999G999G999',
                   'NLS_NUMERIC_CHARACTERS='',.''' );

  RETURN v_run || '-' || p_dv;
END;
/

CREATE OR REPLACE FUNCTION fn_nombre_completo(
  p_pnombre   VARCHAR2,
  p_snombre   VARCHAR2,
  p_appaterno VARCHAR2,
  p_apmaterno VARCHAR2
) RETURN VARCHAR2
IS
BEGIN
  RETURN TRIM(
           p_pnombre || ' ' ||
           CASE
             WHEN p_snombre IS NOT NULL THEN p_snombre || ' '
             ELSE ''
           END ||
           p_appaterno || ' ' ||
           NVL(p_apmaterno, '')
         );
END;
/


-- CALCULOI DE MULTA
CREATE OR REPLACE FUNCTION fn_calcular_multa_no_pago(
  p_periodos_impagos NUMBER,
  p_valor_uf         NUMBER
) RETURN NUMBER
IS
  v_factor_uf NUMBER;
BEGIN

  IF p_periodos_impagos >= 2 THEN
    v_factor_uf := 2;   
  ELSE
    v_factor_uf := 1;  
  END IF;

  RETURN v_factor_uf * p_valor_uf;
END;
/

-- PROCEDIMIEDNTOS

CREATE OR REPLACE PROCEDURE sp_upsert_gc_pago_cero(
  p_anno_mes        IN NUMBER,
  p_id_edif         IN NUMBER,
  p_nombre_edif     IN VARCHAR2,
  p_run_adm         IN VARCHAR2,
  p_nombre_adm      IN VARCHAR2,
  p_nro_depto       IN NUMBER,
  p_run_resp        IN VARCHAR2,
  p_nombre_resp     IN VARCHAR2,
  p_valor_multa     IN NUMBER,
  p_observacion     IN VARCHAR2
)
IS
  v_existe NUMBER;
BEGIN
  SELECT COUNT(*)
    INTO v_existe
    FROM gasto_comun_pago_cero
   WHERE anno_mes_pcgc = p_anno_mes
     AND id_edif       = p_id_edif
     AND nro_depto     = p_nro_depto;

  IF v_existe > 0 THEN
    UPDATE gasto_comun_pago_cero
       SET nombre_edif                 = p_nombre_edif,
           run_administrador           = p_run_adm,
           nombre_admnistrador         = p_nombre_adm,
           run_responsable_pago_gc     = p_run_resp,
           nombre_responsable_pago_gc  = p_nombre_resp,
           valor_multa_pago_cero       = p_valor_multa,
           observacion                 = p_observacion
     WHERE anno_mes_pcgc = p_anno_mes
       AND id_edif       = p_id_edif
       AND nro_depto     = p_nro_depto;

  ELSE
    INSERT INTO gasto_comun_pago_cero(
      anno_mes_pcgc,
      id_edif,
      nombre_edif,
      run_administrador,
      nombre_admnistrador,
      nro_depto,
      run_responsable_pago_gc,
      nombre_responsable_pago_gc,
      valor_multa_pago_cero,
      observacion
    ) VALUES (
      p_anno_mes,
      p_id_edif,
      p_nombre_edif,
      p_run_adm,
      p_nombre_adm,
      p_nro_depto,
      p_run_resp,
      p_nombre_resp,
      p_valor_multa,
      p_observacion
    );
  END IF;
END;
/
CREATE OR REPLACE PROCEDURE sp_generar_info_pago_cero(
  p_anno_mes_proceso IN NUMBER,
  p_valor_uf         IN NUMBER 
)
IS
  v_periodo_base    NUMBER;   
  v_periodo_base_2  NUMBER;    
  v_cnt_pago_base   NUMBER;
  v_cnt_pago_base_2 NUMBER;
  v_impagos         NUMBER;
  v_multa_pesos     NUMBER;
  v_obs             VARCHAR2(80);
BEGIN
  v_periodo_base   := fn_pcgc_periodo_anterior(p_anno_mes_proceso, 1);
  v_periodo_base_2 := fn_pcgc_periodo_anterior(p_anno_mes_proceso, 2);

  DELETE FROM gasto_comun_pago_cero
   WHERE anno_mes_pcgc = p_anno_mes_proceso;

  FOR r IN (
    SELECT g.anno_mes_pcgc,
           g.id_edif,
           e.nombre_edif,
           g.nro_depto,
           e.numrun_adm,
           a.dvrun_adm,
           a.pnombre_adm, a.snombre_adm, a.appaterno_adm, a.apmaterno_adm,
           g.numrun_rpgc,
           rp.dvrun_rpgc,
           rp.pnombre_rpgc, rp.snombre_rpgc, rp.appaterno_rpgc, rp.apmaterno_rpgc,
           g.fecha_pago_gc
      FROM gasto_comun g
      JOIN edificio e
        ON e.id_edif = g.id_edif
      JOIN administrador a
        ON a.numrun_adm = e.numrun_adm
      JOIN responsable_pago_gasto_comun rp
        ON rp.numrun_rpgc = g.numrun_rpgc
     WHERE g.anno_mes_pcgc = p_anno_mes_proceso
     ORDER BY e.nombre_edif, g.nro_depto
  ) LOOP

    SELECT COUNT(*)
      INTO v_cnt_pago_base
      FROM pago_gasto_comun p
     WHERE p.anno_mes_pcgc = v_periodo_base
       AND p.id_edif       = r.id_edif
       AND p.nro_depto     = r.nro_depto;

    IF v_cnt_pago_base = 0 THEN
      SELECT COUNT(*)
        INTO v_cnt_pago_base_2
        FROM pago_gasto_comun p
       WHERE p.anno_mes_pcgc = v_periodo_base_2
         AND p.id_edif       = r.id_edif
         AND p.nro_depto     = r.nro_depto;

      IF v_cnt_pago_base_2 = 0 THEN
        v_impagos := 2;
      ELSE
        v_impagos := 1;
      END IF;

      v_multa_pesos := fn_calcular_multa_no_pago(v_impagos, p_valor_uf);

      IF v_impagos >= 2 THEN
        v_obs := 'Se realizara el corte del combustible y agua a contar del '
                 || TO_CHAR(r.fecha_pago_gc, 'DD/MM/YYYY');
      ELSE
        v_obs := 'Se realizara el corte del combustible y agua';
      END IF;

      sp_upsert_gc_pago_cero(
        p_anno_mes    => p_anno_mes_proceso,
        p_id_edif     => r.id_edif,
        p_nombre_edif => r.nombre_edif,
        p_run_adm     => fn_formatear_run(r.numrun_adm, r.dvrun_adm),
        p_nombre_adm  => fn_nombre_completo(r.pnombre_adm, r.snombre_adm,
                                            r.appaterno_adm, r.apmaterno_adm),
        p_nro_depto   => r.nro_depto,
        p_run_resp    => fn_formatear_run(r.numrun_rpgc, r.dvrun_rpgc),
        p_nombre_resp => fn_nombre_completo(r.pnombre_rpgc, r.snombre_rpgc,
                                            r.appaterno_rpgc, r.apmaterno_rpgc),
        p_valor_multa => v_multa_pesos,
        p_observacion => v_obs
      );

      UPDATE gasto_comun
         SET multa_gc = v_multa_pesos
       WHERE anno_mes_pcgc = p_anno_mes_proceso
         AND id_edif       = r.id_edif
         AND nro_depto     = r.nro_depto;

    END IF;

  END LOOP;

  COMMIT;

EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK;
    RAISE;
END;
/

--EJECUTABLES
DECLARE
  v_periodo_mayo NUMBER;
BEGIN
  v_periodo_mayo := TO_NUMBER(TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE,'YYYY'), 4), 'YYYYMM'));

  sp_generar_info_pago_cero(
    p_anno_mes_proceso => v_periodo_mayo,
    p_valor_uf         => 29509
  );
END;
/

-- CONSULTAS
SELECT anno_mes_pcgc,
       id_edif,
       nombre_edif,
       run_administrador,
       nombre_admnistrador,
       nro_depto,
       run_responsable_pago_gc,
       nombre_responsable_pago_gc,
       valor_multa_pago_cero,
       observacion
  FROM gasto_comun_pago_cero
 WHERE anno_mes_pcgc = TO_NUMBER(TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE,'YYYY'), 4), 'YYYYMM'))
 ORDER BY nombre_edif, nro_depto;

SELECT anno_mes_pcgc,
       id_edif,
       nro_depto,
       fecha_desde_gc,
       fecha_hasta_gc,
       multa_gc
  FROM gasto_comun
 WHERE anno_mes_pcgc = TO_NUMBER(TO_CHAR(ADD_MONTHS(TRUNC(SYSDATE,'YYYY'), 4), 'YYYYMM'))
   AND NVL(multa_gc,0) > 0
 ORDER BY id_edif, nro_depto;


