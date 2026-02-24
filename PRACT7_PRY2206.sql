
CREATE OR REPLACE PACKAGE PKG_MOROSIDAD AS
  g_valor_multa      NUMBER;  -- multa base (sin descuento)
  g_valor_descuento  NUMBER;  -- descuento aplicado (en pesos)

  FUNCTION fn_desc_multa_3ra_edad(
    p_fecha_nacimiento IN DATE,
    p_fecha_atencion   IN DATE,
    p_multa_base       IN NUMBER
  ) RETURN NUMBER;
END PKG_MOROSIDAD;
/
SHOW ERRORS


CREATE OR REPLACE PACKAGE BODY PKG_MOROSIDAD AS
  FUNCTION fn_desc_multa_3ra_edad(
    p_fecha_nacimiento IN DATE,
    p_fecha_atencion   IN DATE,
    p_multa_base       IN NUMBER
  ) RETURN NUMBER
  IS
    v_edad        NUMBER;
    v_porcentaje  NUMBER := 0;
    v_descuento   NUMBER := 0;
  BEGIN
    v_edad := TRUNC(MONTHS_BETWEEN(p_fecha_atencion, p_fecha_nacimiento) / 12);

    IF v_edad > 70 THEN
      BEGIN
        SELECT porcentaje_descto
          INTO v_porcentaje
          FROM porc_descto_3ra_edad
         WHERE v_edad BETWEEN anno_ini AND anno_ter;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          v_porcentaje := 0;
      END;

      v_descuento := ROUND(p_multa_base * (v_porcentaje / 100));
    ELSE
      v_descuento := 0;
    END IF;

    RETURN v_descuento;
  END fn_desc_multa_3ra_edad;
END PKG_MOROSIDAD;
/
SHOW ERRORS


CREATE OR REPLACE FUNCTION FN_NOMBRE_ESPECIALIDAD(
  p_ate_id IN NUMBER
) RETURN VARCHAR2
IS
  v_especialidad VARCHAR2(100);
BEGIN
  SELECT e.nombre
    INTO v_especialidad
    FROM atencion a
    JOIN medico m       ON m.med_run = a.med_run
    JOIN especialidad e ON e.esp_id  = m.esp_id
   WHERE a.ate_id = p_ate_id;

  RETURN v_especialidad;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    RETURN 'SIN_ESPECIALIDAD';
END FN_NOMBRE_ESPECIALIDAD;
/
SHOW ERRORS


BEGIN
  EXECUTE IMMEDIATE 'CREATE SEQUENCE SEQ_ERRORES_PROCESO START WITH 1 INCREMENT BY 1';
EXCEPTION
  WHEN OTHERS THEN
    IF SQLCODE = -955 THEN
      NULL; -- ya existe
    ELSE
      RAISE;
    END IF;
END;
/


CREATE OR REPLACE PROCEDURE SP_GENERAR_PAGO_MOROSO(
  p_anno_acreditacion IN NUMBER DEFAULT EXTRACT(YEAR FROM SYSDATE)
)
IS
  v_anno_objetivo NUMBER;
  v_fec_ini DATE;
  v_fec_fin DATE;

  TYPE t_varray_multas IS VARRAY(7) OF NUMBER;
  v_multas t_varray_multas := t_varray_multas(
    1200, -- 1 Medicina General
    1300, -- 2 Traumatologia
    1700, -- 3 Neurología y Pediatría
    1900, -- 4 Oftalmologia
    1100, -- 5 Geriatría
    2000, -- 6 Ginecología y Gastroenterologia
    2300  -- 7 Dermatologia
  );

  v_multa_dia      NUMBER;
  v_dias_morosidad NUMBER;
  v_multa_base     NUMBER;
  v_multa_final    NUMBER;

  v_edad         NUMBER;
  v_obs          VARCHAR2(100);
  v_especialidad VARCHAR2(100);

  v_err_msg      VARCHAR2(500);

  CURSOR c_pagos IS
    SELECT
      p.pac_run,
      p.dv_run AS pac_dv_run,
      p.pnombre, p.snombre, p.apaterno, p.amaterno,
      p.fecha_nacimiento,
      a.ate_id,
      a.fecha_atencion,
      a.costo AS costo_atencion,
      pa.fecha_venc_pago,
      pa.fecha_pago
    FROM paciente p
    JOIN atencion a       ON a.pac_run = p.pac_run
    JOIN pago_atencion pa ON pa.ate_id = a.ate_id
    WHERE pa.fecha_venc_pago >= v_fec_ini
      AND pa.fecha_venc_pago <  v_fec_fin
    ORDER BY pa.fecha_venc_pago ASC, p.apaterno ASC;

BEGIN
  v_anno_objetivo := p_anno_acreditacion - 1;

  v_fec_ini := TO_DATE('01/01/' || v_anno_objetivo, 'DD/MM/YYYY');
  v_fec_fin := ADD_MONTHS(v_fec_ini, 12);

  EXECUTE IMMEDIATE 'TRUNCATE TABLE PAGO_MOROSO';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE ERRORES_PROCESO';

  FOR r IN c_pagos LOOP
    BEGIN
      SAVEPOINT sp_fila;

      IF TRUNC(r.fecha_pago) > TRUNC(r.fecha_venc_pago) THEN
        v_dias_morosidad := TRUNC(r.fecha_pago) - TRUNC(r.fecha_venc_pago);

        v_especialidad := FN_NOMBRE_ESPECIALIDAD(r.ate_id);

        v_multa_dia := 0;

        IF UPPER(v_especialidad) IN ('MEDICINA GENERAL') THEN
          v_multa_dia := v_multas(1);

        ELSIF UPPER(v_especialidad) IN ('TRAUMATOLOGIA', 'TRAUMATOLOGÍA') THEN
          v_multa_dia := v_multas(2);

        ELSIF UPPER(v_especialidad) IN ('NEUROLOGIA', 'NEUROLOGÍA', 'PEDIATRIA', 'PEDIATRÍA') THEN
          v_multa_dia := v_multas(3);

        ELSIF UPPER(v_especialidad) IN ('OFTALMOLOGIA', 'OFTALMOLOGÍA') THEN
          v_multa_dia := v_multas(4);

        ELSIF UPPER(v_especialidad) IN ('GERIATRIA', 'GERIATRÍA') THEN
          v_multa_dia := v_multas(5);

        ELSIF UPPER(v_especialidad) IN ('GINECOLOGIA', 'GINECOLOGÍA', 'GASTROENTEROLOGIA', 'GASTROENTEROLOGÍA') THEN
          v_multa_dia := v_multas(6);

        ELSIF UPPER(v_especialidad) IN ('DERMATOLOGIA', 'DERMATOLOGÍA') THEN
          v_multa_dia := v_multas(7);

        ELSE
          v_multa_dia := 0;
        END IF;

        v_multa_base := v_dias_morosidad * v_multa_dia;

        PKG_MOROSIDAD.g_valor_multa := v_multa_base;

        v_edad := TRUNC(MONTHS_BETWEEN(r.fecha_atencion, r.fecha_nacimiento) / 12);

        PKG_MOROSIDAD.g_valor_descuento :=
          PKG_MOROSIDAD.fn_desc_multa_3ra_edad(
            r.fecha_nacimiento,
            r.fecha_atencion,
            v_multa_base
          );

        v_multa_final := v_multa_base - PKG_MOROSIDAD.g_valor_descuento;

        IF PKG_MOROSIDAD.g_valor_descuento > 0 THEN
          v_obs := 'Paciente tenia ' || v_edad ||
                   ' a la fecha de atencion. Se aplico descuento paciente mayor a 70 anos';
        ELSE
          v_obs := NULL;
        END IF;

        INSERT INTO pago_moroso(
          pac_run, pac_dv_run, pac_nombre,
          ate_id, fecha_venc_pago, fecha_pago,
          dias_morosidad, especialidad_atencion,
          costo_atencion, monto_multa, observacion
        ) VALUES (
          r.pac_run,
          r.pac_dv_run,
          SUBSTR(r.pnombre || ' ' || r.snombre || ' ' || r.apaterno || ' ' || r.amaterno, 1, 50),
          r.ate_id,
          r.fecha_venc_pago,
          r.fecha_pago,
          v_dias_morosidad,
          SUBSTR(v_especialidad, 1, 30),
          r.costo_atencion,
          v_multa_final,
          v_obs
        );
      END IF;

    EXCEPTION
      WHEN OTHERS THEN
        ROLLBACK TO sp_fila;

        v_err_msg := SUBSTR(SQLERRM, 1, 500);

        INSERT INTO errores_proceso(nro_correlativo, subprograma_error, descripcion_error)
        VALUES (
          SEQ_ERRORES_PROCESO.NEXTVAL,
          'SP_GENERAR_PAGO_MOROSO',
          'ATE_ID=' || r.ate_id || ' - ' || v_err_msg
        );
    END;
  END LOOP;
END SP_GENERAR_PAGO_MOROSO;
/
SHOW ERRORS


-- EJECUCION Y CONSULTA
EXEC SP_GENERAR_PAGO_MOROSO;

SELECT
  pm.pac_run || '-' || pm.pac_dv_run AS run_paciente,
  pm.pac_nombre,
  pm.ate_id,
  TO_CHAR(pm.fecha_venc_pago, 'DD/MM/YYYY') AS fecha_venc_pago,
  TO_CHAR(pm.fecha_pago,      'DD/MM/YYYY') AS fecha_pago,
  pm.dias_morosidad,
  pm.especialidad_atencion,
  pm.costo_atencion,
  pm.monto_multa,
  pm.observacion
FROM pago_moroso pm
JOIN paciente p ON p.pac_run = pm.pac_run
ORDER BY pm.fecha_venc_pago ASC, p.apaterno ASC;

SELECT
  ep.nro_correlativo,
  ep.subprograma_error,
  ep.descripcion_error
FROM errores_proceso ep
ORDER BY ep.nro_correlativo;