
-- CASO 1
VARIABLE b_fecha_proceso VARCHAR2(19)

-- FECHA ACTUAL
EXEC :b_fecha_proceso := TO_CHAR(SYSDATE,'YYYY-MM-DD HH24:MI:SS')

DECLARE
  v_fecha_proceso DATE := TO_DATE(:b_fecha_proceso,'YYYY-MM-DD HH24:MI:SS');

 -- RANGOS
  c_id_min CONSTANT NUMBER := 100;
  c_id_max CONSTANT NUMBER := 320;
  c_step   CONSTANT NUMBER := 10;

  v_total_esperado NUMBER := ROUND((c_id_max - c_id_min) / c_step) + 1;

  v_iter_ok NUMBER := 0;

  v_id_emp      empleado.id_emp%TYPE;
  v_numrun_emp  empleado.numrun_emp%TYPE;
  v_dvrun_emp   empleado.dvrun_emp%TYPE;
  v_appaterno   empleado.appaterno_emp%TYPE;
  v_apmaterno   empleado.apmaterno_emp%TYPE;
  v_pnombre     empleado.pnombre_emp%TYPE;
  v_snombre     empleado.snombre_emp%TYPE;
  v_sueldo_base empleado.sueldo_base%TYPE;
  v_fecha_nac   empleado.fecha_nac%TYPE;
  v_fecha_cont  empleado.fecha_contrato%TYPE;

  v_nom_est_civil   estado_civil.nombre_estado_civil%TYPE;
  v_nombre_empleado usuario_clave.nombre_empleado%TYPE;
  v_nombre_usuario  usuario_clave.nombre_usuario%TYPE;
  v_clave_usuario   usuario_clave.clave_usuario%TYPE;

  -- AUXILIARES
  v_annos_trab     NUMBER;
  v_est_norm       VARCHAR2(60);
  v_run_txt        VARCHAR2(20);
  v_tercer_dig_run VARCHAR2(1);
  v_anio_nac_mas2  VARCHAR2(4);
  v_ult3_sueldo_m1 VARCHAR2(3);
  v_letras_ap      VARCHAR2(2);
  v_mmYYYY         VARCHAR2(6);

BEGIN
  EXECUTE IMMEDIATE 'TRUNCATE TABLE USUARIO_CLAVE';

  v_id_emp := c_id_min;

  WHILE v_id_emp <= c_id_max LOOP

    SELECT  e.numrun_emp,
            e.dvrun_emp,
            e.appaterno_emp,
            e.apmaterno_emp,
            e.pnombre_emp,
            e.snombre_emp,
            e.sueldo_base,
            e.fecha_nac,
            e.fecha_contrato,
            ec.nombre_estado_civil
      INTO  v_numrun_emp,
            v_dvrun_emp,
            v_appaterno,
            v_apmaterno,
            v_pnombre,
            v_snombre,
            v_sueldo_base,
            v_fecha_nac,
            v_fecha_cont,
            v_nom_est_civil
      FROM  empleado e
      JOIN  estado_civil ec ON ec.id_estado_civil = e.id_estado_civil
     WHERE  e.id_emp = v_id_emp;

    v_nombre_empleado :=
        TRIM(v_pnombre)
        || ' ' || TRIM(NVL(v_snombre, ''))
        || ' ' || TRIM(v_appaterno)
        || ' ' || TRIM(v_apmaterno);

    v_annos_trab := TRUNC(MONTHS_BETWEEN(v_fecha_proceso, v_fecha_cont) / 12);
    v_annos_trab := GREATEST(0, v_annos_trab);

    -- NOMBRE_USUARIO:
    --  a) 1RA LETRA MINUSCULA
    --  b) 3 PRIMERAS LETRAS DEL PRIMER NOMBRE
    --  c) LARGO DEL PRIMER NOMBRE
    --  d) '*'
    --  e) ULTIMO DIGITO SUELDO BASE
    --  f) DIGITO VERIFICADOR
    --  g) AÑOS TRABAJADOS
    --  h) SI AÑO SUPERIOR A 10 AGREGAR ´K´
    v_nombre_usuario :=
        LOWER(SUBSTR(v_nom_est_civil, 1, 1))
        || SUBSTR(UPPER(v_pnombre), 1, 3)
        || TO_CHAR(LENGTH(v_pnombre))
        || '*'
        || TO_CHAR(MOD(v_sueldo_base, 10))
        || v_dvrun_emp
        || TRIM(TO_CHAR(v_annos_trab))
        || CASE WHEN v_annos_trab < 10 THEN 'X' ELSE '' END;

    -- CLAVE_USUARIO:
    --  a) 3ER DIGITO DEL RUT
    --  b) AÑO NACIMIENTO MAS 2
    --  c) 3 ULTIMOS DIGITOS SUELDO BASE MENOS 1
    --  d) 2 LETRAS APELLIDO PATERNO (DEPENDE DEL ESTADO CIVIL)
    --  e) IDEN. DEL EMPLEADO
    --  f) MES Y AÑO DEL PROCESO
    v_run_txt        := TO_CHAR(v_numrun_emp);
    v_tercer_dig_run := SUBSTR(v_run_txt, 3, 1);
    v_anio_nac_mas2  := TO_CHAR(EXTRACT(YEAR FROM v_fecha_nac) + 2);
    v_ult3_sueldo_m1 := LPAD(TO_CHAR(MOD((v_sueldo_base - 1), 1000)), 3, '0');

    v_est_norm := UPPER(TRIM(v_nom_est_civil));
    v_est_norm := TRANSLATE(v_est_norm, 'ÁÉÍÓÚÜÑ', 'AEIOUUN');

    IF INSTR(v_est_norm, 'CASAD') > 0
       OR (INSTR(v_est_norm, 'ACUERDO') > 0 AND INSTR(v_est_norm, 'UNION') > 0) THEN
      v_letras_ap := LOWER(SUBSTR(v_appaterno, 1, 2)); -- CASADO / UNION CIVIL
    ELSIF INSTR(v_est_norm, 'DIVOR') > 0 OR INSTR(v_est_norm, 'SOLTER') > 0 THEN
      v_letras_ap := LOWER(SUBSTR(v_appaterno, 1, 1) || SUBSTR(v_appaterno, -1, 1)); -- PRIMERA Y ULTIMA
    ELSIF INSTR(v_est_norm, 'VIUD') > 0 THEN
      v_letras_ap := LOWER(SUBSTR(v_appaterno, -3, 1) || SUBSTR(v_appaterno, -2, 1)); -- ANTEPENULTIMA Y PENULTIMA
    ELSIF INSTR(v_est_norm, 'SEPAR') > 0 THEN
      v_letras_ap := LOWER(SUBSTR(v_appaterno, -2, 2)); -- DOS ULTIMAS
    ELSE
      v_letras_ap := LOWER(SUBSTR(v_appaterno, 1, 2));
    END IF;

    v_mmYYYY := TO_CHAR(v_fecha_proceso, 'MMYYYY');

    v_clave_usuario :=
        v_tercer_dig_run
        || v_anio_nac_mas2
        || v_ult3_sueldo_m1
        || v_letras_ap
        || TO_CHAR(v_id_emp)
        || v_mmYYYY;

    INSERT INTO usuario_clave
      (id_emp, numrun_emp, dvrun_emp, nombre_empleado, nombre_usuario, clave_usuario)
    VALUES
      (v_id_emp, v_numrun_emp, v_dvrun_emp, v_nombre_empleado, v_nombre_usuario, v_clave_usuario);

    v_iter_ok := v_iter_ok + 1;

    v_id_emp := v_id_emp + c_step;

  END LOOP;

  IF v_iter_ok = v_total_esperado THEN
    COMMIT;
  ELSE
    ROLLBACK;
    RAISE_APPLICATION_ERROR(-20001,
      'Proceso incompleto: iter_ok=' || v_iter_ok || ' total_esperado=' || v_total_esperado);
  END IF;

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    ROLLBACK;
    RAISE_APPLICATION_ERROR(-20002, 'No existe empleado para id_emp=' || TO_CHAR(v_id_emp));
  WHEN OTHERS THEN
    ROLLBACK;
    RAISE;
END;
/

 -- CONSULTA
SELECT
  id_emp,
  numrun_emp,
  dvrun_emp,
  nombre_empleado,
  nombre_usuario,
  clave_usuario
FROM usuario_clave
ORDER BY id_emp ASC;





