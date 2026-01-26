-- CASO 1
-- SE TUVO QUE VERIFICAR LOS DATOS POR CARACTERES PERDIDOS, ADJUNTO LOS CAMBIOS EN ESTE SCRIPT
----------------------------
ALTER SESSION DISABLE PARALLEL DML;

BEGIN
  -- 1) ESPECIALIDAD 
  UPDATE especialidad SET nombre = UNISTR('Cirug\00EDa General')                 WHERE esp_id = 100;
  UPDATE especialidad SET nombre = UNISTR('Ortopedia y Traumatolog\00EDa')       WHERE esp_id = 200;
  UPDATE especialidad SET nombre = UNISTR('Dermatolog\00EDa')                   WHERE esp_id = 300;
  UPDATE especialidad SET nombre = UNISTR('Inmunolog\00EDa')                    WHERE esp_id = 400;
  UPDATE especialidad SET nombre = UNISTR('Fisiatr\00EDa')                      WHERE esp_id = 500;
  UPDATE especialidad SET nombre = UNISTR('Neurolog\00EDa')                     WHERE esp_id = 800;
  UPDATE especialidad SET nombre = UNISTR('Otorrinolaringolog\00EDa')           WHERE esp_id = 900;
  UPDATE especialidad SET nombre = UNISTR('Oftalmolog\00EDa')                   WHERE esp_id = 1000;
  UPDATE especialidad SET nombre = UNISTR('Psiquiatr\00EDa Adultos')            WHERE esp_id = 1100;
  UPDATE especialidad SET nombre = UNISTR('Urolog\00EDa')                       WHERE esp_id = 1200;
  UPDATE especialidad SET nombre = UNISTR('Cirug\00EDa Cardiovascular')         WHERE esp_id = 1300;
  UPDATE especialidad SET nombre = UNISTR('Cirug\00EDa Digestiva')              WHERE esp_id = 1400;
  UPDATE especialidad SET nombre = UNISTR('Cardiolog\00EDa')                    WHERE esp_id = 1500;
  UPDATE especialidad SET nombre = UNISTR('Gastroenterolog\00EDa')              WHERE esp_id = 1600;
  UPDATE especialidad SET nombre = UNISTR('Oncolog\00EDa M\00E9dica')            WHERE esp_id = 1700;
  UPDATE especialidad SET nombre = UNISTR('Reumatolog\00EDa')                   WHERE esp_id = 1800;
  UPDATE especialidad SET nombre = UNISTR('Cirug\00EDa Pl\00E1stica')            WHERE esp_id = 1900;

  -- 2) CARGO 
  UPDATE cargo SET nombre = UNISTR('Presidente junta m\00E9dica')               WHERE car_id = 100;
  UPDATE cargo SET nombre = UNISTR('M\00E9dico auditor')                        WHERE car_id = 200;
  UPDATE cargo SET nombre = UNISTR('M\00E9dico atenci\00F3n adulto')             WHERE car_id = 300;
  UPDATE cargo SET nombre = UNISTR('Director m\00E9dico')                       WHERE car_id = 600;
  UPDATE cargo SET nombre = UNISTR('M\00E9dico paciente cr\00EDtico')            WHERE car_id = 700;
  UPDATE cargo SET nombre = UNISTR('M\00E9dico atenci\00F3n urgencia')           WHERE car_id = 800;
  UPDATE cargo SET nombre = UNISTR('M\00E9dico Cirujano')                        WHERE car_id = 900;
  -- 3) UNIDAD 
  UPDATE unidad SET nombre = UNISTR('ATENCI\00D3N AMBULATORIA')                 WHERE uni_id = 100;
  UPDATE unidad SET nombre = UNISTR('ATENCI\00D3N URGENCIA')                    WHERE uni_id = 200;
  UPDATE unidad SET nombre = UNISTR('PACIENTE CR\00CDTICO')                     WHERE uni_id = 300;
  UPDATE unidad SET nombre = UNISTR('ATENCI\00D3N ADULTO')                      WHERE uni_id = 400;
  UPDATE unidad SET nombre = UNISTR('ONCOL\00D3GICA')                           WHERE uni_id = 500;
  UPDATE unidad SET nombre = UNISTR('PSIQUIATR\00CDA Y SALUD MENTAL')           WHERE uni_id = 600;
  UPDATE unidad SET nombre = UNISTR('CIRUG\00CDA')                              WHERE uni_id = 700;
  UPDATE unidad SET nombre = UNISTR('CIRUG\00CDA PL\00C1STICA')                 WHERE uni_id = 800;
  UPDATE unidad SET nombre = UNISTR('CARDIOLOG\00CDA')                          WHERE uni_id = 900;
  UPDATE unidad SET nombre = UNISTR('TRAUMATOLOG\00CDA ADULTO')                 WHERE uni_id = 1000;

  -- 4) PACIENTE 
  -- Apaterno: Muñoz
  UPDATE paciente
  SET apaterno = UNISTR('Mu\00F1oz')
  WHERE apaterno = 'Mu' || UNISTR('\FFFD') || 'oz';
  -- Apaterno: Peña
  UPDATE paciente
  SET apaterno = UNISTR('Pe\00F1a')
  WHERE apaterno = 'Pe' || UNISTR('\FFFD') || 'a';
  -- Amaterno: Acuña
  UPDATE paciente
  SET amaterno = UNISTR('Acu\00F1a')
  WHERE amaterno = 'Acu' || UNISTR('\FFFD') || 'a';

  -- Ormeño
  UPDATE paciente
  SET snombre = UNISTR('Orme\00F1o')
  WHERE snombre = 'Orme' || UNISTR('\FFFD') || 'o';

  UPDATE paciente
  SET apaterno = UNISTR('Orme\00F1o')
  WHERE apaterno = 'Orme' || UNISTR('\FFFD') || 'o';

  UPDATE paciente
  SET amaterno = UNISTR('Orme\00F1o')
  WHERE amaterno = 'Orme' || UNISTR('\FFFD') || 'o';

  -- Amaterno: Muñoz 
  UPDATE paciente
  SET amaterno = UNISTR('Mu\00F1oz')
  WHERE amaterno = 'Mu' || UNISTR('\FFFD') || 'oz';

  -- Amaterno: Peña 
  UPDATE paciente
  SET amaterno = UNISTR('Pe\00F1a')
  WHERE amaterno = 'Pe' || UNISTR('\FFFD') || 'a';

  -- Apaterno: Guiñez 
  UPDATE paciente
  SET apaterno = UNISTR('Gui\00F1ez')
  WHERE apaterno = 'Gui' || UNISTR('\FFFD') || 'ez';

  COMMIT;
END;
/

----------------------------


VAR p_anno_acreditacion NUMBER
EXEC :p_anno_acreditacion := 2026

SET SERVEROUTPUT ON

DECLARE

  v_anno_acreditacion NUMBER := :p_anno_acreditacion;
  v_anno_informar     NUMBER;

  -- Fechas dinámicas
  v_fec_ini_anno   DATE;
  v_fec_ini_sig    DATE;
  v_fec_ref_edad   DATE;

  TYPE t_multas_varray IS VARRAY(7) OF NUMBER(6);
  v_multas t_multas_varray := t_multas_varray(1200,1300,1700,1900,1100,2000,2300);

  CURSOR c_morosos IS
    SELECT
      p.pac_run,
      p.dv_run                          AS pac_dv_run,
      p.pnombre, p.snombre, p.apaterno, p.amaterno,
      p.fecha_nacimiento,
      a.ate_id,
      pa.fecha_venc_pago,
      pa.fecha_pago,
      e.nombre                          AS especialidad_atencion
    FROM pago_atencion pa
      JOIN atencion a      ON a.ate_id  = pa.ate_id
      JOIN paciente p      ON p.pac_run = a.pac_run
      JOIN especialidad e  ON e.esp_id  = a.esp_id
    WHERE
      pa.fecha_pago IS NOT NULL
      AND TRUNC(pa.fecha_pago) > TRUNC(pa.fecha_venc_pago)
      AND pa.fecha_venc_pago >= v_fec_ini_anno
      AND pa.fecha_venc_pago <  v_fec_ini_sig
    ORDER BY
      pa.fecha_venc_pago ASC,
      p.apaterno ASC;

  r c_morosos%ROWTYPE;

  v_dias_morosidad  PAGO_MOROSO.dias_morosidad%TYPE;
  v_multa_dia       NUMBER(6);
  v_multa_base      NUMBER(15);
  v_pct_desc        NUMBER(10);
  v_multa_final     PAGO_MOROSO.monto_multa%TYPE;
  v_edad            NUMBER(3);
  v_nombre_pac      PAGO_MOROSO.pac_nombre%TYPE;
  v_total_insert    NUMBER := 0;

  FUNCTION fn_norm(p_txt VARCHAR2) RETURN VARCHAR2 IS
  BEGIN
    RETURN TRANSLATE(
             UPPER(TRIM(p_txt)),
             'ÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛÑ',
             'AAAAEEEEIIIIOOOOUUUUN'
           );
  END fn_norm;

  FUNCTION fn_multa_dia(p_especialidad VARCHAR2) RETURN NUMBER IS
    v_esp VARCHAR2(200) := fn_norm(p_especialidad);
  BEGIN
    IF v_esp IN ('CIRUGIA GENERAL','DERMATOLOGIA') THEN
      RETURN v_multas(1);
    ELSIF v_esp IN ('ORTOPEDIA Y TRAUMATOLOGIA') THEN
      RETURN v_multas(2);
    ELSIF v_esp IN ('INMUNOLOGIA','OTORRINOLARINGOLOGIA') THEN
      RETURN v_multas(3);
    ELSIF v_esp IN ('FISIATRIA','MEDICINA INTERNA') THEN
      RETURN v_multas(4);
    ELSIF v_esp IN ('MEDICINA GENERAL') THEN
      RETURN v_multas(5);
    ELSIF v_esp IN ('PSIQUIATRIA ADULTOS') THEN
      RETURN v_multas(6);
    ELSIF v_esp IN ('CIRUGIA DIGESTIVA','REUMATOLOGIA') THEN
      RETURN v_multas(7);
    ELSE
      RETURN 0;
    END IF;
  END fn_multa_dia;

BEGIN
  v_anno_informar := v_anno_acreditacion - 1;

  v_fec_ini_anno := TRUNC(TO_DATE('01-01-' || v_anno_informar, 'DD-MM-YYYY'));
  v_fec_ini_sig  := ADD_MONTHS(v_fec_ini_anno, 12);
  v_fec_ref_edad := v_fec_ini_sig - 1;

  DBMS_OUTPUT.PUT_LINE('Año acreditación: '||v_anno_acreditacion);
  DBMS_OUTPUT.PUT_LINE('Año a informar: '||v_anno_informar);

  EXECUTE IMMEDIATE 'TRUNCATE TABLE PAGO_MOROSO';

  OPEN c_morosos;
  LOOP
    FETCH c_morosos INTO r;
    EXIT WHEN c_morosos%NOTFOUND;

    v_dias_morosidad := TRUNC(r.fecha_pago) - TRUNC(r.fecha_venc_pago);
    v_multa_dia      := fn_multa_dia(r.especialidad_atencion);

    v_edad := TRUNC(MONTHS_BETWEEN(v_fec_ref_edad, r.fecha_nacimiento) / 12);

    SELECT MAX(porcentaje_descto)
      INTO v_pct_desc
    FROM porc_descto_3ra_edad
    WHERE v_edad BETWEEN anno_ini AND anno_ter;

    v_pct_desc   := NVL(v_pct_desc, 0);
    v_multa_base := v_dias_morosidad * v_multa_dia;

    IF v_pct_desc > 0 THEN
      v_multa_final := ROUND(v_multa_base * (1 - (v_pct_desc / 100)), 0);
    ELSE
      v_multa_final := ROUND(v_multa_base, 0);
    END IF;

    v_nombre_pac := SUBSTR(
      TRIM(r.pnombre) || ' ' || TRIM(r.snombre) || ' ' || TRIM(r.apaterno) || ' ' || TRIM(r.amaterno),
      1, 50
    );

    INSERT INTO pago_moroso
      (pac_run, pac_dv_run, pac_nombre,
       ate_id, fecha_venc_pago, fecha_pago,
       dias_morosidad, especialidad_atencion, monto_multa)
    VALUES
      (r.pac_run, r.pac_dv_run, v_nombre_pac,
       r.ate_id, r.fecha_venc_pago, r.fecha_pago,
       v_dias_morosidad, SUBSTR(r.especialidad_atencion,1,30), v_multa_final);

    v_total_insert := v_total_insert + 1;
  END LOOP;
  CLOSE c_morosos;

  COMMIT;
  DBMS_OUTPUT.PUT_LINE('Insertados: '||v_total_insert);

EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR: '||SQLERRM);
    RAISE;
END;
/

-- CONSULTA
SELECT COUNT(*) FROM pago_moroso;
SELECT * FROM pago_moroso ORDER BY fecha_venc_pago, pac_nombre;

-- CASO 2

DECLARE
  v_fec_ini_anno  DATE;
  v_fec_ini_act   DATE;
  v_anno_base     NUMBER(4);

  TYPE t_dest_varray IS VARRAY(4) OF VARCHAR2(60);
  v_dest t_dest_varray := t_dest_varray(
    'Servicio de Atención Primaria de Urgencia (SAPU)',  -- (1)
    'Centros de Salud Familiar (CESFAM)',                 -- (2)
    'Consultorios Generales',                             -- (3) 
    'Hospitales del área de la Salud Pública'             -- (4)
  );

  v_max_atenciones NUMBER := 0;

  CURSOR c_med IS
    SELECT
      u.nombre AS unidad,
      m.med_run,
      m.dv_run,
      m.pnombre, m.snombre, m.apaterno, m.amaterno,
      m.telefono
    FROM medico m
    JOIN unidad u ON u.uni_id = m.uni_id
    ORDER BY u.nombre ASC, m.apaterno ASC, m.pnombre ASC, m.med_run ASC;

  r c_med%ROWTYPE; 

  v_total_aten   NUMBER := 0;
  v_run_fmt      VARCHAR2(15);
  v_nombre_med   VARCHAR2(50);
  v_correo       VARCHAR2(25);
  v_destino      VARCHAR2(50);
  v_insertados   NUMBER := 0;

  FUNCTION fn_norm(p_txt VARCHAR2) RETURN VARCHAR2 IS
  BEGIN
    RETURN TRANSLATE(
             UPPER(TRIM(p_txt)),
             'ÁÀÄÂÉÈËÊÍÌÏÎÓÒÖÔÚÙÜÛÑ',
             'AAAAEEEEIIIIOOOOUUUUN'
           );
  END fn_norm;

  FUNCTION fn_destino(p_unidad VARCHAR2, p_total NUMBER) RETURN VARCHAR2 IS
    v_u VARCHAR2(80) := fn_norm(p_unidad);
  BEGIN
    IF v_u IN ('ATENCION ADULTO','ATENCION AMBULATORIA') THEN
      RETURN v_dest(1); -- SAPU

    ELSIF v_u = 'ATENCION URGENCIA' THEN
      IF p_total <= 3 THEN
        RETURN v_dest(1); -- SAPU
      ELSE
        RETURN v_dest(4); -- HOSPITALES
      END IF;

    ELSIF v_u IN ('CARDIOLOGIA','ONCOLOGICA') THEN
      RETURN v_dest(4); -- HOSPITALES

    ELSIF v_u IN ('CIRUGIA','CIRUGIA PLASTICA') THEN
      IF p_total <= 3 THEN
        RETURN v_dest(1); -- SAPU
      ELSE
        RETURN v_dest(4); -- HOSPITALES
      END IF;

    ELSIF v_u = 'PACIENTE CRITICO' THEN
      RETURN v_dest(4); -- HOSPITALES

    ELSIF v_u = 'PSIQUIATRIA Y SALUD MENTAL' THEN
      RETURN v_dest(2); -- CESFAM

    ELSIF v_u = 'TRAUMATOLOGIA ADULTO' THEN
      IF p_total <= 3 THEN
        RETURN v_dest(1); 
      ELSE
        RETURN v_dest(4); 
      END IF;

    ELSE
      RETURN v_dest(3); 
    END IF;
  END fn_destino;

  FUNCTION fn_run_formateado(p_run NUMBER, p_dv VARCHAR2) RETURN VARCHAR2 IS
    v_run8 VARCHAR2(8);
  BEGIN
    v_run8 := LPAD(TO_CHAR(p_run), 8, '0');
    RETURN SUBSTR(v_run8,1,2)||'.'||SUBSTR(v_run8,3,3)||'.'||SUBSTR(v_run8,6,3)||'-'||p_dv;
  END fn_run_formateado;

  FUNCTION fn_correo(p_unidad VARCHAR2, p_apaterno VARCHAR2, p_telefono NUMBER) RETURN VARCHAR2 IS
    v_u2   VARCHAR2(2);
    v_ap   VARCHAR2(60);
    v_ap2  VARCHAR2(2);
    v_tel7 VARCHAR2(7);
  BEGIN
    v_u2 := SUBSTR(fn_norm(p_unidad), 1, 2);

    v_ap := fn_norm(p_apaterno);
    v_ap2 := LOWER(SUBSTR(v_ap, -3, 1) || SUBSTR(v_ap, -2, 1)); 

    v_tel7 := LPAD(TO_CHAR(MOD(p_telefono, 10000000)), 7, '0'); -- 7 ULTIMOS DIGITOS

    RETURN SUBSTR(v_u2 || v_ap2 || v_tel7 || '@medicocktk.cl', 1, 25);
  END fn_correo;

BEGIN

  v_fec_ini_act  := TRUNC(SYSDATE, 'YYYY');        
  v_fec_ini_anno := ADD_MONTHS(v_fec_ini_act, -12); 
  v_anno_base    := EXTRACT(YEAR FROM v_fec_ini_anno);

  DBMS_OUTPUT.PUT_LINE('Año base (año anterior): ' || v_anno_base);
  DBMS_OUTPUT.PUT_LINE('Rango: ['||TO_CHAR(v_fec_ini_anno,'DD/MM/YYYY')||' , '||TO_CHAR(v_fec_ini_act,'DD/MM/YYYY')||')');

  EXECUTE IMMEDIATE 'TRUNCATE TABLE MEDICO_SERVICIO_COMUNIDAD';

  SELECT NVL(MAX(cnt), 0)
    INTO v_max_atenciones
  FROM (
    SELECT a.med_run, COUNT(*) AS cnt
    FROM atencion a
    WHERE a.fecha_atencion >= v_fec_ini_anno
      AND a.fecha_atencion <  v_fec_ini_act
    GROUP BY a.med_run
  );

  DBMS_OUTPUT.PUT_LINE('Máximo anual de atenciones (año base): '||v_max_atenciones);

  OPEN c_med;
  LOOP
    FETCH c_med INTO r;
    EXIT WHEN c_med%NOTFOUND;

    SELECT COUNT(*)
      INTO v_total_aten
    FROM atencion a
    WHERE a.med_run = r.med_run
      AND a.fecha_atencion >= v_fec_ini_anno
      AND a.fecha_atencion <  v_fec_ini_act;

    IF v_total_aten < v_max_atenciones THEN

      v_run_fmt := fn_run_formateado(r.med_run, r.dv_run);

      v_nombre_med := SUBSTR(
        TRIM(r.pnombre) || ' ' || TRIM(r.snombre) || ' ' || TRIM(r.apaterno) || ' ' || TRIM(r.amaterno),
        1, 50
      );

      v_correo := fn_correo(r.unidad, r.apaterno, r.telefono);

      v_destino := fn_destino(r.unidad, v_total_aten);

      INSERT INTO medico_servicio_comunidad
        (unidad, run_medico, nombre_medico, correo_institucional, total_aten_medicas, destinacion)
      VALUES
        (SUBSTR(r.unidad, 1, 50),
         v_run_fmt,
         v_nombre_med,
         v_correo,
         LEAST(v_total_aten, 99), 
         SUBSTR(v_destino, 1, 50));

      v_insertados := v_insertados + 1;
    END IF;

  END LOOP;
  CLOSE c_med;

  COMMIT;
  DBMS_OUTPUT.PUT_LINE('Registros insertados: '||v_insertados);

EXCEPTION
  WHEN OTHERS THEN
    ROLLBACK;
    DBMS_OUTPUT.PUT_LINE('ERROR: '||SQLERRM);
    RAISE;
END;
/

-- CONSULTA
SELECT msc.*
FROM medico_servicio_comunidad msc
JOIN medico m
  ON m.med_run = TO_NUMBER(REPLACE(SUBSTR(msc.run_medico,1,10),'.',''))
ORDER BY msc.unidad, m.apaterno;




