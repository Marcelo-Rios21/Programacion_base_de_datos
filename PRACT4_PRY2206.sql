-- CASO 1

VAR b_fecha_ejecucion VARCHAR2(10)
EXEC :b_fecha_ejecucion := '15-01-2026'  

VAR b_tramo1_inf NUMBER
VAR b_tramo1_sup NUMBER
VAR b_tramo2_inf NUMBER
VAR b_tramo2_sup NUMBER
VAR b_tramo3_inf NUMBER

EXEC :b_tramo1_inf := 500000
EXEC :b_tramo1_sup := 700000
EXEC :b_tramo2_inf := 700001
EXEC :b_tramo2_sup := 900000
EXEC :b_tramo3_inf := 900001

SET SERVEROUTPUT ON

DECLARE

  v_fecha_ejecucion DATE :=
    CASE
      WHEN :b_fecha_ejecucion IS NULL THEN SYSDATE
      ELSE TO_DATE(:b_fecha_ejecucion, 'DD-MM-YYYY')
    END;

  v_ini_anio_ant DATE := ADD_MONTHS(TRUNC(v_fecha_ejecucion, 'YYYY'), -12);
  v_fin_anio_ant DATE := TRUNC(v_fecha_ejecucion, 'YYYY');


  v_tramo1_inf NUMBER := :b_tramo1_inf;
  v_tramo1_sup NUMBER := :b_tramo1_sup;
  v_tramo2_inf NUMBER := :b_tramo2_inf;
  v_tramo2_sup NUMBER := :b_tramo2_sup;
  v_tramo3_inf NUMBER := :b_tramo3_inf;


  TYPE t_varray_puntos IS VARRAY(4) OF NUMBER;
  v_puntos t_varray_puntos := t_varray_puntos(250, 300, 550, 700);


  TYPE t_total_anual_tab IS TABLE OF NUMBER INDEX BY PLS_INTEGER;
  v_total_anual_por_run t_total_anual_tab;


  TYPE t_ref_cursor IS REF CURSOR;
  rc_meses t_ref_cursor;
  v_mes DATE;


  CURSOR c_trans_mes(p_mes DATE) IS
    SELECT
      c.numrun                  AS numrun,
      c.dvrun                   AS dvrun,
      tc.nro_tarjeta            AS nro_tarjeta,
      tt.nro_transaccion        AS nro_transaccion,
      tt.fecha_transaccion      AS fecha_transaccion,
      ttt.nombre_tptran_tarjeta AS tipo_transaccion,
      tt.monto_transaccion      AS monto_transaccion,
      tcl.nombre_tipo_cliente   AS nombre_tipo_cliente,
      CAST(NULL AS NUMBER)      AS puntos_allthebest
    FROM transaccion_tarjeta_cliente tt
         JOIN tarjeta_cliente tc
           ON tc.nro_tarjeta = tt.nro_tarjeta
         JOIN cliente c
           ON c.numrun = tc.numrun
         JOIN tipo_transaccion_tarjeta ttt
           ON ttt.cod_tptran_tarjeta = tt.cod_tptran_tarjeta
         JOIN tipo_cliente tcl
           ON tcl.cod_tipo_cliente = c.cod_tipo_cliente
    WHERE tt.fecha_transaccion >= p_mes
      AND tt.fecha_transaccion <  ADD_MONTHS(p_mes, 1)
      AND tt.fecha_transaccion >= v_ini_anio_ant
      AND tt.fecha_transaccion <  v_fin_anio_ant
    ORDER BY
      TRUNC(tt.fecha_transaccion,'MM') ASC,
      tt.fecha_transaccion ASC,
      c.numrun ASC,
      tt.nro_transaccion ASC;


  TYPE r_transaccion IS RECORD (
    numrun              NUMBER(10),
    dvrun               VARCHAR2(1),
    nro_tarjeta         NUMBER(30),
    nro_transaccion     NUMBER(10),
    fecha_transaccion   DATE,
    tipo_transaccion    VARCHAR2(50),
    monto_transaccion   NUMBER(10),
    nombre_tipo_cliente VARCHAR2(30),
    puntos_allthebest   NUMBER(10)
  );

  TYPE t_trans_tab IS TABLE OF r_transaccion;
  l_trans t_trans_tab;


  v_limit CONSTANT PLS_INTEGER := 500;

  v_monto_compras   NUMBER := 0;
  v_puntos_compras  NUMBER := 0;
  v_monto_avances   NUMBER := 0;
  v_puntos_avances  NUMBER := 0;
  v_monto_savances  NUMBER := 0;
  v_puntos_savances NUMBER := 0;

  v_bloques_100k        NUMBER := 0;
  v_puntos_norm         NUMBER := 0;
  v_puntos_extra        NUMBER := 0;
  v_extra_por_100k      NUMBER := 0;
  v_total_anual_cliente NUMBER := 0;

  v_total_det_insert NUMBER := 0;
  v_total_res_insert NUMBER := 0;


  FUNCTION es_cliente_con_extra(p_nombre_tipo_cliente VARCHAR2) RETURN BOOLEAN IS
  BEGIN
    RETURN REGEXP_LIKE(UPPER(NVL(p_nombre_tipo_cliente, '')), 'DUE|PENSION|TERCERA');
  END;

  FUNCTION categoria_transaccion(p_tipo_transaccion VARCHAR2) RETURN VARCHAR2 IS
    v_txt VARCHAR2(200) := UPPER(NVL(p_tipo_transaccion,''));
  BEGIN
    IF REGEXP_LIKE(v_txt, 'SUPER') THEN
      RETURN 'SAVANCE';
    ELSIF REGEXP_LIKE(v_txt, 'AVANCE') THEN
      RETURN 'AVANCE';
    ELSE
      RETURN 'COMPRA';
    END IF;
  END;

BEGIN

  IF v_tramo1_inf IS NULL OR v_tramo1_sup IS NULL
     OR v_tramo2_inf IS NULL OR v_tramo2_sup IS NULL
     OR v_tramo3_inf IS NULL THEN
    RAISE_APPLICATION_ERROR(-20001, 'Tramos paramétricos incompletos (BIND).');
  END IF;

  IF v_tramo1_inf > v_tramo1_sup OR v_tramo2_inf > v_tramo2_sup THEN
    RAISE_APPLICATION_ERROR(-20002, 'Tramos paramétricos inválidos (inf > sup).');
  END IF;


  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTOS_TARJETA_CATB';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_PUNTOS_TARJETA_CATB';


  FOR r IN (
    SELECT c.numrun AS numrun,
           SUM(tt.monto_transaccion) AS total_anual
    FROM transaccion_tarjeta_cliente tt
         JOIN tarjeta_cliente tc ON tc.nro_tarjeta = tt.nro_tarjeta
         JOIN cliente c          ON c.numrun = tc.numrun
    WHERE tt.fecha_transaccion >= v_ini_anio_ant
      AND tt.fecha_transaccion <  v_fin_anio_ant
    GROUP BY c.numrun
  ) LOOP
    v_total_anual_por_run(r.numrun) := r.total_anual;
  END LOOP;


  OPEN rc_meses FOR
    SELECT DISTINCT TRUNC(tt.fecha_transaccion, 'MM') AS mes
    FROM transaccion_tarjeta_cliente tt
    WHERE tt.fecha_transaccion >= v_ini_anio_ant
      AND tt.fecha_transaccion <  v_fin_anio_ant
    ORDER BY mes ASC;

  LOOP
    FETCH rc_meses INTO v_mes;
    EXIT WHEN rc_meses%NOTFOUND;

    v_monto_compras   := 0;  v_puntos_compras  := 0;
    v_monto_avances   := 0;  v_puntos_avances  := 0;
    v_monto_savances  := 0;  v_puntos_savances := 0;

    OPEN c_trans_mes(v_mes);

    LOOP
      FETCH c_trans_mes BULK COLLECT INTO l_trans LIMIT v_limit;
      EXIT WHEN l_trans.COUNT = 0;

      FOR i IN 1 .. l_trans.COUNT LOOP
        v_bloques_100k := FLOOR(NVL(l_trans(i).monto_transaccion, 0) / 100000);
        v_puntos_norm  := v_bloques_100k * v_puntos(1);

        v_puntos_extra   := 0;
        v_extra_por_100k := 0;

        IF es_cliente_con_extra(l_trans(i).nombre_tipo_cliente) THEN
          IF v_total_anual_por_run.EXISTS(l_trans(i).numrun) THEN
            v_total_anual_cliente := v_total_anual_por_run(l_trans(i).numrun);
          ELSE
            v_total_anual_cliente := 0;
          END IF;

          IF v_total_anual_cliente BETWEEN v_tramo1_inf AND v_tramo1_sup THEN
            v_extra_por_100k := v_puntos(2);
          ELSIF v_total_anual_cliente BETWEEN v_tramo2_inf AND v_tramo2_sup THEN
            v_extra_por_100k := v_puntos(3);
          ELSIF v_total_anual_cliente >= v_tramo3_inf THEN
            v_extra_por_100k := v_puntos(4);
          ELSE
            v_extra_por_100k := 0;
          END IF;

          v_puntos_extra := v_bloques_100k * v_extra_por_100k;
        END IF;

        l_trans(i).puntos_allthebest := v_puntos_norm + v_puntos_extra;

        CASE categoria_transaccion(l_trans(i).tipo_transaccion)
          WHEN 'COMPRA' THEN
            v_monto_compras  := v_monto_compras + NVL(l_trans(i).monto_transaccion, 0);
            v_puntos_compras := v_puntos_compras + NVL(l_trans(i).puntos_allthebest, 0);
          WHEN 'AVANCE' THEN
            v_monto_avances  := v_monto_avances + NVL(l_trans(i).monto_transaccion, 0);
            v_puntos_avances := v_puntos_avances + NVL(l_trans(i).puntos_allthebest, 0);
          WHEN 'SAVANCE' THEN
            v_monto_savances  := v_monto_savances + NVL(l_trans(i).monto_transaccion, 0);
            v_puntos_savances := v_puntos_savances + NVL(l_trans(i).puntos_allthebest, 0);
        END CASE;
      END LOOP;

      FORALL i IN 1 .. l_trans.COUNT
        INSERT INTO detalle_puntos_tarjeta_catb
          (numrun, dvrun, nro_tarjeta, nro_transaccion, fecha_transaccion,
           tipo_transaccion, monto_transaccion, puntos_allthebest)
        VALUES
          (l_trans(i).numrun, l_trans(i).dvrun, l_trans(i).nro_tarjeta, l_trans(i).nro_transaccion,
           l_trans(i).fecha_transaccion, l_trans(i).tipo_transaccion, l_trans(i).monto_transaccion,
           l_trans(i).puntos_allthebest);

      v_total_det_insert := v_total_det_insert + l_trans.COUNT;
    END LOOP;

    CLOSE c_trans_mes;

    INSERT INTO resumen_puntos_tarjeta_catb
      (mes_anno,
       monto_total_compras,  total_puntos_compras,
       monto_total_avances,  total_puntos_avances,
       monto_total_savances, total_puntos_savances)
    VALUES
      (TO_CHAR(v_mes, 'MMYYYY'),
       v_monto_compras,  v_puntos_compras,
       v_monto_avances,  v_puntos_avances,
       v_monto_savances, v_puntos_savances);

    v_total_res_insert := v_total_res_insert + 1;
  END LOOP;

  CLOSE rc_meses;

  COMMIT;

  DBMS_OUTPUT.PUT_LINE('Proceso ejecutado con fecha_ejecucion = ' || TO_CHAR(v_fecha_ejecucion,'DD-MM-YYYY'));
  DBMS_OUTPUT.PUT_LINE('Se procesó año anterior: ' || TO_CHAR(v_ini_anio_ant,'YYYY') ||
                       ' (rango ' || TO_CHAR(v_ini_anio_ant,'DD-MM-YYYY') || ' a ' ||
                       TO_CHAR(v_fin_anio_ant,'DD-MM-YYYY') || ' excl.)');
  DBMS_OUTPUT.PUT_LINE('DETALLE insertadas: ' || v_total_det_insert);
  DBMS_OUTPUT.PUT_LINE('RESUMEN insertadas: ' || v_total_res_insert);

EXCEPTION
  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: ' || SQLERRM);
    ROLLBACK;
    RAISE;
END;
/

-- CONSULTA
SELECT
  numrun,
  dvrun,
  nro_tarjeta,
  nro_transaccion,
  fecha_transaccion,
  tipo_transaccion,
  monto_transaccion,
  puntos_allthebest
FROM detalle_puntos_tarjeta_catb
ORDER BY fecha_transaccion, numrun, nro_transaccion;

-- CASO 2

VAR b_fecha_ejecucion VARCHAR2(10)
EXEC :b_fecha_ejecucion := '31-12-2026'

SET SERVEROUTPUT ON

DECLARE

  v_fecha_ejecucion DATE :=
    CASE
      WHEN :b_fecha_ejecucion IS NULL THEN SYSDATE
      ELSE TO_DATE(:b_fecha_ejecucion, 'DD-MM-YYYY')
    END;

  v_ini_anio DATE := TRUNC(v_fecha_ejecucion, 'YYYY');
  v_fin_anio DATE := ADD_MONTHS(TRUNC(v_fecha_ejecucion, 'YYYY'), 12);


  TYPE r_tramo IS RECORD (tramo_inf NUMBER, tramo_sup NUMBER, porc NUMBER);
  TYPE t_tramos_tab IS TABLE OF r_tramo INDEX BY PLS_INTEGER;
  v_tramos t_tramos_tab;
  v_tramos_count PLS_INTEGER := 0;


  CURSOR c_meses IS
    SELECT DISTINCT TRUNC(tt.fecha_transaccion, 'MM') AS mes
    FROM transaccion_tarjeta_cliente tt
    JOIN tipo_transaccion_tarjeta ttt
      ON ttt.cod_tptran_tarjeta = tt.cod_tptran_tarjeta
    WHERE tt.fecha_transaccion >= v_ini_anio
      AND tt.fecha_transaccion <  v_fin_anio
      AND UPPER(ttt.nombre_tptran_tarjeta) LIKE '%AVANCE%'
    ORDER BY mes ASC;

  v_mes DATE;


  CURSOR c_trans_mes(p_mes DATE) IS
    SELECT
      c.numrun                   AS numrun,
      c.dvrun                    AS dvrun,
      tc.nro_tarjeta             AS nro_tarjeta,
      tt.nro_transaccion         AS nro_transaccion,
      tt.fecha_transaccion       AS fecha_transaccion,
      SUBSTR(ttt.nombre_tptran_tarjeta, 1, 40) AS tipo_transaccion,
      tt.monto_total_transaccion AS monto_total_transaccion,
      CAST(NULL AS NUMBER)       AS aporte_sbif
    FROM transaccion_tarjeta_cliente tt
    JOIN tarjeta_cliente tc
      ON tc.nro_tarjeta = tt.nro_tarjeta
    JOIN cliente c
      ON c.numrun = tc.numrun
    JOIN tipo_transaccion_tarjeta ttt
      ON ttt.cod_tptran_tarjeta = tt.cod_tptran_tarjeta
    WHERE tt.fecha_transaccion >= p_mes
      AND tt.fecha_transaccion <  ADD_MONTHS(p_mes, 1)
      AND tt.fecha_transaccion >= v_ini_anio
      AND tt.fecha_transaccion <  v_fin_anio
      AND UPPER(ttt.nombre_tptran_tarjeta) LIKE '%AVANCE%'
    ORDER BY tt.fecha_transaccion ASC, c.numrun ASC;


  TYPE r_trans IS RECORD (
    numrun NUMBER(10), dvrun VARCHAR2(1),
    nro_tarjeta NUMBER(30), nro_transaccion NUMBER(10),
    fecha_transaccion DATE, tipo_transaccion VARCHAR2(40),
    monto_total_transaccion NUMBER(10), aporte_sbif NUMBER(10)
  );
  TYPE t_trans_tab IS TABLE OF r_trans;
  l_trans t_trans_tab;
  v_limit CONSTANT PLS_INTEGER := 500;


  v_monto_avance  NUMBER := 0;
  v_aporte_avance NUMBER := 0;
  v_monto_super   NUMBER := 0;
  v_aporte_super  NUMBER := 0;


  v_porc   NUMBER := 0;
  v_aporte NUMBER := 0;

  v_total_det_insert NUMBER := 0;
  v_total_res_insert NUMBER := 0;



  FUNCTION es_super_avance(p_tipo VARCHAR2) RETURN BOOLEAN IS
    v_txt VARCHAR2(200) := UPPER(NVL(p_tipo,''));
  BEGIN
    RETURN REGEXP_LIKE(v_txt, 'S(Ú|U)PER');
  END;

  FUNCTION obtener_porc_aporte(p_monto_total NUMBER) RETURN NUMBER IS
  BEGIN
    FOR i IN 1 .. v_tramos_count LOOP
      IF p_monto_total BETWEEN v_tramos(i).tramo_inf AND v_tramos(i).tramo_sup THEN
        RETURN v_tramos(i).porc;
      END IF;
    END LOOP;
    RETURN NULL;
  END;

BEGIN

  FOR r IN (
    SELECT tramo_inf_av_sav tramo_inf,
           tramo_sup_av_sav tramo_sup,
           porc_aporte_sbif porc
    FROM tramo_aporte_sbif
    ORDER BY tramo_inf_av_sav
  ) LOOP
    v_tramos_count := v_tramos_count + 1;
    v_tramos(v_tramos_count).tramo_inf := r.tramo_inf;
    v_tramos(v_tramos_count).tramo_sup := r.tramo_sup;
    v_tramos(v_tramos_count).porc      := r.porc;
  END LOOP;

  IF v_tramos_count = 0 THEN
    RAISE_APPLICATION_ERROR(-20010, 'No existen tramos en TRAMO_APORTE_SBIF.');
  END IF;


  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_APORTE_SBIF';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_APORTE_SBIF';


  OPEN c_meses;
  LOOP
    FETCH c_meses INTO v_mes;
    EXIT WHEN c_meses%NOTFOUND;

    v_monto_avance  := 0; v_aporte_avance := 0;
    v_monto_super   := 0; v_aporte_super  := 0;


    OPEN c_trans_mes(v_mes);
    LOOP
      FETCH c_trans_mes BULK COLLECT INTO l_trans LIMIT v_limit;
      EXIT WHEN l_trans.COUNT = 0;

      FOR i IN 1 .. l_trans.COUNT LOOP
        v_porc := obtener_porc_aporte(NVL(l_trans(i).monto_total_transaccion,0));

        IF v_porc IS NULL THEN
          RAISE_APPLICATION_ERROR(-20011, 'Monto sin tramo: ' || l_trans(i).monto_total_transaccion);
        END IF;

        v_aporte := ROUND(NVL(l_trans(i).monto_total_transaccion,0) * v_porc / 100);
        l_trans(i).aporte_sbif := v_aporte;

        IF es_super_avance(l_trans(i).tipo_transaccion) THEN
          v_monto_super  := v_monto_super  + NVL(l_trans(i).monto_total_transaccion,0);
          v_aporte_super := v_aporte_super + NVL(l_trans(i).aporte_sbif,0);
        ELSE
          v_monto_avance  := v_monto_avance  + NVL(l_trans(i).monto_total_transaccion,0);
          v_aporte_avance := v_aporte_avance + NVL(l_trans(i).aporte_sbif,0);
        END IF;
      END LOOP;

      FORALL i IN 1 .. l_trans.COUNT
        INSERT INTO detalle_aporte_sbif
          (numrun, dvrun, nro_tarjeta, nro_transaccion, fecha_transaccion,
           tipo_transaccion, monto_transaccion, aporte_sbif)
        VALUES
          (l_trans(i).numrun, l_trans(i).dvrun, l_trans(i).nro_tarjeta, l_trans(i).nro_transaccion,
           l_trans(i).fecha_transaccion, l_trans(i).tipo_transaccion,
           l_trans(i).monto_total_transaccion, l_trans(i).aporte_sbif);

      v_total_det_insert := v_total_det_insert + l_trans.COUNT;
    END LOOP;
    CLOSE c_trans_mes;


    INSERT INTO resumen_aporte_sbif
      (mes_anno, tipo_transaccion, monto_total_transacciones, aporte_total_abif)
    VALUES
      (TO_CHAR(v_mes,'MMYYYY'), 'Avance', v_monto_avance, v_aporte_avance);

    INSERT INTO resumen_aporte_sbif
      (mes_anno, tipo_transaccion, monto_total_transacciones, aporte_total_abif)
    VALUES
      (TO_CHAR(v_mes,'MMYYYY'), 'Súper Avance', v_monto_super, v_aporte_super);

    v_total_res_insert := v_total_res_insert + 2;

  END LOOP;
  CLOSE c_meses;

  COMMIT;

  DBMS_OUTPUT.PUT_LINE('Fecha_ejecucion = ' || TO_CHAR(v_fecha_ejecucion,'DD-MM-YYYY'));
  DBMS_OUTPUT.PUT_LINE('Año procesado = ' || TO_CHAR(v_ini_anio,'YYYY'));
  DBMS_OUTPUT.PUT_LINE('DETALLE insertadas: ' || v_total_det_insert);
  DBMS_OUTPUT.PUT_LINE('RESUMEN insertadas: ' || v_total_res_insert);

EXCEPTION
  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('ERROR: ' || SQLERRM);
    ROLLBACK;
    RAISE;
END;
/


----------------------------
SELECT
  d.numrun,
  d.dvrun,
  d.nro_tarjeta,
  d.nro_transaccion,
  TO_CHAR(d.fecha_transaccion, 'DD/MM/YYYY') AS fecha_transaccion,
  d.tipo_transaccion,
  d.monto_transaccion AS monto_total_transaccion,
  d.aporte_sbif
FROM detalle_aporte_sbif d
ORDER BY
  d.fecha_transaccion ASC,
  d.numrun ASC,
  d.nro_transaccion ASC;

SELECT
  r.mes_anno,
  r.tipo_transaccion,
  r.monto_total_transacciones,
  r.aporte_total_abif
FROM resumen_aporte_sbif r
ORDER BY
  TO_DATE(r.mes_anno, 'MMYYYY') ASC,
  r.tipo_transaccion ASC;




