-- CASO 1
CREATE OR REPLACE TRIGGER tr_total_consumos
AFTER INSERT OR UPDATE OR DELETE ON consumo
FOR EACH ROW
DECLARE
  PROCEDURE aplicar_delta(p_id_huesped NUMBER, p_delta NUMBER) IS
  BEGIN
    UPDATE total_consumos
       SET monto_consumos = ROUND(NVL(monto_consumos, 0) + p_delta)
     WHERE id_huesped = p_id_huesped;

    IF SQL%ROWCOUNT = 0 THEN
      INSERT INTO total_consumos (id_huesped, monto_consumos)
      VALUES (p_id_huesped, ROUND(p_delta));
    END IF;
  END aplicar_delta;

BEGIN
  IF INSERTING THEN
    aplicar_delta(:NEW.id_huesped, ROUND(:NEW.monto));

  ELSIF DELETING THEN
    aplicar_delta(:OLD.id_huesped, -ROUND(:OLD.monto));

  ELSIF UPDATING THEN
    IF :NEW.id_huesped = :OLD.id_huesped THEN
      aplicar_delta(:OLD.id_huesped, ROUND(:NEW.monto) - ROUND(:OLD.monto));
    ELSE
      aplicar_delta(:OLD.id_huesped, -ROUND(:OLD.monto));
      aplicar_delta(:NEW.id_huesped,  ROUND(:NEW.monto));
    END IF;
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    RAISE;
END;
/

DECLARE
  v_id_nuevo consumo.id_consumo%TYPE;
BEGIN
  SELECT NVL(MAX(id_consumo), 0) + 1
    INTO v_id_nuevo
    FROM consumo;

  INSERT INTO consumo (id_consumo, id_reserva, id_huesped, monto)
  VALUES (v_id_nuevo, 1587, 340006, 150);

  DELETE FROM consumo
   WHERE id_consumo = 11473;

  UPDATE consumo
     SET monto = 95
   WHERE id_consumo = 10688;

  COMMIT; 
END;
/

-- CONSULTAS
SELECT id_consumo, id_reserva, id_huesped, monto
FROM consumo
WHERE id_huesped IN (340003, 340004, 340006, 340008, 340009)
ORDER BY id_consumo;

SELECT id_huesped, monto_consumos
FROM total_consumos
WHERE id_huesped IN (340003, 340004, 340006, 340008, 340009)
ORDER BY id_huesped;

-- CASO 2
-- AUXILIAR
CREATE OR REPLACE PROCEDURE pr_registrar_error (
  p_nomsubprograma IN VARCHAR2,
  p_msg_error      IN VARCHAR2
) AS
BEGIN
  INSERT INTO reg_errores (id_error, nomsubprograma, msg_error)
  VALUES (sq_error.NEXTVAL, p_nomsubprograma, SUBSTR(p_msg_error, 1, 300));
END;
/

CREATE OR REPLACE PACKAGE pkg_cobranza AS
  g_monto_tours_usd NUMBER;

  FUNCTION fn_monto_tours_usd (p_id_huesped IN NUMBER)
    RETURN NUMBER;
END pkg_cobranza;
/

CREATE OR REPLACE PACKAGE BODY pkg_cobranza AS
  FUNCTION fn_monto_tours_usd (p_id_huesped IN NUMBER)
    RETURN NUMBER
  IS
    v_monto NUMBER;
  BEGIN
    SELECT NVL(SUM(t.valor_tour * NVL(ht.num_personas, 1)), 0)
      INTO v_monto
      FROM huesped_tour ht
      JOIN tour t ON t.id_tour = ht.id_tour
     WHERE ht.id_huesped = p_id_huesped;

    g_monto_tours_usd := ROUND(v_monto);
    RETURN g_monto_tours_usd;

  EXCEPTION
    WHEN OTHERS THEN
      g_monto_tours_usd := 0;
      RETURN 0;
  END fn_monto_tours_usd;
END pkg_cobranza;
/

-- FUNCIONES
CREATE OR REPLACE FUNCTION fn_agencia (p_id_huesped IN NUMBER)
  RETURN VARCHAR2
IS
  v_agencia agencia.nom_agencia%TYPE;
BEGIN
  SELECT a.nom_agencia
    INTO v_agencia
    FROM huesped h
    JOIN agencia a ON a.id_agencia = h.id_agencia
   WHERE h.id_huesped = p_id_huesped;

  RETURN v_agencia;

EXCEPTION
  WHEN OTHERS THEN
    pr_registrar_error(
      'Error en la función FN AGENCIA al recuperar la agencia del huesped con Id ' || p_id_huesped,
      SQLERRM
    );
    RETURN 'NO REGISTRA AGENCIA';
END;
/

CREATE OR REPLACE FUNCTION fn_consumos (p_id_huesped IN NUMBER)
  RETURN NUMBER
IS
  v_consumos NUMBER;
BEGIN
  SELECT monto_consumos
    INTO v_consumos
    FROM total_consumos
   WHERE id_huesped = p_id_huesped;

  RETURN ROUND(NVL(v_consumos, 0));

EXCEPTION
  WHEN NO_DATA_FOUND THEN
    pr_registrar_error(
      'Error en la función FN CONSUMOS al recuperar los consumos del cliente con Id ' || p_id_huesped,
      SQLERRM
    );
    RETURN 0;

  WHEN OTHERS THEN
    pr_registrar_error(
      'Error en la función FN CONSUMOS al recuperar los consumos del cliente con Id ' || p_id_huesped,
      SQLERRM
    );
    RETURN 0;
END;
/

-- PROCEDIMIENTO

CREATE OR REPLACE PROCEDURE sp_generar_detalle_diario (
  p_fecha_proceso IN DATE,
  p_tipo_cambio   IN NUMBER
) IS
  CURSOR c_salidas IS
    SELECT r.id_reserva, r.id_huesped, r.estadia
      FROM reserva r
     WHERE TRUNC(r.ingreso) + r.estadia = TRUNC(p_fecha_proceso);

  v_nombre              VARCHAR2(60);
  v_agencia             VARCHAR2(40);

  -- USD 
  v_base_aloj_usd       NUMBER;
  v_cargo_persona_usd   NUMBER;
  v_personas            NUMBER;
  v_alojamiento_usd     NUMBER;

  v_consumos_usd        NUMBER;
  v_tours_usd           NUMBER;

  v_subtotal_usd        NUMBER;

  v_pct_tramo           NUMBER;
  v_pct_norm            NUMBER;
  v_desc_consumos_usd   NUMBER;

  -- CLP 
  v_alojamiento_clp     NUMBER;
  v_consumos_clp        NUMBER;
  v_tours_clp           NUMBER;
  v_subtotal_clp        NUMBER;
  v_desc_consumos_clp   NUMBER;
  v_desc_agencia_clp    NUMBER;
  v_total_clp           NUMBER;

BEGIN
  -- LIMPIEZA
  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE detalle_diario_huespedes';
  EXCEPTION WHEN OTHERS THEN
    DELETE FROM detalle_diario_huespedes;
  END;

  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE reg_errores';
  EXCEPTION WHEN OTHERS THEN
    DELETE FROM reg_errores;
  END;

  FOR r IN c_salidas LOOP

    SELECT SUBSTR(h.nom_huesped || ' ' || h.appat_huesped || ' ' || h.apmat_huesped, 1, 60)
      INTO v_nombre
      FROM huesped h
     WHERE h.id_huesped = r.id_huesped;

    v_agencia := fn_agencia(r.id_huesped);

    SELECT ROUND(NVL(SUM((ha.valor_habitacion + ha.valor_minibar) * r.estadia), 0))
      INTO v_base_aloj_usd
      FROM detalle_reserva dr
      JOIN habitacion ha ON ha.id_habitacion = dr.id_habitacion
     WHERE dr.id_reserva = r.id_reserva;

    SELECT NVL(MAX(ht.num_personas), 1)
      INTO v_personas
      FROM huesped_tour ht
     WHERE ht.id_huesped = r.id_huesped;

    -- $35.000 CLP por persona 
    v_cargo_persona_usd := ROUND(35000 / p_tipo_cambio);

    v_alojamiento_usd := ROUND(v_base_aloj_usd + (v_cargo_persona_usd * v_personas));

    v_consumos_usd := ROUND(fn_consumos(r.id_huesped));
    v_tours_usd    := ROUND(pkg_cobranza.fn_monto_tours_usd(r.id_huesped));

    -- ✅ SUBTOTAL
    v_subtotal_usd := ROUND(v_alojamiento_usd + v_consumos_usd);

    BEGIN
      SELECT tc.pct
        INTO v_pct_tramo
        FROM tramos_consumos tc
       WHERE v_consumos_usd BETWEEN tc.vmin_tramo AND tc.vmax_tramo;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN v_pct_tramo := 0;
      WHEN OTHERS THEN v_pct_tramo := 0;
    END;

    v_pct_norm := CASE WHEN v_pct_tramo > 1 THEN v_pct_tramo/100 ELSE v_pct_tramo END;

    v_desc_consumos_usd := ROUND(v_consumos_usd * v_pct_norm);

    v_alojamiento_clp   := v_alojamiento_usd   * p_tipo_cambio;
    v_consumos_clp      := v_consumos_usd      * p_tipo_cambio;
    v_tours_clp         := v_tours_usd         * p_tipo_cambio;

    v_subtotal_clp      := v_subtotal_usd      * p_tipo_cambio;
    v_desc_consumos_clp := v_desc_consumos_usd * p_tipo_cambio;

    IF UPPER(v_agencia) = 'VIAJES ALBERTI' THEN
      v_desc_agencia_clp := ROUND(v_subtotal_clp * 0.12);
    ELSE
      v_desc_agencia_clp := 0;
    END IF;

    -- ✅ TOTAL
    v_total_clp := (v_subtotal_clp + v_tours_clp) - v_desc_consumos_clp - v_desc_agencia_clp;

    INSERT INTO detalle_diario_huespedes (
      id_huesped, nombre, agencia,
      alojamiento, consumos, tours,
      subtotal_pago, descuento_consumos, descuentos_agencia, total
    ) VALUES (
      r.id_huesped, v_nombre, v_agencia,
      v_alojamiento_clp, v_consumos_clp, v_tours_clp,
      v_subtotal_clp, v_desc_consumos_clp, v_desc_agencia_clp, v_total_clp
    );

  END LOOP;

  COMMIT;
END;
/

-- EJECUCION
BEGIN
  sp_generar_detalle_diario(
    p_fecha_proceso => TO_DATE('18/08/2021','DD/MM/YYYY'),
    p_tipo_cambio   => 915
  );
END;
/

-- CONSULTAS
SELECT
  id_huesped,
  nombre,
  agencia,
  alojamiento,
  consumos,
  subtotal_pago,
  descuento_consumos,
  descuentos_agencia,
  total
FROM detalle_diario_huespedes
ORDER BY id_huesped;

SELECT id_error, nomsubprograma, msg_error
FROM reg_errores
ORDER BY id_error;


-- CONSULTA DE REVISION DE FECHAS DE LA FIGURA 3
SELECT *
FROM reserva
WHERE id_huesped IN (
  340008, 340015, 340037, 340049, 340096,
  340121, 340133, 340138, 340150, 340157
)
ORDER BY ingreso, id_huesped, id_reserva;