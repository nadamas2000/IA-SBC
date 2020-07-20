
                    ;;********************
                    ;;* CLASES ONTOLOGIA *
                    ;;********************

;;; Se cargan con el bat con la orden (load ontologia.pont)

                    ;;************************
                    ;;* INSTANCIAS ONTOLOGIA *
                    ;;************************
;;; Se cargan con el bat con la orden (load-instances ontologia.pins)

                    ;;******************
                    ;;* CLASES PROPIAS *
                    ;;******************

                    ;;***************
                    ;;* DEFTEMPLATE *
                    ;;***************

;;; Ejemplo de template, porque Template, porque sólo hay un user activo, igual que solución, sino tendría que ser una clase ambas
(deftemplate Usuario "plantilla de Usuario"
    (slot nombre (type STRING))
    (slot edad (type INTEGER))
    (multislot patologias (type INSTANCE))
    (multislot listSug (type INSTANCE))
    (multislot listCal (type INSTANCE))
    (multislot listEst (type INSTANCE))
    (multislot listCom (type INSTANCE))
)

(deftemplate Sesion "Dia de ejercicios"
    (multislot ejercicio (type INSTANCE))
)

;;; Falta definir bien sesion, junto lo de calentamiento y estiramientos, en función del tipo de sesion(ejercicio)
(deftemplate Planificacion  "plantilla planificacion sesiones a realizar"
    (multislot sesion)
)

                    ;;****************
                    ;;* DEFFUNCTIONS *
                    ;;****************

(deffunction finalizar () "Termina el programa"
	(printout t "---------------- Programa Finalizado ---------------- " crlf crlf)
	(halt)
)

(deffunction pregunta-general (?pregunta)
	(format t "%s" ?pregunta)
	(bind ?respuesta (read))
	?respuesta
)

(deffunction pregunta-opciones (?question $?allowed-values)
   (format t "%s "?question)
   (progn$ (?curr-value $?allowed-values)
        (format t "[%s]" ?curr-value)
    )
   (printout t ": ")
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (format t "%s "?question)
      (progn$ (?curr-value $?allowed-values)
        (format t "[%s]" ?curr-value)
      )
      (printout t ": ")
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer
)

(deffunction pregunta-opciones2 (?question $?allowed-values)
   (format t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (format t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer
)

;;; Funcion para hacer una pregunta de tipo si/no
(deffunction pregunta-si-no (?question)
   (bind ?response (pregunta-opciones2 ?question si no s n))
   (if (or (eq ?response si) (eq ?response s))
       then TRUE
       else FALSE)
)

;;; Funcion para hacer una pregunta con respuesta numerica unica
(deffunction pregunta-numerica (?pregunta ?rangini ?rangfi)
    (format t "%s [%d, %d]: " ?pregunta ?rangini ?rangfi)
    (bind ?respuesta (read))
    (while (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi))) do
        (format t "%s [%d, %d]: " ?pregunta ?rangini ?rangfi)
        (bind ?respuesta (read))
    )
    ?respuesta
)

;;; Funcion para hacer una pregunta con un conjunto definido de valores de repuesta
(deffunction pregunta-lista (?pregunta $?valores_posibles)
	(format t "%s" ?pregunta)
	(bind ?resposta (readline))
	(bind ?res (str-explode ?resposta))
	?res
)

	;;; Modulo PRINT MESSAGE && initialRule
(defmodule MAIN (export ?ALL))

(deffunction MAIN::print-bienvenida ()
	(printout t "====================================================================" crlf)
    (printout t "=         Sistema de recomendacion de sesiones de                  =" crlf)
    (printout t "=       ejerccicios para personas de la tercera edad               =" crlf)
    (printout t "====================================================================" crlf)
    (printout t crlf)
    (printout t "¡Bienvenido al sistema de prevención de Salud para la tercera edad." crlf)
    (printout t "A continuación se le formularán una serie de preguntas para poder recomendarle contenidos." crlf)
    (printout t crlf)
)

(defrule MAIN::initialRule "Regla inicial"
	(initial-fact)
    =>
    (print-bienvenida)
    (focus seleccion-usuario)
)


	;;; Modulo DATOS_USUARIOS
(defmodule seleccion-usuario
	(import MAIN ?ALL)
	(export ?ALL)
)

(deffunction seleccion-usuario::carga-datos (?id)
	(if (eq ?id 1)
		then (assert (Usuario	(nombre "Pepe")
					(edad 69)
					(patologias [ontologia_Class20010] [ontologia_Class20013])))
	)
	(if (eq ?id 2)
		then (assert (Usuario	(nombre "Juan")
					(edad 80)
					(patologias [ontologia_Class20011] [ontologia_Class20014])))
	)
	(if (eq ?id 3)
		then (assert (Usuario	(nombre "Maria")
					(edad 62)
					(patologias [ontologia_Class20012])))
	)
)

(deffunction seleccion-usuario::print-usuarios ()
	(printout t "  ID  |    Nombre" crlf)
    (printout t "---------------------" crlf)
    (printout t "  1   |    Pepe" crlf)
    (printout t "  2   |    Juan" crlf)
    (printout t "  3   |    Maria" crlf)
)

(defrule seleccion-usuario::load-usuarios "Determinar los datos del usuario a cargar"
	(declare (salience 10))
	(not (Usuario))
	=>
    (bind ?respuesta (pregunta-si-no "¿Desea cargar datos de un usuario? [si(s)][no(n)]: "))
    (if (eq ?respuesta FALSE)
        then (assert (nuevo_usuario))
    )
)

(defrule seleccion-usuario::lista-usuarios "Muestra la lista de usuarios"
	(not (Usuario))
	(not (nuevo_usuario))
	=>
	(print-usuarios)
    (bind ?id (pregunta-numerica "Que usuario desea cargar?" 1 3))
    (carga-datos ?id)
	(focus hacer_preguntas)
)

(defrule seleccion-usuario::new-usuario "Introduce los datos del nuevo usuario"
	(not (Usuario))
	(nuevo_usuario)
	=>
	(bind ?nombre (pregunta-general "Nombre: "))
	(bind ?edad (pregunta-general "Edad: "))

    (printout t crlf "--- Listado General ---" crlf)
    (format t "1.- Patologia General %n")
    (format t "2.- Patologia Articular Superior %n")
    (format t "3.- Patologia Articular Inferior %n")
    (format t "(Nota: Introduce el numero de aquel grupo de Patologias que tengas, separadas por un espacio)%n%n")
    (bind $?grup (pregunta-lista "Grupo: "))

    (bind $?listControl (create$ ))
    (progn$ (?g $?grup)
        (if (and (not(member General $?listControl)) (eq ?g 1) )
            then
                (bind $?listControl (insert$ ?listControl 1 General))
        )
        (if (and (not(member Articulacion_superior $?listControl)) (eq ?g 2))
            then
                (bind $?listControl (insert$ ?listControl 1 Articulacion_superior))
        )
        (if (and (not(member Articulacion_inferior $?listControl)) (eq ?g 3))
            then
                (bind $?listControl (insert$ ?listControl 1 Articulacion_inferior))
        )
    )

    (bind $?allPatologias (create$ ))
    (progn$ (?class $?listControl)
        (printout t ?class crlf)
        (bind $?allPatologias (insert$ ?allPatologias 1 (find-all-instances ((?inst ?class)) TRUE)))
    )

    (printout t crlf "--- Listado de Patologias ---" crlf)
    (loop-for-count (?i 1 (length$ ?allPatologias)) do
        (format t "%d.- " ?i)
        (format t "%s -> " (send (nth$ ?i ?allPatologias) get-Nombre))
        (format t "%s%n" (nth$ ?i ?allPatologias))
    )
	(format t "(Nota: Introduce el numero de aquellas Patologias que tengas, separadas por un espacio)%n%n")
	(bind $?pat (pregunta-lista "Patologia: "))

    (bind $?lista (create$ ))
    (progn$ (?num_patologia $?pat)
        (if (and (> ?num_patologia 0) (<= ?num_patologia (length$ $?allPatologias)))
            then
                (bind $?lista (insert$ ?lista 1 (nth$ ?num_patologia ?allPatologias)))
        )
    )

	(assert (Usuario (nombre ?nombre)
					 (edad ?edad)
					 (patologias $?lista)))
)

(defrule seleccion-usuario::imprimir-usuario "Imprimir Usuario nuevo"
	(Usuario)
	(nuevo_usuario)
	?user <- (Usuario (nombre ?name) (edad ?age) (patologias $?list_pat))
	=>
	(format t "--- Nuevo Usuario ---%n")
	(format t "Nombre: %s%n" ?name)
	(format t "Edad: %d%n" ?age)
	(printout t "Patologias: " $?list_pat crlf)
	(format t "%n")

	(focus hacer_preguntas)
)


	;;; Modulo hacer_preguntas
(defmodule hacer_preguntas
    (import MAIN ?ALL)
    (export ?ALL)
)

;;; ------------  Reglas del modulo hacer_preguntas --------------------

(defrule hacer_preguntas::NameRule1 "Descripción de la Regla"
	(Usuario)
	=>
	;;; Aquí sólo debemos hacer las preguntas para la inferencia
	;;; Aquí recogemos la información concreta para luego hacerla abstracta

    (format t "Get la información concreta to Abstracción%n%n")

	(bind ?respuesta (pregunta-si-no "Continuar? [si(s)][no(n)]: "))
	(if (eq ?respuesta FALSE)
        then
                (printout t "No esta disponible esa opción. " crlf)
				(finalizar)
    )
	(focus inferir_datos)
)


	;;; Modulo inferir_datos
(defmodule inferir_datos
    (import MAIN ?ALL)
	(import hacer_preguntas ?ALL)
    (export ?ALL)
)

;;; ------------  Reglas del modulo inferir_datos --------------------

(defrule inferir_datos::NameRule2 "Descripción de la Regla"
	?user_fact <- (Usuario (patologias $?list_patologia))
	=>
	;;; Aquí tenemos que sacar, después de aplicar las respuestas, que nos ha dado el usuario, los ejercicios recomendados

	;;; Tenemos la información concreta y hay que pasarla a abstracta
	;;; Hay que jugar con las reglas arquetípicas, para aplicar las transiciones

	   ;;; Aqui desarem els exercicis recomanats
    (format t "Get los Ejercicios Recomendados%n%n")

	(bind $?lista (create$ ))

	;;; iterem per les patologies de l'usuari
    (progn$ (?patologia $?list_patologia)
        (if (eq (class ?patologia) General)
            then
                (bind $?list_paliatiu (send (instance-address * ?patologia) get-paliar_general))
                (progn$ (?paliatiu $?list_paliatiu)
                        ;;; obtenim els exercicis recomanats per la patologia, filtrant contrarecomanats
                    (progn$ (?pat $?list_patologia)
                        (bind $?list_ejercicios_contraindicados (send (instance-address * ?pat) get-ejercicios_contraindicados))
                        (if (not(member ?paliatiu $?list_ejercicios_contraindicados))
                            then
                                (if (not(member ?paliatiu $?lista))
                                    then
                                        (bind $?lista (insert$ ?lista 1 ?paliatiu))
                                )
                        )
                    )
                )
        )
        (if (eq (class ?patologia) Articulacion_inferior)
            then
                (bind $?list_paliatiu (send (instance-address * ?patologia) get-fortalecer_articulacion_inferior))
                (progn$ (?paliatiu $?list_paliatiu)
                        ;;; obtenim els exercicis recomanats per la patologia, filtrant contrarecomanats
                    (progn$ (?pat $?list_patologia)
                        (bind $?list_ejercicios_contraindicados (send (instance-address * ?pat) get-ejercicios_contraindicados))
                        (if (not(member ?paliatiu $?list_ejercicios_contraindicados))
                            then
                                (if (not(member ?paliatiu $?lista))
                                    then
                                        (bind $?lista (insert$ ?lista 1 ?paliatiu))
                                )
                        )
                    )
                )
        )
        (if (eq (class ?patologia) Articulacion_superior)
            then
                (bind $?list_paliatiu (send (instance-address * ?patologia) get-fortalecer_articulacion_superior))
                (progn$ (?paliatiu $?list_paliatiu)
                        ;;; obtenim els exercicis recomanats per la patologia, filtrant contrarecomanats
                    (progn$ (?pat $?list_patologia)
                        (bind $?list_ejercicios_contraindicados (send (instance-address * ?pat) get-ejercicios_contraindicados))
                        (if (not(member ?paliatiu $?list_ejercicios_contraindicados))
                            then
                                (if (not(member ?paliatiu $?lista))
                                    then
                                        (bind $?lista (insert$ ?lista 1 ?paliatiu))
                                )
                        )
                    )
                )
        )
    )

    ;;; assert de la llista d'exercicis recomanats que pot fer l'usuari (faltaria organitzarlos en una sessions)
    ;;;  (assert Llista (paliativos ?lista))

    (progn$ (?sug $?lista)
        (printout t ?sug)
        (printout t " -> ")
        (printout t (send (instance-address * ?sug) get-Nombre) crlf)
    )

    (modify ?user_fact (listSug $?lista))

    (if (eq (length$ $?lista) 0)
        then
            (printout t "Sugerir unos ejercicios alternativos, la solucion vacia, no es posible!" crlf)
    )

	(bind ?respuesta (pregunta-si-no "Continuar? [si(s)][no(n)]: "))
	(if (eq ?respuesta FALSE)
        then
                (printout t "No esta disponible esa opción. " crlf)
				(finalizar)
    )
	(focus filtrado)
)


	;;; Modulo filtrado
(defmodule filtrado
    (import MAIN ?ALL)
	(import hacer_preguntas ?ALL)
	(import inferir_datos ?ALL)
    (export ?ALL)
)

;;; ------------  Reglas del modulo filtrado --------------------

(defrule filtrado::NameRule3 "Descripción de la Regla"
	?user_fact <- (Usuario (nombre ?nombre) (edad ?edad) (patologias $?list_pat) (listSug $?list_sug))
	=>
	;;; Hay que quitar las abstracción de los datos (si es necesario)
	;;; Hay que jugar con las reglas arquetípicas, para aplicar las transiciones

	(format t "Introducimos Ejercicios Recomendados to Sesion%n%n")

        ;;; Haces una loop instroduciendo todos los ejercicios de una sesion
	;;;(assert (Sesion (ejercicio ?)))

	(format t "--- Usuario ---%n")
	(format t "Nombre: %s%n" ?nombre)
	(format t "Edad: %d%n" ?edad)
	(printout t "Patologias: " $?list_pat crlf)
    (printout t "Sugerencias: " $?list_sug crlf)
	(format t "%n")


        ;;; Añadimos los calentamientos/Estiramientos/Comentarios definidos en la ontologia
    (bind $?list_cal (find-all-instances ((?inst Calentamiento)) TRUE))
    (bind $?list_est (find-all-instances ((?inst Estiramiento)) TRUE))
    (bind $?list_com (find-all-instances ((?inst Comentario)) TRUE))

    (modify ?user_fact (listCal $?list_cal) (listEst $?list_est) (listCom $?list_com))

	(bind ?respuesta (pregunta-si-no "Continuar? [si(s)][no(n)]: "))
	(if (eq ?respuesta FALSE)
        then
                (printout t "No esta disponible esa opción. " crlf)
				(finalizar)
    )
	(focus valorar_preferencias)
)


	;;; Modulo valorar_preferencias
(defmodule valorar_preferencias
    (import MAIN ?ALL)
	(import hacer_preguntas ?ALL)
	(import inferir_datos ?ALL)
	(import filtrado ?ALL)
    (export ?ALL)
)

;;; ------------  Reglas del modulo valorar_preferencias --------------------

(defrule valorar_preferencias::NameRule4 "Descripción de la Regla"
	(Usuario (nombre ?nombre) (edad ?edad) (patologias $?list_pat) (listCal $?list_cal) (listSug $?list_sug) (listEst $?list_est) (listCom $?list_com))
	=>
	;;; Hay que quitar las abstracción de los datos (si es necesario)
	;;; Hay que jugar con las reglas arquetípicas, para aplicar las transiciones

	(format t "Introducimos Sesiones to Planificacion%n%n")
		;;; Introducimos lsa sesiones en la planificación
		;;; cumpliendo con las restricciones
	;;;(assert (Planificacion (sesion ?)))

    (format t "--- Usuario ---%n")
	(format t "Nombre: %s%n" ?nombre)
	(format t "Edad: %d%n" ?edad)
	(printout t "Patologias: " $?list_pat crlf)
    (printout t "Calentamientos: " $?list_cal crlf)
    (printout t "Sugerencias: " $?list_sug crlf)
    (printout t "Estiramientos: " $?list_est crlf)
    (printout t "Comentarios: " $?list_com crlf)
	(format t "%n")

	(bind ?respuesta (pregunta-si-no "Continuar? [si(s)][no(n)]: "))
	(if (eq ?respuesta FALSE)
        then
                (printout t "No esta disponible esa opción. " crlf)
				(finalizar)
    )
	(focus recomendaciones)
)


	;;; Modulo recomendaciones
(defmodule recomendaciones
    (import MAIN ?ALL)
	(import hacer_preguntas ?ALL)
	(import inferir_datos ?ALL)
	(import filtrado ?ALL)
	(import valorar_preferencias ?ALL)
    (export ?ALL)
)

;;; ------------  Reglas del modulo recomendaciones --------------------

(defrule recomendaciones::NameRule5 "Descripción de la Regla"
    (Usuario (nombre ?nombre) (edad ?edad) (patologias $?list_pat) (listCal $?list_cal) (listSug $?list_sug) (listEst $?list_est) (listCom $?list_com))
	;;; ?plan <- (Planificacion (sesion ?ses))
	=>
	;;; Ya tenemos los datos concretos, que serían la solución del Análisis

	(format t "%n--- Imprimimos la Planificacion ---%n%n")
	(format t "Nombre: %s%n" ?nombre)
    (format t "Edad: %d%n" ?edad)
    (format t "Patologias:%n")
    (progn$ (?pat $?list_pat)
        (printout t "    ")
        (printout t (send (instance-address * ?pat) get-Nombre) crlf)
    )
    (printout t crlf "Sugerencias: " crlf)
    (loop-for-count (?i 1 6) do
        (format t " ---------------- Dia %d ---------------- %n%n" ?i)

        (if (<= ?i (length$ $?list_cal))
            then
                (format t "  Calentamiento:   %s%n" (send (nth$ ?i $?list_cal) get-Nombre))
            else
                (format t "  Calentamiento:   Calentamiento %d%n" ?i)
        )

        (if (<= ?i (length$ $?list_sug))
            then
                (format t "  Sesion:          %s%n" (send (nth$ ?i $?list_sug) get-Nombre))
            else
                (format t "  Sesion:          NetFlix %d%n" ?i)
        )

        (if (<= ?i (length$ $?list_est))
            then
                (format t "  Estiramiento:    %s%n" (send (nth$ ?i $?list_est) get-Nombre))
            else
                (format t "  Estiramiento:    Estiramiento %d%n" ?i)
        )

        (if (<= ?i (length$ $?list_com))
            then
                (format t "  Comentario:      %s%n%n" (send (nth$ ?i $?list_com) get-Nombre))
            else
                (format t "  Comentario:      Comentario %d%n%n" ?i)
        )
    )

	;;; Hacemos un loop, para imprimir la planificación
	;;; Conjunto de sesiones, y cada una, conjunto de ejercicios

	;;; Sesion X
	;;; Calentar
	;;; Ejercicios (1 o más)
	;;; Estirar

    (close) ;;; Esto es, porque a veces, se queda avierto, o el nombre en uso, para desvincularlo
	(open "data.txt" mydata "w")

    (format mydata "%n--- Imprimimos la Planificacion ---%n%n")
	(format mydata "Nombre: %s%n" ?nombre)
    (format mydata "Edad: %d%n" ?edad)
    (format mydata "Patologias:%n")
    (progn$ (?pat $?list_pat)
        (printout mydata "    ")
        (printout mydata (send (instance-address * ?pat) get-Nombre) crlf)
    )
    (printout mydata crlf "Sugerencias: " crlf)
    (loop-for-count (?i 1 6) do
        (format mydata " ---------------- Dia %d ---------------- %n%n" ?i)

        (if (<= ?i (length$ $?list_cal))
            then
                (format mydata "  Calentamiento:   %s%n" (send (nth$ ?i $?list_cal) get-Nombre))
            else
                (format mydata "  Calentamiento:   Calentamiento %d%n" ?i)
        )

        (if (<= ?i (length$ $?list_sug))
            then
                (format mydata "  Sesion:          %s%n" (send (nth$ ?i $?list_sug) get-Nombre))
            else
                (format mydata "  Sesion:          NetFlix %d%n" ?i)
        )

        (if (<= ?i (length$ $?list_est))
            then
                (format mydata "  Estiramiento:    %s%n" (send (nth$ ?i $?list_est) get-Nombre))
            else
                (format mydata "  Estiramiento:    Estiramiento %d%n" ?i)
        )

        (if (<= ?i (length$ $?list_com))
            then
                (format mydata "  Comentario:      %s%n%n" (send (nth$ ?i $?list_com) get-Nombre))
            else
                (format mydata "  Comentario:      Comentario %d%n%n" ?i)
        )
    )

	(close)
	(finalizar)
)
