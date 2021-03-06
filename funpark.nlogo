globals [
  attendance        ;; asistencia        ; La asistencia actual al parque.
  history           ;; historia          ; lista de valores pasados de asistencia
  home-patches      ;; casa-parches      ; Agentes de parches verdes que representan la zona residencial.
  funpark-patches   ;; parches-parque    ; conjunto de agentes de parches azules que representan el área de la parquera
  crowded-patch     ;; parche-concurrido ; Parche donde mostramos la etiqueta "concurrido"
  entrada-patches   ;; parche-entrada    ; Parche donde está la gente que decide ir y no puede entrar por que el parque esta lleno.

]


turtles-own [
  dinero         ;; dinero con que ingresa cada persona
  strategies      ;; estrategias       ; lista de estrategias
  best-strategy   ;; mejor estrategia  ; índice de la mejor estrategia actual
  attend?         ;;  ¿asistir?        ; Es cierto si el agente actualmente planea asistir a la parquera
  prediction      ;;  predicción       ; predicción actual de la asistencia del parque

]

to setup
  clear-all
  set-default-shape turtles "person"


  ;; create the 'homes'
  set home-patches patches with [pycor < -5  ]
  ask home-patches [ set pcolor orange ]

  ;; create the 'parque'
  set funpark-patches patches with [pycor > 0]
  ask funpark-patches [ set pcolor blue ]

   ;; create the 'entrada'
  set entrada-patches patches with [pycor < 0 and pycor > -5  ]
  ask entrada-patches [ set pcolor yellow ]

   ;; Inicializar aleatoriamente la asistencia anterior para que los agentes tengan un historial.
   ;; trabajar desde el principio
  set history n-values (memory-size * 2) [random 100]
   ;; la historia es el doble de la memoria, porque necesitamos al menos un valor de memoria de la historia
   ;; para cada punto en la memoria proparque qué tan bien habrían funcionado las estrategias
  set attendance first history

   ;; use una de las etiquetas de parche para indicar visualmente si
   ;; el parque esta "lleno de gente"
  ask patch (0.5 * max-pxcor) (0.5 * max-pycor) [
    set crowded-patch self
    set plabel-color red   ]

   ;; Crea los agentes y dales estrategias al azar.
   ;; Estas son las únicas estrategias que tendrán estos agentes, aunque
   ;; Pueden cambiar cual de estas "bolsas de estrategias" usan cada tick.
  create-turtles poblacion
  [ set color white
    move-to-empty-one-of home-patches
    set strategies n-values number-strategies [random-strategy]
    set best-strategy first strategies
    update-strategies  ]

  ;; la mitad de población es negra y la otra sigue blanca
  ask n-of (poblacion / 2) turtles ;la mitad de la poblacion cambio el color a negro
  [ set color black ]

  ;; cada persona nace con un dinero aleatorio
  ask turtles
    [ set dinero random 100 ]

   ;; iniciar el reloj
  reset-ticks
end


to go
  ;;  actualizar las variables globales
  ask crowded-patch [ set plabel "" ]
  ;; Cada agente predice la asistencia al parque y decide si ir o no.
  ask turtles [
    set prediction predict-attendance best-strategy sublist history 0 memory-size
    set attend? (prediction <= overcrowding-threshold and dinero > 10)  ;; verdadero o falso
  ]

  ;; Dependiendo de su decisión, los agentes van al parque o se quedan en casa.
 ;; Se definen dos variables temporales para evaluar también el área de cola
  	ask turtles
  	[
    ifelse attend?
      [ move-to-empty-one-of funpark-patches
        set attendance attendance + 1   ]
      [move-to-empty-one-of home-patches]
  	]

  ;; Si el parque está lleno indica que en la vista.
  ;; Se evaluará también que si el agente desea ir pero el parque está lleno, entonces
  ;; pasará a la zona amarilla de cola en donde tendrá una experiencia negativa
  ;; olvidados es una variable temporal que tiene las personas que no lograron ingresar al parque adecuadamente

  set attendance count turtles-on funpark-patches
  if attendance > overcrowding-threshold
  [ ask crowded-patch [ set plabel "PARQUE LLENO" ]
    let diferencia attendance - overcrowding-threshold
    let olvidados n-of diferencia turtles-on funpark-patches
    ask olvidados [move-to-empty-one-of entrada-patches]
  ]

  ;; las personas que estén en el parche azul les quitan 10 de dinero
  ask turtles
  [if pcolor = blue  [ set dinero dinero - costo-entrada ] ]

;; Adición del descuento
ask turtles
  [if ticks mod 7 = 0 and pcolor = blue [set dinero dinero + costo-entrada * (1 - (porcentaje-descuento / 100)) ]]
;; Adición de dinero quincenal
ask turtles
  [if ticks mod 15 = 0 [set dinero dinero + dinero-quincenal ] ]


  ;; actualizar el historial de asistencia
  ;; eliminar la asistencia más antigua y anteponer la asistencia más reciente
  set history fput attendance but-last history
  ;; Los agentes deciden cual es la nueva mejor estrategia.
  ask turtles [ update-strategies ]
  ;; adelantar el reloj
  tick
end

;; determina qué estrategia hubiera predicho los mejores resultados si se hubiera utilizado en esta ronda.
;; La mejor estrategia es la que tiene la suma de las diferencias más pequeñas entre los
;; Asistencia actual y la asistencia prevista para cada uno de los precedentes.
;; semanas (regresando semanas de tamaño de memoria)
;; esto no cambia las estrategias en absoluto, pero sí cambia (potencialmente) la
;; Actualmente se está utilizando y actualiza el rendimiento de todas las estrategias.
to update-strategies
  ;; Inicializar el mejor puntaje a un máximo, que es el puntaje más bajo posible
  let best-score memory-size * 100 + 1
  foreach strategies [ the-strategy ->
    let score 0
    let week 1
    repeat memory-size [
      set prediction predict-attendance the-strategy sublist history week (week + memory-size)
      set score score + abs (item (week - 1) history - prediction)
      set week week + 1
    ]
    if (score <= best-score) [
      set best-score score
      set best-strategy the-strategy
    ]
  ]
end

;; Esto reporta una estrategia aleatoria. una estrategia es solo un conjunto de pesos de -1.0 a 1.0 que
;; determina cuánto énfasis se pone en cada período de tiempo anterior al hacer
;; una predicción de asistencia para el siguiente período de tiempo
to-report random-strategy
  report n-values (memory-size + 1) [1.0 - random-float 2.0]
end

;; Esto reporta la predicción de un agente de la asistencia actual.
;; Usando una estrategia particular y parte del historial de asistencia.
;; Más específicamente, la estrategia es luego descrita por la fórmula
;; p (t) = x (t - 1) * a (t - 1) + x (t - 2) * a (t -2) + ..
;; ... + x (t - TAMAÑO DE MEMORIA) * a (t - TAMAÑO DE MEMORIA) + c * 100,
;; donde p (t) es la predicción en el tiempo t, x (t) es la asistencia de la parquera en el tiempo t,
;; a (t) es el peso para el tiempo t, c es una constante y MEMORY-SIZE es un parámetro externo.
to-report predict-attendance [strategy subhistory]
 ;; El primer elemento de la estrategia es la constante, c, en la fórmula de predicción.
 ;; Uno puede pensar en ello como la predicción del agente de la asistencia del parque
 ;; en ausencia de cualquier otro dato
 ;; Luego multiplicamos cada semana en la historia por su respectivo peso.
  report 100 * first strategy + sum (map [ [weight week] -> weight * week ] butfirst strategy subhistory)
end

;; En este modelo realmente no importa exactamente qué parche
;; una tortuga está encendida, solo si la tortuga está en la zona de origen
;; o la zona del parque. Sin emparquego, para hacer una bonita visualización.
;; Este procedimiento se utiliza para garantizar que solo tenemos uno.
;; tortuga por parche.
to move-to-empty-one-of [locations]  ;; procedimiento de tortuga
  move-to one-of locations
  while [any? other turtles-here] [
    move-to one-of locations
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
243
17
671
446
-1
-1
12.0
1
24
1
1
1
0
1
1
1
-17
17
-17
17
1
1
1
ticks
30.0

BUTTON
160
358
223
391
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
30
358
96
391
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
31
34
221
67
memory-size
memory-size
1
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
31
69
221
102
number-strategies
number-strategies
1
20
10.0
1
1
NIL
HORIZONTAL

PLOT
689
27
1129
239
Pretensión de asistencia
Tiempo
Asistencia
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"attendance" 1.0 0 -16777216 true "" "plot attendance"
"threshold" 1.0 0 -2674135 true "" ";; plot a threshold line -- an attendance level above this line makes the bar\n;; is unappealing, but below this line is appealing\nplot-pen-reset\nplotxy 0 overcrowding-threshold\nplotxy plot-x-max overcrowding-threshold"

SLIDER
31
104
221
137
overcrowding-threshold
overcrowding-threshold
0
200
80.0
10
1
NIL
HORIZONTAL

SLIDER
30
147
221
180
poblacion
poblacion
0
200
100.0
10
1
NIL
HORIZONTAL

PLOT
690
261
1131
440
Personas en cola
Tiempo
Personas en cola
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles-on entrada-patches"
"pen-1" 1.0 0 -5298144 true "" "plot-pen-reset\nplotxy 0 overcrowding-threshold\nplotxy plot-x-max overcrowding-threshold"

SLIDER
28
190
219
223
costo-entrada
costo-entrada
0
40
10.0
2.5
1
NIL
HORIZONTAL

SLIDER
29
232
219
265
dinero-quincenal
dinero-quincenal
0
100
40.0
1
1
NIL
HORIZONTAL

SLIDER
29
275
219
308
porcentaje-descuento
porcentaje-descuento
0
100
50.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## ACKNOWLEDGMENT

This model is made for educational purposes and is based on "Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo", by Uri Wilensky & William Rand.

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

## WHAT IS IT?

Funpark es un parque de diversiones el cual está presentando una baja asistencia los días previos al pago de quincena, por esta razón la administración del parque quiere implementar una estrategia de descuento cada 7 días para incentivar la asistencia.

El modelo muestra como es el comportamiento de las personas que quieren asistir a el parque de diversiones teniendo en cuenta la cantidad de asistencias pasadas, esto con el fin de que la estadía sea lo más amena posible ya que cuando el parque se llena, la estadía se hace desagradable, además si las personas quieren asistir al parque y este se encuentra lleno no se les permite el ingreso quedándose en las taquillas del parque.

El costo de entrada al parque es de 10 unidades monetarias por lo que si los agentes no poseen dicha cantidad no intentaran ir al parque, los agentes reciben cada 15 días la cantidad de 40 unidades monetarias haciendo énfasis en el pago quincenal de las personas.

Funpark basa y adapta sus estrategias de predicción para los agentes en la propuesta por (Arthur, 1994) en su modelo de El Farol y también utiliza la implementación de Aprendizaje autónomo implementada por (Rand & Wilensky, 2007) en Netlogo para este mismo modelo. Funpark añade elementos importantes y múltiples restricciones, principalmente las mencionadas en relación con el dinero y los descuentos.


## HOW IT WORKS


Para las predicciones se utilizó la ecuación de aprendizaje autónomo propuesta por (Rand & Wilensky, 2007) la cual se detalla a continuación: 

P(t) = x(t - 1)*a(t - 1) + x(t - 2)*a(t -2) + ... + x(t - MEMORY)*a(t - MEMORY) + c * 100


## HOW TO USE IT
En la parte izquierda de la interfaz del modelo (Figura 1) se encuentran los parámetros de entrada modificables mediante deslizadores. En el área central se encuentra la interfaz gráfica del modelo: El color azul representa el área del parque de diversiones y las personas que ingresan han pagado un costo de entrada; el color amarillo representa a los agentes que no lograron acceder al parque debido a la capacidad definida en los parámetros y se consideran en cola o con una experiencia no tan agradable.

En el área derecha hay dos gráficas, en las que se puede visualizar la cantidad de personas que tienen como objetivo asistir al parque y la cantidad de personas que quedan “en cola” por el sobrepaso de la capacidad del parque definida.



## CREDITS AND REFERENCES

This model is adapted from:

Rand, W. and Wilensky, U. (1997). NetLogo El Farol model. http://ccl.northwestern.edu/netlogo/models/ElFarol. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2019 Daniel Chavarría, et al.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Daniel Chavarría at ldchavarriam@unal.edu.co
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
