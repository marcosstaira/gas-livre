globals
[
  tick-delta                      ;; how much we advance the tick counter this time through
  max-tick-delta                  ;; the largest tick-delta is allowed to be
  init-avg-speed init-avg-energy  ;; initial averages
  avg-speed avg-energy            ;; current averages
  fast medium slow                ;; current counts
  percent-fast percent-medium     ;; percentage of the counts
  percent-slow                    ;; percentage of the counts

]

breed [ particles particle ]

particles-own
[
  speed mass energy          ;; particle info
  last-collision
]


to setup
  clear-all
  set-default-shape particles "circle"
  set max-tick-delta 0.1073
  make-particles
  update-variables
  set init-avg-speed avg-speed
  set init-avg-energy avg-energy
  reset-ticks
end

to go
  ask particles [ move ]
  ask particles
  [ if colidir? [check-for-collision] ]
  ifelse (rastro?)
    [ ask particle 0 [ pen-down ] ]
    [ ask particle 0 [ pen-up ] ]
  tick-advance tick-delta
  if floor ticks > floor (ticks - tick-delta)
  [
    update-variables
    update-plots
  ]
  calculate-tick-delta

  display
end

to update-variables
  set medium count particles with [color = green]
  set slow count particles with [color = blue]
  set fast count particles with [color = red]
  set percent-medium (medium / count particles) * 100
  set percent-slow (slow / count particles) * 100
  set percent-fast (fast / count particles) * 100
  set avg-speed  mean [speed] of particles
  set avg-energy  mean [energy] of particles
end



to calculate-tick-delta
  ;; tick-delta is calculated in such way that even the fastest
  ;; particle will jump at most 1 patch length in a tick. As
  ;; particles jump (speed * tick-delta) at every tick, making
  ;; tick length the inverse of the speed of the fastest particle
  ;; (1/max speed) assures that. Having each particle advance at most
  ;; one patch-length is necessary for them not to jump over each other
  ;; without colliding.
  ifelse any? particles with [speed > 0]
    [ set tick-delta min list (1 / (ceiling max [speed] of particles)) max-tick-delta ]
    [ set tick-delta max-tick-delta ]
end



to move  ;; particle procedure
  if patch-ahead (speed * tick-delta) != patch-here
    [ set last-collision nobody ]
  jump (speed * tick-delta)
end

to check-for-collision  ;; particle procedure
  ;; Here we impose a rule that collisions only take place when there
  ;; are exactly two particles per patch.

  if count other particles-here = 1
  [
    ;; the following conditions are imposed on collision candidates:
    ;;   1. they must have a lower who number than my own, because collision
    ;;      code is asymmetrical: it must always happen from the point of view
    ;;      of just one particle.
    ;;   2. they must not be the same particle that we last collided with on
    ;;      this patch, so that we have a chance to leave the patch after we've
    ;;      collided with someone.
    let candidate one-of other particles-here with
      [who < [who] of myself and myself != last-collision]
    ;; we also only collide if one of us has non-zero speed. It's useless
    ;; (and incorrect, actually) for two particles with zero speed to collide.
    if (candidate != nobody) and (speed > 0 or [speed] of candidate > 0)
    [
      collide-with candidate
      set last-collision candidate
      ask candidate [ set last-collision myself ]
    ]
  ]
end

;; implements a collision with another particle.
;;
;; THIS IS THE HEART OF THE PARTICLE SIMULATION, AND YOU ARE STRONGLY ADVISED
;; NOT TO CHANGE IT UNLESS YOU REALLY UNDERSTAND WHAT YOU'RE DOING!
;;
;; The two particles colliding are self and other-particle, and while the
;; collision is performed from the point of view of self, both particles are
;; modified to reflect its effects. This is somewhat complicated, so I'll
;; give a general outline here:
;;   1. Do initial setup, and determine the heading between particle centers
;;      (call it theta).
;;   2. Convert the representation of the velocity of each particle from
;;      speed/heading to a theta-based vector whose first component is the
;;      particle's speed along theta, and whose second component is the speed
;;      perpendicular to theta.
;;   3. Modify the velocity vectors to reflect the effects of the collision.
;;      This involves:
;;        a. computing the velocity of the center of mass of the whole system
;;           along direction theta
;;        b. updating the along-theta components of the two velocity vectors.
;;   4. Convert from the theta-based vector representation of velocity back to
;;      the usual speed/heading representation for each particle.
;;   5. Perform final cleanup and update derived quantities.
to collide-with [ other-particle ] ;; particle procedure
  ;;; PHASE 1: initial setup

  ;; for convenience, grab some quantities from other-particle
  let mass2 [mass] of other-particle
  let speed2 [speed] of other-particle
  let heading2 [heading] of other-particle

  ;; since particles are modeled as zero-size points, theta isn't meaningfully
  ;; defined. we can assign it randomly without affecting the model's outcome.
  let theta (random-float 360)



  ;;; PHASE 2: convert velocities to theta-based vector representation

  ;; now convert my velocity from speed/heading representation to components
  ;; along theta and perpendicular to theta
  let v1t (speed * cos (theta - heading))
  let v1l (speed * sin (theta - heading))

  ;; do the same for other-particle
  let v2t (speed2 * cos (theta - heading2))
  let v2l (speed2 * sin (theta - heading2))



  ;;; PHASE 3: manipulate vectors to implement collision

  ;; compute the velocity of the system's center of mass along theta
  let vcm (((mass * v1t) + (mass2 * v2t)) / (mass + mass2) )

  ;; now compute the new velocity for each particle along direction theta.
  ;; velocity perpendicular to theta is unaffected by a collision along theta,
  ;; so the next two lines actually implement the collision itself, in the
  ;; sense that the effects of the collision are exactly the following changes
  ;; in particle velocity.
  set v1t (2 * vcm - v1t)
  set v2t (2 * vcm - v2t)



  ;;; PHASE 4: convert back to normal speed/heading

  ;; now convert my velocity vector into my new speed and heading
  set speed sqrt ((v1t ^ 2) + (v1l ^ 2))
  set energy (0.5 * mass * (speed ^ 2))
  ;; if the magnitude of the velocity vector is 0, atan is undefined. but
  ;; speed will be 0, so heading is irrelevant anyway. therefore, in that
  ;; case we'll just leave it unmodified.
  if v1l != 0 or v1t != 0
    [ set heading (theta - (atan v1l v1t)) ]

  ;; and do the same for other-particle
  ask other-particle [
    set speed sqrt ((v2t ^ 2) + (v2l ^ 2))
    set energy (0.5 * mass * (speed ^ 2))
    if v2l != 0 or v2t != 0
      [ set heading (theta - (atan v2l v2t)) ]
  ]

  ;; PHASE 5: final updates

  ;; now recolor, since color is based on quantities that may have changed
  recolor
  ask other-particle
    [ recolor ]
end

to recolor  ;; particle procedure
  ifelse speed < (0.5 * 10)
  [
    set color blue
  ]
  [
    ifelse speed > (1.5 * 10)
      [ set color red ]
      [ set color green ]
  ]
end

;;;
;;; drawing procedures
;;;


;; creates initial particles
to make-particles
  create-particles numero-de-particulas
  [
    setup-particle
    random-position
    recolor
  ]
  calculate-tick-delta
end


to setup-particle  ;; particle procedure
  set speed velocidade-inicial-particula
  set mass massa-particulas
  set energy (0.5 * mass * (speed ^ 2))
  set last-collision nobody
end


;; place particle at random location inside the box.
to random-position ;; particle procedure
  setxy ((1 + min-pxcor) + random-float ((2 * max-pxcor) - 2))
        ((1 + min-pycor) + random-float ((2 * max-pycor) - 2))
end

to-report last-n [n the-list]
  ifelse n >= length the-list
    [ report the-list ]
    [ report last-n n butfirst the-list ]
end

;; histogram procedure
to draw-vert-line [ xval ]
  plotxy xval plot-y-min
  plot-pen-down
  plotxy xval plot-y-max
  plot-pen-up
end


; Copyright 1997 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
492
10
824
343
-1
-1
4.0
1
10
1
1
1
0
1
1
1
-40
40
-40
40
1
1
1
ticks
30.0

BUTTON
3
46
96
79
iniciar/parar
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
4
13
96
46
configura????o
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
97
11
303
44
numero-de-particulas
numero-de-particulas
1
1000
101.0
1
1
NIL
HORIZONTAL

MONITOR
18
254
150
299
velocidade m??dia
avg-speed
2
1
11

PLOT
614
386
904
582
Histograma de Energia
Energia
N??mero
0.0
400.0
0.0
10.0
false
true
"" "set-plot-x-range 0 (0.5 * (velocidade-inicial-particula * 2) * (velocidade-inicial-particula  * 2) * massa-particulas)\nset-plot-y-range 0 ceiling (numero-de-particulas / 6)"
PENS
"R??pidas" 10.0 1 -2674135 true "set-histogram-num-bars 40" "histogram [ energy ] of particles with [color = red]"
"M??dias" 10.0 1 -10899396 true "set-histogram-num-bars 40" "histogram [ energy ] of particles with [color = green]"
"Lentas" 10.0 1 -13345367 true "set-histogram-num-bars 40" "histogram [ energy ] of particles with [color = blue]"
"Energia M??dia" 1.0 0 -7500403 true "" "plot-pen-reset  draw-vert-line avg-energy"
"M??dia Velocidade Inicial" 1.0 0 -16777216 true "draw-vert-line init-avg-energy" ""

MONITOR
173
254
305
299
energia m??dia
avg-energy
2
1
11

PLOT
15
385
304
582
Contador de Velocidade
Tempo
Contador (%)
0.0
20.0
0.0
100.0
true
true
"" "set-plot-y-range 0 100"
PENS
"r??pida" 1.0 0 -2674135 true "" "plotxy ticks percent-fast"
"m??dia" 1.0 0 -10899396 true "" "plotxy ticks percent-medium"
"lenta" 1.0 0 -13345367 true "" "plotxy ticks percent-slow"

SWITCH
97
44
200
77
colidir?
colidir?
0
1
-1000

PLOT
312
385
606
582
Histograma de Velocidade
Velocidade
N??mero
0.0
50.0
0.0
100.0
false
true
"" "set-plot-x-range 0 (velocidade-inicial-particula * 2)\nset-plot-y-range 0 ceiling (numero-de-particulas / 6)"
PENS
"R??pidas" 5.0 1 -2674135 true "set-histogram-num-bars 40" "histogram [ speed ] of particles with [color = red]"
"M??dias" 5.0 1 -10899396 true "set-histogram-num-bars 40" "histogram [ speed ] of particles with [color = green]"
"Lentas" 5.0 1 -13345367 true "set-histogram-num-bars 40" "histogram [ speed ] of particles with [color = blue]"
"Velocidade M??dia" 1.0 0 -7500403 true "" "plot-pen-reset   draw-vert-line avg-speed"
"M??dia Velociade Inicial" 1.0 0 -16777216 true "draw-vert-line init-avg-speed" ""

MONITOR
17
312
141
357
porcentagem r??pida
percent-fast
0
1
11

MONITOR
147
312
274
357
porcecntagem m??dia
percent-medium
0
1
11

MONITOR
278
312
413
357
porcentagem lenta
percent-slow
0
1
11

SWITCH
200
44
303
77
rastro?
rastro?
0
1
-1000

SLIDER
8
88
195
121
velocidade-inicial-particula
velocidade-inicial-particula
1
20
14.0
1
1
NIL
HORIZONTAL

SLIDER
8
128
194
161
massa-particulas
massa-particulas
1
20
3.0
1
1
NIL
HORIZONTAL

BUTTON
200
77
303
110
limpar rastro
clear-drawing
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## O QUE ???

Este modelo ?? o modelo de g??s mais simples do conjunto de modelos "Laborat??rio de Gases". As part??culas est??o se movendo e colidindo umas com as outras sem restri????es externas, como gravidade ou recipientes. Neste modelo, as part??culas s??o modeladas como perfeitamente el??sticas sem energia, exceto sua energia cin??tica - que ?? devido ao seu movimento. As colis??es entre part??culas s??o el??sticas. As part??culas s??o coloridas de acordo com sua velocidade - azul para lento, verde para m??dio e vermelho para alto.

Este modelo faz parte de uma s??rie de modelos GasLab. Eles usam as mesmas regras b??sicas para simular o comportamento dos gases. Cada modelo integra diferentes recursos para destacar diferentes aspectos do comportamento do g??s.

O princ??pio b??sico dos modelos ?? que as part??culas de g??s t??m duas a????es elementares: movem-se e colidem - seja com outras part??culas ou com quaisquer outros objetos, como paredes.

## COMO FUNCIONA

O princ??pio b??sico de todos os modelos "Laborat??rio de Gases" ?? o seguinte algor??tmo 

1) Uma part??cula se move em linha reta sem alterar sua velocidade, a menos que colida com outra part??cula ou salte na parede.
2) Duas part??culas "colidem" se se encontrarem no mesmo patch (a vis??o do NetLogo ?? composta por uma grade de pequenos quadrados chamados patches). Neste modelo, duas part??culas s??o direcionadas para que colidam na origem.
3) ?? escolhido um ??ngulo de colis??o das part??culas, como se fossem duas bolas s??lidas que se chocam, e esse ??ngulo descreve a dire????o da linha que liga seus centros.
4) As part??culas trocam momento e energia apenas ao longo desta linha, conforme a conserva????o de momento e energia para colis??es el??sticas.
5) Cada part??cula recebe sua nova velocidade, dire????o e energia.

## COMO UTILIZ??-LO

Configura????es iniciais:
- N??MERO DE PART??CULAS: o n??mero de part??culas de g??s.
- RASTRO ?: Desenha o caminho de uma part??cula individual.
- COLIDE ?: Liga e desliga colis??es entre part??culas.
- VELOCIDADE-INICIAL-PARTICULA: a velocidade inicial de cada part??cula - todas come??am com a mesma velocidade.
- MASSA DE PART??CULAS: a massa de cada part??cula - todas t??m a mesma massa.

Como na maioria dos modelos NetLogo, a primeira etapa ?? pressionar CONFIGURAR. Ele coloca nas condi????es iniciais que voc?? definiu com os controles deslizantes. Certifique-se de esperar at?? que o bot??o SETUP pare antes de pressionar GO.
O bot??o INICIAR executa os modelos repetidamente. Este ?? um bot??o "para sempre".

Monitores:
- Monitores POR CENTO R??PIDO, POR CENTO M??DIO, POR CENTO LENTO: porcentagem de part??culas com velocidades diferentes: r??pido (vermelho), m??dio (verde) e lento (azul).
- VELOCIDADE M??DIA: velocidade m??dia das part??culas.
- ENERGIA M??DIA: energia cin??tica m??dia das part??culas.

Parcelas:
- CONTAGEM DE VELOCIDADE: plota o n??mero de part??culas em cada faixa de velocidade (r??pida, m??dia ou lenta).
- HISTOGRAMA DE VELOCIDADE: distribui????o da velocidade de todas as part??culas. A linha cinza ?? o valor m??dio e a linha preta ?? a m??dia inicial. Os valores exibidos para a velocidade s??o dez vezes os valores reais.
- HISTOGRAMA DE ENERGIA: a distribui????o das energias de todas as part??culas, calculada como (m * v ^ 2) / 2. A linha cinza ?? o valor m??dio e a linha preta ?? a m??dia inicial.

Inicialmente, todas as part??culas t??m a mesma velocidade, mas dire????es aleat??rias. Portanto, os primeiros gr??ficos de histograma de velocidade e energia devem mostrar apenas uma coluna cada. Conforme as part??culas colidem repetidamente, elas trocam energia e partem em novas dire????es, e as velocidades s??o dispersas - algumas part??culas ficam mais r??pidas, outras ficam mais lentas, e o gr??fico mostrar?? essa mudan??a.

## QUEST??ES NOT??VEIS

O que est?? acontecendo com o n??mero de part??culas de cores diferentes? Por que existem mais part??culas azuis do que vermelhas?

Voc?? pode observar as colis??es e mudan??as de cor conforme elas acontecem? Por exemplo, quando uma part??cula vermelha atinge uma part??cula verde, que cor cada uma delas se torna?

Por que a velocidade m??dia (velocidade m??dia) cai? Isso viola a conserva????o de energia?

Este g??s est?? em um "espa??o infinito" - sem limites, sem obstru????es, mas ainda com um tamanho finito! Existe uma situa????o f??sica como esta?

Observe a part??cula cujo caminho ?? tra??ado no desenho. Observe como o caminho "envolve" o mundo. O tra??o se assemelha ao movimento browniano? Voc?? consegue reconhecer quando ocorre uma colis??o? Que fatores afetam a frequ??ncia das colis??es? E a "angularidade" do caminho? Voc?? poderia faz??-lo permanecer "local" ou viajar pelo mundo todo?

## QUEST??ES A SE TENTAR

Coloque todas as part??culas em uma parte do mundo, ou com o mesmo t??tulo - o que acontece? Isso corresponde a uma possibilidade f??sica?

Experimente configura????es diferentes, especialmente os extremos. Os histogramas s??o diferentes? O padr??o de rastreamento muda?

Existem outras quantidades interessantes para acompanhar?

Procure ou calcule o n??mero * real *, o tamanho, a massa e a velocidade das part??culas em um g??s t??pico. Quando voc?? compara esses n??meros com os do modelo, fica surpreso que o modelo funcione t??o bem? Que fen??menos f??sicos poderiam ser observados se realmente houvesse um pequeno n??mero de grandes part??culas no espa??o ao nosso redor?

Costumamos dizer que o espa??o sideral ?? um v??cuo. Isso ?? verdade mesmo? Quantas part??culas haveria em um espa??o do tamanho deste computador?
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

clock
true
0
Circle -7500403 true true 30 30 240
Polygon -16777216 true false 150 31 128 75 143 75 143 150 158 150 158 75 173 75
Circle -16777216 true false 135 135 30

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
NetLogo 6.1.1
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
