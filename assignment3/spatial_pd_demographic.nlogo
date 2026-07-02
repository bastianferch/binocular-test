globals [seed init-collisions agent-color-alpha-min agent-color-wealth-max agent-shape basecolor-coop basecolor-def births-coop births-def deaths-coop deaths-def]
turtles-own [basecolor wealth age mutant]
breed [cooperators coop]
breed [defectors def]

to init-coop [location]
  ; Run in a turtle-creation context
  ; Initialize a cooperator
  init-common location
  set births-coop births-coop + 1
  set basecolor basecolor-coop
end

to init-def [location]
  ; Run in a turtle-creation context
  ; Initialize a defector
  init-common location
  set births-def births-def + 1
  set basecolor basecolor-def
end

to init-common [location]
  ; Run in a turtle-creation context - called by breed specific Initialization
  ; Initialize a turtle-agent

  ; Go to random unoccupied location if simulation just started, go to determined location otherwise
  ifelse (ticks = 0)
  [ move-to one-of patches
    while [any? other turtles-here] [ move-to one-of patches ] ]
  [ move-to location ]

  ; Random-birth-age at the start? (only effective for densly populated starting-grids)
  ifelse (ticks = 0 and agent-init-random-birth-age)
  [ set age random agent-maxage ]
  [ set age 0 ]

  ; Initial Wealth
  set wealth agent-init-endowment
  set shape agent-shape
end

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks

  ; Set some constants
  set-gui-params
  init-patches

  ; Initialize random number generator
  set seed new-seed
  random-seed seed

  ; Create predetermined ratio of cooperator/defector agent-turtles)
  create-cooperators round(agent-number * agent-init-coop-ratio ) [ init-coop nobody ]
  create-defectors round(agent-number * (1 - agent-init-coop-ratio)) [ init-def nobody ]

  ; Draw initial state
  update-coloring
  update-graphs
end

to go

  ask turtles
  [
   ; Move randomly one square
   move

   ; Play PD against all neighbors
   play-pd

   ; Procreate
   procreate

   ; Age
   set age age + 1
   if ((age > agent-maxage) and (agent-maxage > 0)) [ agent-die ]
  ]

  ; Stop if no-one is left
  if count turtles = 0 [ stop ]

  ; Update Fitness Coloring and Diagrams
  update-coloring
  update-graphs

  ; The show must go on...
  tick
end

to move
  ; Run inside a turtle context
  ; Move if there is empty space in sight
  let me self
  let local-neighbors get-neighbors
  ask local-neighbors
  [ if not (any? turtles-here)
    [ let targetpatch self
      ask me [move-to targetpatch] stop ] ]
end

to-report get-neighbors
  ; Run inside a turtle context
  ; Determines shape of interaction neighborhood
  if neighborhood = "vonNeumann" [report neighbors4]
  if neighborhood = "Moore" [report neighbors]
end

to play-pd
  ; Run inside a turtle context
  let me self
  let local-neighbors get-neighbors
  let im-alive true

  ; Play against all Neighbors
  ask local-neighbors
  [
    if im-alive
    [
      ask turtles-here
      [
        let him self

        if is-coop? me and is-coop? him
        [ ; both cooperate
          ask me [ set wealth (wealth + pd-r) if (wealth < 0) [ set im-alive false agent-die ] ]
          set wealth (wealth + pd-r) if (wealth < 0) [ agent-die ]
        ]
        if is-def? me and is-def? him
        [ ; both defect
          ask me [ set wealth (wealth + pd-p) if (wealth < 0) [ set im-alive false agent-die ] ]
          set wealth (wealth + pd-p) if (wealth < 0) [ agent-die ]
        ]
        if is-coop? me and is-def? him
        [ ; moving agent gets the sucker's payoff
          ask me [ set wealth (wealth + pd-s) if (wealth < 0) [ set im-alive false agent-die ] ]
          set wealth (wealth + pd-t) if (wealth < 0) [ agent-die ]
        ]
        if is-def? me and is-coop? him
        [ ; passive agent gets sucker's payoff
          ask me [ set wealth (wealth + pd-t) if (wealth < 0) [ set im-alive false agent-die ] ]
          set wealth (wealth + pd-s) if (wealth < 0) [ agent-die ]
        ]
      ]
    ]
  ]
end

to procreate
  ; Run inside a turtle context
  ; Always remember who you are
  let me self
  let local-neighbors get-neighbors

  ; Check if there is a free place in the neighborhood
  let procreation-location nobody
  ask local-neighbors [ if (not (any? turtles-here)) [ set procreation-location self stop ] ]

  if is-patch? procreation-location
  [
    ; Enough wealth to procreate?
    if (wealth >= agent-procreation-min-wealth)
    [
      ; Check if the offspring is a mutant
      let mutant-child false
      let hatch-breed "coop"
      if is-def? me [ set hatch-breed "def" ]
      if (random 100 < (mutation-rate * 100))
      [
        set mutant-child true
        ifelse is-coop? self [ set hatch-breed "def" ] [ set hatch-breed "coop" ]
      ]
      ; Produce the offspring
      ifelse hatch-breed = "coop"
      [ hatch-cooperators 1 [ init-coop procreation-location ] ]
      [ hatch-defectors 1 [ init-def procreation-location ] ]

      ; Offspring has costs
      set wealth (wealth - agent-init-endowment)
    ]
  ]
end

to agent-die
  ; Everything must come to an end ... eventually ...
  ifelse is-coop? self [ set deaths-coop deaths-coop + 1 ] [ set deaths-def deaths-def + 1 ]
  die
end

to set-gui-params
  ; Set initial color of Agents
  set agent-color-wealth-max 2000
  set basecolor-coop (list 0 127 0)
  set basecolor-def (list 127 0 0)
  set agent-shape "square"
end

to update-coloring
  ; Run from a global context
  ; Update the coloring of turtle-agents
  ask turtles
  [
    let tempcolor []
    set tempcolor (replace-item ifelse-value (is-coop? self) [ 1 ] [ 0 ] basecolor ((min (list (round ((wealth / agent-color-wealth-max ) * (255 - 128))) (255 - 128))) + 128 ))
    set color tempcolor
  ]

end

to update-graphs
  ; Run from a global context
  ; Updates all diagrams

  set-current-plot "Number of Agents"
  set-current-plot-pen "num_coop"
  plotxy ticks count cooperators
  set-current-plot-pen "num_def"
  plotxy ticks count defectors

  set-current-plot "Average Wealth"
  if count cooperators > 0
  [
    set-current-plot-pen "avg_wealth_coop"
    plotxy ticks mean [wealth] of cooperators
  ]
  if count defectors > 0
  [
    set-current-plot-pen "avg_wealth_def"
    plotxy ticks mean [wealth] of defectors
  ]

  set-current-plot "Birth Death Diagram"
  if count cooperators > 0
  [
    set-current-plot-pen "births_coop"
    plotxy ticks births-coop
    set-current-plot-pen "deaths_coop"
    plotxy ticks deaths-coop
  ]
  if count defectors > 0
  [
    set-current-plot-pen "births_def"
    plotxy ticks births-def
    set-current-plot-pen "deaths_def"
    plotxy ticks deaths-def
  ]
    set-current-plot-pen "births_total"
    plotxy ticks (births-def + births-coop)
    set-current-plot-pen "deaths_total"
    plotxy ticks (deaths-def + deaths-coop)

  set-current-plot "Age Distribution"
  set-current-plot-pen "age_distribution"
  let plot-maxage ifelse-value (agent-maxage > 0) [ agent-maxage ] [ max [age] of turtles ]
  set plot-maxage ifelse-value (plot-maxage > 0) [ plot-maxage ] [ 1 ]
  set-plot-y-range 0 1
  set-plot-x-range 0 (plot-maxage)
  set-histogram-num-bars min list (histogram-granularity) ( plot-maxage )
  histogram [age] of turtles

  set-current-plot "Wealth Distribution"
  set-current-plot-pen "wealth_distribution"
  set-plot-y-range 0 1
  set-plot-x-range 0 (max [wealth] of turtles)
  set-histogram-num-bars min list (histogram-granularity) ( max [wealth] of turtles )
  histogram [wealth] of turtles

  set deaths-coop 0
  set deaths-def 0
  set births-coop 0
  set births-def 0
end

to init-patches
  ; Color all patches (space) according to choice
  if patch-background-color = "grey" [ ask patches [ set pcolor (list 200 200 200) ] ]
  if patch-background-color = "darkgrey" [ ask patches [ set pcolor (list 80 80 80) ] ]
  if patch-background-color = "white" [ ask patches [ set pcolor (list 255 255 255) ] ]
  if patch-background-color = "black" [ ]
end
@#$#@#$#@
GRAPHICS-WINDOW
358
10
984
637
-1
-1
12.36
1
8
1
1
1
0
1
1
1
0
49
0
49
1
1
1
ticks
30.0

BUTTON
10
10
87
43
NIL
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

BUTTON
9
47
87
80
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
96
10
351
43
agent-number
agent-number
0
(world-width * world-height) + 1
280.0
10
1
NIL
HORIZONTAL

INPUTBOX
96
52
146
112
pd-r
5.0
1
0
Number

INPUTBOX
150
52
200
112
pd-s
-6.0
1
0
Number

INPUTBOX
96
116
146
176
pd-t
6.0
1
0
Number

INPUTBOX
150
116
200
176
pd-p
-5.0
1
0
Number

BUTTON
9
84
87
117
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
204
122
351
167
neighborhood
neighborhood
"vonNeumann" "Moore"
1

SLIDER
204
206
351
239
agent-maxage
agent-maxage
0
300
100.0
1
1
NIL
HORIZONTAL

SLIDER
204
242
351
275
agent-procreation-min-wealth
agent-procreation-min-wealth
0
20
10.0
1
1
NIL
HORIZONTAL

SLIDER
204
85
352
118
agent-init-endowment
agent-init-endowment
0
100
6.0
1
1
NIL
HORIZONTAL

SLIDER
204
278
351
311
mutation-rate
mutation-rate
0
1
0.0
0.01
1
NIL
HORIZONTAL

PLOT
8
323
351
489
Number of agents
time
num agents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"num_coop" 1.0 0 -10899396 true "" ""
"num_def" 1.0 0 -2674135 true "" ""

MONITOR
8
276
95
321
num_coop
count cooperators
0
1
11

MONITOR
97
276
186
321
num_def
count defectors
0
1
11

MONITOR
8
227
95
272
num_agents_ttl
count turtles
17
1
11

MONITOR
97
227
186
272
coop/def ratio
(count cooperators) / (count defectors)
2
1
11

SLIDER
204
48
351
81
agent-init-coop-ratio
agent-init-coop-ratio
0
1
0.5
0.01
1
NIL
HORIZONTAL

CHOOSER
993
10
1118
55
patch-background-color
patch-background-color
"black" "darkgrey" "grey" "white"
3

PLOT
8
493
351
659
Average wealth
time
avg wealth
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"avg_wealth_coop" 1.0 0 -10899396 true "" ""
"avg_wealth_def" 1.0 0 -2674135 true "" ""

PLOT
993
97
1261
269
Age Distribution
age group
count
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"age_distribution" 1.0 1 -13791810 true "" ""

PLOT
993
274
1261
449
Wealth Distribution
wealth group
count
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"wealth_distribution" 1.0 1 -955883 true "" ""

SWITCH
204
170
351
203
agent-init-random-birth-age
agent-init-random-birth-age
0
1
-1000

BUTTON
8
126
87
159
go steps
repeat simulation-steps [ go ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
8
163
87
223
simulation-steps
500.0
1
0
Number

SLIDER
993
59
1261
92
histogram-granularity
histogram-granularity
0
100
10.0
1
1
NIL
HORIZONTAL

PLOT
994
484
1262
659
Birth Death Diagram
time
num agents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"births_total" 1.0 0 -13791810 true "" ""
"deaths_total" 1.0 0 -16777216 true "" ""
"births_coop" 1.0 0 -14835848 true "" ""
"births_def" 1.0 0 -11221820 true "" ""
"deaths_coop" 1.0 0 -5825686 true "" ""
"deaths_def" 1.0 0 -2064490 true "" ""

@#$#@#$#@
## WHAT IS IT?

This is a replication of the Demographic Prisoners's Dilemma, which features a spatial iterated prisoner's dilemma with mortal aging agents. It was first described by Joshua M. Epstein in "Zones of Cooperation in Demographic Prisoner's Dilemma". A classic prisoner's dilemma probably is the most famous non-zero-sum game and still an important foundation for a variety of models in social science.

## HOW IT WORKS

The demographic prisoner's dilemma shows that cooperation is able to prevail in a spatial iterated prisoners dilemma, with zero-memory strategies (fixed strategy of cooperate or defect - assigned during birth) and without the possiblity to identify the type of the opponent (tag-less agents). This is achieved by the ability to procreate and the possibility to die during the game by incorporating negative payoff values (for mutual defection and the sucker's payoff), and a maximum age, though the latter is not essential for cooperation to survive.

Green fields indicate a patch occupied by a cooperator-turtle, red fields indicate a patch occupied by a defector-turtle. The darker the shade of the color, the more wealth the turtle has amassed. (Only one turtle may occupy a patch at a time.)
 

## HOW TO USE IT

The main parameters of the model, that can be changed by interface are:

agent-number (the initial number of agents on the grid)  
agent-init-coop-ration (the cooperator/defector ratio in initial population)  
agent-init-endowment (the inital wealth of the agents)  
agent-init-random-age (whether the initial population starts at age 0 or is given a random age)  
neighborhood (whether agents percieve and play in a von-Neumann [4 patches] or a Moore neighborhood [8 patches])  
agent-maxage (the age at which agents would die by old age; 0 results in no limitation of the lifespan)  
agent-procreation-minimum-wealth (the minimum wealth required by an agent to be able to procreate)  
mutation-rate (the chance that offspring has the opposite strategy than his parent)

The interaction payoff of the prisoner's dilemma can also be changed:

pd-r ("reward" payoff)  
pd-s ("sucker's" payoff)  
pd-t ("temptation" payoff)  
pd-p ("punishment" payoff)  
The following payoff structure is required for the game to be a prisoner's dilemma: pd-t > pd-r > pd-p > pd-s, furthermore it is required that pd-s < 0, pd-p < 0 so that agents may die from game-interactions, and pd-r > 0 and pd-t > 0, so that agents can thrive at all.

Finally there are three visual elements which affect the visual display and must only be changed before setup:

patch-background-color (changes the color of the background)  
histogram-granularity (defines in how many classes the displayed data is divided - changes the number of histogram bars)

## THINGS TO TRY

Epstein originally experimented with a finite vs. an infinite lifespan, mutation rates, as well as different (lower) values for pd-r. For low values of pd-r, cooperators have a hard time surviving - try larger world sizes.

## EXTENDING THE MODEL

The Model can easily be extended to use different (irregular) neighborhoods or deeper vision; or a metabolism. Other extensions might be varing the strict defection strategy with kinship selection.

## CREDITS AND REFERENCES

This replication was inspired by Wolfgang Radax and programmed by Bernhard Rengs - http://www.econ.tuwien.ac.at/rengs/netlogo/  
Original Model and Simulation by Joshua Epstein - described in "Zones of Cooperation in Demographic Prisoner's Dilemma", (1998) Complexity, Vol. 4, No. 2 
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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>count cooperators</metric>
    <metric>count defectors</metric>
    <enumeratedValueSet variable="agent-maxage">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pd-r">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="histogram-granularity">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pd-s">
      <value value="-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-procreation-min-wealth">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-init-coop-ratio">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-steps">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pd-t">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-number">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-init-random-birth-age">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patch-background-color">
      <value value="&quot;white&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent-init-endowment">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pd-p">
      <value value="-5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
