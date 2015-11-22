;; ----------- GLOBALS -----------
breed [persons person]
breed [companies company]
breed [matchings matching]

matchings-own [
  person-list
  company-list
]
persons-own [
  skills location salary
  activity company-type
  employed partner
  productivity
  satisfaction
]
companies-own [
  skills location salary
  activity company-type
  filled partner
]
globals [
  U V U1 V1
  convergence convergencemax epsilon
  MQT FQT UF MPF UCM UWM
  Npairs
  world-size
  salary-max
  plot-values
  plottime
  activities-number
  company-types-number
]

;; ----------- SETUP -----------

;; Setup function
to setup
  __clear-all-and-reset-ticks
  setup-globals
  setup-turtles
  setup-plot
end

;; Setup with arguments (used for plot-fig2)
to setup2 [tab U0 V0]
  __clear-all-and-reset-ticks
  setup2-globals U0 V0
  setup-turtles
  setup-plot
  set plot-values tab
end

;; Initialization of global variables
to setup-globals
  set U unemployment
  set U1 unemployment
  set V vacancy
  set V1 vacancy
  ;;-------
  set MQT matching_quality_treshold
  set FQT firing_quality_treshold
  set UF unexpected_firing
  set MPF max_productivity_fluctuation
  set UCM unexpected_company_motivation
  set UWM unexpected_worker_motivation
  set Npairs pairs_number
  ;;-------
  set world-size 20
  set salary-max 20000
  set company-types-number 5
  set activities-number 20
  ;;-------
  set convergence 0
  set convergenceMax 50
  set epsilon max (list ((unemployment + vacancy) * 3 / 100) 1)
  ;;-------
  set plottime 0
end

;; Initialization of global variables for plot-fig2
to setup2-globals [U0 V0]
  setup-globals
  set U U0
  set U1 U0
  set V V0
  set V1 V0
end

;; Initialization of agents
to setup-turtles
  create-persons U [
    set skills (list random 2 random 2 random 2 random 2 random 2)
    set location random world-size
    set salary random salary-max
    ;;-------
    set employed 0
    set partner 0
    set productivity random-float 1
    set activity random activities-number
    set company-type 1 + (random (company-types-number - 1))
    ;;-------
    set satisfaction random-float 1
  ]
  create-companies V [
    set skills (list random 2 random 2 random 2 random 2 random 2)
    set location random world-size
    set salary random salary-max
    set activity random activities-number
    set company-type random company-types-number
    ;;-------
    set filled 0
    set partner 0
  ]
  create-matchings 1 [
    set person-list []
    set company-list []
    foreach sort persons [set person-list lput ? person-list]
    foreach sort companies [set company-list lput ? company-list]
  ]
end

;; ----------- GO -----------

;; Plots the graph representing the final values of u and v with multiple initial values for U and V
to plot-fig2
  set plot-values  []
  
  let U0 100
  while [U0 < 500] [
    let V0 100
    while [V0 < 500] [
      setup2 plot-values U0 V0
      while [checkConvergence != 1] [ go ]
      set plot-values lput (list (U / U0) (V / U0)) plot-values
      
      set V0 V0 + 100
    ]
    set U0 U0 + 100
  ]
  
  set-current-plot "fig2"
  foreach plot-values [
    let x item 0 ?
    let y item 1 ?
    plotxy x y
  ] 
end

;; Repeated every tick
to go
  if(checkConvergence = 1) [ stop ]
  
  if(firing_type = "default") [ firePersons ]
  if(firing_type = "random") [ firePersonsGlobal ]
  if(firing_type = "decision") [ firePersonsDecision ]
  
  ifelse(matching_type = "default") [ doMatches ]
  [ doMatchesGlobal ]  

  update-plot
  tick
end

;; Checks if the unemployement rate is stable over the last ticks
to-report checkConvergence  
  ifelse ( abs(U - U1) < epsilon and abs(V - V1) < epsilon ) [ set convergence convergence + 1 ]
  [ set U1 U
    set V1 V
    set convergence 0
  ]
  
  ifelse(convergence = convergenceMax) [report 1] [report 0]
end

;; Processes all the firing for one tick
to firePersons
  ask persons [
    if (employed > 0) [
      let current-productivity productivity - MPF + 2 * random-float MPF
      if (current-productivity < FQT or random-float 1 < UF) [ fire self partner ]
    ]
  ]
end

to firePersonsGlobal
  let cpt (random (unemployment - U) )
  ask persons [
    if (employed > 0 and cpt > 0) [
      fire self partner
      set cpt cpt - 1
    ]
  ]
end

to firePersonsDecision
  ask persons [
    if (employed > 0) [
      set satisfaction (random-normal satisfaction satisfaction_deviation)
      if (satisfaction > 1) [set satisfaction 1]
      if (satisfaction < 0) [set satisfaction 0]
    
      if(satisfaction < satisfaction_treshold) [ fire self partner ]
    ]
  ]
end

;; Processes all the matches for one tick
to doMatches
  let cpt 0
  ask matchings [
    while [cpt < Npairs] [
      if (person-list != [] and company-list != []) [
        let a one-of person-list
        let b one-of company-list
        if (matching-quality a b >= MQT) [
          match a b
        ]
      ]
      set cpt cpt + 1
    ]
  ]
end

to doMatchesGlobal
  let cpt globalMatching
  ask matchings [
    while [cpt > 0] [
      if (person-list != [] and company-list != []) [
        let a one-of person-list
        let b one-of company-list
        match a b
      ]
      set cpt cpt - 1
    ]
  ]
end

to-report globalMatching 
  if (matching_type = "global_naive") [
    report V * (1 - exp( - U / V ) )
  ]
  if (matching_type = "global_L") [
    let L unemployment
    report V * (1 - exp( - U / ( L - U + V ) ) )
  ]
  if (matching_type = "global_K") [
    let K mismatch
    report V * (1 - exp( - K * U / V ) )
  ]
  if (matching_type = "global_s") [
    let s fraction
    report V * (1 - exp( - s * U / V ) )
  ]
end

;; Processes the match between a person and a company
to match [a b]
    ask a [
      set employed 1
      set partner b
    ]
    ask b [
      set filled 1
      set partner a
    ]
    ask matchings [
      set person-list remove a person-list
      set company-list remove b company-list
    ]
    set U U - 1
    set V V - 1
end

;; Processes the firing of a person relatively to a company
to fire [a b]
  ask a [ set employed 0 ]
  ask b [ set filled 0 ]
  ask matchings [
    set person-list lput a person-list
    set company-list lput b company-list
  ]
  set U U + 1
  set V V + 1
end

;; Returns a float between 0 and 1 which corresponds to the relevance of a matching between a person and a company
to-report matching-quality [a b]
  let res 0
  let a-skills 0
  let b-skills 0
  let a-location 0
  let b-location 0
  let a-salary 0
  let b-salary 0
  let a-company 0
  let b-company 0
  let a-activity 0
  let b-activity 0

  ask a [
    set a-skills skills
    set a-location location
    set a-salary salary
    set a-activity activity
    set a-company company-type
  ]
  ask b [
    set b-skills skills
    set b-location location
    set b-salary salary
    set b-activity activity
    set b-company company-type
  ]
  
  let i 0
  while [ i < 5 ] [
    if (item i a-skills = item i b-skills) [ set res res + 1 ]
    set i i + 1 
  ]
  set res res / 5 ;; skills

  if(a-location = b-location) [ set res res + 1 ] ;; location
  if(a-activity = b-activity) [ set res res + 1 ] ;; activity
  
  let temp max (list a-company b-company)
  set res res + (1 - abs(a-company - b-company) / temp ) ;; company type

  set temp max (list a-salary b-salary)
  set res res + (1 - abs(a-salary - b-salary) / temp ) ;; salary

  set res res / 5 ;;normalize
  
  ;; bonuses
  if (random-float 1 < UCM) [ set res res * 1.1 ]
  if (random-float 1 < UWM) [ set res res * 1.1 ]
  
  report res
end

;; ----------- GRAPHICS -----------

;; Setup function for plotting agents
to setup-plot
  ask persons [
    let loc -15 + location * 30 / world-size
    let xco -15 + random-float 10
    setxy xco loc
  ]
  ask companies [
    let loc -15 + location * 30 / world-size
    let xco 5 + random-float 10
    setxy xco loc
  ]
  ask matchings [
    set size 3
    set heading 0
    set color orange
  ]
  
  update-plot
  set-default-shape persons "person"
  set-default-shape companies "house"
  set-default-shape matchings "circle"
end

;; Plotting function for agents
to update-plot
  ask persons [
    ifelse employed > 0
      [ set color green ]
      [ set color blue ]
    set size 1.4
  ]
  ask companies [
    ifelse filled > 0
      [ set color green ]
      [ set color red ]
    set size 1.4
  ]
  ask matchings [
  ]
  
  drawPartners
  update-plot-chomage
end

to drawPartners
  clear-links
  ask persons [
    if(partner != 0) [ create-link-with partner ]
  ]
end

;; Plotting function for the unemployement graph
to update-plot-chomage
  set-current-plot "unemployment"
  set plottime (plottime + 1)
  
  plotxy plottime U
end
@#$#@#$#@
GRAPHICS-WINDOW
90
10
483
424
16
16
11.61
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
720
61
892
94
unemployment
unemployment
10
500
190
10
1
NIL
HORIZONTAL

SLIDER
720
92
892
125
vacancy
vacancy
10
500
500
10
1
NIL
HORIZONTAL

SLIDER
722
217
1014
250
matching_quality_treshold
matching_quality_treshold
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
722
256
1014
289
firing_quality_treshold
firing_quality_treshold
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
723
292
1015
325
unexpected_firing
unexpected_firing
0
0.2
0.1
0.005
1
NIL
HORIZONTAL

SLIDER
723
329
1014
362
max_productivity_fluctuation
max_productivity_fluctuation
0
1
0.4
0.01
1
NIL
HORIZONTAL

SLIDER
720
132
1015
165
unexpected_company_motivation
unexpected_company_motivation
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
720
166
1015
199
unexpected_worker_motivation
unexpected_worker_motivation
0
1
0.31
0.01
1
NIL
HORIZONTAL

SLIDER
895
61
1017
94
pairs_number
pairs_number
0
100
40
1
1
NIL
HORIZONTAL

BUTTON
11
11
84
44
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
10
50
84
83
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

BUTTON
1
530
91
563
NIL
plot-fig2
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
115
440
483
649
fig2
u
v
0.0
1.0
0.0
4.0
true
true
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
496
10
696
160
unemployment
ticks
U
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

TEXTBOX
788
24
938
42
Simulation parameters
13
0.0
1

CHOOSER
722
401
834
446
matching_type
matching_type
"default" "global_naive" "global_K" "global_L" "global_s"
4

SLIDER
838
386
1010
419
mismatch
mismatch
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
838
420
1010
453
fraction
fraction
0
1
0.6
0.1
1
NIL
HORIZONTAL

CHOOSER
721
478
835
523
firing_type
firing_type
"default" "random" "decision"
2

SLIDER
836
468
1012
501
satisfaction_deviation
satisfaction_deviation
0
0.5
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
836
502
1012
535
satisfaction_treshold
satisfaction_treshold
0
0.6
0.2
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

doge
true
0
Rectangle -2674135 true false 75 60 105 75
Rectangle -2674135 true false 60 75 75 150
Rectangle -2674135 true false 45 150 60 165
Rectangle -2674135 true false 45 165 60 180
Rectangle -2674135 true false 30 180 45 210
Rectangle -2674135 true false 15 165 30 180
Rectangle -2674135 true false 0 150 15 165
Rectangle -2674135 true false 105 75 165 90
Rectangle -2674135 true false 165 60 180 75
Rectangle -2674135 true false 180 75 195 105
Rectangle -2674135 true false 195 105 210 120
Rectangle -2674135 true false 210 120 225 150
Rectangle -2674135 true false 225 150 240 225
Rectangle -2674135 true false 210 225 225 240
Rectangle -2674135 true false 60 240 210 255
Rectangle -2674135 true false 60 225 75 240
Rectangle -2674135 true false 15 225 60 240
Rectangle -2674135 true false 45 210 60 225
Rectangle -955883 true false 45 180 60 195
Rectangle -955883 true false 75 150 90 210
Rectangle -955883 true false 60 150 75 210
Rectangle -955883 true false 75 120 135 150
Rectangle -955883 true false 90 90 180 120
Rectangle -955883 true false 150 120 180 135
Rectangle -955883 true false 135 120 150 135
Rectangle -955883 true false 165 135 210 150
Rectangle -955883 true false 180 105 195 120
Rectangle -955883 true false 90 150 225 165
Rectangle -955883 true false 120 165 180 180
Rectangle -1184463 true false 75 75 90 120
Rectangle -1184463 true false 90 75 105 90
Rectangle -1184463 true false 165 75 180 90
Rectangle -1184463 true false 90 165 120 195
Rectangle -1184463 true false 120 180 195 195
Rectangle -1184463 true false 105 195 135 210
Rectangle -1184463 true false 120 210 210 225
Rectangle -1184463 true false 195 195 210 210
Rectangle -1184463 true false 210 165 225 210
Rectangle -7500403 true true 45 195 60 210
Rectangle -7500403 true true 60 210 120 225
Rectangle -7500403 true true 90 195 105 210
Rectangle -7500403 true true 75 225 210 240
Rectangle -7500403 true true 210 210 225 225
Rectangle -1 true false 150 135 165 150
Rectangle -1 true false 195 120 210 135
Rectangle -16777216 true false 135 195 195 210
Rectangle -16777216 true false 135 135 150 150
Rectangle -16777216 true false 180 120 195 135
Rectangle -16777216 true false 180 165 210 165
Rectangle -16777216 true false 195 180 210 195

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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
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
