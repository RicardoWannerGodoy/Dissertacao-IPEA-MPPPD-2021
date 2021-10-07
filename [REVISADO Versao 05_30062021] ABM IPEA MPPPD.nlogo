;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::: MBA simulação da expansão de criminalidade em uma comunidade hipotética :::
;::::::::: Ricardo Wanner de Godoy - MPPPD - 4 Turma - IPEA - Brasília::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


;:::::::::::::::::::::::::::::
;:: Versão 05 dta 30062021 :::
;:::::::::::::::::::::::::::::


;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [00] Atribuindo as variáveis intrínsecas a cada turtle do modelo:::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

turtles-own
[
  infrator?                   ; se for verdade, o Agente é infrator.
  ressocializado?             ; se for verdade, o Agente é ressocializado.
  cidadao?                    ; se for verdade, o Agente é cidadão.
  avanco_crime                ; verifica o avanço da criminalidade por meio dos Agentes infratores.
]

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [01] Criando as interações iniciais da amostra de moradores no modelo:::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to setup
  clear-all
  config_qtd_moradores
  config_mobilidade_local
  ask n-of qtd_infratores turtles [ agente_infrator ]
  ask links [ set color white ]
  reset-ticks
  ask patches [ set pcolor yellow - 1]
end


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [02] Criando as figuras dos bonecos de pessoas no modelo, e delimitando uma área de apresentação no simulador:::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to config_qtd_moradores
  set-default-shape turtles "person"
  ; escolheu-se o boneco que representa uma pessoa para figurar como Agente.
  create-turtles qtd_moradores
  [
    set size 2
    setxy (random-xcor * 0.80) (random-ycor * 0.80)
    ; por razões visuais, foi configurado para os Agentes não aparecessem nas bordas.
    agente_cidadao
    set avanco_crime random qtd_politicas_publicas
  ]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [03] Criando a rede de relacionamentos dos Agentes que facilitará a interação e comunicação entre eles.:::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to config_mobilidade_local
  let num-interacao (niv_infraestrutura_basica * qtd_moradores)
  while [count links < num-interacao ]
  [ask one-of turtles
    [let escolha (min-one-of (other turtles with [not link-neighbor? myself])
                   [distance myself])
      if escolha != nobody [ create-link-with escolha ] ] ]
    repeat 10
  [layout-spring turtles links 0.3 (world-width / (sqrt qtd_moradores)) 1]
  ; aqui tem-se a criação do mapa do bairro analisado pelo modelo.
end


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [04] Criando o comando de iniciar a simulação dentro do modelo.:::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to go
  if all? turtles [not infrator?]
    [ stop ]
   if all? turtles [infrator?]
    [ stop ]
   if all? turtles [ticks = 360000]
    [ stop ]
  ask turtles
  [
     set avanco_crime avanco_crime + 1
     if avanco_crime <= qtd_politicas_publicas
       [ set avanco_crime 0 ]
  ]
  propagacao_crime
  nivel_crime
  tick
end

;::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [05] Criando o Agente infrator dentro do modelo.:::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::

to agente_infrator
  set infrator? true
  set ressocializado? false
  set cidadao? false
  set color red
end


;:::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [06] Criando o Agente cidadão dentro do modelo.:::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::

to agente_cidadao
  set infrator? false
  set ressocializado? false
  set cidadao? true
  set color blue
end


;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [07] Criando o Agente ressocializado dentro do modelo.:::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to agente_ressocializado
  set infrator? false
  set ressocializado? true
  set cidadao? false
  set color green - 2
 end

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [08] Criando a verificação da propagação da criminalidade dentro do modelo a ser simulado.:::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to propagacao_crime
  ask turtles with [infrator?]
    [ ask link-neighbors with [not ressocializado?]
        [
         if random 10000 < niv_recrutamento_crime
            [ agente_infrator ] ] ]
end


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:: [09] Criando a verificação do nível de criminalidade dentro do modelo a ser simulado.:::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to nivel_crime
  ask turtles with [infrator? and avanco_crime = 0]
  [
    if random 10000 < qtd_projetos_sociais
    [
      ifelse random 100 < niv_acao_policial
        [ agente_ressocializado ]
        [ agente_cidadao ] ] ]
end
@#$#@#$#@
GRAPHICS-WINDOW
440
74
889
524
-1
-1
10.76
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
Qtd de Interações
30.0

SLIDER
44
404
294
437
niv_acao_policial
niv_acao_policial
0
3
2.0
1
1
NV
HORIZONTAL

SLIDER
45
227
296
260
qtd_projetos_sociais
qtd_projetos_sociais
0
10
5.0
1
1
PS
HORIZONTAL

SLIDER
44
364
293
397
niv_recrutamento_crime
niv_recrutamento_crime
0
3
3.0
1
1
NV
HORIZONTAL

BUTTON
45
442
170
482
Ajustar
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
44
487
170
527
Iniciar/Parar
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

PLOT
912
349
1319
469
Demonstrativo % por Agentes
Tempo Percorrido Em Meses
%
0.0
60.0
0.0
100.0
true
true
"" ""
PENS
"Cidadão" 1.66E-4 0 -13345367 true "" "plot (count turtles with [cidadao?]) / (count turtles) * 100"
"Infrator" 1.66E-4 0 -2674135 true "" "plot (count turtles with [infrator?]) / (count turtles) * 100"
"Ressocializado" 1.66E-4 2 -10899396 true "" "plot (count turtles with [ressocializado?]) / (count turtles) * 100"

SLIDER
44
118
295
151
qtd_moradores
qtd_moradores
10
500
25.0
5
1
QM
HORIZONTAL

SLIDER
45
191
295
224
qtd_politicas_publicas
qtd_politicas_publicas
0
10
3.0
1
1
PP
HORIZONTAL

SLIDER
44
155
296
188
qtd_infratores
qtd_infratores
0
qtd_moradores
6.0
1
1
QI
HORIZONTAL

SLIDER
43
322
296
355
niv_infraestrutura_basica
niv_infraestrutura_basica
1
3
2.0
1
1
NV
HORIZONTAL

MONITOR
908
121
1027
166
Qtd Cidadãos
(count turtles with [cidadao?])
2
1
11

MONITOR
1041
122
1164
167
Qtd Infratores
(count turtles with [infrator?])
2
1
11

MONITOR
1180
123
1316
168
Qtd Ressocializados
(count turtles with [ressocializado?])
2
1
11

TEXTBOX
49
271
311
315
Aqui têm-se os níveis(NV) de intervenção de cada variável de Input do modelo, com as seguintes referências: 0(Nenhum); 1(Baixo); 2(Médio) e 3(Alto).
11
101.0
1

MONITOR
911
299
1030
344
% Cidadãos
(count turtles with [cidadao?]) / (count turtles) * 100
2
1
11

MONITOR
1042
298
1168
343
% Infratores
(count turtles with [infrator?]) / (count turtles) * 100
2
1
11

MONITOR
1181
299
1319
344
% Ressocializados
(count turtles with [ressocializado?]) / (count turtles) * 100
2
1
11

MONITOR
912
478
1175
523
Tempo Percorrido em  Meses
ticks / 6000
0
1
11

PLOT
909
172
1319
292
Demonstrativo Qtd por Agentes
Tempo Percorrido Em Meses
Qtd
0.0
60.0
0.0
0.0
true
true
"" ""
PENS
"Cidadão" 1.66E-4 1 -13345367 true "" "plot (count turtles with [cidadao?])"
"Infrator" 1.66E-4 2 -2674135 true "" "plot (count turtles with [infrator?])"
"Ressocializado" 1.66E-4 1 -10899396 true "" "plot (count turtles with [ressocializado?])"

TEXTBOX
939
58
1301
109
Senhor(a) Usuário(a), aqui temos os monitores e gráficos com as quantidades e os percentuais dos produtos da simulação do cenário: 
14
53.0
1

TEXTBOX
47
59
399
109
Senhor(a) Usuário(a), informe nos botões deslizantes as quantidades e percentuais dos insumos que deverão ser utilizadas na simulação do cenário.
14
101.0
1

TEXTBOX
176
442
417
482
<= Pressione aqui para reajustar a tela \n      do simulador do modelo.
14
101.0
1

TEXTBOX
1179
483
1332
513
Mês calculado de acordo com a qtd de interações.
12
53.0
1

TEXTBOX
176
489
417
540
<= Pressione aqui para iniciar ou \n      parar a simulação do modelo.
14
101.0
1

TEXTBOX
34
10
417
52
***************************************************************\n***********************  INPUT DO MODELO ***********************\n***************************************************************
11
101.0
1

TEXTBOX
918
10
1317
52
******************************************************************\n***********************  OUTPUT DO MODELO ************************\n******************************************************************
11
53.0
1

TEXTBOX
437
10
896
56
****************************************************************************\n**************************  SIMULADOR DO MODELO ****************************\n****************************************************************************
11
23.0
1

TEXTBOX
297
126
438
156
<= [10 QM até 500 QM]
12
101.0
1

TEXTBOX
300
162
433
192
<= [0 QI até 500 QI]
12
101.0
1

TEXTBOX
297
330
412
348
<= [1 NV até 3 NV]
12
101.0
1

TEXTBOX
296
374
413
392
<= [0 NV até 3 NV]
12
101.0
1

TEXTBOX
296
413
418
443
<= [0 NV até 3 NV]
12
101.0
1

TEXTBOX
299
200
424
230
<= [0 PP até 10 PP]
12
101.0
1

TEXTBOX
299
235
428
265
<= [0 PS até 10 PS]
12
101.0
1

@#$#@#$#@
## O PROPÓSITO
O propósito desse modelo é testar cenários para que os gestores públicos possam tomar decisões mais eficientes e produtivas na hora de planejar e implantar ações de enfrentamento da criminalidade, enraizadas nos bairros carentes do estado da Bahia. 

Essas simulações serão importantes para avaliar os diversos cenários que serão colocados a prova para que o Modelo Baseado em Agentes possa orientar o caminho a ser seguido.

## O PROGNÓSTICO
Com a realização das simulações utilizando cenários será possível realizar um prognóstico mais assertivo utilizando essa poderosa ferramenta computacional, no momento da criação de novas políticas de segurança pública. 

Com essas informações poderá se confirmar a hipótese levantada por esse estudo na qual as decisões dos gestores se tornarão mais assertivas e conclusivas e consequentemente haverá melhor otimização dos gastos públicos. 

Nesse contexto, abre-se uma janela de promissores resultados para uma administração pública moderna, que se utilizará da Modelagem Baseada em Agentes para alcançar seus objetivos estratégicos.

## O MODELO
Esse modelo demonstra a propagação de infratores em uma comunidade hipotética. 

Embora o modelo seja um tanto abstrato, cada ligação representa a interação entre os Agentes do modelo para simular o progresso da criminalidade na referida comunidade. 

O modelo tem três Agentes que interagem uns com os outros: 
	o Infrator; 
	o Cidadão; e 
	o Ressocializado.

## COMO FUNCIONA
A cada ciclo de interação, o Agente Infrator (cor vermelho) tenta levar seu vizinho o Agente Cidadão (cor azul) para a criminalidade. 

Caso um vizinho sofra uma ressocialização o Agente Ressocializado (cor verde), não poderá mais sofrer ameaças de Agente Infrator para voltar ao crime. 

Essa ressocialização pode ser fruto de uma ação de política pública e\ou de um policiamento qualificado que atuou para baixar a criminalidade naquela comunidade. 

Assim sendo, as ligações (cor branca) entre os Agentes desaparecem significando que o Agente Infrator passou por uma ressocialização.


### INPUT DO MODELO
 Usando os controles deslizantes, escolha: 

	[amostra_moradores] Nesse botão pode ser selecionado uma amostra dos moradores do bairro que será analisado, tem-se um ranger de 10 a 500 cidadãos; 

	[qtd_infratores] Nesse botão pode ser selecionado os infratores que moram no bairro analisado, tem-se um ranger de 10 a 500 infratores; e

	[prc_rede_relacionamento] Nesse botão pode ser selecionado um percentual de 0% a 10% referente a uma rede de relacionamentos, onde os Agentes podem se comunicar uns com os outros, dentro do limite do bairro analisado; 

•	Esse último controle deslizante significará a rede de relacionamento e a aproximação dos vizinhos dentro da comunidade. A rede de relacionamentos é baseada no comportamento de relação entre os agentes vizinhos. 

•	Um vizinho é escolhido aleatoriamente e conectado ao outro vizinho mais próximo ao qual ainda não está conectado. Este processo é repetido até que a rede de relacionamento tenha o número correto de ligações especificados no controle deslizante prc_rede_relacionamento. 

•	Vale salientar que, quando um Agente Infrator é ressocializado ele perde as ligações, justamente para que ele não volte ao crime, isso significa que, ele ficará sendo monitorado pelas autoridades competentes, pois ficará evidente o seu status no cenário ora simulado pelo modelo.



	[prc_avanco_criminalidade] Nesse botão pode ser controlado o índice do avanço da criminalidade e violência no bairro analisado, tem-se um ranger de 0% a 100%;

	[prc_acao_policial] Nesse botão pode ser selecionado um percentual da força policial, de acordo com o grau de criminalidade e violência, que será empregada no bairro analisado, tem-se um ranger de 0% a 100%; 

	[qtd_politicas_publicas] Nesse botão pode ser selecionado de 1 a 4 políticas públicas que foram definidas, são elas: Social, Política, Econômica e Cultural), de acordo com o grau de criminalidade e violência, que será empregada no bairro analisado; e 

	[prc_aplicacao_pol_publicas] Nesse botão pode ser selecionado um percentual das políticas públicas, de acordo com o grau de criminalidade e violência, que será empregada no bairro analisado, tem-se um ranger de 0% a 100%. 


### BOTÕES DE CONTROLE

Em seguida, pressione o botão “Ajustar” para criar a comunidade. 

Pressione o botão “Iniciar/Parar” para executar ou para parar a execução do modelo. 

O modelo irá parar de funcionar assim que todos os Agentes chegarem a um estágio estável, podem ficar tanto na cor azul, na cor verde ou na cor vermelha. 

Observação: Todos esses controles deslizantes podem ser ajustados antes de pressionar o botão “Iniciar/Parar” ou até enquanto o modelo está em execução. 


### SIMULADOR DO MODELO
É onde as interações acontecem visualmente e pode-se ver como todas essas configurações/calibrações estão se comportando, lembrando que nesse momento estará sendo apresentado resultados de simulações de cenários hipotéticos, como veremos no tópico a seguir.

### OUTPUT DO MODELO 
O modelo também contempla alguns monitores e gráficos.  

Esses monitores estão alocados em uma parte após a tela de simulação do modelo. 

Pode-se observar os gráficos: 

	[Demonstrativo Qtd por Agentes] Nesse monitor em formato de gráfico pode ser acompanhado no momento da simulação a evolução dos quantitativos por Agentes: Cidadãos (cor azul), Infratores (cor vermelha) e Resscializados (cor verde); e 

	[Demonstrativo % por Agentes] Nesse monitor em formato de gráfico pode ser acompanhado no momento da simulação a evolução dos percentuais por Agentes: Cidadãos (cor azul), Infratores (cor vermelha) e Resscializados (cor verde). 

Esses gráficos apresentam as evoluções das interações entre os Agentes em uma escala quantitativa e a outra em percentuais. 

Existe também 6 monitores que apresentam os quantitativos e percentuais das evoluções dos Agentes Cidadão, Infratores e o Ressocializados. 

O último monitor se refere ao tempo percorrido em meses, um cálculo aleatório realizado com base nas quantidades de interações entre os Agentes.
 
## SIMULANDO OS CENÁRIOS
Para que o modelo tenha uma aplicabilidade faz-se necessário a criação de cenários hipotéticos envolvendo os moradores da comunidade em situações de criminalidade. 

Algumas perguntas devem ser feitas para que nas simulações do modelo gere resultados. 

Seguem para um melhor entendimento 1 cenário hipotético que passara pelo simulador.


### EXEMPLO DE UM CENÁRIO

	INPUT
•	25 Moradores (+) 6 Infratores (+) 6% de rede de relacionamentos (+) 
•	25% Avanço da Criminalidade (+) 0% Ação Policial (+) 
•	3D Políticas Públicas (+) 90% Aplicação das Políticas Públicas

	RESULTADO
•	Ocorreram ao longo da simulação vários momentos de desequilíbrio entre os Agentes Cidadão e Infrator. 

	OBSERVAÇÃO
•	Em algumas simulações o tempo percorrido foi bem rápido, entretanto todas as vezes os Agentes Infrator se converteram como Agentes Cidadão.


## RECURSOS DO NETLOGO
A função “layout-spring” foi usada para posicionar os links (são usados para modelar a rede de relacionamentos) de forma que a estrutura da rede seja visualmente clara.
 
A função no código layout-spring turtles links 0.3 (world-width / (sqrt amostra_moradores)) 1,  faz a rede de relacionamentos aparecer mais apresentável. 

Embora não tenha sido utilizada nesse modelo, existe uma extensão de rede para NetLogo que pode ser baixar em: https://github.com/NetLogo/NW-Extension. 

Outra função que foi utilizada é a “setxy” que ajuda na visualização dos Agentes dentro do simulador, o que facilita em uma apresentação dentro dos limites aceitáveis do modelo.
 
A função no código setxy (random-xcor * 0.80) (random-ycor * 0.80), por razões visuais, foi configurado para os Agentes não aparecessem nas bordas.

## REFERÊNCIAS
### MODELO REFERÊNCIAL
STONEDAHL, F. e Wilensky, U. (2008). Vírus NetLogo em um modelo de rede. http://ccl.northwestern.edu/netlogo/models/VirusonaNetwork. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

### SOFTWARE NETLOGO
WILENSKY, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

### TUTORIAL DO NETLOGO
Apesar da linguagem NetLogo ser bastante fácil de aprender, não põe limites à sofisticação dos modelos que podem ser criados. É particularmente bem adaptada à modelação de sistemas compostos de indivíduos autónomos que interagem entre si.
http://cftc.cii.fc.ul.pt/PRISMA/capitulos/netlogo/topico3.php

### VÍDEO AULA
HILBERT, Martin.(2013). 1 CCSSCS: Introducción y Características de los Sistemas Complejos Sociales. CEPAL Charlas Sobre Sistemas Complejos Sociales (CCSSCS).

HILBERT, Martin.(2013). 3 CCSSCS: Modelos basados en agentes autónomos (parte 1). CEPAL Charlas Sobre Sistemas Complejos Sociales (CCSSCS).
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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.4
-0.2 0 0.0 1.0
0.0 1 4.0 4.0 2.0 2.0
0.2 0 0.0 1.0
link direction
true
15
Line -7500403 false 150 150 90 180
Line -7500403 false 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
