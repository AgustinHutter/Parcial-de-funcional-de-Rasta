module Library where
import PdePreludat


data Posta = Posta{ alto:: Number, largo::Number, nivel::String, pais::String} deriving(Show)
type Atleta= Posta -> Vistas
type Vistas = Number
type Chiste=(String,Number)

elite::Atleta
elite posta= alto posta + largo posta

carismatico:: [Chiste]->Atleta
carismatico chistes = (carismaticoSegunDificultad chistes) . nivel

carismaticoSegunDificultad:: [Chiste]->String->Vistas               
carismaticoSegunDificultad chistes "Dificil" = avg (map snd chistes)
carismaticoSegunDificultad (chiste:_) _ = snd chiste

fiel:: Vistas->Atleta
fiel fans _ = fans

resultado:: Atleta-> Posta ->Vistas
resultado atleta = atleta

postaBase :: Posta
postaBase = Posta 12 15 "Dificil" "Argentina"

-- resultado elite postaBase 
-- resultado (arismatico [("Habia una veztruz",50),("toc toc",15)]) postaBase
-- resultado (fieles 500) postaBase

        [elite , 
        carismatico [("Habia una veztruz",50),("toc toc",15)] , 
        fiel 500 ]

------------------------------------
conDificultad posta = posta {nivel = "Dificil"}
type Potenciador:: Atleta -> Atleta

patasDeRana :: Potenciador
patasDeRana atleta = atleta.conDificultad

outfit:: Number->Potenciador
outfit calidad atleta  = (*calidad).atleta 

-- resultado ((outfit 2).elite) postaBase 
-- resultado ((outfit 2).(arismatico [("Habia una veztruz",50),("toc toc",15)])) postaBase
-- resultado ((outfit 2).(fieles 500)) postaBase

tienePataDeRana:: Atleta->Bool
tienePataDeRana = (== "Dificil").nivel

filtroPataDeRana:: [Atleta]->[Atleta]
filterPataDeRana = filter tienePataDeRana

-------------------------------------
type Evento :: [Posta]
base:: Posta
base= Posta 0 0 "Facil" "Argentina"
conAlto:: Number->Posta->Posta
conAlto alto posta = posta{alto=alto}
conLargo::Number->Posta->Posta
conLargo largo posta = posta{largo=largo}

evento = [conLargo 100 base, (conLargo 10).(conAlto 200).conDificultad base, base]

supera:: Atleta->Posta->Bool
MejorSegun:: (a->Number) -> [a]->a

superaEvento:: Atleta->Evento->Bool
superaEvento atleta = all (supera atleta)

quienesSuperanEvento:: [Atleta]->Evento->[Atleta]
quienesSuperanEvento atletas evento = filter (flip.superaEvento evento) atletas

puntaje :: Atleta -> Evento -> Number
puntaje atleta = sum.map (resultado atleta)

MejorSegun puntaje (quienesSuperanEvento atletas evento)

cambiarPais::Posta->String->Posta
cambiarPais posta nuevoPais=posta{pais=nuevoPais}

hinchada::String->Potenciador
hinchada pais atleta = atleta.(nuevoPais pais) 
